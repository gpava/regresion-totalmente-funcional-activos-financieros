# ==============================================================================
# GARCH(p,q) PARA LA TESIS: VOLUMEN -> RENDIMIENTO [VERSIÓN S4-SEGURA]
# Extensión replicable de GARCH.R y GARCH2.R
#
# REQUISITO:
# Ejecutar previamente FDA_tesis_Version_Final.R hasta disponer de:
#   df_rendimientos_final, df_volumen_log, vol_cols, etiquetas_empresas
#
# Convención matemática usada en la tesis:
#   GARCH(p,q):
#     p = número de rezagos de la varianza condicional sigma^2
#     q = número de rezagos ARCH de epsilon^2
#
# IMPORTANTE en rugarch:
#   garchOrder = c(q, p), es decir, primero ARCH(q) y luego GARCH(p).
# ==============================================================================

library(rugarch)
library(dplyr)
library(tidyr)
library(purrr)

# Semilla para reproducibilidad en caso de que el solver híbrido recurra a métodos estocásticos.
set.seed(123)

# ------------------------------------------------------------------------------
# 1. Estandarización del volumen logarítmico
# ------------------------------------------------------------------------------

df_volumen_log_estandarizado <- df_volumen_log %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(vol_cols),
      ~ as.numeric(scale(.x))
    )
  )

# Homogeneizar nombres de rendimientos, conservando la escala ORIGINAL
# de los rendimientos logarítmicos (no multiplicar por 100 en esta versión).
names(df_rendimientos_final) <- sub(
  "^R_Cierre_",
  "R_",
  names(df_rendimientos_final)
)

empresas_garch <- sub(
  "^R_",
  "",
  grep("^R_", names(df_rendimientos_final), value = TRUE)
)

# ------------------------------------------------------------------------------
# 2. Familia candidata GARCH(p,q)
# ------------------------------------------------------------------------------

candidatos_pq <- data.frame(
  p = c(1, 1, 2, 2),
  q = c(1, 2, 1, 2)
)

# ------------------------------------------------------------------------------
# 3. Construcción de la base por empresa ALINEANDO POR FECHA
# ------------------------------------------------------------------------------

preparar_datos_garch <- function(
    empresa,
    df_rendimientos = df_rendimientos_final,
    df_volumen = df_volumen_log_estandarizado
) {
  columna_rend <- paste0("R_", empresa)
  columna_vol  <- paste0("Vol_", empresa)

  if (!columna_rend %in% names(df_rendimientos)) {
    stop("No existe la columna de rendimiento: ", columna_rend)
  }
  if (!columna_vol %in% names(df_volumen)) {
    stop("No existe la columna de volumen: ", columna_vol)
  }

  datos_r <- df_rendimientos %>%
    dplyr::transmute(
      Fecha = as.Date(Fecha),
      Rendimiento = .data[[columna_rend]]
    )

  datos_v <- df_volumen %>%
    dplyr::transmute(
      Fecha = as.Date(Fecha),
      Volumen_log_z = .data[[columna_vol]]
    )

  datos <- dplyr::inner_join(datos_r, datos_v, by = "Fecha") %>%
    dplyr::arrange(Fecha)

  if (anyNA(datos)) {
    stop("La base alineada de ", empresa, " contiene valores faltantes.")
  }

  if (anyDuplicated(datos$Fecha)) {
    stop("Hay fechas duplicadas en la base alineada de ", empresa, ".")
  }

  datos
}

# Verificación explícita del tamaño muestral por empresa.
tabla_malla_garch <- dplyr::bind_rows(
  lapply(empresas_garch, function(emp) {
    dat <- preparar_datos_garch(emp)
    data.frame(
      Empresa = emp,
      N = nrow(dat),
      Fecha_inicial = min(dat$Fecha),
      Fecha_final = max(dat$Fecha),
      row.names = NULL
    )
  })
)

print(tabla_malla_garch)

# ------------------------------------------------------------------------------
# 4. Ajuste de un modelo sGARCH(p,q)
# ------------------------------------------------------------------------------

ajustar_un_garch <- function(
    empresa,
    p,
    q,
    ar = 0,
    incluir_volumen = TRUE,
    lag_lb = 10
) {
  datos <- preparar_datos_garch(empresa)

  regresor <- NULL
  if (incluir_volumen) {
    regresor <- matrix(datos$Volumen_log_z, ncol = 1)
    colnames(regresor) <- "Volumen_log_z"
  }

  spec <- rugarch::ugarchspec(
    variance.model = list(
      model = "sGARCH",
      # rugarch usa c(ARCH(q), GARCH(p))
      garchOrder = c(q, p)
    ),
    mean.model = list(
      armaOrder = c(ar, 0),
      include.mean = TRUE,
      external.regressors = regresor
    ),
    distribution.model = "std"
  )

  fit <- tryCatch(
    rugarch::ugarchfit(
      spec = spec,
      data = datos$Rendimiento,
      solver = "hybrid",
      fit.control = list(stationarity = 1)
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(list(
      empresa = empresa,
      p = p,
      q = q,
      ar = ar,
      incluir_volumen = incluir_volumen,
      datos = datos,
      ajuste = NULL,
      error = conditionMessage(fit),
      lag_lb = lag_lb
    ))
  }

  list(
    empresa = empresa,
    p = p,
    q = q,
    ar = ar,
    incluir_volumen = incluir_volumen,
    datos = datos,
    ajuste = fit,
    error = NA_character_,
    lag_lb = lag_lb
  )
}

# ------------------------------------------------------------------------------
# 5. Extracción homogénea de resultados y diagnósticos
# ------------------------------------------------------------------------------

extraer_resumen_garch <- function(obj) {
  if (is.null(obj$ajuste)) {
    return(data.frame(
      Empresa = obj$empresa,
      p = obj$p,
      q = obj$q,
      AR = obj$ar,
      N = nrow(obj$datos),
      Mu = NA_real_,
      Delta_Volumen = NA_real_,
      P_Volumen_Robusto = NA_real_,
      Omega = NA_real_,
      Persistencia = NA_real_,
      Shape_t = NA_real_,
      LogLik = NA_real_,
      AIC = NA_real_,
      BIC = NA_real_,
      P_LB_Residuos = NA_real_,
      P_LB_Cuadrados = NA_real_,
      Convergencia = NA_integer_,
      Error = obj$error,
      row.names = NULL
    ))
  }

  fit <- obj$ajuste

  # IMPORTANTE: no se usa coef(), fitted(), residuals(), likelihood(),
  # infocriteria() ni ningún otro extractor genérico sobre uGARCHfit.
  # Se lee el slot S4 directamente para evitar problemas de despacho.
  fit_slot <- methods::slot(fit, "fit")

  cf <- fit_slot[["coef"]]
  robustos <- fit_slot[["robust.matcoef"]]
  residuos <- as.numeric(fit_slot[["residuals"]])
  sigma <- as.numeric(fit_slot[["sigma"]])
  llh <- as.numeric(fit_slot[["LLH"]])
  ipars <- fit_slot[["ipars"]]
  convergencia <- as.integer(fit_slot[["convergence"]])

  if (is.null(cf) || length(cf) == 0) {
    stop("No fue posible extraer coeficientes del slot fit para ", obj$empresa)
  }

  alpha_names <- grep("^alpha[0-9]+$", names(cf), value = TRUE)
  beta_names  <- grep("^beta[0-9]+$", names(cf), value = TRUE)

  persistencia <- sum(
    c(cf[alpha_names], cf[beta_names]),
    na.rm = TRUE
  )

  z <- residuos / sigma
  z <- z[is.finite(z)]

  lb_res <- stats::Box.test(
    z,
    lag = obj$lag_lb,
    type = "Ljung-Box",
    fitdf = obj$ar
  )

  lb_sq <- stats::Box.test(
    z^2,
    lag = obj$lag_lb,
    type = "Ljung-Box",
    fitdf = 0
  )

  obtener <- function(nombre) {
    if (nombre %in% names(cf)) as.numeric(cf[nombre]) else NA_real_
  }

  p_robusto <- function(nombre) {
    if (
      !is.null(robustos) &&
      nombre %in% rownames(robustos) &&
      ncol(robustos) >= 4
    ) {
      as.numeric(robustos[nombre, 4])
    } else {
      NA_real_
    }
  }

  # Criterios de información calculados manualmente con las mismas
  # fórmulas normalizadas por observación utilizadas por rugarch.
  n_obs <- length(residuos)
  n_par <- if (!is.null(ipars) && ncol(ipars) >= 4) {
    sum(ipars[, 4], na.rm = TRUE)
  } else {
    length(cf)
  }

  aic <- (-2 * llh) / n_obs + (2 * n_par) / n_obs
  bic <- (-2 * llh) / n_obs + (n_par * log(n_obs)) / n_obs

  data.frame(
    Empresa = obj$empresa,
    p = obj$p,
    q = obj$q,
    AR = obj$ar,
    N = nrow(obj$datos),
    Mu = obtener("mu"),
    Delta_Volumen = obtener("mxreg1"),
    P_Volumen_Robusto = p_robusto("mxreg1"),
    Omega = obtener("omega"),
    Persistencia = persistencia,
    Shape_t = obtener("shape"),
    LogLik = llh,
    AIC = aic,
    BIC = bic,
    P_LB_Residuos = as.numeric(lb_res$p.value),
    P_LB_Cuadrados = as.numeric(lb_sq$p.value),
    Convergencia = convergencia,
    Error = NA_character_,
    row.names = NULL
  )
}

extraer_parametros_varianza <- function(obj) {
  if (is.null(obj$ajuste)) return(NULL)

  fit_slot <- methods::slot(obj$ajuste, "fit")
  cf <- fit_slot[["coef"]]

  nombres <- c(
    "omega",
    grep("^alpha[0-9]+$", names(cf), value = TRUE),
    grep("^beta[0-9]+$", names(cf), value = TRUE)
  )
  nombres <- nombres[nombres %in% names(cf)]

  data.frame(
    Empresa = obj$empresa,
    p = obj$p,
    q = obj$q,
    AR = obj$ar,
    Parametro = nombres,
    Estimacion = as.numeric(cf[nombres]),
    row.names = NULL
  )
}

# ------------------------------------------------------------------------------
# 6. Ajuste y selección por empresa
#
# Regla:
#   A) Estimar los cuatro GARCH(p,q) con media AR(0)+volumen.
#   B) Un candidato es adecuado si:
#        - converge,
#        - persistencia < 1,
#        - Ljung-Box(z) > 0.05,
#        - Ljung-Box(z^2) > 0.05.
#   C) Si existe al menos un candidato adecuado, elegir el de menor BIC.
#   D) Si ninguno es adecuado, estimar AR(1)+volumen para los mismos p,q,
#      combinar candidatos y volver a aplicar la regla.
#   E) Si aún ninguno pasa todos los diagnósticos, conservar el modelo convergente
#      de menor BIC y marcarlo explícitamente como "Requiere cautela".
# ------------------------------------------------------------------------------

ajustar_y_seleccionar_empresa <- function(empresa) {
  objetos_ar0 <- lapply(seq_len(nrow(candidatos_pq)), function(k) {
    ajustar_un_garch(
      empresa = empresa,
      p = candidatos_pq$p[k],
      q = candidatos_pq$q[k],
      ar = 0,
      incluir_volumen = TRUE
    )
  })

  resumen_ar0 <- dplyr::bind_rows(lapply(objetos_ar0, extraer_resumen_garch)) %>%
    dplyr::mutate(
      Adecuado =
        Convergencia == 0 &
        is.finite(Persistencia) &
        Persistencia < 1 &
        P_LB_Residuos > 0.05 &
        P_LB_Cuadrados > 0.05
    )

  if (any(resumen_ar0$Adecuado, na.rm = TRUE)) {
    idx <- which(resumen_ar0$Adecuado)[
      which.min(resumen_ar0$BIC[resumen_ar0$Adecuado])
    ]

    return(list(
      empresa = empresa,
      candidatos = objetos_ar0,
      tabla = resumen_ar0,
      final = objetos_ar0[[idx]],
      final_resumen = resumen_ar0[idx, , drop = FALSE] %>%
        dplyr::mutate(Estado_final = "Adecuado")
    ))
  }

  # Solo se amplía la ecuación de media cuando ningún AR(0) candidato
  # consigue un diagnóstico global adecuado.
  objetos_ar1 <- lapply(seq_len(nrow(candidatos_pq)), function(k) {
    ajustar_un_garch(
      empresa = empresa,
      p = candidatos_pq$p[k],
      q = candidatos_pq$q[k],
      ar = 1,
      incluir_volumen = TRUE
    )
  })

  resumen_ar1 <- dplyr::bind_rows(lapply(objetos_ar1, extraer_resumen_garch)) %>%
    dplyr::mutate(
      Adecuado =
        Convergencia == 0 &
        is.finite(Persistencia) &
        Persistencia < 1 &
        P_LB_Residuos > 0.05 &
        P_LB_Cuadrados > 0.05
    )

  objetos_todos <- c(objetos_ar0, objetos_ar1)
  resumen_todos <- dplyr::bind_rows(resumen_ar0, resumen_ar1)

  if (any(resumen_todos$Adecuado, na.rm = TRUE)) {
    adecuados <- which(resumen_todos$Adecuado)
    idx <- adecuados[which.min(resumen_todos$BIC[adecuados])]

    return(list(
      empresa = empresa,
      candidatos = objetos_todos,
      tabla = resumen_todos,
      final = objetos_todos[[idx]],
      final_resumen = resumen_todos[idx, , drop = FALSE] %>%
        dplyr::mutate(Estado_final = "Adecuado")
    ))
  }

  # Si ningún modelo pasa todos los diagnósticos, no se oculta el problema.
  elegibles <- which(
    resumen_todos$Convergencia == 0 &
    is.finite(resumen_todos$BIC) &
    is.finite(resumen_todos$Persistencia) &
    resumen_todos$Persistencia < 1
  )

  if (length(elegibles) == 0) {
    stop("Ningún modelo convergió de forma admisible para ", empresa)
  }

  idx <- elegibles[which.min(resumen_todos$BIC[elegibles])]

  list(
    empresa = empresa,
    candidatos = objetos_todos,
    tabla = resumen_todos,
    final = objetos_todos[[idx]],
    final_resumen = resumen_todos[idx, , drop = FALSE] %>%
      dplyr::mutate(Estado_final = "Requiere cautela")
  )
}

# ------------------------------------------------------------------------------
# 6.1 Prueba mínima antes del barrido completo
# ------------------------------------------------------------------------------
# Esta prueba obliga a detener el proceso en un punto identificable si la
# instalación de rugarch o alguno de sus solvers presenta un problema.
empresa_prueba_garch <- empresas_garch[1]
message("Prueba mínima GARCH para: ", empresa_prueba_garch)
obj_prueba_garch <- ajustar_un_garch(
  empresa = empresa_prueba_garch,
  p = 1,
  q = 1,
  ar = 0,
  incluir_volumen = TRUE
)

if (is.null(obj_prueba_garch$ajuste)) {
  stop("Falló ugarchfit en la prueba mínima: ", obj_prueba_garch$error)
}

message("Clase del ajuste: ", paste(class(obj_prueba_garch$ajuste), collapse = ", "))
resumen_prueba_garch <- extraer_resumen_garch(obj_prueba_garch)
print(resumen_prueba_garch)
message("Prueba mínima completada correctamente. Iniciando selección completa...")

resultados_seleccion <- setNames(
  lapply(empresas_garch, ajustar_y_seleccionar_empresa),
  empresas_garch
)

# Tabla con TODOS los candidatos evaluados.
tabla_candidatos_garch <- dplyr::bind_rows(
  lapply(resultados_seleccion, function(x) x$tabla)
)

# Tabla de modelos finales por empresa.
tabla_modelos_garch_finales <- dplyr::bind_rows(
  lapply(resultados_seleccion, function(x) x$final_resumen)
) %>%
  dplyr::mutate(
    Empresa = dplyr::recode(Empresa, !!!etiquetas_empresas),
    Modelo = paste0("AR(", AR, ")-GARCH(", p, ",", q, ")")
  ) %>%
  dplyr::select(
    Empresa,
    Modelo,
    N,
    BIC,
    AIC,
    Persistencia,
    Shape_t,
    P_LB_Residuos,
    P_LB_Cuadrados,
    Estado_final
  )

print(tabla_modelos_garch_finales)

# Parámetros de varianza de los modelos finales en formato largo.
tabla_parametros_garch_finales <- dplyr::bind_rows(
  lapply(resultados_seleccion, function(x) {
    extraer_parametros_varianza(x$final)
  })
) %>%
  dplyr::mutate(
    Empresa = dplyr::recode(Empresa, !!!etiquetas_empresas),
    Modelo = paste0("AR(", AR, ")-GARCH(", p, ",", q, ")")
  )

print(tabla_parametros_garch_finales)

# ------------------------------------------------------------------------------
# 7. Aporte incremental del volumen para la especificación final
#
# Se compara, manteniendo el mismo AR y el mismo GARCH(p,q):
#   M0: sin volumen
#   M1: con volumen contemporáneo
# ------------------------------------------------------------------------------

comparar_volumen_modelo_final <- function(resultado_empresa) {
  final <- resultado_empresa$final

  con_vol <- final
  sin_vol <- ajustar_un_garch(
    empresa = final$empresa,
    p = final$p,
    q = final$q,
    ar = final$ar,
    incluir_volumen = FALSE
  )

  if (is.null(con_vol$ajuste) || is.null(sin_vol$ajuste)) {
    return(data.frame(
      Empresa = final$empresa,
      p = final$p,
      q = final$q,
      AR = final$ar,
      Delta_Volumen = NA_real_,
      P_Volumen_Robusto = NA_real_,
      Delta_AIC = NA_real_,
      Delta_BIC = NA_real_,
      LR = NA_real_,
      P_LR = NA_real_,
      row.names = NULL
    ))
  }

  fit1 <- con_vol$ajuste
  fit0 <- sin_vol$ajuste

  fit1_slot <- methods::slot(fit1, "fit")
  fit0_slot <- methods::slot(fit0, "fit")

  cf1 <- fit1_slot[["coef"]]
  robustos <- fit1_slot[["robust.matcoef"]]

  delta <- if ("mxreg1" %in% names(cf1)) as.numeric(cf1["mxreg1"]) else NA_real_
  p_delta <- if (
    !is.null(robustos) &&
    "mxreg1" %in% rownames(robustos) &&
    ncol(robustos) >= 4
  ) {
    as.numeric(robustos["mxreg1", 4])
  } else {
    NA_real_
  }

  ll0 <- as.numeric(fit0_slot[["LLH"]])
  ll1 <- as.numeric(fit1_slot[["LLH"]])

  res0 <- as.numeric(fit0_slot[["residuals"]])
  res1 <- as.numeric(fit1_slot[["residuals"]])
  ipars0 <- fit0_slot[["ipars"]]
  ipars1 <- fit1_slot[["ipars"]]

  n0 <- length(res0)
  n1 <- length(res1)
  k0 <- if (!is.null(ipars0) && ncol(ipars0) >= 4) sum(ipars0[, 4], na.rm = TRUE) else length(fit0_slot[["coef"]])
  k1 <- if (!is.null(ipars1) && ncol(ipars1) >= 4) sum(ipars1[, 4], na.rm = TRUE) else length(cf1)

  aic0 <- (-2 * ll0) / n0 + (2 * k0) / n0
  aic1 <- (-2 * ll1) / n1 + (2 * k1) / n1
  bic0 <- (-2 * ll0) / n0 + (k0 * log(n0)) / n0
  bic1 <- (-2 * ll1) / n1 + (k1 * log(n1)) / n1

  LR <- 2 * (ll1 - ll0)
  p_lr <- if (is.finite(LR) && LR >= 0) {
    stats::pchisq(LR, df = 1, lower.tail = FALSE)
  } else {
    NA_real_
  }

  data.frame(
    Empresa = final$empresa,
    p = final$p,
    q = final$q,
    AR = final$ar,
    Delta_Volumen = delta,
    P_Volumen_Robusto = p_delta,
    Delta_AIC = aic1 - aic0,
    Delta_BIC = bic1 - bic0,
    LR = LR,
    P_LR = p_lr,
    row.names = NULL
  )
}

tabla_comparacion_volumen <- dplyr::bind_rows(
  lapply(resultados_seleccion, comparar_volumen_modelo_final)
) %>%
  dplyr::mutate(
    Empresa = dplyr::recode(Empresa, !!!etiquetas_empresas),
    Modelo = paste0("AR(", AR, ")-GARCH(", p, ",", q, ")"),
    Evidencia_volumen = dplyr::case_when(
      P_Volumen_Robusto < 0.05 & Delta_AIC < 0 & Delta_BIC < 0 ~
        "Evidencia fuerte",
      P_Volumen_Robusto < 0.05 & (Delta_AIC < 0 | Delta_BIC < 0) ~
        "Evidencia parcial",
      P_Volumen_Robusto < 0.10 ~
        "Evidencia débil",
      TRUE ~
        "Sin evidencia estadística"
    )
  ) %>%
  dplyr::select(
    Empresa,
    Modelo,
    Delta_Volumen,
    P_Volumen_Robusto,
    Delta_AIC,
    Delta_BIC,
    LR,
    P_LR,
    Evidencia_volumen
  )

print(tabla_comparacion_volumen)

# ------------------------------------------------------------------------------
# 8. Exportación de resultados para las tablas de la tesis
# ------------------------------------------------------------------------------

utils::write.csv(
  tabla_malla_garch,
  "tabla_malla_garch.csv",
  row.names = FALSE
)

utils::write.csv(
  tabla_candidatos_garch,
  "tabla_candidatos_garch.csv",
  row.names = FALSE
)

utils::write.csv(
  tabla_modelos_garch_finales,
  "tabla_modelos_garch_finales.csv",
  row.names = FALSE
)

utils::write.csv(
  tabla_parametros_garch_finales,
  "tabla_parametros_garch_finales.csv",
  row.names = FALSE
)

utils::write.csv(
  tabla_comparacion_volumen,
  "tabla_comparacion_volumen.csv",
  row.names = FALSE
)

cat("\n============================================================\n")
cat("Proceso GARCH(p,q) finalizado.\n")
cat("Revise especialmente:\n")
cat("  tabla_modelos_garch_finales\n")
cat("  tabla_parametros_garch_finales\n")
cat("  tabla_comparacion_volumen\n")
cat("============================================================\n")

# ================================================================
# ANÁLISIS DE ROBUSTEZ: SIMETRÍA
# Test de sesgo de signo de Engle y Ng
# ================================================================

extraer_signbias <- function(resultado_empresa) {
  
  fit <- resultado_empresa$final$ajuste
  
  if (is.null(fit)) {
    return(NULL)
  }
  
  sb <- rugarch::signbias(fit)
  
  data.frame(
    Empresa = resultado_empresa$empresa,
    
    P_Sign_Bias =
      as.numeric(sb["Sign Bias", "prob"]),
    
    P_Negative_Sign_Bias =
      as.numeric(sb["Negative Sign Bias", "prob"]),
    
    P_Positive_Sign_Bias =
      as.numeric(sb["Positive Sign Bias", "prob"]),
    
    P_Joint_Effect =
      as.numeric(sb["Joint Effect", "prob"]),
    
    row.names = NULL
  )
}

tabla_simetria_garch <- dplyr::bind_rows(
  lapply(
    resultados_seleccion,
    extraer_signbias
  )
)

tabla_simetria_garch <- tabla_simetria_garch %>%
  dplyr::mutate(
    
    Empresa = dplyr::recode(
      Empresa,
      !!!etiquetas_empresas
    ),
    
    Conclusion_Simetria =
      dplyr::case_when(
        
        P_Joint_Effect < 0.01 ~
          "Evidencia fuerte de asimetría",
        
        P_Joint_Effect < 0.05 ~
          "Evidencia de asimetría",
        
        P_Joint_Effect < 0.10 ~
          "Evidencia débil de asimetría",
        
        TRUE ~
          "Sin evidencia de asimetría"
      )
  )

print(tabla_simetria_garch)

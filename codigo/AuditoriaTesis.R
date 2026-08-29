# ==============================================================================
# AUDITORÍA FINAL PARA RECONSTRUIR LA SECCIÓN 5.1 DE LA TESIS
# ==============================================================================
# IMPORTANTE:
# Ejecutar AL FINAL del script, una vez construidos:
# lista_empresas, df_precios, df_rendimientos_final,
# df_volumen, df_volumen_imput, df_volumen_log,
# matriz_volumen, matriz_rendimientos,
# fd_volumen, fd_rendimientos,
# smooth_vol_final, smooth_rend_final,
# matriz_cov_vol y matriz_cov_rend.
#
# ==============================================================================


# ==============================================================================
# 0. COMPROBAR QUE EXISTEN LOS OBJETOS NECESARIOS
# ==============================================================================

objetos_necesarios <- c(
  "lista_empresas",
  "etiquetas_empresas",
  "df_precios",
  "df_rendimientos_final",
  "df_volumen",
  "df_volumen_imput",
  "df_volumen_log",
  "matriz_volumen",
  "matriz_rendimientos",
  "fd_volumen",
  "fd_rendimientos",
  "smooth_vol_final",
  "smooth_rend_final",
  "matriz_cov_vol",
  "matriz_cov_rend",
  "resultados_rend_opt",
  "resultados_K_rend",
  "best_nb_vol",
  "best_lambda_vol",
  "lambda_rend_final"
)

faltantes_objetos <- objetos_necesarios[
  !vapply(
    objetos_necesarios,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(faltantes_objetos) > 0) {
  stop(
    paste0(
      "Faltan los siguientes objetos antes de ejecutar la auditoría:\n",
      paste(faltantes_objetos, collapse = ", ")
    )
  )
}

cat("\n========================================\n")
cat("AUDITORÍA SECCIÓN 5.1\n")
cat("========================================\n")


# ==============================================================================
# 1. AUDITORÍA DE LOS ARCHIVOS ORIGINALES
# ==============================================================================

tabla_fuentes_originales <- purrr::imap_dfr(
  lista_empresas,
  function(df, id) {
    
    tibble::tibble(
      ID = id,
      Empresa = unname(etiquetas_empresas[id]),
      
      N_registros = nrow(df),
      
      N_fechas_unicas =
        dplyr::n_distinct(df$Fecha),
      
      Fecha_inicial =
        min(df$Fecha, na.rm = TRUE),
      
      Fecha_final =
        max(df$Fecha, na.rm = TRUE),
      
      Fechas_duplicadas =
        sum(duplicated(df$Fecha)),
      
      NA_Fecha =
        sum(is.na(df$Fecha)),
      
      NA_Cierre =
        sum(is.na(df$Cierre)),
      
      NA_Volumen =
        sum(is.na(df$Volumen)),
      
      Cierre_no_positivo =
        sum(
          df$Cierre <= 0,
          na.rm = TRUE
        ),
      
      Volumen_negativo =
        sum(
          df$Volumen < 0,
          na.rm = TRUE
        ),
      
      Volumen_cero =
        sum(
          df$Volumen == 0,
          na.rm = TRUE
        )
    )
  }
)

cat("\n--- 1. Bases originales ---\n")
print(tabla_fuentes_originales)


# ==============================================================================
# 2. CONSTRUIR Y AUDITAR LA MALLA COMÚN
# ==============================================================================

fechas_comunes_originales <- crear_base_fechas(
  lista_empresas
)$Fecha

n_malla_comun_original <- length(
  fechas_comunes_originales
)

df_precios_audit <- crear_dataframe_ancho(
  lista_empresas = lista_empresas,
  variable = "Cierre",
  prefijo = "Cierre"
)

df_volumen_audit <- crear_dataframe_ancho(
  lista_empresas = lista_empresas,
  variable = "Volumen",
  prefijo = "Vol"
)


# ------------------------------------------------------------------------------
# Función auxiliar para clasificar ausencias producidas por la armonización
# ------------------------------------------------------------------------------

clasificar_ausencias <- function(x) {
  
  n <- length(x)
  
  validos <- which(!is.na(x))
  
  if (length(validos) == 0) {
    return(
      c(
        Ausencias_totales = n,
        Ausencias_iniciales = n,
        Ausencias_internas = 0,
        Ausencias_finales = 0
      )
    )
  }
  
  primera <- min(validos)
  ultima <- max(validos)
  
  iniciales <- if (primera > 1) {
    sum(is.na(x[seq_len(primera - 1)]))
  } else {
    0
  }
  
  finales <- if (ultima < n) {
    sum(is.na(x[(ultima + 1):n]))
  } else {
    0
  }
  
  totales <- sum(is.na(x))
  
  internas <- totales -
    iniciales -
    finales
  
  c(
    Ausencias_totales = totales,
    Ausencias_iniciales = iniciales,
    Ausencias_internas = internas,
    Ausencias_finales = finales
  )
}


tabla_armonizacion <- purrr::imap_dfr(
  lista_empresas,
  function(df, id) {
    
    col_precio <- paste0(
      "Cierre_",
      id
    )
    
    col_volumen <- paste0(
      "Vol_",
      id
    )
    
    aus_precio <- clasificar_ausencias(
      df_precios_audit[[col_precio]]
    )
    
    aus_vol <- clasificar_ausencias(
      df_volumen_audit[[col_volumen]]
    )
    
    tibble::tibble(
      ID = id,
      
      Empresa =
        unname(
          etiquetas_empresas[id]
        ),
      
      Fechas_originales =
        dplyr::n_distinct(df$Fecha),
      
      Malla_comun_original =
        n_malla_comun_original,
      
      Posiciones_añadidas =
        n_malla_comun_original -
        dplyr::n_distinct(df$Fecha),
      
      Porcentaje_añadido =
        round(
          100 *
            (
              n_malla_comun_original -
                dplyr::n_distinct(df$Fecha)
            ) /
            n_malla_comun_original,
          3
        ),
      
      Ausencias_precio =
        as.integer(
          aus_precio[
            "Ausencias_totales"
          ]
        ),
      
      Precio_iniciales =
        as.integer(
          aus_precio[
            "Ausencias_iniciales"
          ]
        ),
      
      Precio_internas =
        as.integer(
          aus_precio[
            "Ausencias_internas"
          ]
        ),
      
      Precio_finales =
        as.integer(
          aus_precio[
            "Ausencias_finales"
          ]
        ),
      
      Ausencias_volumen =
        as.integer(
          aus_vol[
            "Ausencias_totales"
          ]
        )
    )
  }
)

cat("\n--- 2. Armonización temporal ---\n")
print(tabla_armonizacion)

cat(
  "\nTamaño de la unión original de fechas:",
  n_malla_comun_original,
  "\n"
)

cat(
  "¿Todas las empresas comienzan en la misma fecha?:",
  length(
    unique(
      tabla_fuentes_originales$Fecha_inicial
    )
  ) == 1,
  "\n"
)

cat(
  "¿Todas las empresas terminan en la misma fecha?:",
  length(
    unique(
      tabla_fuentes_originales$Fecha_final
    )
  ) == 1,
  "\n"
)


# ==============================================================================
# 3. ¿EL FILL HACIA ARRIBA MODIFICÓ REALMENTE LOS PRECIOS?
# ==============================================================================

# Versión únicamente LOCF hacia adelante
precios_solo_down <- df_precios_audit %>%
  dplyr::arrange(Fecha) %>%
  tidyr::fill(
    where(is.numeric),
    .direction = "down"
  )

# Versión equivalente a la función actualmente utilizada
precios_down_up <- precios_solo_down %>%
  tidyr::fill(
    where(is.numeric),
    .direction = "up"
  )

cols_precio <- grep(
  "^Cierre_",
  names(df_precios_audit),
  value = TRUE
)

tabla_fill_up <- purrr::map_dfr(
  cols_precio,
  function(col) {
    
    id <- sub(
      "^Cierre_",
      "",
      col
    )
    
    modificadas_up <- sum(
      is.na(
        precios_solo_down[[col]]
      ) &
        !is.na(
          precios_down_up[[col]]
        )
    )
    
    tibble::tibble(
      ID = id,
      Empresa =
        unname(
          etiquetas_empresas[id]
        ),
      Posiciones_rellenadas_hacia_arriba =
        modificadas_up
    )
  }
)

cat("\n--- 3. Impacto del fill hacia arriba ---\n")
print(tabla_fill_up)

cat(
  "\nTOTAL de posiciones modificadas por fill(up):",
  sum(
    tabla_fill_up$
      Posiciones_rellenadas_hacia_arriba
  ),
  "\n"
)

if (
  sum(
    tabla_fill_up$
    Posiciones_rellenadas_hacia_arriba
  ) == 0
) {
  
  cat(
    "RESULTADO: fill(up) NO tuvo efecto.",
    "Puede eliminarse del código definitivo sin cambiar resultados.\n"
  )
  
} else {
  
  cat(
    "ADVERTENCIA: fill(up) sí modificó posiciones iniciales.",
    "Estas deben revisarse antes de reconstruir 5.1.\n"
  )
}


# ==============================================================================
# 4. CUANTIFICAR LOCF, GAM Y RENDIMIENTOS CERO
# ==============================================================================

# df_volumen ya tiene eliminada la fecha 2021-07-01
# y debe coincidir con los rendimientos finales.

tabla_reconstruccion <- purrr::imap_dfr(
  lista_empresas,
  function(df, id) {
    
    col_precio <- paste0(
      "Cierre_",
      id
    )
    
    col_vol <- paste0(
      "Vol_",
      id
    )
    
    col_rend <- paste0(
      "R_",
      id
    )
    
    precio_raw <-
      df_precios_audit[[col_precio]]
    
    volumen_raw_final <- df_volumen[[col_vol]]
    
    rendimiento_final <-
      df_rendimientos_final[[col_rend]]
    
    # El rendimiento elimina la primera fila
    precio_actual_raw <-precio_raw[-1]
    
    precio_previo_raw <-precio_raw[-length(precio_raw)]
    
    if (length(precio_actual_raw) !=length(rendimiento_final)) {
      stop(
        paste(
          "Longitudes incompatibles para",
          id
        )
      )
    }
    
    tol <- 1e-14
    
    cero_total <-abs(rendimiento_final) < tol
    
    # Cuando no existe precio original en la jornada actual,
    # LOCF arrastra el último precio disponible.
    cero_asociado_locf <-cero_total & is.na(precio_actual_raw)
    
    cero_con_precio_observado <-cero_total & !is.na(precio_actual_raw)
    
    # Jornada observada inmediatamente después de una
    # posición ausente.
    reapertura_tras_hueco <-!is.na(precio_actual_raw) & is.na(precio_previo_raw)
    
    tibble::tibble(
      ID = id,
      Empresa =
        unname(
          etiquetas_empresas[id]
        ),
      
      Precios_completados_LOCF = sum(is.na(precio_actual_raw)),
      
      Volumenes_reconstruidos_GAM = sum(is.na(volumen_raw_final)),
      
      Porcentaje_GAM =
        round(
          100 *
            sum(
              is.na(volumen_raw_final)
            ) /
            length(volumen_raw_final),
          3
        ),
      
      Rendimientos_cero_total =
        sum(
          cero_total,
          na.rm = TRUE
        ),
      
      Rendimientos_cero_asociados_LOCF =
        sum(
          cero_asociado_locf,
          na.rm = TRUE
        ),
      
      Rendimientos_cero_con_precio_observado =
        sum(
          cero_con_precio_observado,
          na.rm = TRUE
        ),
      
      Reapariciones_tras_hueco =
        sum(
          reapertura_tras_hueco,
          na.rm = TRUE
        )
    )
  }
)

cat("\n--- 4. Reconstrucción LOCF/GAM ---\n")
print(tabla_reconstruccion)


# ==============================================================================
# 5. COMPROBACIONES DE LA MALLA ANALÍTICA FINAL
# ==============================================================================

fechas_rend_final <-
  as.Date(
    df_rendimientos_final$Fecha
  )

fechas_vol_final <-
  as.Date(
    df_volumen$Fecha
  )

nombres_rend <-
  colnames(
    matriz_rendimientos
  )

nombres_vol <-
  colnames(
    matriz_volumen
  )

tabla_checks_finales <- tibble::tibble(
  
  Comprobacion = c(
    "N jornadas rendimientos",
    "N jornadas volumen",
    "Fechas volumen = rendimiento",
    "N empresas rendimientos",
    "N empresas volumen",
    "Orden de empresas idéntico",
    "NA matriz rendimientos",
    "NA matriz volumen",
    "No finitos rendimientos",
    "No finitos volumen",
    "Fecha inicial rendimiento",
    "Fecha final rendimiento",
    "Fecha inicial volumen",
    "Fecha final volumen"
  ),
  
  Resultado = c(
    as.character(
      nrow(df_rendimientos_final)
    ),
    
    as.character(
      nrow(df_volumen)
    ),
    
    as.character(
      identical(
        fechas_rend_final,
        fechas_vol_final
      )
    ),
    
    as.character(
      ncol(matriz_rendimientos)
    ),
    
    as.character(
      ncol(matriz_volumen)
    ),
    
    as.character(
      identical(
        nombres_rend,
        nombres_vol
      )
    ),
    
    as.character(
      sum(
        is.na(
          matriz_rendimientos
        )
      )
    ),
    
    as.character(
      sum(
        is.na(
          matriz_volumen
        )
      )
    ),
    
    as.character(
      sum(
        !is.finite(
          matriz_rendimientos
        )
      )
    ),
    
    as.character(
      sum(
        !is.finite(
          matriz_volumen
        )
      )
    ),
    
    as.character(
      min(fechas_rend_final)
    ),
    
    as.character(
      max(fechas_rend_final)
    ),
    
    as.character(
      min(fechas_vol_final)
    ),
    
    as.character(
      max(fechas_vol_final)
    )
  )
)

cat("\n--- 5. Malla analítica final ---\n")
print(tabla_checks_finales)


# ==============================================================================
# 6. R2 DESCRIPTIVO EXACTO DE RECONSTRUCCIÓN DEL SUAVIZADO
# ==============================================================================

calcular_metricas_reconstruccion <- function(
    Y,
    fdobj,
    argvals,
    etiquetas_empresas
) {
  
  Yhat <- fda::eval.fd(
    argvals,
    fdobj
  )
  
  resid <- Y - Yhat
  
  SSE <- colSums(
    resid^2,
    na.rm = TRUE
  )
  
  media_y <- colMeans(
    Y,
    na.rm = TRUE
  )
  
  SST <- sapply(
    seq_len(ncol(Y)),
    function(j) {
      sum(
        (
          Y[, j] -
            media_y[j]
        )^2,
        na.rm = TRUE
      )
    }
  )
  
  R2_recon <- 1 -
    SSE / SST
  
  RMS <- sqrt(
    colMeans(
      resid^2,
      na.rm = TRUE
    )
  )
  
  sd_y <- apply(
    Y,
    2,
    sd,
    na.rm = TRUE
  )
  
  RMS_rel <- RMS / sd_y
  
  ids <- colnames(Y)
  
  nombres <- unname(
    etiquetas_empresas[ids]
  )
  
  nombres[
    is.na(nombres)
  ] <- ids[
    is.na(nombres)
  ]
  
  tibble::tibble(
    ID = ids,
    Empresa = nombres,
    RMS = RMS,
    Desviacion_original = sd_y,
    RMS_relativo = RMS_rel,
    Indice_antiguo_1_menos_RMSrel2 =
      1 - RMS_rel^2,
    R2_reconstruccion =
      R2_recon
  )
}


tabla_metricas_volumen <- calcular_metricas_reconstruccion(
  Y = matriz_volumen,
  fdobj = fd_volumen,
  argvals = tiempo_vol,
  etiquetas_empresas =
    etiquetas_empresas
)

tabla_metricas_rendimiento <- calcular_metricas_reconstruccion(
  Y = matriz_rendimientos,
  fdobj = fd_rendimientos,
  argvals = tiempo_rend,
  etiquetas_empresas =
    etiquetas_empresas
)

cat("\n--- 6A. Métricas exactas volumen ---\n")
print(tabla_metricas_volumen)

cat("\n--- 6B. Métricas exactas rendimiento ---\n")
print(tabla_metricas_rendimiento)


# ==============================================================================
# 7. FUNCIÓN DE RUGOSIDAD DE UNA REPRESENTACIÓN FUNCIONAL
# ==============================================================================

calcular_rugosidad_fd <- function(
    fdobj,
    orden_derivada = 2
) {
  
  Lfd <- fda::int2Lfd(
    orden_derivada
  )
  
  P <- fda::eval.penalty(
    fdobj$basis,
    Lfd
  )
  
  C <- as.matrix(
    fdobj$coefs
  )
  
  sapply(
    seq_len(ncol(C)),
    function(j) {
      
      cj <- C[, j]
      
      as.numeric(
        t(cj) %*%
          P %*%
          cj
      )
    }
  )
}


rug_vol_final <- calcular_rugosidad_fd(
  fd_volumen
)

rug_rend_final <- calcular_rugosidad_fd(
  fd_rendimientos
)


# ==============================================================================
# 8. RESUMEN FINAL DEL SUAVIZADO DEL VOLUMEN
# ==============================================================================

tabla_resumen_suavizado_volumen <- tibble::tibble(
  
  Parametro = c(
    "Número de bases final",
    "Lambda final",
    "GCV total final",
    "Grados de libertad efectivos",
    "RMS relativo medio",
    "RMS relativo mediano",
    "R2 reconstrucción medio",
    "R2 reconstrucción mediano",
    "Rugosidad mediana D2"
  ),
  
  Valor = c(
    best_nb_vol,
    best_lambda_vol,
    sum(
      smooth_vol_final$gcv
    ),
    as.numeric(
      smooth_vol_final$df
    )[1],
    mean(
      tabla_metricas_volumen$
        RMS_relativo,
      na.rm = TRUE
    ),
    median(
      tabla_metricas_volumen$
        RMS_relativo,
      na.rm = TRUE
    ),
    mean(
      tabla_metricas_volumen$
        R2_reconstruccion,
      na.rm = TRUE
    ),
    median(
      tabla_metricas_volumen$
        R2_reconstruccion,
      na.rm = TRUE
    ),
    median(
      rug_vol_final,
      na.rm = TRUE
    )
  )
)

cat("\n--- 8. Suavizado volumen ---\n")
print(tabla_resumen_suavizado_volumen)


# ==============================================================================
# 9. AUDITORÍA DE GCV Y SELECCIÓN FINAL DE RENDIMIENTOS
# ==============================================================================

fila_K150 <- resultados_K_rend %>%
  dplyr::filter(
    K_Bases == 150
  )

if (nrow(fila_K150) != 1) {
  stop(
    "No se encontró una única fila K=150 en resultados_K_rend."
  )
}

lambda_gcv_K150 <-
  fila_K150$Best_Lambda[1]

gcv_gcv_K150 <-
  fila_K150$Min_GCV[1]

mejor_global_rend <- resultados_rend_opt %>%
  dplyr::arrange(
    Min_GCV
  ) %>%
  dplyr::slice(1)


lambda_figura_actual <- if (
  exists(
    "lambda_rend_gcv"
  )
) {
  lambda_rend_gcv
} else {
  NA_real_
}


tabla_control_lambda_rend <- tibble::tibble(
  
  Concepto = c(
    "Lambda preliminar GCV (K=140)",
    "Mejor lambda GCV con K=150",
    "Lambda usado actualmente en figura comparativa",
    "Lambda final elegido",
    "Mejor K barrido conjunto",
    "Mejor lambda barrido conjunto"
  ),
  
  Valor = c(
    lambda_rend_preliminar,
    lambda_gcv_K150,
    lambda_figura_actual,
    lambda_rend_final,
    mejor_global_rend$N_Bases,
    mejor_global_rend$Best_Lambda
  )
)

cat("\n--- 9A. Control de lambdas de rendimiento ---\n")
print(tabla_control_lambda_rend)


# ==============================================================================
# 10. COMPARACIÓN CUANTITATIVA DE LAMBDAS PARA K = 150
# ==============================================================================

evaluar_lambda_rend <- function(lambda_i) {
  
  base_i <- fda::create.bspline.basis(
    rangeval = rango_rend,
    nbasis = 150,
    norder = 4
  )
  
  fdPar_i <- fda::fdPar(
    base_i,
    Lfdobj = 2,
    lambda = lambda_i
  )
  
  sm_i <- fda::smooth.basis(
    tiempo_rend,
    matriz_rendimientos,
    fdPar_i
  )
  
  fd_i <- sm_i$fd
  
  met_i <- calcular_metricas_reconstruccion(
    Y = matriz_rendimientos,
    fdobj = fd_i,
    argvals = tiempo_rend,
    etiquetas_empresas =
      etiquetas_empresas
  )
  
  rug_i <- calcular_rugosidad_fd(
    fd_i
  )
  
  tibble::tibble(
    Lambda = lambda_i,
    
    GCV_total =
      sum(
        sm_i$gcv
      ),
    
    EDF =
      as.numeric(
        sm_i$df
      )[1],
    
    RMS_rel_medio =
      mean(
        met_i$RMS_relativo,
        na.rm = TRUE
      ),
    
    RMS_rel_mediano =
      median(
        met_i$RMS_relativo,
        na.rm = TRUE
      ),
    
    R2_recon_medio =
      mean(
        met_i$R2_reconstruccion,
        na.rm = TRUE
      ),
    
    R2_recon_mediano =
      median(
        met_i$R2_reconstruccion,
        na.rm = TRUE
      ),
    
    Rugosidad_mediana =
      median(
        rug_i,
        na.rm = TRUE
      ),
    
    Rugosidad_media =
      mean(
        rug_i,
        na.rm = TRUE
      )
  )
}


lambdas_comparar_rend <- unique(
  c(
    10,
    100,
    lambda_gcv_K150
  )
)

tabla_comparacion_lambda_rend <-
  purrr::map_dfr(
    lambdas_comparar_rend,
    evaluar_lambda_rend
  ) %>%
  dplyr::arrange(
    Lambda
  )

cat("\n--- 10. Comparación de lambdas K=150 ---\n")
print(tabla_comparacion_lambda_rend)


# ==============================================================================
# 11. RESUMEN DEL MODELO FUNCIONAL FINAL DE RENDIMIENTOS
# ==============================================================================

tabla_resumen_suavizado_rend <- tibble::tibble(
  
  Parametro = c(
    "Número de bases final",
    "Lambda final",
    "GCV total final",
    "Grados de libertad efectivos",
    "RMS relativo medio",
    "RMS relativo mediano",
    "R2 reconstrucción medio",
    "R2 reconstrucción mediano",
    "Rugosidad mediana D2"
  ),
  
  Valor = c(
    nbasis_rend_final,
    lambda_rend_final,
    sum(
      smooth_rend_final$gcv
    ),
    as.numeric(
      smooth_rend_final$df
    )[1],
    mean(
      tabla_metricas_rendimiento$
        RMS_relativo,
      na.rm = TRUE
    ),
    median(
      tabla_metricas_rendimiento$
        RMS_relativo,
      na.rm = TRUE
    ),
    mean(
      tabla_metricas_rendimiento$
        R2_reconstruccion,
      na.rm = TRUE
    ),
    median(
      tabla_metricas_rendimiento$
        R2_reconstruccion,
      na.rm = TRUE
    ),
    median(
      rug_rend_final,
      na.rm = TRUE
    )
  )
)

cat("\n--- 11. Suavizado rendimiento ---\n")
print(tabla_resumen_suavizado_rend)


# ==============================================================================
# 12. COMPROBACIÓN DEL RANGO MUESTRAL
# ==============================================================================

coef_vol <- as.matrix(
  fd_volumen$coefs
)

coef_rend <- as.matrix(
  fd_rendimientos$coefs
)

coef_vol_centrado <- sweep(
  coef_vol,
  1,
  rowMeans(coef_vol),
  FUN = "-"
)

coef_rend_centrado <- sweep(
  coef_rend,
  1,
  rowMeans(coef_rend),
  FUN = "-"
)

rango_coef_vol <-
  qr(
    coef_vol_centrado
  )$rank

rango_coef_rend <-
  qr(
    coef_rend_centrado
  )$rank

tabla_rango_funcional <- tibble::tibble(
  
  Variable = c(
    "Volumen",
    "Rendimiento"
  ),
  
  N_empresas = c(
    ncol(coef_vol),
    ncol(coef_rend)
  ),
  
  Rango_numerico_curvas_centradas = c(
    rango_coef_vol,
    rango_coef_rend
  ),
  
  Cota_teorica_N_menos_1 = c(
    ncol(coef_vol) - 1,
    ncol(coef_rend) - 1
  )
)

cat("\n--- 12. Rango funcional ---\n")
print(tabla_rango_funcional)


# ==============================================================================
# 13. RESUMEN DE LA DIAGONAL DE LAS COVARIANZAS
# ==============================================================================

if (
  nrow(matriz_cov_vol) !=
  length(fechas_vol_final)
) {
  stop(
    "La dimensión de matriz_cov_vol no coincide con las fechas del volumen."
  )
}

if (
  nrow(matriz_cov_rend) !=
  length(fechas_rend_final)
) {
  stop(
    "La dimensión de matriz_cov_rend no coincide con las fechas de rendimiento."
  )
}


diag_cov_vol <- diag(
  matriz_cov_vol
)

diag_cov_rend <- diag(
  matriz_cov_rend
)


tabla_diag_cov_vol <- tibble::tibble(
  Fecha = fechas_vol_final,
  Varianza_transversal = diag_cov_vol
) %>%
  dplyr::arrange(
    dplyr::desc(
      Varianza_transversal
    )
  )


tabla_diag_cov_rend <- tibble::tibble(
  Fecha = fechas_rend_final,
  Varianza_transversal = diag_cov_rend
) %>%
  dplyr::arrange(
    dplyr::desc(
      Varianza_transversal
    )
  )


top5_cov_vol <- tabla_diag_cov_vol %>%
  dplyr::slice_head(
    n = 5
  )

top5_cov_rend <- tabla_diag_cov_rend %>%
  dplyr::slice_head(
    n = 5
  )

cat("\n--- 13A. Cinco mayores dispersiones transversales volumen ---\n")
print(top5_cov_vol)

cat("\n--- 13B. Cinco mayores dispersiones transversales rendimiento ---\n")
print(top5_cov_rend)


# ==============================================================================
# 14. TABLA MAESTRA DE PARÁMETROS QUE CONECTA 5.1 CON 5.2
# ==============================================================================

tabla_parametros_51 <- tibble::tibble(
  
  Objeto = c(
    "Volumen funcional X_i(s)",
    "Rendimiento funcional Y_i(t)"
  ),
  
  Transformacion = c(
    "log(1 + Volumen)",
    "log(P_t / P_{t-1})"
  ),
  
  N_empresas = c(
    ncol(matriz_volumen),
    ncol(matriz_rendimientos)
  ),
  
  N_jornadas = c(
    nrow(matriz_volumen),
    nrow(matriz_rendimientos)
  ),
  
  N_bases = c(
    best_nb_vol,
    nbasis_rend_final
  ),
  
  Lambda = c(
    best_lambda_vol,
    lambda_rend_final
  ),
  
  Derivada_penalizada = c(
    2,
    2
  ),
  
  Papel_modelo_52 = c(
    "Predictor funcional",
    "Respuesta funcional"
  )
)

cat("\n--- 14. Parámetros finales 5.1 -> 5.2 ---\n")
print(tabla_parametros_51)


# ==============================================================================
# 15. EXPORTAR TODO A EXCEL
# ==============================================================================

archivo_auditoria <- "Auditoria_Seccion_5_1.xlsx"

writexl::write_xlsx(
  list(
    
    "01_Fuentes_originales" =
      tabla_fuentes_originales,
    
    "02_Armonizacion" =
      tabla_armonizacion,
    
    "03_Fill_up" =
      tabla_fill_up,
    
    "04_LOCF_GAM" =
      tabla_reconstruccion,
    
    "05_Checks_finales" =
      tabla_checks_finales,
    
    "06_Metricas_volumen" =
      tabla_metricas_volumen,
    
    "07_Metricas_rendimiento" =
      tabla_metricas_rendimiento,
    
    "08_Resumen_volumen" =
      tabla_resumen_suavizado_volumen,
    
    "09_Control_lambda_rend" =
      tabla_control_lambda_rend,
    
    "10_Comparacion_lambda_rend" =
      tabla_comparacion_lambda_rend,
    
    "11_Resumen_rendimiento" =
      tabla_resumen_suavizado_rend,
    
    "12_Rango_funcional" =
      tabla_rango_funcional,
    
    "13_Top_cov_volumen" =
      top5_cov_vol,
    
    "14_Top_cov_rendimiento" =
      top5_cov_rend,
    
    "15_Parametros_5_1" =
      tabla_parametros_51,
    
    "16_GCV_rend_K" =
      resultados_K_rend,
    
    "17_GCV_rend_conjunto" =
      resultados_rend_opt
    
  ),
  path = archivo_auditoria
)

cat("\n========================================\n")
cat("AUDITORÍA TERMINADA\n")
cat("========================================\n")

cat(
  "Archivo generado:",
  normalizePath(
    archivo_auditoria,
    mustWork = FALSE
  ),
  "\n"
)


# ==============================================================================
# 16. RESUMEN DE ALERTAS IMPORTANTES
# ==============================================================================

cat("\n=== RESUMEN PARA LA TESIS ===\n")

cat(
  "Malla común original:",
  n_malla_comun_original,
  "fechas\n"
)

cat(
  "Malla final rendimiento:",
  nrow(df_rendimientos_final),
  "fechas\n"
)

cat(
  "Malla final volumen:",
  nrow(df_volumen),
  "fechas\n"
)

cat(
  "Fechas finales idénticas:",
  identical(
    fechas_rend_final,
    fechas_vol_final
  ),
  "\n"
)

cat(
  "Empresas en el mismo orden:",
  identical(
    nombres_rend,
    nombres_vol
  ),
  "\n"
)

cat(
  "Posiciones afectadas por fill(up):",
  sum(
    tabla_fill_up$
      Posiciones_rellenadas_hacia_arriba
  ),
  "\n"
)

cat(
  "Lambda GCV K=150:",
  lambda_gcv_K150,
  "\n"
)

cat(
  "Lambda de la figura comparativa actual:",
  lambda_figura_actual,
  "\n"
)

cat(
  "Lambda final de rendimiento:",
  lambda_rend_final,
  "\n"
)

cat(
  "K final volumen:",
  best_nb_vol,
  "| Lambda volumen:",
  best_lambda_vol,
  "\n"
)

cat(
  "K final rendimiento:",
  nbasis_rend_final,
  "| Lambda rendimiento:",
  lambda_rend_final,
  "\n"
)

cat(
  "Rango numérico volumen centrado:",
  rango_coef_vol,
  "<=",
  ncol(coef_vol) - 1,
  "\n"
)

cat(
  "Rango numérico rendimiento centrado:",
  rango_coef_rend,
  "<=",
  ncol(coef_rend) - 1,
  "\n"
)


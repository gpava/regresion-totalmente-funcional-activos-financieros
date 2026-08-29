cat("\014")
rm(list = ls())

# PAQUETES

paquetes <- c(
  "tidyverse", "lubridate", "dplyr", "ggplot2", "plotly",
  "imputeTS", "fda", "cowplot", "ggthemes", "corrplot",
  "ggcorrplot", "Hmisc", "zoo", "PerformanceAnalytics",
  "mgcv", "tidyr", "scales","readr",
  "writexl","rugarch"
)
paquetes_faltantes <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}
invisible(lapply(paquetes, library, character.only = TRUE))

# FUNCIONES

convertir_valor <- function(s) {
  s <- as.character(s)
  s <- trimws(s)
  if (is.na(s) || s == "" || s == "-") return(NA_real_)
  s_up <- toupper(s)
  sufijo <- substr(s_up, nchar(s_up), nchar(s_up))
  tiene_sufijo <- sufijo %in% c("K", "M", "B")
  num_str <- if (tiene_sufijo) {
    substr(s_up, 1, nchar(s_up) - 1)
  } else {
    s_up
  }
  num_str <- gsub("\\.", "", num_str)
  num_str <- gsub(",", ".", num_str)
  valor <- suppressWarnings(as.numeric(num_str))
  if (is.na(valor)) return(NA_real_)
  multiplicador <- dplyr::case_when(
    !tiene_sufijo ~ 1,
    sufijo == "K" ~ 1e3,
    sufijo == "M" ~ 1e6,
    sufijo == "B" ~ 1e9,
    TRUE ~ 1
  )
  valor * multiplicador
}

leer_datos <- function(ruta) {
  datos <- read.csv(ruta, sep = ";", stringsAsFactors = FALSE)

  fecha <- gsub("/", "-", datos[, 1])
  fecha <- lubridate::dmy(fecha)

  cierre <- as.numeric(gsub(",", ".", gsub("\\.", "", datos[, 2])))

  volumen <- lapply(datos[, 6], convertir_valor)
  volumen <- unlist(volumen)

  data.frame(
    Fecha = fecha,
    Cierre = cierre,
    Volumen = volumen
  ) %>%
    dplyr::arrange(Fecha)
}

crear_base_fechas <- function(lista_empresas) {
  lista_empresas %>%
    purrr::map(~ dplyr::select(.x, Fecha)) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(Fecha) %>%
    dplyr::arrange(Fecha)
}

crear_dataframe_ancho <- function(lista_empresas, variable, prefijo) {
  df_ancho <- crear_base_fechas(lista_empresas)

  for (nombre in names(lista_empresas)) {
    df_tmp <- lista_empresas[[nombre]] %>%
      dplyr::select(Fecha, dplyr::all_of(variable)) %>%
      dplyr::arrange(Fecha) %>%
      dplyr::rename(!!paste0(prefijo, "_", nombre) := dplyr::all_of(variable))

    df_ancho <- df_ancho %>%
      dplyr::left_join(df_tmp, by = "Fecha")
  }

  df_ancho
}

calcular_rendimientos_log <- function(df_precios, imputar_precios = TRUE) {
  df_base <- df_precios %>%
    dplyr::arrange(Fecha)

  if (imputar_precios) {
    # LOCF: si una acción no tiene precio en una fecha,
    # se arrastra el último precio observado.
    df_base <- df_base %>%
      tidyr::fill(where(is.numeric), .direction = "down")
    # Robustez: si alguna serie empieza con NA, se rellena hacia arriba.
    df_base <- df_base %>%
      tidyr::fill(where(is.numeric), .direction = "up")
  }
  df_rendimientos <- df_base %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("Cierre_"),
        ~ c(NA_real_, diff(log(.x))),
        .names = "R_{sub('^Cierre_', '', .col)}"
      )
    ) %>%
    dplyr::select(Fecha, dplyr::starts_with("R_")) %>%
    dplyr::slice(-1)
  rownames(df_rendimientos) <- NULL
  df_rendimientos
}
preparar_long_rendimientos <- function(df_rendimientos, etiquetas_empresas) {
  df_rendimientos %>%
    tidyr::pivot_longer(
      cols = -Fecha,
      names_to = "Empresa",
      values_to = "Rendimiento"
    ) %>%
    dplyr::mutate(
      Empresa = gsub("^R_", "", Empresa),
      Empresa = dplyr::recode(Empresa, !!!etiquetas_empresas),
      Empresa = factor(Empresa, levels = unname(etiquetas_empresas))
    )
}

graficar_superficie_matriz <- function(
  grid_x,
  grid_y,
  matriz_z,
  titulo_z,
  titulo_x,
  titulo_y,
  tipo = c("surface", "contour"),
  colorscale = "Jet",
  tick_digits = 4
) {
  tipo <- match.arg(tipo)
  zmin_local <- min(matriz_z, na.rm = TRUE)
  zmax_local <- max(matriz_z, na.rm = TRUE)
  ticks_local <- pretty(c(zmin_local, zmax_local), n = 6)
  ticktext_local <- sprintf(paste0("%.", tick_digits, "f"), ticks_local)
  hover_local <- paste0(
    titulo_x, ": %{x:.0f}<br>",
    titulo_y, ": %{y:.0f}<br>",
    titulo_z, ": %{z:.", tick_digits, "f}<extra></extra>"
  )
  if (tipo == "surface") {
    plotly::plot_ly(
      x = grid_x,
      y = grid_y,
      z = matriz_z,
      type = "surface",
      colorscale = colorscale,
      cmin = zmin_local,
      cmax = zmax_local,
      hovertemplate = hover_local,
      colorbar = list(
        title = titulo_z,
        tickmode = "array",
        tickvals = ticks_local,
        ticktext = ticktext_local
      )
    ) %>%
      plotly::layout(
        scene = list(
          xaxis = list(title = titulo_x),
          yaxis = list(title = titulo_y),
          zaxis = list(title = titulo_z, tickformat = paste0(".", tick_digits, "f"))
        )
      )
  } else {
    plotly::plot_ly(
      x = grid_x,
      y = grid_y,
      z = matriz_z,
      type = "contour",
      colorscale = colorscale,
      zmin = zmin_local,
      zmax = zmax_local,
      hovertemplate = hover_local,
      colorbar = list(
        title = titulo_z,
        tickmode = "array",
        tickvals = ticks_local,
        ticktext = ticktext_local
      ),
      contours = list(
        coloring = "heatmap",
        showlines = TRUE
      )
    ) %>%
      plotly::layout(
        dragmode = "zoom",
        xaxis = list(title = titulo_x),
        yaxis = list(title = titulo_y)
      )
  }
}
calcular_metricas_fd <- function(Y_real, fd_suavizado, argvals) {
  Y_hat <- eval.fd(argvals, fd_suavizado)
  residuos <- Y_real - Y_hat
  RMS <- apply(residuos, 2, function(r) sqrt(mean(r^2, na.rm = TRUE)))
  sd_y <- apply(Y_real, 2, function(y) sd(y, na.rm = TRUE))
  rango_y <- apply(Y_real, 2, function(y) diff(range(y, na.rm = TRUE)))
  RMS_rel <- ifelse(is.finite(sd_y) & sd_y > 0, RMS / sd_y, NA_real_)
  RMS_rango <- ifelse(is.finite(rango_y) & rango_y > 0, RMS / rango_y, NA_real_)
  data.frame(
    Serie = colnames(Y_real),
    RMS = RMS,
    Desviacion_Original = sd_y,
    RMS_Relativo = RMS_rel,
    RMS_Rango = RMS_rango,
    row.names = NULL
  ) %>%
    dplyr::arrange(dplyr::desc(RMS_Relativo))
}

imputar_volumen_gam_log1p <- function(df, vol_col, k = NULL, method = "REML") {
  df <- df %>% dplyr::arrange(Fecha)
  n <- nrow(df)
  if (is.null(k)) k <- max(10, min(40, floor(n / 12)))
  tnum <- as.numeric(df$Fecha)
  dow <- lubridate::wday(df$Fecha, label = TRUE, week_start = 1)
  y <- as.numeric(df[[vol_col]])
  z <- log1p(y)
  idx <- which(!is.na(z))
  if (length(idx) < 12) {
    z2 <- z
    z2 <- zoo::na.locf(z2, na.rm = FALSE)
    z2 <- zoo::na.locf(z2, fromLast = TRUE, na.rm = FALSE)
    z2[is.na(z2)] <- 0
    y_hat <- pmax(expm1(z2), 0)
    y_hat[!is.na(y)] <- y[!is.na(y)]
    return(y_hat)
  }
  ajuste <- mgcv::gam(
    z ~ s(tnum, k = k, bs = "cr") + dow,
    data = data.frame(z = z[idx], tnum = tnum[idx], dow = dow[idx]),
    method = method
  )
  z_pred <- predict(ajuste, newdata = data.frame(tnum = tnum, dow = dow))
  y_hat <- pmax(expm1(z_pred), 0)
  # Se respetan los datos observados: solo se imputan faltantes.
  y_hat[!is.na(y)] <- y[!is.na(y)]
  y_hat
}

evaluar_gcv_fda <- function(Y, argvals, rangeval, nbasis, log10lambda, Lfdobj) {
  basis_obj <- fda::create.bspline.basis(rangeval = rangeval, nbasis = nbasis)
  lambda <- 10^log10lambda
  fdPar_obj <- fda::fdPar(basis_obj, Lfdobj, lambda)
  smooth_list <- fda::smooth.basis(
    argvals = argvals,
    y = Y,
    fdParobj = fdPar_obj
  )
  list(
    lambda = lambda,
    log10lambda = log10lambda,
    df_total = sum(smooth_list$df),
    gcv_total = sum(smooth_list$gcv),
    fd = smooth_list$fd,
    smooth = smooth_list
  )
}

calcular_R2_t <- function(y_observado, y_estimado) {
  
  residuos <- y_observado - y_estimado
  
  SSE_t <- rowSums(residuos^2, na.rm = TRUE)
  SST_t <- rowSums(y_observado^2, na.rm = TRUE)
  
  R2_t <- 1 - SSE_t / SST_t
  
  R2_t[!is.finite(R2_t)] <- NA_real_
  
  R2_t
}
# LECTURA DE DATOS

directorio_empresas <- file.path("datos", "originales")
archivos_empresas <- c(
  bancolombia   = "Bancolombia.csv",
  bogota        = "Bancodebogota.csv",
  bvc           = "Bolsavaloresdecolombia.csv",
  ceargos       = "CementosArgos.csv",
  celsia        = "CelsiaSA.csv",
  ecopetrol     = "Ecopetrol.csv",
  corpcolombia  = "Corporacionfinancieradecolombia.csv",
  bolivar       = "Grupobolivar.csv",
  mineros       = "Mineros SA (MAS).csv",
  nutresa       = "Nutresa (NCH).csv",
  terpel        = "Organizacion Terpel SA (TPL).csv",
  promigas      = "Promigas (PMG).csv",
  suramericana  = "Suramericana (SIS).csv"
)
etiquetas_empresas <- c(
  bancolombia   = "Bancolombia",
  bogota        = "Banco de Bogotá",
  bvc           = "Bolsa de Valores de Colombia",
  ceargos       = "Cementos Argos",
  celsia        = "Celsia S.A.",
  ecopetrol     = "Ecopetrol",
  corpcolombia  = "Corporación Financiera de Colombia",
  bolivar       = "Grupo Bolívar",
  mineros       = "Mineros",
  nutresa       = "Nutresa",
  terpel        = "Terpel",
  promigas      = "Promigas",
  suramericana  = "Suramericana"
)
lista_empresas <- purrr::map(
  archivos_empresas,
  ~ leer_datos(file.path(directorio_empresas, .x))
)
names(lista_empresas) <- names(archivos_empresas)

# PRECIOS Y RENDIMIENTOS LOGARÍTMICOS

fechas_base <- crear_base_fechas(lista_empresas)
df_precios <- crear_dataframe_ancho(
  lista_empresas = lista_empresas,
  variable = "Cierre",
  prefijo = "Cierre"
)
# Rendimientos sin imputar, útiles para visualizar datos faltantes.
df_rendimientos_sin_imputar <- calcular_rendimientos_log(
  df_precios = df_precios,
  imputar_precios = FALSE
)
df_rendimientos_interpolados <- df_rendimientos_sin_imputar
for (j in 2:ncol(df_rendimientos_interpolados)) {
  df_rendimientos_interpolados[[j]] <- imputeTS::na_interpolation(
    df_rendimientos_interpolados[[j]],
    option = "spline"
  )
}
rownames(df_rendimientos_interpolados) <- NULL
# Pasar los rendimientos interpolados a formato largo
df_rend_interpolados_long <- preparar_long_rendimientos(
  df_rendimientos_interpolados,
  etiquetas_empresas
)

# Asegurar formato Date
df_rend_interpolados_long$Fecha <- as.Date(
  df_rend_interpolados_long$Fecha
)
# Fechas que aparecerán en el eje X
fechas_eje <- seq(
  from = as.Date("2021-09-01"),
  to   = max(df_rend_interpolados_long$Fecha, na.rm = TRUE),
  by   = "3 months"
)

# Meses en español
meses_es <- c(
  "ene", "feb", "mar", "abr",
  "may", "jun", "jul", "ago",
  "sep", "oct", "nov", "dic"
)

# Etiquetas en español
etiquetas_eje <- paste(
  meses_es[as.integer(format(fechas_eje, "%m"))],
  format(fechas_eje, "%Y")
)


p_rendimientos_imputados_spline <- ggplot(
  df_rend_interpolados_long,
  aes(
    x = Fecha,
    y = Rendimiento,
    color = Empresa,
    text = paste0(
      "Empresa: ", Empresa,
      "<br>Fecha: ", Fecha,
      "<br>Rendimiento: ", round(Rendimiento, 4)
    )
  )
) +
  geom_point(
    alpha = 0.7,
    size = 1.4
  ) +
  scale_x_date(
    breaks = fechas_eje,
    labels = etiquetas_eje
  ) +
  labs(
    x = "Fecha",
    y = "Rendimiento",
    color = "Empresa"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

grafica_rendimientos_interpolados <- plotly::ggplotly(
  p_rendimientos_imputados_spline,
  tooltip = "text"
)

grafica_rendimientos_interpolados
# Gráfica de rendimientos sin imputar
df_rend_long <- preparar_long_rendimientos(
  df_rendimientos_sin_imputar,
  etiquetas_empresas
)

# Asegurar que Fecha sea de tipo Date
df_rend_long$Fecha <- as.Date(df_rend_long$Fecha)

p_rendimientos <- ggplot(
  df_rend_long,
  aes(
    x = Fecha,
    y = Rendimiento,
    color = Empresa,
    text = paste0(
      "Empresa: ", Empresa,
      "<br>Fecha: ", Fecha,
      "<br>Rendimiento: ", round(Rendimiento, 4)
    )
  )
) +
  geom_point(
    alpha = 0.7,
    size = 1.4
  ) +
  scale_x_date(
    breaks = fechas_eje,
    labels = etiquetas_eje
  ) +
  labs(
    x = "Fecha",
    y = "Rendimiento",
    color = "Empresa"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

grafica_rendimientos <- plotly::ggplotly(
  p_rendimientos,
  tooltip = "text"
)

grafica_rendimientos

# Rendimientos finales: imputación LOCF en precios y luego cálculo de retornos.
df_rendimientos_final <- calcular_rendimientos_log(
  df_precios = df_precios,
  imputar_precios = TRUE
)

head(df_rendimientos_final, 15)
matriz_rendimientos <- as.matrix(df_rendimientos_final %>% dplyr::select(-Fecha))
colnames(matriz_rendimientos) <- gsub("^R_", "", colnames(matriz_rendimientos))
n_dias_rend <- nrow(matriz_rendimientos)
n_acciones <- ncol(matriz_rendimientos)
tiempo_rend <- seq_len(n_dias_rend)
rango_rend <- c(1, n_dias_rend)

# ==============================================================================
# 4. FDA DE RENDIMIENTOS
# ==============================================================================

# ----------------------------------------------------------------------
# 4.1 Selección preliminar de lambda para nbasis fijo
# ----------------------------------------------------------------------

nbasis_rend_preliminar <- 140
base_rend_preliminar <- fda::create.bspline.basis(
  rangeval = rango_rend,
  nbasis = nbasis_rend_preliminar,
  norder = 4
)

lambdas_log_rend <- seq(-4, 6, by = 0.5)
gcv_rend_preliminar <- rep(NA_real_, length(lambdas_log_rend))

for (i in seq_along(lambdas_log_rend)) {
  lambda_i <- 10^lambdas_log_rend[i]

  fdPar_i <- fda::fdPar(
    base_rend_preliminar,
    Lfdobj = 2,
    lambda = lambda_i
  )

  smooth_i <- fda::smooth.basis(
    tiempo_rend,
    matriz_rendimientos,
    fdPar_i
  )

  gcv_rend_preliminar[i] <- sum(smooth_i$gcv)
}

idx_lambda_rend_preliminar <- which.min(gcv_rend_preliminar)
lambda_rend_preliminar <- 10^lambdas_log_rend[idx_lambda_rend_preliminar]

cat("Lambda preliminar sugerido por GCV:", lambda_rend_preliminar, "\n")

plot(
  lambdas_log_rend,
  gcv_rend_preliminar,
  type = "b",
  lwd = 2,
  xlab = "log10(lambda)",
  ylab = "GCV total",
  main = "Validación GCV para rendimientos"
)
abline(v = lambdas_log_rend[idx_lambda_rend_preliminar], col = "black", lty = 2)
points(
  lambdas_log_rend[idx_lambda_rend_preliminar],
  min(gcv_rend_preliminar),
  pch = 19,
  cex = 1.2
)

# ----------------------------------------------------------------------
# 4.2 Barrido conjunto: número de bases K y lambda
# ----------------------------------------------------------------------

nbasis_grid_rend <- c(80, 100, 125, 150)
lambdas_log_grid_rend <- seq(-2, 6, by = 0.5)

resultados_rend_opt <- data.frame()

cat("Iniciando optimización conjunta de rendimientos: bases + lambda...\n")

for (nb in nbasis_grid_rend) {
  base_temp <- fda::create.bspline.basis(rango_rend, nb, norder = 4)

  gcv_temp <- rep(NA_real_, length(lambdas_log_grid_rend))

  for (i in seq_along(lambdas_log_grid_rend)) {
    lambda_i <- 10^lambdas_log_grid_rend[i]

    fdPar_i <- fda::fdPar(base_temp, Lfdobj = 2, lambda = lambda_i)

    smooth_i <- fda::smooth.basis(
      tiempo_rend,
      matriz_rendimientos,
      fdPar_i
    )

    gcv_temp[i] <- sum(smooth_i$gcv)
  }

  idx_best <- which.min(gcv_temp)
  lambda_best_nb <- 10^lambdas_log_grid_rend[idx_best]

  fdPar_best <- fda::fdPar(
    base_temp,
    Lfdobj = 2,
    lambda = lambda_best_nb
  )

  smooth_best <- fda::smooth.basis(
    tiempo_rend,
    matriz_rendimientos,
    fdPar_best
  )

  y_hat_best <- fda::eval.fd(tiempo_rend, smooth_best$fd)
  residuos_best <- matriz_rendimientos - y_hat_best

  rms_global <- sqrt(mean(residuos_best^2, na.rm = TRUE))
  sd_global <- sd(as.vector(matriz_rendimientos), na.rm = TRUE)
  rms_rel_global <- rms_global / sd_global

  resultados_rend_opt <- rbind(
    resultados_rend_opt,
    data.frame(
      N_Bases = nb,
      Best_Lambda = lambda_best_nb,
      Log_Lambda = lambdas_log_grid_rend[idx_best],
      Min_GCV = gcv_temp[idx_best],
      RMS = rms_global,
      RMS_Relativo = rms_rel_global
    )
  )
}

resultados_rend_opt <- resultados_rend_opt %>%
  dplyr::arrange(Min_GCV)

cat("=== TABLA DE RESULTADOS DE OPTIMIZACIÓN: RENDIMIENTOS ===\n")
print(resultados_rend_opt)
# ----------------------------------------------------------------------
# 4.3 Justificación visual de K
# ----------------------------------------------------------------------

nbasis_barrido_rend <- seq(40, 180, by = 10)
lambdas_log_barrido_rend <- seq(-2, 6, by = 0.5)

resultados_K_rend <- data.frame()

cat("Iniciando barrido para curva GCV vs K...\n")

for (nb in nbasis_barrido_rend) {
  base_temp <- fda::create.bspline.basis(rango_rend, nb, norder = 4)
  gcv_temp <- rep(NA_real_, length(lambdas_log_barrido_rend))

  for (i in seq_along(lambdas_log_barrido_rend)) {
    lambda_i <- 10^lambdas_log_barrido_rend[i]

    fdPar_i <- fda::fdPar(base_temp, Lfdobj = 2, lambda = lambda_i)

    smooth_i <- fda::smooth.basis(
      tiempo_rend,
      matriz_rendimientos,
      fdPar_i
    )

    gcv_temp[i] <- sum(smooth_i$gcv)
  }

  idx_best <- which.min(gcv_temp)

  resultados_K_rend <- rbind(
    resultados_K_rend,
    data.frame(
      K_Bases = nb,
      Min_GCV = gcv_temp[idx_best],
      Best_Lambda = 10^lambdas_log_barrido_rend[idx_best]
    )
  )

  cat("Bases:", nb, "| Min GCV:", gcv_temp[idx_best], "\n")
}

idx_min_K_rend <- which.min(resultados_K_rend$Min_GCV)
best_K_rend_gcv <- resultados_K_rend$K_Bases[idx_min_K_rend]
min_gcv_K_rend <- resultados_K_rend$Min_GCV[idx_min_K_rend]

fila_K150 <- subset(resultados_K_rend, K_Bases == 150)
gcv_K150 <- fila_K150$Min_GCV

par(mfrow = c(1, 1), mar = c(5, 5, 4, 2))

plot(
  resultados_K_rend$K_Bases,
  resultados_K_rend$Min_GCV,
  type = "b",
  pch = 21,
  bg = "white",
  col = "black",
  lwd = 1.5,
  cex = 1.2,
  main = "Selección de número de bases K por GCV mínimo",
  xlab = "Número de funciones base K",
  ylab = "GCV total mínimo sobre lambda",
  xaxt = "n"
)

axis(
  side = 1,
  at = nbasis_barrido_rend,
  labels = nbasis_barrido_rend,
  las = 1,
  cex.axis = 0.8
)

abline(v = best_K_rend_gcv, col = "black", lty = 2, lwd = 2)
abline(v = 150, col = "red", lty = 2, lwd = 2)

points(best_K_rend_gcv, min_gcv_K_rend, pch = 19, col = "black", cex = 1.5)

if (length(gcv_K150) == 1) {
  points(150, gcv_K150, pch = 19, col = "red", cex = 1.6)
}

# ----------------------------------------------------------------------
# 4.4 Comparación visual de lambdas con K = 150
# ----------------------------------------------------------------------

nbasis_rend_final <- 150
base_rend_final <- fda::create.bspline.basis(
  rango_rend,
  nbasis_rend_final,
  norder = 4
)

lambda_rend_gcv <- 1e5
lambda_rend_mid <- 100
lambda_rend_low <- 10

fdPar_rend_gcv <- fda::fdPar(base_rend_final, Lfdobj = 2, lambda = lambda_rend_gcv)
fdPar_rend_mid <- fda::fdPar(base_rend_final, Lfdobj = 2, lambda = lambda_rend_mid)
fdPar_rend_low <- fda::fdPar(base_rend_final, Lfdobj = 2, lambda = lambda_rend_low)

fd_rend_gcv <- fda::smooth.basis(tiempo_rend, matriz_rendimientos, fdPar_rend_gcv)$fd
fd_rend_mid <- fda::smooth.basis(tiempo_rend, matriz_rendimientos, fdPar_rend_mid)$fd
fd_rend_low <- fda::smooth.basis(tiempo_rend, matriz_rendimientos, fdPar_rend_low)$fd

accion_idx <- 2
nombre_accion <- colnames(matriz_rendimientos)[accion_idx]

par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

plotfit.fd(
  matriz_rendimientos[, accion_idx],
  tiempo_rend,
  fd_rend_gcv[accion_idx],
  main = paste(nombre_accion, "- Lambda 100,000 (sugerido GCV)"),
  cex.pch = 0.5,
  col = "red"
)

plotfit.fd(
  matriz_rendimientos[, accion_idx],
  tiempo_rend,
  fd_rend_mid[accion_idx],
  main = paste(nombre_accion, "- Lambda 100 (balanceado)"),
  cex.pch = 0.5,
  col = "red"
)

plotfit.fd(
  matriz_rendimientos[, accion_idx],
  tiempo_rend,
  fd_rend_low[accion_idx],
  main = paste(nombre_accion, "- Lambda 10 (reactivo)"),
  cex.pch = 0.5,
  col = "red"
)

# ----------------------------------------------------------------------
# 4.5 Modelo funcional final para rendimientos
# ----------------------------------------------------------------------
graphics.off()
lambda_rend_final <- 100

fdPar_rend_final <- fda::fdPar(
  base_rend_final,
  Lfdobj = 2,
  lambda = lambda_rend_final
)

smooth_rend_final <- fda::smooth.basis(
  tiempo_rend,
  matriz_rendimientos,
  fdPar_rend_final
)

fd_rendimientos <- smooth_rend_final$fd

fd_rendimientos$fdnames <- list(
  Time = "Días",
  Stock = colnames(matriz_rendimientos),
  Return = "Rendimiento logarítmico"
)

plot(fd_rendimientos, main = "Rendimientos suavizados (B-spline)")
media_rendimientos <- fda::mean.fd(fd_rendimientos)
lines(media_rendimientos, col = "black", lty = 2, lwd = 3)
legend(
  "topleft",
  legend = c("Media"),
  col = c("black"),
  lty = c(2),
  lwd = c(3),
  bty = "n"
)

tabla_resumen_rend <- data.frame(
  Parametro = c("Número de bases", "Lambda", "GCV global total"),
  Valor = c(
    as.character(nbasis_rend_final),
    as.character(lambda_rend_final),
    sprintf("%.8f", sum(smooth_rend_final$gcv))
  )
)

tabla_desempeno_rend <- calcular_metricas_fd(
  Y_real = matriz_rendimientos,
  fd_suavizado = fd_rendimientos,
  argvals = tiempo_rend
)

cat("=== TABLA 1: PARÁMETROS DEL MODELO DE RENDIMIENTOS ===\n")
print(tabla_resumen_rend)

cat("\n=== TABLA 2: VALIDACIÓN POR EMPRESA: RENDIMIENTOS ===\n")
print(tabla_desempeno_rend)

cat("\nRMS relativo promedio de rendimientos:",
    mean(tabla_desempeno_rend$RMS_Relativo, na.rm = TRUE), "\n")

# ----------------------------------------------------------------------
# 4.6 Superficie de covarianza de rendimientos
# ----------------------------------------------------------------------

cov_rendimientos <- fda::var.fd(fd_rendimientos)

rango_cov_rend <- fd_rendimientos$basis$rangeval
grid_cov_rend <- seq(rango_cov_rend[1], rango_cov_rend[2], length.out = 490)

matriz_cov_rend <- fda::eval.bifd(
  grid_cov_rend,
  grid_cov_rend,
  cov_rendimientos
)

sup_cov_rend <- graficar_superficie_matriz(
  grid_x = grid_cov_rend,
  grid_y = grid_cov_rend,
  matriz_z = matriz_cov_rend,
  titulo_z = "c(t,s)",
  titulo_x = "s",
  titulo_y = "t",
  tipo = "surface",
  colorscale = "Jet",
  tick_digits = 4
)

sup_cov_rend

contorno_cov_rend <- graficar_superficie_matriz(
  grid_x = grid_cov_rend,
  grid_y = grid_cov_rend,
  matriz_z = matriz_cov_rend,
  titulo_z = "c(t,s)",
  titulo_x = "s",
  titulo_y = "t",
  tipo = "contour",
  colorscale = "Jet",
  tick_digits = 4
)

contorno_cov_rend

# ----------------------------------------------------------------------
# 4.7 Gráficas de ajuste y residuales para rendimientos
# ----------------------------------------------------------------------

matriz_rend_suavizada <- fda::eval.fd(tiempo_rend, fd_rendimientos)
matriz_residuales_rend <- matriz_rendimientos - matriz_rend_suavizada

indices_rend_graficar <- c(1, 2)

titulos_rend_izq <- c(
  "Bancolombia",
  "Banco de Bogotá"
)

titulos_rend_der <- c(
  "Residuales | Bancolombia",
  "Residuales | Banco de Bogotá"
)

col_base <- "#CA717E"

col_puntos_fila <- c("gray70", grDevices::adjustcolor(col_base, alpha.f = 0.35))
col_linea_fila <- c("black", col_base)

op <- par(no.readonly = TRUE)
on.exit(par(op), add = TRUE)

par(
  mfrow = c(2, 2),
  mar = c(3.9, 4.3, 2.4, 1.0),
  oma = c(0.4, 0.4, 1.8, 0.2),
  mgp = c(2.6, 0.7, 0),
  tcl = -0.25,
  las = 1
)

contador <- 0

for (i in indices_rend_graficar) {
  contador <- contador + 1

  y_obs <- matriz_rendimientos[, i]
  y_fit <- matriz_rend_suavizada[, i]
  y_res <- matriz_residuales_rend[, i]

  rms_i <- sqrt(mean(y_res^2, na.rm = TRUE))

  m_obs <- max(abs(c(y_obs, y_fit)), na.rm = TRUE)
  if (!is.finite(m_obs) || m_obs == 0) m_obs <- 1
  ylim_obs <- c(-1, 1) * m_obs * 1.05

  m_res <- max(abs(y_res), na.rm = TRUE)
  if (!is.finite(m_res) || m_res == 0) m_res <- 1
  ylim_res <- c(-1, 1) * m_res * 1.05

  plot(
    tiempo_rend,
    y_obs,
    type = "n",
    main = titulos_rend_izq[contador],
    xlab = "Tiempo (días)",
    ylab = "Rendimientos",
    ylim = ylim_obs
  )
  grid(col = "lightgray", lty = "dotted")
  points(tiempo_rend, y_obs, pch = 19, col = col_puntos_fila[contador], cex = 0.55)
  lines(tiempo_rend, y_fit, col = col_linea_fila[contador], lwd = 2.5)

  plot(
    tiempo_rend,
    y_res,
    type = "n",
    main = titulos_rend_der[contador],
    xlab = "Tiempo (días)",
    ylab = "Residual",
    ylim = ylim_res
  )
  grid(col = "lightgray", lty = "dotted")
  points(tiempo_rend, y_res, pch = 19, col = col_puntos_fila[contador], cex = 0.55)
  abline(h = 0, col = "black", lty = 2, lwd = 1.1)

  legend(
    "topright",
    legend = sprintf("RMS = %.3f", rms_i),
    bty = "n",
    cex = 1.0,
    text.font = 2
  )
}

mtext(
  "Comparación FDA: puntos = observaciones, línea = suavizado",
  outer = TRUE,
  line = 0.4,
  font = 2,
  cex = 1.05
)

# ----------------------------------------------------------------------
# 4.8 Prueba adicional de bases con lambda fijo
# ----------------------------------------------------------------------

lambda_rend_fijo <- 100
bases_rend_a_probar <- c(150, 200, 250, 300, 365, 400)
resultados_bases_rend <- data.frame()

cat("Iniciando prueba de número de bases con lambda =", lambda_rend_fijo, "...\n")

for (nb in bases_rend_a_probar) {
  base_temp <- fda::create.bspline.basis(rango_rend, nb, norder = 4)
  fdPar_temp <- fda::fdPar(base_temp, Lfdobj = 2, lambda = lambda_rend_fijo)

  smooth_temp <- fda::smooth.basis(tiempo_rend, matriz_rendimientos, fdPar_temp)

  gcv_total <- sum(smooth_temp$gcv)

  y_hat_temp <- fda::eval.fd(tiempo_rend, smooth_temp$fd)
  residuos_temp <- matriz_rendimientos - y_hat_temp

  rms_abs <- sqrt(colMeans(residuos_temp^2, na.rm = TRUE))
  sd_orig <- apply(matriz_rendimientos, 2, sd, na.rm = TRUE)
  rms_rel_promedio <- mean(rms_abs / sd_orig, na.rm = TRUE)

  resultados_bases_rend <- rbind(
    resultados_bases_rend,
    data.frame(
      N_Bases = nb,
      GCV_Total = gcv_total,
      RMS_Rel_Promedio = rms_rel_promedio
    )
  )
}

print(resultados_bases_rend)

par(mfrow = c(1, 2))

plot(
  resultados_bases_rend$N_Bases,
  resultados_bases_rend$RMS_Rel_Promedio,
  type = "b",
  col = "black",
  lwd = 2,
  main = "Efecto en RMS relativo",
  xlab = "Número de bases",
  ylab = "RMS relativo promedio"
)

plot(
  resultados_bases_rend$N_Bases,
  resultados_bases_rend$GCV_Total,
  type = "b",
  col = "black",
  lwd = 2,
  main = "Efecto en GCV",
  xlab = "Número de bases",
  ylab = "GCV total"
)

# ==============================================================================
# 5. VOLUMEN DE LOS ACTIVOS
# ==============================================================================

df_volumen <- crear_dataframe_ancho(
  lista_empresas = lista_empresas,
  variable = "Volumen",
  prefijo = "Vol"
)

# Se conserva tu decisión de eliminar esta fecha.
fecha_eliminar <- as.Date("2021-07-01")

df_volumen <- df_volumen %>%
  dplyr::filter(Fecha != fecha_eliminar)

vol_cols <- grep("^Vol_", names(df_volumen), value = TRUE)

df_volumen_imput <- df_volumen

for (col in vol_cols) {
  df_volumen_imput[[col]] <- imputar_volumen_gam_log1p(
    df = df_volumen_imput,
    vol_col = col
  )
}

cat("NA restantes por columna de volumen:\n")
print(sapply(vol_cols, function(c) sum(is.na(df_volumen_imput[[c]]))))

Sys.setlocale("LC_TIME", "es_ES.UTF-8")

df_vol_long <- df_volumen_imput %>%
  dplyr::mutate(Fecha = as.Date(Fecha)) %>%
  dplyr::select(Fecha, dplyr::all_of(vol_cols)) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(vol_cols),
    names_to = "Empresa",
    values_to = "Volumen"
  ) %>%
  dplyr::mutate(
    Empresa = gsub("^Vol_", "", Empresa),
    Empresa = dplyr::recode(Empresa, !!!etiquetas_empresas)
  )

p_volumen <- ggplot(
  df_vol_long,
  aes(
    x = Fecha,
    y = Volumen,
    color = Empresa,
    text = paste0(
      "Empresa: ", Empresa,
      "<br>Fecha: ", Fecha,
      "<br>Volumen: ", scales::comma(Volumen)
    )
  )
) +
  geom_point(alpha = 0.7, size = 1.4) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Fecha", y = "Volumen", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")
graphics.off()
plotly::ggplotly(p_volumen, tooltip = "text")

p_volumen_log <- ggplot(
  df_vol_long,
  aes(
    x = Fecha,
    y = log1p(Volumen),
    color = Empresa,
    text = paste0(
      "Empresa: ", Empresa,
      "<br>Fecha: ", Fecha,
      "<br>Volumen: ", scales::comma(Volumen),
      "<br>log(1 + Vol): ", round(log1p(Volumen), 4)
    )
  )
) +
  geom_point(alpha = 0.7, size = 1.4) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(x = "Fecha", y = "log(1 + Volumen)", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

plotly::ggplotly(p_volumen_log, tooltip = "text")

df_volumen_log <- df_volumen_imput %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(vol_cols), ~ log1p(.x)))

cat("NA restantes después de log1p:\n")
print(sapply(vol_cols, function(c) sum(is.na(df_volumen_log[[c]]))))

# ==============================================================================
# 6. FDA DE VOLUMEN
# ==============================================================================

vol_cols <- grep("^Vol_", names(df_volumen_log), value = TRUE)

matriz_volumen <- as.matrix(df_volumen_log[, vol_cols])
colnames(matriz_volumen) <- gsub("^Vol_", "", colnames(matriz_volumen))

n_dias_vol <- nrow(matriz_volumen)
tiempo_vol <- seq_len(n_dias_vol)
rango_vol <- c(1, n_dias_vol)

Lfd_vol <- fda::int2Lfd(2)

nbasis_grid_vol <- seq(5, 25, by = 1)
log10lambda_grid_vol <- seq(1, 10, by = 1)

resultados_vol_lista <- list()

for (nb in nbasis_grid_vol) {
  cat("\n============================\n")
  cat("Probando nbasis volumen =", nb, "\n")
  cat("============================\n")

  gcv_vec <- rep(NA_real_, length(log10lambda_grid_vol))
  df_vec <- rep(NA_real_, length(log10lambda_grid_vol))

  for (i in seq_along(log10lambda_grid_vol)) {
    lg <- log10lambda_grid_vol[i]

    tmp <- evaluar_gcv_fda(
      Y = matriz_volumen,
      argvals = tiempo_vol,
      rangeval = rango_vol,
      nbasis = nb,
      log10lambda = lg,
      Lfdobj = Lfd_vol
    )

    gcv_vec[i] <- tmp$gcv_total
    df_vec[i] <- tmp$df_total
  }

  idx_best <- which.min(gcv_vec)
  best_log10lambda_nb <- log10lambda_grid_vol[idx_best]
  best_lambda_nb <- 10^best_log10lambda_nb

  resultados_vol_lista[[as.character(nb)]] <- list(
    nbasis = nb,
    log10lambda_grid = log10lambda_grid_vol,
    gcv_vec = gcv_vec,
    df_vec = df_vec,
    best_log10lambda = best_log10lambda_nb,
    best_lambda = best_lambda_nb
  )

  plot(
    log10lambda_grid_vol,
    gcv_vec,
    type = "b",
    lwd = 2,
    main = paste("GCV total vs log10(lambda) | Número de bases =", nb),
    xlab = "log10(lambda)",
    ylab = "GCV total"
  )
  points(best_log10lambda_nb, min(gcv_vec), pch = 19, cex = 1.2)
  legend(
    "topleft",
    legend = c("GCV total", paste0("lambda óptimo = 10^", best_log10lambda_nb)),
    lty = c(1, 2),
    lwd = c(2, 2),
    bty = "n"
  )
}

summary_vol <- do.call(
  rbind,
  lapply(resultados_vol_lista, function(x) {
    idx_best <- which.min(x$gcv_vec)

    data.frame(
      nbasis = x$nbasis,
      best_log10lambda = x$log10lambda_grid[idx_best],
      best_lambda = 10^(x$log10lambda_grid[idx_best]),
      best_gcv = x$gcv_vec[idx_best],
      best_df_total = x$df_vec[idx_best]
    )
  })
)

summary_vol <- summary_vol %>%
  dplyr::arrange(best_gcv)

print(summary_vol)

best_nb_vol <- summary_vol$nbasis[1]
best_log10lambda_vol <- summary_vol$best_log10lambda[1]
best_lambda_vol <- summary_vol$best_lambda[1]

cat("Mejor base volumen K*:", best_nb_vol, "\n")
cat("Mejor log10(lambda*) volumen:", best_log10lambda_vol, "\n")
cat("Mejor lambda* volumen:", best_lambda_vol, "\n")

summary_vol_K <- summary_vol %>%
  dplyr::arrange(nbasis)

plot(
  summary_vol_K$nbasis,
  summary_vol_K$best_gcv,
  type = "b",
  lwd = 2,
  main = "Selección de número de bases K por GCV mínimo",
  xlab = "Número de funciones base K",
  ylab = "GCV total mínimo sobre lambda"
)

abline(v = best_nb_vol, lty = 2, lwd = 2)
points(best_nb_vol, min(summary_vol$best_gcv), pch = 19, cex = 1.2)

legend(
  "top",
  legend = paste0("Número de funciones base = ", best_nb_vol),
  lty = 2,
  lwd = 2,
  bty = "n"
)

cat(
  "\nMejor configuración volumen:\n",
  "Número de funciones base =", best_nb_vol, "\n",
  "lambda =", best_lambda_vol, "\n"
)

base_vol_final <- fda::create.bspline.basis(
  rangeval = rango_vol,
  nbasis = best_nb_vol
)

fdPar_vol_final <- fda::fdPar(
  base_vol_final,
  Lfd_vol,
  best_lambda_vol
)

smooth_vol_final <- fda::smooth.basis(
  argvals = tiempo_vol,
  y = matriz_volumen,
  fdParobj = fdPar_vol_final
)

fd_volumen <- smooth_vol_final$fd
gcv_vol_final <- sum(smooth_vol_final$gcv)

fd_volumen$fdnames <- list(
  Time = "Días",
  Stock = colnames(matriz_volumen),
  Volume = "log(1 + Volumen)"
)

cat("GCV total final volumen =", gcv_vol_final, "\n")

plot(
  fd_volumen,
  xlab = "Tiempo (días)",
  ylab = expression(log(1 + Volumen)),
  main = "Volumen suavizado (B-splines)"
)

media_volumen <- fda::mean.fd(fd_volumen)
lines(media_volumen, col = "black", lty = 2, lwd = 3)

legend(
  "topleft",
  legend = c("Media"),
  col = c("black"),
  lty = c(2),
  lwd = c(3),
  bty = "n",
  inset = c(0, 0.10)
)

# ----------------------------------------------------------------------
# 6.1 Superficie de covarianza de volumen
# ----------------------------------------------------------------------

cov_volumen <- fda::var.fd(fd_volumen)

rango_cov_vol <- fd_volumen$basis$rangeval
grid_cov_vol <- seq(rango_cov_vol[1], rango_cov_vol[2], length.out = 490)

matriz_cov_vol <- fda::eval.bifd(
  grid_cov_vol,
  grid_cov_vol,
  cov_volumen
)

sup_cov_vol <- graficar_superficie_matriz(
  grid_x = grid_cov_vol,
  grid_y = grid_cov_vol,
  matriz_z = matriz_cov_vol,
  titulo_z = "c(t,s)",
  titulo_x = "s",
  titulo_y = "t",
  tipo = "surface",
  colorscale = "Jet",
  tick_digits = 4
)

sup_cov_vol

contorno_cov_vol <- graficar_superficie_matriz(
  grid_x = grid_cov_vol,
  grid_y = grid_cov_vol,
  matriz_z = matriz_cov_vol,
  titulo_z = "c(t,s)",
  titulo_x = "s",
  titulo_y = "t",
  tipo = "contour",
  colorscale = "Jet",
  tick_digits = 4
)

contorno_cov_vol

# ----------------------------------------------------------------------
# 6.2 Ajuste y residuales por empresa para volumen
# ----------------------------------------------------------------------

empresas_vol_graficar <- 1:min(2, ncol(matriz_volumen))

titulos_vol_izq <- c(
  "Bancolombia",
  "Banco de Bogotá"
)

titulos_vol_der <- c(
  "Residuales | Bancolombia",
  "Residuales | Banco de Bogotá"
)

matriz_vol_suavizada <- fda::eval.fd(tiempo_vol, fd_volumen)

cols <- seq_along(empresas_vol_graficar)
pt_col <- grDevices::adjustcolor(cols, alpha.f = 0.35)
ln_col <- grDevices::adjustcolor(cols, alpha.f = 0.95)

op <- par(
  mfrow = c(length(empresas_vol_graficar), 2),
  mar = c(5.2, 4.2, 2.5, 1.2),
  oma = c(1.0, 0, 2.0, 0)
)

for (k in seq_along(empresas_vol_graficar)) {
  j <- empresas_vol_graficar[k]

  plot(
    tiempo_vol,
    matriz_volumen[, j],
    pch = 16,
    cex = 0.6,
    col = pt_col[k],
    xlab = "Tiempo (días)",
    ylab = "log(1 + Vol)",
    main = titulos_vol_izq[k]
  )
  grid()
  lines(tiempo_vol, matriz_vol_suavizada[, j], lwd = 2.3, col = ln_col[k])

  residuo_vol_j <- matriz_volumen[, j] - matriz_vol_suavizada[, j]

  plot(
    tiempo_vol,
    residuo_vol_j,
    pch = 16,
    cex = 0.6,
    col = pt_col[k],
    xlab = "Tiempo (días)",
    ylab = "Residual",
    main = titulos_vol_der[k]
  )
  grid()
  abline(h = 0, lty = 2)

  mtext(
    sprintf("RMS = %.3f", sqrt(mean(residuo_vol_j^2, na.rm = TRUE))),
    side = 3,
    line = -1.2,
    adj = 1,
    cex = 0.9
  )
}

mtext(
  "Comparación FDA: puntos = observaciones, línea = suavizado",
  outer = TRUE,
  cex = 1.1
)

par(op)
graphics.off()
tabla_rms_vol <- calcular_metricas_fd(
  Y_real = matriz_volumen,
  fd_suavizado = fd_volumen,
  argvals = tiempo_vol
)

print(tabla_rms_vol)

cat("\nResumen RMS volumen:\n")
cat("  Mediana RMS:", median(tabla_rms_vol$RMS, na.rm = TRUE), "\n")
cat("  Mediana RMS relativo:", median(tabla_rms_vol$RMS_Relativo, na.rm = TRUE), "\n")
cat("  Máximo RMS relativo:", max(tabla_rms_vol$RMS_Relativo, na.rm = TRUE), "\n")

comparar_lambdas_vol <- function(lambdas) {
  out <- lapply(lambdas, function(lambda_i) {
    basis_i <- fda::create.bspline.basis(
      rangeval = rango_vol,
      nbasis = best_nb_vol
    )

    fdPar_i <- fda::fdPar(basis_i, Lfd_vol, lambda_i)
    smooth_i <- fda::smooth.basis(tiempo_vol, matriz_volumen, fdPar_i)

    y_hat_i <- fda::eval.fd(tiempo_vol, smooth_i$fd)
    resid_i <- matriz_volumen - y_hat_i

    RMS_i <- apply(resid_i, 2, function(r) sqrt(mean(r^2, na.rm = TRUE)))
    sd_i <- apply(matriz_volumen, 2, function(y) sd(y, na.rm = TRUE))
    RMS_rel_i <- ifelse(sd_i > 0, RMS_i / sd_i, NA_real_)

    data.frame(
      lambda = lambda_i,
      RMS_median = median(RMS_i, na.rm = TRUE),
      RMSrel_median = median(RMS_rel_i, na.rm = TRUE),
      RMSrel_max = max(RMS_rel_i, na.rm = TRUE),
      df = smooth_i$df
    )
  })

  do.call(rbind, out)
}

lambdas_vol_comparar <- best_lambda_vol * c(0.1, 1, 10)
print(comparar_lambdas_vol(lambdas_vol_comparar))

# ==============================================================================
# 7. MODELO FUNCIONAL: VOLUMEN -> RENDIMIENTO
# ==============================================================================

# X(t): volumen funcional suavizado
# Y(t): rendimiento funcional suavizado
# Ambos se centran antes de estimar beta(s,t).

xfd_vol_centrado <- fda::center.fd(fd_volumen)
yfd_rend_centrado <- fda::center.fd(fd_rendimientos)

x_basis <- xfd_vol_centrado$basis
y_basis <- yfd_rend_centrado$basis

beta_coef_base <- fda::bifd(
  coef = matrix(0, x_basis$nbasis, y_basis$nbasis),
  sbasisobj = x_basis,
  tbasisobj = y_basis
)

# ----------------------------------------------------------------------
# 7.1 Grid search de lambdas para beta(s,t)
# ----------------------------------------------------------------------

lambdas_log_modelo <- seq(3, 10, by = 1)
lambdas_grid_modelo <- 10^lambdas_log_modelo

gcv_modelo_mat <- matrix(
  NA_real_,
  nrow = length(lambdas_grid_modelo),
  ncol = length(lambdas_grid_modelo)
)

rownames(gcv_modelo_mat) <- paste0("Ls=1e", lambdas_log_modelo)
colnames(gcv_modelo_mat) <- paste0("Lt=1e", lambdas_log_modelo)

cat("Iniciando optimización de regularización del modelo funcional...\n")

total_iter <- length(lambdas_grid_modelo)^2
contador_iter <- 0

for (i in seq_along(lambdas_grid_modelo)) {
  for (j in seq_along(lambdas_grid_modelo)) {
    lambda_s_i <- lambdas_grid_modelo[i]
    lambda_t_j <- lambdas_grid_modelo[j]

    beta_par_i <- fda::bifdPar(
      bifdobj = beta_coef_base,
      Lfdobjs = fda::int2Lfd(2),
      Lfdobjt = fda::int2Lfd(2),
      lambdas = lambda_s_i,
      lambdat = lambda_t_j
    )

    beta_list_i <- list(
      fda::fdPar(y_basis, Lfdobj = 2, lambda = 1e4),
      beta_par_i
    )

    modelo_i <- fda::linmod(
      xfdobj = xfd_vol_centrado,
      yfdobj = yfd_rend_centrado,
      beta_list_i
    )

    gcv_modelo_mat[i, j] <- sum(modelo_i$gcv)

    contador_iter <- contador_iter + 1
    cat(
      sprintf(
        "\rProgreso: %d / %d | Ls: 1e%d, Lt: 1e%d",
        contador_iter,
        total_iter,
        lambdas_log_modelo[i],
        lambdas_log_modelo[j]
      )
    )
  }
}

cat("\nOptimización completada.\n")

idx_min_modelo <- which(
  gcv_modelo_mat == min(gcv_modelo_mat, na.rm = TRUE),
  arr.ind = TRUE
)

best_lambda_s_gcv <- lambdas_grid_modelo[idx_min_modelo[1]]
best_lambda_t_gcv <- lambdas_grid_modelo[idx_min_modelo[2]]

cat("\n------------------------------------------------\n")
cat("RESULTADOS DE LA OPTIMIZACIÓN DEL MODELO:\n")
cat("Mejor lambda s (volumen):     ", sprintf("%.0e", best_lambda_s_gcv), "\n")
cat("Mejor lambda t (rendimiento): ", sprintf("%.0e", best_lambda_t_gcv), "\n")
cat("------------------------------------------------\n")

# ----------------------------------------------------------------------
# 7.2 Modelo optimizado por GCV
# ----------------------------------------------------------------------

beta_par_gcv <- fda::bifdPar(
  bifdobj = beta_coef_base,
  Lfdobjs = fda::int2Lfd(2),
  Lfdobjt = fda::int2Lfd(2),
  lambdas = best_lambda_s_gcv,
  lambdat = best_lambda_t_gcv
)

beta_list_gcv <- list(
  fda::fdPar(y_basis, Lfdobj = 2, lambda = 1e4),
  beta_par_gcv
)

modelo_gcv <- fda::linmod(
  xfdobj = xfd_vol_centrado,
  yfdobj = yfd_rend_centrado,
  beta_list_gcv
)

grid_beta_s_gcv <- seq(x_basis$rangeval[1], x_basis$rangeval[2], length.out = 60)
grid_beta_t_gcv <- seq(y_basis$rangeval[1], y_basis$rangeval[2], length.out = 60)

beta_gcv_mat <- fda::eval.bifd(
  grid_beta_s_gcv,
  grid_beta_t_gcv,
  modelo_gcv$beta1estbifd
)

sup_beta_gcv <- graficar_superficie_matriz(
  grid_x = grid_beta_s_gcv,
  grid_y = grid_beta_t_gcv,
  matriz_z = beta_gcv_mat,
  titulo_z = "beta(t,s)",
  titulo_x = "s (Volumen)",
  titulo_y = "t (Rendimiento)",
  tipo = "surface",
  colorscale = "Jet",
  tick_digits = 6
)

sup_beta_gcv

grid_r2_gcv <- seq(y_basis$rangeval[1], y_basis$rangeval[2], length.out = 100)

y_estim_gcv <- fda::eval.fd(grid_r2_gcv, modelo_gcv$yhatfdobj)
y_obser_gcv <- fda::eval.fd(grid_r2_gcv, yfd_rend_centrado)

R2_t_gcv <- calcular_R2_t(
  y_observado = y_obser_gcv,
  y_estimado = y_estim_gcv
)

plot(
  grid_r2_gcv,
  R2_t_gcv,
  type = "l",
  lwd = 2,
  col = "black",
  ylim = c(min(R2_t_gcv, 0, na.rm = TRUE), 1),
  main = "Modelo optimizado mediante GCV",
  ylab = "Coeficiente de determinación funcional",
  xlab = "Tiempo (días)"
)
abline(h = mean(R2_t_gcv, na.rm = TRUE), col = "red", lty = 2)
legend(
  "bottomright",
  legend = c(
    "Coeficiente de determinación",
    paste("Media:", round(mean(R2_t_gcv, na.rm = TRUE), 3))
  ),
  col = c("black", "red"),
  lty = c(1, 2)
)

# ==============================================================================
# ANÁLISIS DE SENSIBILIDAD DE LA SUPERFICIE beta(t,s)
# ==============================================================================

lambda_s_opt <- 1e3
lambda_t_opt <- 1e8

escenarios_sensibilidad <- data.frame(
  escenario = c(
    "A. Menos suavizado",
    "B. Modelo elegido",
    "C. Más suavizado"
  ),
  lambda_s = c(
    lambda_s_opt / 10,
    lambda_s_opt,
    lambda_s_opt * 10
  ),
  lambda_t = c(
    lambda_t_opt / 10,
    lambda_t_opt,
    lambda_t_opt * 10
  )
)

# ----------------------------------------------------------
# 2. Grilla para evaluar beta(t,s)
# ----------------------------------------------------------

grid_s_sens <- seq(
  from = min(x_basis$rangeval),
  to   = max(x_basis$rangeval),
  length.out = 150
)

grid_t_sens <- seq(
  from = min(y_basis$rangeval),
  to   = max(y_basis$rangeval),
  length.out = 150
)

# ----------------------------------------------------------
# 3. Función para ajustar cada escenario de sensibilidad
# ----------------------------------------------------------

calcular_escenario_sensibilidad <- function(nombre_escenario, lambda_s, lambda_t) {
  
  # Coeficiente bivariado beta(s,t)
  beta_par_temp <- fda::bifdPar(
    beta_coef_base,
    fda::int2Lfd(2),
    fda::int2Lfd(2),
    lambda_s,
    lambda_t
  )
  
  # Lista de coeficientes del modelo funcional
  beta_list_temp <- list(
    fda::fdPar(
      yfd_rend_centrado$basis,
      Lfdobj = 2,
      lambda = 1e5
    ),
    beta_par_temp
  )
  
  # Modelo funcional
  modelo_temp <- fda::linmod(
    xfd_vol_centrado,
    yfd_rend_centrado,
    beta_list_temp
  )
  
  # Evaluar beta(t,s)
  beta_mat_temp <- fda::eval.bifd(
    grid_s_sens,
    grid_t_sens,
    modelo_temp$beta1estbifd
  )
  
  # Evaluar R2 funcional medio
  y_estim_temp <- fda::eval.fd(grid_t_sens, modelo_temp$yhatfdobj)
  y_obser_temp <- fda::eval.fd(grid_t_sens, yfd_rend_centrado)
  
  resid_temp <- y_obser_temp - y_estim_temp
  
  var_y_temp <- apply(y_obser_temp, 1, var, na.rm = TRUE)
  var_res_temp <- apply(resid_temp, 1, var, na.rm = TRUE)
  
  r2_t_temp <- 1 - var_res_temp / var_y_temp
  r2_medio_temp <- mean(r2_t_temp[is.finite(r2_t_temp)], na.rm = TRUE)
  
  # Importante:
  # t(beta_mat_temp) corrige la orientación visual:
  # eje x = s volumen
  # eje y = t rendimiento
  beta_mat_plot <- beta_mat_temp
  
  df_temp <- expand.grid(
    s = grid_s_sens,
    t = grid_t_sens
  ) %>%
    mutate(
      beta = as.vector(beta_mat_plot),
      escenario = nombre_escenario,
      lambda_s = lambda_s,
      lambda_t = lambda_t,
      r2_medio = r2_medio_temp
    )
  
  return(df_temp)
}

# ----------------------------------------------------------
# 4. Construir dataframe completo de sensibilidad
# ----------------------------------------------------------

df_sensibilidad_plot <- do.call(
  rbind,
  lapply(seq_len(nrow(escenarios_sensibilidad)), function(i) {
    calcular_escenario_sensibilidad(
      nombre_escenario = escenarios_sensibilidad$escenario[i],
      lambda_s = escenarios_sensibilidad$lambda_s[i],
      lambda_t = escenarios_sensibilidad$lambda_t[i]
    )
  })
)

# ----------------------------------------------------------
# 5. Etiquetas de los cuadros superiores
# ----------------------------------------------------------



# Función para escribir 1e3 como 10^3
formato_potencia_10 <- function(x) {
  exponente <- round(log10(x))
  paste0("10^", exponente)
}

df_sensibilidad_plot <- df_sensibilidad_plot %>%
  mutate(
    lambda_s_lab = formato_potencia_10(lambda_s),
    lambda_t_lab = formato_potencia_10(lambda_t),
    escenario_label = paste0(
      escenario,
      "\n",
      "lambda_s = ", lambda_s_lab,
      "\n",
      "lambda_t = ", lambda_t_lab
      #"\n",
      #"R² promedio = ", round(r2_medio, 4)
    )
  )


# ----------------------------------------------------------
# 6. Escala global completa de beta(t,s)
# ----------------------------------------------------------
# Se usa el mínimo y máximo real de los tres escenarios.
# Esto permite comparar los tres mapas con la misma escala.

beta_lim <- range(df_sensibilidad_plot$beta, na.rm = TRUE)
beta_breaks <- pretty(beta_lim, n = 7)

# Etiquetas completas, sin notación micro
beta_labels <- function(x) {
  sprintf("%.6f", x)
}

# Paleta tipo Jet, coherente con las demás superficies del código
jet_cols <- c(
  "#00007F",
  "#0000FF",
  "#007FFF",
  "#00FFFF",
  "#7FFF7F",
  "#FFFF00",
  "#FF7F00",
  "#FF0000",
  "#7F0000"
)

# ----------------------------------------------------------
# 7. Gráfico final: heatmap + líneas de contorno
# ----------------------------------------------------------

grafico_sensibilidad_contorno <- ggplot(
  df_sensibilidad_plot,
  aes(x = s, y = t, z = beta)
) +
  geom_raster(
    aes(fill = beta),
    interpolate = TRUE
  ) +
  geom_contour(
    color = "black",
    linewidth = 0.25,
    alpha = 0.65,
    bins = 12
  ) +
  facet_wrap(
    ~ escenario_label,
    nrow = 1
  ) +
  scale_fill_gradientn(
    colours = jet_cols,
    limits = beta_lim,
    breaks = beta_breaks,
    labels = beta_labels,
    oob = scales::squish,
    name = expression(hat(beta)(t,s))
  ) +
  coord_equal(expand = FALSE) +
  labs(
    title = expression(
      "Análisis de sensibilidad de la superficie " * hat(beta)(t,s)
    ),
    subtitle = "Comparación del efecto de la penalización sobre el coeficiente funcional",
    x = "s (Volumen)",
    y = "t (Rendimiento)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 18,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 12,
      margin = margin(b = 18)
    ),
    
    # Cuadros superiores de cada gráfica
    strip.background = element_rect(
      fill = "gray90",
      color = "gray45",
      linewidth = 0.6
    ),
    strip.text = element_text(
      face = "bold",
      size = 10.5,
      color = "gray20",
      lineheight = 1.05,
      margin = margin(t = 7, r = 5, b = 7, l = 5)
    ),
    
    panel.grid = element_blank(),
    panel.spacing = unit(1.4, "lines"),
    
    axis.title = element_text(
      face = "bold",
      size = 13
    ),
    axis.text = element_text(
      size = 11,
      color = "gray25"
    ),
    
    legend.position = "right",
    legend.title = element_text(
      face = "bold",
      size = 12
    ),
    legend.text = element_text(
      size = 10
    ),
    
    plot.margin = margin(
      t = 20,
      r = 25,
      b = 20,
      l = 20
    )
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barheight = unit(6, "cm"),
      barwidth = unit(0.5, "cm"),
      ticks = TRUE,
      frame.colour = "gray35",
      frame.linewidth = 0.5
    )
  )

grafico_sensibilidad_contorno


# ----------------------------------------------------------------------
# 7.3 Modelo final manual: superficie más lisa
# ----------------------------------------------------------------------

# Estos son los parámetros por análisis de sensiblidad.
lambda_s_final <- 1e3
lambda_t_final <- 1e8

beta_par_final <- fda::bifdPar(
  bifdobj = beta_coef_base,
  Lfdobjs = fda::int2Lfd(2),
  Lfdobjt = fda::int2Lfd(2),
  lambdas = lambda_s_final,
  lambdat = lambda_t_final
)

beta_list_final <- list(
  fda::fdPar(yfd_rend_centrado$basis, Lfdobj = 2, lambda = 1e5),
  beta_par_final
)

modelo_final <- fda::linmod(
  xfdobj = xfd_vol_centrado,
  yfdobj = yfd_rend_centrado,
  beta_list_final
)

# IMPORTANTE:
# Aquí se calculan zmin, zmax, ticks y hover desde beta_final_mat,
# no desde la covarianza. Esto evita que la superficie final herede escalas
# de superficies anteriores.

grid_beta_s_final <- seq(x_basis$rangeval[1], x_basis$rangeval[2], length.out = 490)
grid_beta_t_final <- seq(y_basis$rangeval[1], y_basis$rangeval[2], length.out = 490)

beta_final_mat <- fda::eval.bifd(
  grid_beta_s_final,
  grid_beta_t_final,
  modelo_final$beta1estbifd
)

sup_beta_final <- graficar_superficie_matriz(
  grid_x = grid_beta_s_final,
  grid_y = grid_beta_t_final,
  matriz_z = beta_final_mat,
  titulo_z = "beta(t,s)",
  titulo_x = "s (Volumen)",
  titulo_y = "t (Rendimiento)",
  tipo = "surface",
  colorscale = "Jet",
  tick_digits = 6
)

sup_beta_final

contorno_beta_final <- graficar_superficie_matriz(
  grid_x = grid_beta_s_final,
  grid_y = grid_beta_t_final,
  matriz_z = t(beta_final_mat),
  titulo_z = "beta(t,s)",
  titulo_x = "s (Volumen)",
  titulo_y = "t (Rendimiento)",
  tipo = "contour",
  colorscale = "Jet",
  tick_digits = 6
)

contorno_beta_final

# ============================================================
# MAPA FINAL DE BETA(s,t) 
# ============================================================

# Fechas reales de cada dominio funcional
fechas_s <- as.Date(df_volumen_log$Fecha)
fechas_t <- as.Date(df_rendimientos_final$Fecha)

# Comprobar que las dimensiones coincidan
stopifnot(
  length(fechas_s) == nrow(beta_final_mat),
  length(fechas_t) == ncol(beta_final_mat)
)

# Comprobar si ambas mallas temporales coinciden
cat("¿Las mallas temporales coinciden?: ",
    identical(fechas_s, fechas_t), "\n")

# beta_final_mat:
# filas    -> s = volumen
# columnas -> t = rendimiento

df_beta_final <- expand.grid(
  s = seq_along(fechas_s),
  t = seq_along(fechas_t)
)

df_beta_final$beta <- as.vector(beta_final_mat)

# Marcas para mostrar fechas reales en los ejes
breaks_s <- unique(round(seq(1, length(fechas_s), length.out = 9)))
breaks_t <- unique(round(seq(1, length(fechas_t), length.out = 9)))

labels_s <- format(fechas_s[breaks_s], "%b\n%Y")
labels_t <- format(fechas_t[breaks_t], "%b\n%Y")

jet_cols <- c(
  "#00007F",
  "#0000FF",
  "#007FFF",
  "#00FFFF",
  "#7FFF7F",
  "#FFFF00",
  "#FF7F00",
  "#FF0000",
  "#7F0000"
)

# Rango REAL de beta(t,s)
beta_lim <- range(
  df_beta_final$beta,
  na.rm = TRUE
)

# Marcas legibles para la leyenda
beta_breaks <- pretty(
  beta_lim,
  n = 7
)

# Conservar solamente marcas dentro del rango real
beta_breaks <- beta_breaks[
  beta_breaks >= beta_lim[1] &
    beta_breaks <= beta_lim[2]
]

# Etiquetas SIN notación científica
beta_labels <- function(x) {
  sprintf("%.6f", x)
}

cat("Rango de beta:\n")
print(beta_lim)

cat("\nMarcas de la leyenda:\n")
print(beta_breaks)

grafico_beta_fechas <- ggplot(
  df_beta_final,
  aes(x = s, y = t, z = beta)
) +
  geom_raster(
    aes(fill = beta),
    interpolate = TRUE
  ) +
  geom_contour(
    color = "black",
    linewidth = 0.22,
    alpha = 0.55,
    bins = 12
  ) +
  scale_fill_gradientn(
    colours = jet_cols,
    limits = beta_lim,
    breaks = beta_breaks,
    labels = beta_labels,
    oob = scales::squish,
    name = expression(hat(beta)(t,s))
  ) +
  scale_x_continuous(
    breaks = breaks_s,
    labels = labels_s,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = breaks_t,
    labels = labels_t,
    expand = c(0, 0)
  ) +
  labs(
    title = expression(
      "Mapa de contorno de la superficie estimada " * hat(beta)(t,s)
    ),
    x = "s: fecha del volumen de negociación",
    y = "t: fecha del rendimiento logarítmico"
  ) +
  coord_equal() +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.title = element_text(face = "bold")
  )

grafico_beta_fechas

ggplot2::ggsave(
  filename = "contorno_beta_mapa_fechas.png",
  plot = grafico_beta_fechas,
  width = 10,
  height = 8.5,
  units = "in",
  dpi = 400,
  bg = "white"
)

# ============================================================
# RESUMEN DE LA SUPERFICIE BETA POR FECHA DEL VOLUMEN s
# ============================================================

resumen_beta_s <- data.frame(
  s = seq_len(nrow(beta_final_mat)),
  Fecha = fechas_s,
  
  # Signo promedio de beta(s,t) sobre todo t
  Beta_media = rowMeans(
    beta_final_mat,
    na.rm = TRUE
  ),
  
  # Magnitud global de la asociación en cada s
  Beta_RMS = sqrt(
    rowMeans(beta_final_mat^2, na.rm = TRUE)
  ),
  
  # Proporción del dominio t donde beta es positiva
  Prop_beta_positiva = rowMeans(
    beta_final_mat > 0,
    na.rm = TRUE
  )
)



# Si s y t poseen la misma malla temporal,
# se puede observar también la diagonal contemporánea
if (nrow(beta_final_mat) == ncol(beta_final_mat)) {
  resumen_beta_s$Beta_contemporanea <- diag(beta_final_mat)
}

# Fechas históricas de interés
eventos_mercado <- data.frame(
  Fecha = as.Date(c(
    "2022-02-01",
    "2022-03-30",
    "2022-06-21",
    "2022-10-20",
    "2022-11-21",
    "2023-02-28",
    "2023-03-15",
    "2023-04-27",
    "2023-05-24"
  )),
  Evento = c(
    "Reanudación de negociación de Grupo SURA en contexto de OPAs",
    "Reanudación de Banco de Bogotá tras escisión de BAC Holding",
    "Primera jornada bursátil después de la elección presidencial",
    "Reactivación de Nutresa tras autorización de OPA de IHC",
    "Caída de Nutresa tras declararse desierta la OPA de IHC",
    "Reacción del mercado a cambios ministeriales y reforma de salud",
    "Movimiento extraordinario de BVC durante integración bursátil regional",
    "Cambio del ministro de Hacienda y reacción de los mercados",
    "Memorando de entendimiento GEA-Gilinski sobre Nutresa y Grupo SURA"
  )
)

# Función que busca el día bursátil más cercano
indice_mas_cercano <- function(fecha, vector_fechas) {
  which.min(abs(as.numeric(vector_fechas - fecha)))
}

eventos_mercado$Indice_s <- sapply(
  eventos_mercado$Fecha,
  indice_mas_cercano,
  vector_fechas = fechas_s
)

eventos_mercado$Fecha_en_datos <- fechas_s[
  eventos_mercado$Indice_s
]

# Incorporar los valores de beta correspondientes
tabla_eventos_beta <- eventos_mercado %>%
  
  dplyr::left_join(
    
    resumen_beta_s %>%
      dplyr::select(
        s,
        Beta_media,
        Beta_RMS,
        Prop_beta_positiva,
        dplyr::any_of("Beta_contemporanea")
      ),
    
    by = c("Indice_s" = "s")
  ) %>%
  
  dplyr::select(
    Indice_s,
    Fecha,
    Fecha_en_datos,
    Evento,
    Beta_media,
    Beta_RMS,
    Prop_beta_positiva,
    dplyr::any_of("Beta_contemporanea")
  )

print(tabla_eventos_beta)

View(tabla_eventos_beta)

str(tabla_eventos_beta)

print(
  tabla_eventos_beta,
  row.names = FALSE
)

tabla_eventos_beta_formateada <- tabla_eventos_beta %>%
  dplyr::mutate(
    Beta_media = round(Beta_media, 7),
    Beta_RMS = round(Beta_RMS, 7),
    Prop_beta_positiva = round(Prop_beta_positiva, 3),
    dplyr::across(
      dplyr::any_of("Beta_contemporanea"),
      ~ round(.x, 7)
    )
  )

print(
  tabla_eventos_beta_formateada,
  row.names = FALSE
)

cat("Número de fechas volumen:", length(fechas_s), "\n")
cat("Número de fechas rendimiento:", length(fechas_t), "\n")

cat(
  "¿Las fechas coinciden exactamente?:",
  identical(fechas_s, fechas_t),
  "\n"
)

# Mostrar diferencias si existen
if (!identical(fechas_s, fechas_t)) {
  
  cat("\nFechas que están en volumen pero no en rendimiento:\n")
  print(setdiff(fechas_s, fechas_t))
  
  cat("\nFechas que están en rendimiento pero no en volumen:\n")
  print(setdiff(fechas_t, fechas_s))
}

#--------------------------------------------------------------------------

# ================================================================
# MAPA DE CONTORNO BETA(t,s) + EVENTOS HISTÓRICOS
# Orientación correcta:
#   eje X = s = tiempo del volumen
#   eje Y = t = tiempo del rendimiento
# ================================================================

# beta_final_mat:
# filas    = s
# columnas = t

df_beta_mapa <- expand.grid(
  s = seq_len(nrow(beta_final_mat)),
  t = seq_len(ncol(beta_final_mat))
)

df_beta_mapa$beta <- as.vector(beta_final_mat)

# Código corto para identificar cada acontecimiento
eventos_plot <- tabla_eventos_beta %>%
  dplyr::mutate(
    Codigo = paste0("E", dplyr::row_number())
  )

# Fechas para los ejes
breaks_fechas <- unique(
  round(
    seq(
      1,
      length(fechas_s),
      length.out = 9
    )
  )
)

labels_fechas <- format(
  fechas_s[breaks_fechas],
  "%b\n%Y"
)

# Límites simétricos alrededor de cero
lim_beta <- max(
  abs(df_beta_mapa$beta),
  na.rm = TRUE
)

grafico_beta_eventos <- ggplot2::ggplot(
  df_beta_mapa,
  ggplot2::aes(
    x = s,
    y = t,
    z = beta
  )
) +
  
  # Superficie
  ggplot2::geom_raster(
    ggplot2::aes(fill = beta),
    interpolate = TRUE
  ) +
  
  # Curvas de nivel
  ggplot2::geom_contour(
    color = "black",
    linewidth = 0.20,
    alpha = 0.45,
    bins = 12
  ) +
  
  # Diagonal t = s:
  # asociación contemporánea
  ggplot2::geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    linewidth = 0.8,
    color = "black"
  ) +
  
  # Líneas verticales para las fechas de los eventos
  ggplot2::geom_vline(
    data = eventos_plot,
    ggplot2::aes(
      xintercept = Indice_s
    ),
    inherit.aes = FALSE,
    linetype = "dotted",
    linewidth = 0.55,
    color = "black"
  ) +
  
  # Código E1, E2, ..., E9
  ggplot2::geom_text(
    data = eventos_plot,
    ggplot2::aes(
      x = Indice_s,
      y = max(df_beta_mapa$t) - 5,
      label = Codigo
    ),
    inherit.aes = FALSE,
    angle = 90,
    vjust = -0.2,
    size = 3,
    fontface = "bold"
  ) +
  
  # Escala divergente centrada en cero
  ggplot2::scale_fill_gradientn(
    colours = jet_cols,
    limits = beta_lim,
    breaks = beta_breaks,
    labels = beta_labels,
    oob = scales::squish,
    name = expression(hat(beta)(t,s))
  ) +
  
  # Fechas reales
  ggplot2::scale_x_continuous(
    breaks = breaks_fechas,
    labels = labels_fechas,
    expand = c(0, 0)
  ) +
  
  ggplot2::scale_y_continuous(
    breaks = breaks_fechas,
    labels = labels_fechas,
    expand = c(0, 0)
  ) +
  
  ggplot2::labs(
    title = expression(
      "Mapa de contorno de la superficie " *
        hat(beta)(t,s)
    ),
    subtitle =
      "Líneas verticales: eventos de mercado | Diagonal: asociación contemporánea",
    x = "s: fecha del volumen de negociación",
    y = "t: fecha del rendimiento logarítmico"
  ) +
  
  ggplot2::coord_equal(
    expand = FALSE
  ) +
  
  ggplot2::theme_bw(
    base_size = 12
  ) +
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold"
    ),
    
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5
    ),
    
    legend.position = "right",
    
    legend.title = ggplot2::element_text(
      face = "bold",
      size = 11
    ),
    
    legend.text = ggplot2::element_text(
      size = 10
    ),
    
    panel.grid = ggplot2::element_blank()
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barheight = grid::unit(6, "cm"),
      barwidth = grid::unit(0.55, "cm"),
      ticks = TRUE,
      frame.colour = "gray30",
      frame.linewidth = 0.5
    )
  )

grafico_beta_eventos

tabla_eventos_figura <- eventos_plot %>%
  dplyr::select(
    Codigo,
    Fecha,
    Evento,
    Beta_media,
    Beta_RMS,
    Prop_beta_positiva,
    Beta_contemporanea
  )

print(tabla_eventos_figura)

tabla_clave_eventos <- eventos_plot %>%
  dplyr::select(
    Codigo,
    Fecha,
    Evento,
    Beta_media,
    Beta_RMS,
    Prop_beta_positiva,
    Beta_contemporanea
  ) %>%
  dplyr::mutate(
    Beta_media = sprintf("%.6f", Beta_media),
    Beta_RMS = sprintf("%.6f", Beta_RMS),
    Prop_beta_positiva = sprintf(
      "%.1f%%",
      100 * Prop_beta_positiva
    ),
    Beta_contemporanea = sprintf(
      "%.6f",
      Beta_contemporanea
    )
  )

print(
  tabla_clave_eventos,
  row.names = FALSE
)

View(tabla_clave_eventos)


# ==============================================================================
# FIGURA 1
# MAPA DE CONTORNO beta(t,s) CON EVENTOS
# ==============================================================================

# Menos fechas en los ejes para mejorar legibilidad
breaks_mapa <- unique(
  round(
    seq(
      1,
      length(fechas_s),
      length.out = 7
    )
  )
)

labels_mapa <- paste0(
  meses_es[
    as.integer(
      format(fechas_s[breaks_mapa], "%m")
    )
  ],
  "\n",
  format(fechas_s[breaks_mapa], "%Y")
)

grafico_beta_eventos <- ggplot2::ggplot(
  df_beta_mapa,
  ggplot2::aes(
    x = s,
    y = t,
    z = beta
  )
) +
  
  # Superficie
  ggplot2::geom_raster(
    ggplot2::aes(fill = beta),
    interpolate = TRUE
  ) +
  
  # Curvas de nivel secundarias
  ggplot2::geom_contour(
    color = "black",
    linewidth = 0.20,
    alpha = 0.40,
    bins = 14
  ) +
  
  # Contorno beta = 0
  ggplot2::geom_contour(
    breaks = 0,
    color = "black",
    linewidth = 0.85,
    alpha = 1
  ) +
  
  # Diagonal contemporánea t = s
  ggplot2::geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    linewidth = 0.85,
    color = "black"
  ) +
  
  # Eventos
  ggplot2::geom_vline(
    data = eventos_plot,
    ggplot2::aes(
      xintercept = Indice_s
    ),
    inherit.aes = FALSE,
    linetype = "dotted",
    linewidth = 0.55,
    color = "black",
    alpha = 0.8
  ) +
  
  # Etiquetas E1-E9
  ggplot2::geom_text(
    data = eventos_plot,
    ggplot2::aes(
      x = Indice_s,
      y = max(df_beta_mapa$t) - 8,
      label = Codigo
    ),
    inherit.aes = FALSE,
    angle = 90,
    vjust = -0.15,
    size = 4,
    fontface = "bold",
    color = "black"
  ) +
  
  # Paleta original tipo Jet
  ggplot2::scale_fill_gradientn(
    colours = jet_cols,
    limits = beta_lim,
    breaks = beta_breaks,
    labels = beta_labels,
    oob = scales::squish,
    name = expression(hat(beta)(t,s))
  ) +
  
  # Fechas
  ggplot2::scale_x_continuous(
    breaks = breaks_mapa,
    labels = labels_mapa,
    expand = c(0, 0)
  ) +
  
  ggplot2::scale_y_continuous(
    breaks = breaks_mapa,
    labels = labels_mapa,
    expand = c(0, 0)
  ) +
  
  ggplot2::labs(
    title = expression(
      "Mapa de contorno de la superficie estimada " *
        hat(beta)(t,s)
    ),
    subtitle = paste0(
      "E1-E9: eventos de mercado  |  ",
      "línea discontinua: t = s  |  ",
      "línea continua gruesa: ",
      "\u03B2(t,s) = 0"
    ),
    x = "s: fecha del volumen de negociación",
    y = "t: fecha del rendimiento logarítmico"
  ) +
  
  ggplot2::coord_equal(
    expand = FALSE
  ) +
  
  ggplot2::theme_bw(
    base_size = 13
  ) +
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      size = 16,
      margin = ggplot2::margin(b = 6)
    ),
    
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5,
      size = 11,
      margin = ggplot2::margin(b = 12)
    ),
    
    axis.title = ggplot2::element_text(
      face = "bold",
      size = 13
    ),
    
    axis.text = ggplot2::element_text(
      size = 10
    ),
    
    panel.grid = ggplot2::element_blank(),
    
    legend.position = "right",
    
    legend.title = ggplot2::element_text(
      face = "bold",
      size = 11
    ),
    
    legend.text = ggplot2::element_text(
      size = 10
    ),
    
    plot.margin = ggplot2::margin(
      t = 15,
      r = 20,
      b = 15,
      l = 15
    )
  ) +
  
  ggplot2::guides(
    fill = ggplot2::guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barheight = grid::unit(7, "cm"),
      barwidth = grid::unit(0.65, "cm"),
      ticks = TRUE,
      frame.colour = "gray30",
      frame.linewidth = 0.5
    )
  )

grafico_beta_eventos

ggplot2::ggsave(
  filename = "Figura_beta_mapa_eventos.png",
  plot = grafico_beta_eventos,
  width = 10,
  height = 8.5,
  units = "in",
  dpi = 400,
  bg = "white"
)


# ==============================================================================
# POSICIONES DE LAS ETIQUETAS PARA LOS GRÁFICOS TEMPORALES
# ==============================================================================

rango_media <- diff(
  range(
    resumen_beta_s$Beta_media,
    na.rm = TRUE
  )
)

eventos_plot$Etiqueta_media <- eventos_plot$Beta_media +
  rep(
    c(0.08, -0.11),
    length.out = nrow(eventos_plot)
  ) * rango_media

# ==============================================================================
# FIGURA 2
# DIRECCIÓN PROMEDIO beta_bar(s)
# ==============================================================================

grafico_beta_media <- ggplot2::ggplot(
  resumen_beta_s,
  ggplot2::aes(
    x = s,
    y = Beta_media
  )
) +
  
  # Línea de referencia
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.75,
    color = "gray35"
  ) +
  
  # Curva
  ggplot2::geom_line(
    linewidth = 1.15,
    color = "black"
  ) +
  
  # Eventos
  ggplot2::geom_vline(
    data = eventos_plot,
    ggplot2::aes(
      xintercept = Indice_s
    ),
    inherit.aes = FALSE,
    linetype = "dotted",
    linewidth = 0.55,
    color = "gray40"
  ) +
  
  # Puntos de los eventos
  ggplot2::geom_point(
    data = eventos_plot,
    ggplot2::aes(
      x = Indice_s,
      y = Beta_media
    ),
    inherit.aes = FALSE,
    size = 3.2,
    color = "black"
  ) +
  
  # Etiquetas E1-E9 alternadas
  ggplot2::geom_text(
    data = eventos_plot,
    ggplot2::aes(
      x = Indice_s,
      y = Etiqueta_media,
      label = Codigo
    ),
    inherit.aes = FALSE,
    size = 4,
    fontface = "bold"
  ) +
  
  ggplot2::scale_x_continuous(
    breaks = breaks_fechas,
    labels = labels_fechas,
    expand = ggplot2::expansion(
      mult = c(0.01, 0.01)
    )
  ) +
  
  ggplot2::scale_y_continuous(
    labels = function(x) {
      sprintf("%.6f", x)
    },
    expand = ggplot2::expansion(
      mult = c(0.12, 0.12)
    )
  ) +
  
  ggplot2::labs(
    title = expression(
      "Dirección promedio de la asociación funcional " *
        bar(beta)(s)
    ),
    subtitle = paste0(
      "Valores positivos y negativos indican la dirección ",
      "promedio de la asociación para cada fecha s del volumen"
    ),
    x = "Fecha del volumen de negociación",
    y = expression(bar(beta)(s))
  ) +
  
  ggplot2::theme_bw(
    base_size = 13
  ) +
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5,
      size = 11,
      margin = ggplot2::margin(b = 10)
    ),
    
    axis.title = ggplot2::element_text(
      face = "bold",
      size = 13
    ),
    
    axis.text = ggplot2::element_text(
      size = 10
    ),
    
    panel.grid.minor = ggplot2::element_blank(),
    
    plot.margin = ggplot2::margin(
      15, 20, 15, 20
    )
  )

grafico_beta_media

ggplot2::ggsave(
  filename = "Figura_beta_media_eventos.png",
  plot = grafico_beta_media,
  width = 11,
  height = 5.5,
  units = "in",
  dpi = 400,
  bg = "white"
)

# ==============================================================================
# POSICIONES PARA ETIQUETAS DEL RMS
# ==============================================================================

rango_rms <- diff(
  range(
    resumen_beta_s$Beta_RMS,
    na.rm = TRUE
  )
)

eventos_plot$Etiqueta_RMS <- eventos_plot$Beta_RMS +
  rep(
    c(0.08, -0.10),
    length.out = nrow(eventos_plot)
  ) * rango_rms


# ==============================================================================
# FIGURA 3
# INTENSIDAD FUNCIONAL beta_RMS(s)
# ==============================================================================

grafico_beta_rms <- ggplot2::ggplot(
  resumen_beta_s,
  ggplot2::aes(
    x = s,
    y = Beta_RMS
  )
) +
  
  # Curva de intensidad
  ggplot2::geom_line(
    linewidth = 1.15,
    color = "black"
  ) +
  
  # Eventos
  ggplot2::geom_vline(
    data = eventos_plot,
    ggplot2::aes(
      xintercept = Indice_s
    ),
    inherit.aes = FALSE,
    linetype = "dotted",
    linewidth = 0.55,
    color = "gray40"
  ) +
  
  # Puntos
  ggplot2::geom_point(
    data = eventos_plot,
    ggplot2::aes(
      x = Indice_s,
      y = Beta_RMS
    ),
    inherit.aes = FALSE,
    size = 3.2,
    color = "black"
  ) +
  
  # Etiquetas E1-E9
  ggplot2::geom_text(
    data = eventos_plot,
    ggplot2::aes(
      x = Indice_s,
      y = Etiqueta_RMS,
      label = Codigo
    ),
    inherit.aes = FALSE,
    size = 4,
    fontface = "bold"
  ) +
  
  ggplot2::scale_x_continuous(
    breaks = breaks_fechas,
    labels = labels_fechas,
    expand = ggplot2::expansion(
      mult = c(0.01, 0.01)
    )
  ) +
  
  ggplot2::scale_y_continuous(
    labels = function(x) {
      sprintf("%.6f", x)
    },
    expand = ggplot2::expansion(
      mult = c(0.05, 0.14)
    )
  ) +
  
  ggplot2::labs(
    title = expression(
      "Intensidad de la asociación funcional " *
        beta[RMS](s)
    ),
    subtitle = paste0(
      "Valores más altos identifican ventanas del volumen ",
      "con asociaciones de mayor magnitud con la trayectoria del rendimiento"
    ),
    x = "Fecha del volumen de negociación",
    y = expression(beta[RMS](s))
  ) +
  
  ggplot2::theme_bw(
    base_size = 13
  ) +
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5,
      size = 11,
      margin = ggplot2::margin(b = 10)
    ),
    
    axis.title = ggplot2::element_text(
      face = "bold",
      size = 13
    ),
    
    axis.text = ggplot2::element_text(
      size = 10
    ),
    
    panel.grid.minor = ggplot2::element_blank(),
    
    plot.margin = ggplot2::margin(
      15, 20, 15, 20
    )
  )

grafico_beta_rms
ggplot2::ggsave(
  filename = "Figura_beta_RMS_eventos.png",
  plot = grafico_beta_rms,
  width = 11,
  height = 5.5,
  units = "in",
  dpi = 400,
  bg = "white"
)


# ----------------------------------------------------------------------
# 7.4 R2 funcional del modelo final
# ----------------------------------------------------------------------

grid_r2_final <- seq(y_basis$rangeval[1], y_basis$rangeval[2], length.out = 490)

y_estim_final <- fda::eval.fd(grid_r2_final, modelo_final$yhatfdobj)
y_obser_final <- fda::eval.fd(grid_r2_final, yfd_rend_centrado)

residuos_final <- y_obser_final - y_estim_final

R2_t_final <- calcular_R2_t(
  y_observado = y_obser_final,
  y_estimado = y_estim_final
)

ylim_R2_final <- c(min(R2_t_final, 0, na.rm = TRUE), 1)

plot(
  grid_r2_final,
  R2_t_final,
  type = "l",
  lwd = 2,
  col = "black",
  ylim = ylim_R2_final,
  main = "Modelo con los parámetros finales",
  ylab = "Coeficiente de determinación funcional",
  xlab = "Tiempo (días)"
)

abline(h = mean(R2_t_final, na.rm = TRUE), col = "red", lty = 2)

legend(
  "bottomright",
  legend = c(
    "Coeficiente de determinación",
    paste("Media:", round(mean(R2_t_final, na.rm = TRUE), 3))
  ),
  col = c("black", "red"),
  lty = c(1, 2)
)


# ==============================================================================
# 8. DIAGNÓSTICO DEL MODELO FINAL
# ==============================================================================

# ----------------------------------------------------------------------
# 8.1 R2 por serie y comparación observado vs estimado
# ----------------------------------------------------------------------

# Residuales del modelo CENTRADO
fd_residuo_centrado <- yfd_rend_centrado - modelo_final$yhatfdobj

# Integral del error cuadrático por empresa
SSE_serie_fd <- diag(
  fda::inprod(
    fd_residuo_centrado,
    fd_residuo_centrado
  )
)

# Variabilidad total respecto de la media transversal:
MSV_serie_fd <- diag(
  fda::inprod(
    yfd_rend_centrado,
    yfd_rend_centrado
  )
)
# Coeficiente de determinación individual
R2_serie_fd <- 1 - SSE_serie_fd / MSV_serie_fd

names(R2_serie_fd) <- colnames(y_obser_final)

tabla_R2_verificacion <- data.frame(
  Empresa = names(R2_serie_fd),
  SSE_i   = SSE_serie_fd,
  MSV_i   = MSV_serie_fd,
  R2_i    = R2_serie_fd
)
print(tabla_R2_verificacion)

hist(
  R2_serie_fd,
  breaks = "Sturges",
  main = "Distribución de los coeficientes de determinación individual",
  xlab = "Coeficiente de determinación",
  ylab = "Frecuencia"
)

hist(
  R2_t_final,
  main = "Histograma del coeficiente de determinación funcional",
  xlab = "Coeficiente de determinación",
  ylab = "Frecuencia"
)

RMS_modelo <- colMeans(
  (y_obser_final - y_estim_final)^2,
  na.rm = TRUE
)

MSV_modelo <- colMeans(
  y_obser_final^2,
  na.rm = TRUE
)

plot(
  MSV_modelo,
  RMS_modelo,
  xlim = c(0, max(MSV_modelo, na.rm = TRUE) * 1.1),
  ylim = c(0, max(MSV_modelo, na.rm = TRUE) * 1.1),
  main = "Evaluación de la varianza explicada",
  xlab = "Varianza total",
  ylab = "Varianza residual",
  pch = 19,
  col = "black",
  cex = 1.2,
  las = 1
)

abline(a = 0, b = 1, col = "black", lwd = 2)
abline(a = 0, b = 0.5, col = "black", lwd = 2, lty = 2)
abline(a = 0, b = 0.2, col = "black", lwd = 2, lty = 3)

legend(
  "topleft",
  legend = c("Observaciones", "R² = 0", "R² = 0.5", "R² = 0.8"),
  col = c("black", "black", "black", "black"),
  pch = c(19, NA, NA, NA),
  lty = c(NA, 1, 2, 3),
  lwd = 2,
  bty = "n"
)
# ----------------------------------------------------------------------
# 8.2 Residuales como funciones
# ----------------------------------------------------------------------

fd_error_modelo <- yfd_rend_centrado - modelo_final$yhatfdobj

plot(
  fd_error_modelo,
  main = "Pronóstico de las curvas con el modelo funcional"
)

boxplot.fd(
  fd_error_modelo,
  main = "Boxplot residuales del modelo funcional",
  xlab = "Días",
  ylab = "Residuales"
)

media_error_modelo <- fda::mean.fd(fd_error_modelo)
sd_error_modelo <- fda::std.fd(fd_error_modelo)

lines(sd_error_modelo, col = "red", lwd = 3)
lines(media_error_modelo, col = "black", lty = 2, lwd = 3)

legend(
  "topleft",
  legend = c("Media", "Desviación estándar"),
  col = c("black", "red"),
  lty = c(2, 1),
  lwd = c(3, 3),
  bty = "n"
)

# ==============================================================================
# 8.3 DIAGNÓSTICOS ADICIONALES DE SUPUESTOS
# ==============================================================================
# ----------------------------------------------------------------------
# Objetos comunes para los diagnósticos residuales
# ----------------------------------------------------------------------

# ==============================================================================
# 8.3 DIAGNÓSTICOS ADICIONALES DE SUPUESTOS
# ==============================================================================

grid_diag <- seq(
  yfd_rend_centrado$basis$rangeval[1],
  yfd_rend_centrado$basis$rangeval[2],
  length.out = 490
)

matriz_observada_diag <- fda::eval.fd(
  grid_diag,
  yfd_rend_centrado
)

matriz_predicha_diag <- fda::eval.fd(
  grid_diag,
  modelo_final$yhatfdobj
)

matriz_residuos_diag <-
  matriz_observada_diag -
  matriz_predicha_diag

# Objeto funcional residual coherente con el modelo centrado
fd_error_modelo <-
  yfd_rend_centrado -
  modelo_final$yhatfdobj

stopifnot(
  all(dim(matriz_observada_diag) == c(490, 13)),
  all(dim(matriz_predicha_diag) == c(490, 13)),
  all(dim(matriz_residuos_diag) == c(490, 13)),
  length(grid_diag) == 490
)

# ----------------------------------------------------------------------
# 1. Residuales vs valores ajustados: diagnóstico de linealidad funcional
# ----------------------------------------------------------------------

res_vec <- as.vector(matriz_residuos_diag)
fit_vec <- as.vector(matriz_predicha_diag)

df_res_fit <- data.frame(
  Ajustado = fit_vec,
  Residual = res_vec
) %>%
  dplyr::filter(is.finite(Ajustado), is.finite(Residual))

ggplot(df_res_fit, aes(x = Ajustado, y = Residual)) +
  geom_point(alpha = 0.45, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Linealidad funcional",
    x = "Valor ajustado",
    y = "Residuales"
  ) +
  theme(
    # Centra y pone en negrita el título
    plot.title = element_text(hjust = 0.5, face = "bold"),
    
    # Asegura fondos blancos
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA),
    
    # AQUÍ SE CREA EL RECUADRO ALREDEDOR:
    # fill = NA es crucial para que no tape los puntos de la gráfica
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

# 2. Media cero del error funcional
media_res_t <- rowMeans(matriz_residuos_diag, na.rm = TRUE)
sd_res_t <- apply(matriz_residuos_diag, 1, sd, na.rm = TRUE)
n_curvas <- ncol(matriz_residuos_diag)

df_media_res <- data.frame(
  Tiempo = grid_diag,
  MediaResidual = media_res_t,
  BandaSup = media_res_t + 1.96 * sd_res_t / sqrt(n_curvas),
  BandaInf = media_res_t - 1.96 * sd_res_t / sqrt(n_curvas)
)

ggplot(df_media_res, aes(x = Tiempo, y = MediaResidual)) +
  geom_ribbon(aes(ymin = BandaInf, ymax = BandaSup), alpha = 0.25) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Curva media residual con banda aproximada",
    x = "Tiempo",
    y = expression(bar(epsilon)(t))
  ) +
  theme(
    # Centra y pone en negrita el título
    plot.title = element_text(hjust = 0.5, face = "bold"),
    
    # Asegura fondos blancos
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA),
    
    # AQUÍ SE CREA EL RECUADRO ALREDEDOR:
    # fill = NA es crucial para que no tape los puntos de la gráfica
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

# 3. Homocedasticidad funcional: varianza residual por empresa
tabla_var_residual <- data.frame(
  Empresa = colnames(matriz_residuos_diag),
  RMS_residual = sqrt(colMeans(matriz_residuos_diag^2, na.rm = TRUE)),
  Varianza_residual = apply(matriz_residuos_diag, 2, var, na.rm = TRUE)
) %>%
  dplyr::arrange(dplyr::desc(RMS_residual))

print(tabla_var_residual)

# 4. Superficie de covarianza residual
cov_residual <- fda::var.fd(fd_error_modelo)

grid_cov_res <- seq(
  fd_error_modelo$basis$rangeval[1],
  fd_error_modelo$basis$rangeval[2],
  length.out = 150
)

matriz_cov_res <- fda::eval.bifd(
  grid_cov_res,
  grid_cov_res,
  cov_residual
)

sup_cov_res <- graficar_superficie_matriz(
  grid_x = grid_cov_res,
  grid_y = grid_cov_res,
  matriz_z = matriz_cov_res,
  titulo_z = "Cov residual",
  titulo_x = "s",
  titulo_y = "t",
  tipo = "surface",
  colorscale = "Jet",
  tick_digits = 6
)

sup_cov_res

# 5. Independencia entre curvas: correlación entre residuales por empresa
cor_residual_empresas <- cor(matriz_residuos_diag, use = "pairwise.complete.obs")

corrplot::corrplot(
  cor_residual_empresas,
  method = "color",
  type = "upper",
  tl.cex = 0.7,
  title = "Correlación entre curvas residuales",
  mar = c(0, 0, 2, 0)
)

# ----------------------------------------------------------------------
# Histograma final 
# ----------------------------------------------------------------------

res_limpios <- as.vector(matriz_residuos_diag)
res_limpios <- res_limpios[is.finite(res_limpios)]

png(
  filename = "HistoResiduales.png",
  width = 1600,
  height = 1000,
  res = 180
)

par(
  mar = c(5, 5, 4, 2),
  las = 1
)

hist(
  res_limpios,
  breaks = "FD",
  col = "gray85",
  border = "black",
  main = "Distribución empírica de los residuales",
  xlab = "Residual",
  ylab = "Frecuencia"
)

abline(
  v = 0,
  lty = 2,
  lwd = 1.5,
  col = "black"
)

dev.off()

qqnorm(res_limpios, main = "QQ-plot de los residuales", xlab = "Cuantiles teóricos",
       ylab = "Cuantiles muestrales")
qqline(res_limpios, col = "red", lwd = 2)

media_res <- mean(res_limpios)
sd_res <- sd(res_limpios)

asimetria_res <- mean(((res_limpios - media_res) / sd_res)^3)
curtosis_res <- mean(((res_limpios - media_res) / sd_res)^4)
exceso_curtosis_res <- curtosis_res - 3

tabla_normalidad_res <- data.frame(
  Media = media_res,
  Desviacion = sd_res,
  Asimetria = asimetria_res,
  Curtosis = curtosis_res,
  Exceso_Curtosis = exceso_curtosis_res
)

print(tabla_normalidad_res)

# Shapiro solo como diagnóstico descriptivo, porque con muchos datos suele rechazar normalidad.
set.seed(123)
muestra_shapiro <- sample(res_limpios, size = min(5000, length(res_limpios)))
print(shapiro.test(muestra_shapiro))

# ================================================================
# COMPROBACIÓN DEL INTERCEPTO FUNCIONAL
# ================================================================

# 1. Evaluar el intercepto funcional
alpha_hat <- as.vector(
  fda::eval.fd(
    tiempo_rend,
    modelo_final$beta0estfd
  )
)

# 2. Resumen básico
cat("\n====================================\n")
cat("INTERCEPTO FUNCIONAL ESTIMADO\n")
cat("====================================\n")

cat("Mínimo alpha(t):",
    min(alpha_hat, na.rm = TRUE), "\n")

cat("Máximo alpha(t):",
    max(alpha_hat, na.rm = TRUE), "\n")

cat("Máximo valor absoluto:",
    max(abs(alpha_hat), na.rm = TRUE), "\n")

cat("Media:",
    mean(alpha_hat, na.rm = TRUE), "\n")

cat("Desviación estándar:",
    sd(alpha_hat, na.rm = TRUE), "\n")

# RMS funcional discreto del intercepto
rms_alpha <- sqrt(
  mean(alpha_hat^2, na.rm = TRUE)
)

cat("RMS alpha(t):",
    rms_alpha, "\n")


# ================================================================
# 3. COMPARAR CON LA MAGNITUD DE LA RESPUESTA CENTRADA
# ================================================================

Y_c_mat <- fda::eval.fd(
  tiempo_rend,
  yfd_rend_centrado
)

rms_Y_c <- sqrt(
  mean(
    Y_c_mat^2,
    na.rm = TRUE
  )
)

cat("\nRMS global de Y centrada:",
    rms_Y_c, "\n")

cat(
  "RMS(alpha) / RMS(Y centrada):",
  rms_alpha / rms_Y_c,
  "\n"
)

cat(
  "Porcentaje relativo:",
  100 * rms_alpha / rms_Y_c,
  "%\n"
)


# ================================================================
# 4. COMPARAR CON LOS VALORES AJUSTADOS
# ================================================================

Yhat_mat <- fda::eval.fd(
  tiempo_rend,
  modelo_final$yhatfdobj
)

rms_Yhat <- sqrt(
  mean(
    Yhat_mat^2,
    na.rm = TRUE
  )
)

cat("\nRMS global de Y ajustada:",
    rms_Yhat, "\n")

cat(
  "RMS(alpha) / RMS(Y ajustada):",
  rms_alpha / rms_Yhat,
  "\n"
)

cat(
  "Porcentaje relativo respecto al ajuste:",
  100 * rms_alpha / rms_Yhat,
  "%\n"
)
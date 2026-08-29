# ==============================================================================
# Observaciones_Extremas_Tesis.R
# ==============================================================================
# Reconstrucción reproducible de la Sección 5.1.1:
# "Revisión de observaciones extremas"
#
# PROCEDIMIENTO DOCUMENTADO:
#   1) Para cada una de las 13 empresas, seleccionar los 3 rendimientos
#      logarítmicos de mayor magnitud absoluta.
#   2) Para cada empresa, seleccionar los 3 volúmenes OBSERVADOS más altos.
#   3) Para cada empresa, seleccionar los 3 volúmenes OBSERVADOS POSITIVOS
#      más bajos.
#   4) Revisar separadamente la existencia de volúmenes iguales a cero.
#   5) Conservar todas las observaciones: la selección es descriptiva y NO
#      constituye una regla automática de eliminación de atípicos.
#
# IMPORTANTE:
#   - Este script NO usa MAD, z-scores ni pruebas formales de outliers.
#   - Los rendimientos se toman de df_rendimientos_final, es decir, después
#     de completar precios mediante LOCF y calcular los retornos.
#   - Los volúmenes extremos se seleccionan sobre df_volumen, antes de la
#     imputación GAM, para conservar el significado de "volumen observado".
#   - Las columnas de interpretación financiera quedan vacías, tal como en
#     el archivo de control original; su revisión es posterior y manual.
#
# OBJETOS QUE DEBEN EXISTIR ANTES DE EJECUTAR ESTE SCRIPT:
#   df_rendimientos_final
#   df_volumen
#
# Estos objetos son creados por FDA_tesis_Version_Final.R.
# ==============================================================================


# ==============================================================================
# 0. PAQUETES
# ==============================================================================

paquetes_revision <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "writexl"
)

faltantes_revision <- paquetes_revision[
  !vapply(paquetes_revision, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes_revision) > 0) {
  install.packages(faltantes_revision)
}

invisible(lapply(paquetes_revision, library, character.only = TRUE))


# ==============================================================================
# 1. COMPROBACIÓN DE OBJETOS DE ENTRADA
# ==============================================================================

objetos_requeridos <- c(
  "df_rendimientos_final",
  "df_volumen"
)

objetos_faltantes <- objetos_requeridos[
  !vapply(objetos_requeridos, exists, logical(1), envir = .GlobalEnv)
]

if (length(objetos_faltantes) > 0) {
  stop(
    paste0(
      "Faltan los siguientes objetos: ",
      paste(objetos_faltantes, collapse = ", "),
      ". Ejecute primero el preprocesamiento de FDA_tesis_Version_Final.R."
    )
  )
}

df_rend_revision <- df_rendimientos_final
df_vol_revision  <- df_volumen

df_rend_revision$Fecha <- as.Date(df_rend_revision$Fecha)
df_vol_revision$Fecha  <- as.Date(df_vol_revision$Fecha)

rend_cols_revision <- grep(
  "^R_",
  names(df_rend_revision),
  value = TRUE
)

vol_cols_revision <- grep(
  "^Vol_",
  names(df_vol_revision),
  value = TRUE
)

if (length(rend_cols_revision) != 13) {
  stop(
    "Se esperaban 13 columnas de rendimiento y se encontraron ",
    length(rend_cols_revision), "."
  )
}

if (length(vol_cols_revision) != 13) {
  stop(
    "Se esperaban 13 columnas de volumen y se encontraron ",
    length(vol_cols_revision), "."
  )
}


# ==============================================================================
# 2. ETIQUETAS UTILIZADAS EN EL ARCHIVO DE CONTROL
# ==============================================================================

# Se conserva deliberadamente "CorpColombia" porque así aparece en
# revision_observaciones_extremas.xlsx y en las figuras de esta revisión.

etiquetas_revision <- c(
  bancolombia  = "Bancolombia",
  bogota       = "Banco de Bogotá",
  bvc          = "Bolsa de Valores de Colombia",
  ceargos      = "Cementos Argos",
  celsia       = "Celsia S.A.",
  ecopetrol    = "Ecopetrol",
  corpcolombia = "CorpColombia",
  bolivar      = "Grupo Bolívar",
  mineros      = "Mineros",
  nutresa      = "Nutresa",
  terpel       = "Terpel",
  promigas     = "Promigas",
  suramericana = "Suramericana"
)

claves_rend <- sub("^R_", "", rend_cols_revision)
claves_vol  <- sub("^Vol_", "", vol_cols_revision)

claves_sin_etiqueta <- setdiff(
  union(claves_rend, claves_vol),
  names(etiquetas_revision)
)

if (length(claves_sin_etiqueta) > 0) {
  stop(
    "Faltan etiquetas para: ",
    paste(claves_sin_etiqueta, collapse = ", ")
  )
}


# ==============================================================================
# 3. DATOS EN FORMATO LARGO
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 Rendimientos finales
# ------------------------------------------------------------------------------

rendimientos_long <- df_rend_revision %>%
  dplyr::select(
    Fecha,
    dplyr::all_of(rend_cols_revision)
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(rend_cols_revision),
    names_to = "Empresa_clave",
    values_to = "Rendimiento"
  ) %>%
  dplyr::mutate(
    Empresa_clave = sub("^R_", "", Empresa_clave),
    Empresa = unname(etiquetas_revision[Empresa_clave])
  ) %>%
  dplyr::filter(
    !is.na(Fecha),
    is.finite(Rendimiento),
    !is.na(Empresa)
  ) %>%
  dplyr::select(
    Fecha,
    Empresa,
    Rendimiento
  )


# ------------------------------------------------------------------------------
# 3.2 Volúmenes observados
# ------------------------------------------------------------------------------

# Se usa df_volumen, NO df_volumen_imput.
# De este modo los extremos corresponden a cantidades efectivamente observadas
# y no a valores reconstruidos mediante GAM.

volumen_observado_long <- df_vol_revision %>%
  dplyr::select(
    Fecha,
    dplyr::all_of(vol_cols_revision)
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(vol_cols_revision),
    names_to = "Empresa_clave",
    values_to = "Volumen"
  ) %>%
  dplyr::mutate(
    Empresa_clave = sub("^Vol_", "", Empresa_clave),
    Empresa = unname(etiquetas_revision[Empresa_clave])
  ) %>%
  dplyr::filter(
    !is.na(Fecha),
    !is.na(Empresa)
  ) %>%
  dplyr::select(
    Fecha,
    Empresa,
    Volumen
  )


# ==============================================================================
# 4. TRES RENDIMIENTOS DE MAYOR MAGNITUD ABSOLUTA POR EMPRESA
# ==============================================================================

tabla_rendimientos_extremos <- rendimientos_long %>%
  dplyr::mutate(
    Magnitud_absoluta = abs(Rendimiento)
  ) %>%
  dplyr::group_by(Empresa) %>%
  dplyr::arrange(
    dplyr::desc(Magnitud_absoluta),
    Fecha,
    .by_group = TRUE
  ) %>%
  dplyr::slice_head(n = 3) %>%
  dplyr::mutate(
    Puesto = dplyr::row_number(),
    Direccion = dplyr::if_else(
      Rendimiento >= 0,
      "Aumento",
      "Disminución"
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    Empresa,
    Puesto
  ) %>%
  dplyr::transmute(
    Fecha,
    Empresa,
    Puesto,
    Rendimiento,
    Magnitud_absoluta,
    Direccion,
    Evento_posible = NA_character_,
    Fuente_consultada = NA_character_,
    Enlace_fuente = NA_character_,
    Nivel_confianza = NA_character_,
    Decision = "Conservar",
    Justificacion = NA_character_
  )


# ==============================================================================
# 5. TRES VOLÚMENES OBSERVADOS MÁS ALTOS POR EMPRESA
# ==============================================================================

tabla_volumen_alto <- volumen_observado_long %>%
  dplyr::filter(
    is.finite(Volumen)
  ) %>%
  dplyr::group_by(Empresa) %>%
  dplyr::arrange(
    dplyr::desc(Volumen),
    Fecha,
    .by_group = TRUE
  ) %>%
  dplyr::slice_head(n = 3) %>%
  dplyr::mutate(
    Puesto = dplyr::row_number()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    Empresa,
    Puesto
  ) %>%
  dplyr::transmute(
    Fecha,
    Empresa,
    Puesto,
    Volumen,
    Volumen_log = log1p(Volumen),
    Tipo_revision = "Volumen alto",
    Evento_posible = NA_character_,
    Fuente_consultada = NA_character_,
    Enlace_fuente = NA_character_,
    Nivel_confianza = NA_character_,
    Decision = "Conservar",
    Justificacion = NA_character_
  )


# ==============================================================================
# 6. TRES VOLÚMENES OBSERVADOS POSITIVOS MÁS BAJOS POR EMPRESA
# ==============================================================================

tabla_volumen_bajo <- volumen_observado_long %>%
  dplyr::filter(
    is.finite(Volumen),
    Volumen > 0
  ) %>%
  dplyr::group_by(Empresa) %>%
  dplyr::arrange(
    Volumen,
    Fecha,
    .by_group = TRUE
  ) %>%
  dplyr::slice_head(n = 3) %>%
  dplyr::mutate(
    Puesto = dplyr::row_number()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    Empresa,
    Puesto
  ) %>%
  dplyr::transmute(
    Fecha,
    Empresa,
    Puesto,
    Volumen,
    Volumen_log = log1p(Volumen),
    Tipo_revision = "Volumen bajo positivo",
    Evento_posible = NA_character_,
    Fuente_consultada = NA_character_,
    Enlace_fuente = NA_character_,
    Nivel_confianza = NA_character_,
    Decision = "Conservar",
    Justificacion = NA_character_
  )


# ==============================================================================
# 7. REVISIÓN SEPARADA DE VOLÚMENES IGUALES A CERO
# ==============================================================================

tabla_resumen_ceros <- volumen_observado_long %>%
  dplyr::group_by(Empresa) %>%
  dplyr::summarise(
    Numero_ceros = sum(Volumen == 0, na.rm = TRUE),
    Numero_observados = sum(!is.na(Volumen)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Porcentaje_ceros = dplyr::if_else(
      Numero_observados > 0,
      100 * Numero_ceros / Numero_observados,
      NA_real_
    )
  ) %>%
  dplyr::arrange(Empresa) %>%
  dplyr::select(
    Empresa,
    Numero_ceros,
    Porcentaje_ceros
  )

tabla_fechas_ceros <- volumen_observado_long %>%
  dplyr::filter(
    !is.na(Volumen),
    Volumen == 0
  ) %>%
  dplyr::arrange(
    Fecha,
    Empresa
  ) %>%
  dplyr::select(
    Fecha,
    Empresa,
    Volumen
  )


# ==============================================================================
# 8. RESUMEN DE FECHAS EN LAS QUE COINCIDEN EMPRESAS SELECCIONADAS
# ==============================================================================

resumir_fechas_revision <- function(tabla) {
  tabla %>%
    dplyr::group_by(Fecha) %>%
    dplyr::summarise(
      Numero_empresas = dplyr::n_distinct(Empresa),
      Empresas = paste(
        sort(unique(Empresa)),
        collapse = ", "
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      dplyr::desc(Numero_empresas),
      Fecha
    )
}

tabla_fechas_rendimientos <- resumir_fechas_revision(
  tabla_rendimientos_extremos
)

tabla_fechas_volumen_alto <- resumir_fechas_revision(
  tabla_volumen_alto
)

tabla_fechas_volumen_bajo <- resumir_fechas_revision(
  tabla_volumen_bajo
)


# ==============================================================================
# 9. COMPROBACIONES DE REPRODUCIBILIDAD
# ==============================================================================

conteo_por_empresa <- function(tabla) {
  tabla %>%
    dplyr::count(Empresa, name = "n") %>%
    dplyr::arrange(Empresa)
}

conteo_rend <- conteo_por_empresa(tabla_rendimientos_extremos)
conteo_v_alto <- conteo_por_empresa(tabla_volumen_alto)
conteo_v_bajo <- conteo_por_empresa(tabla_volumen_bajo)

stopifnot(
  nrow(tabla_rendimientos_extremos) == 39,
  nrow(tabla_volumen_alto) == 39,
  nrow(tabla_volumen_bajo) == 39,
  all(conteo_rend$n == 3),
  all(conteo_v_alto$n == 3),
  all(conteo_v_bajo$n == 3),
  all.equal(
    tabla_rendimientos_extremos$Magnitud_absoluta,
    abs(tabla_rendimientos_extremos$Rendimiento),
    tolerance = 1e-12
  ) == TRUE,
  all.equal(
    tabla_volumen_alto$Volumen_log,
    log1p(tabla_volumen_alto$Volumen),
    tolerance = 1e-12
  ) == TRUE,
  all.equal(
    tabla_volumen_bajo$Volumen_log,
    log1p(tabla_volumen_bajo$Volumen),
    tolerance = 1e-12
  ) == TRUE
)

cat("\n============================================================\n")
cat("REVISIÓN DE OBSERVACIONES EXTREMAS\n")
cat("============================================================\n")
cat("Rendimientos seleccionados :", nrow(tabla_rendimientos_extremos), "\n")
cat("Volúmenes altos            :", nrow(tabla_volumen_alto), "\n")
cat("Volúmenes bajos positivos  :", nrow(tabla_volumen_bajo), "\n")
cat(
  "Total de registros revisión:",
  nrow(tabla_rendimientos_extremos) +
    nrow(tabla_volumen_alto) +
    nrow(tabla_volumen_bajo),
  "\n"
)
cat(
  "Volúmenes iguales a cero  :",
  sum(tabla_resumen_ceros$Numero_ceros),
  "\n"
)
cat("============================================================\n\n")


# ==============================================================================
# 10. EXPORTACIÓN DEL ARCHIVO DE CONTROL
# ==============================================================================

archivo_salida_excel <- "revision_observaciones_extremas_reproducida.xlsx"

writexl::write_xlsx(
  x = list(
    Rendimientos = tabla_rendimientos_extremos,
    Volumen_alto = tabla_volumen_alto,
    Volumen_bajo = tabla_volumen_bajo,
    Resumen_ceros = tabla_resumen_ceros,
    Fechas_ceros = tabla_fechas_ceros,
    Fechas_rendimientos = tabla_fechas_rendimientos,
    Fechas_volumen_alto = tabla_fechas_volumen_alto,
    Fechas_volumen_bajo = tabla_fechas_volumen_bajo
  ),
  path = archivo_salida_excel
)

cat(
  "Archivo Excel generado:\n",
  normalizePath(
    archivo_salida_excel,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n\n"
)


# ==============================================================================
# 11. GRÁFICAS DE LA SECCIÓN 5.1.1
# ==============================================================================

meses_es_revision <- c(
  "ene", "feb", "mar", "abr", "may", "jun",
  "jul", "ago", "sep", "oct", "nov", "dic"
)

etiquetas_fecha_es <- function(x) {
  paste(
    meses_es_revision[as.integer(format(x, "%m"))],
    format(x, "%Y")
  )
}

tema_revision_extremos <- ggplot2::theme_minimal(
  base_size = 10
) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      size = 11
    ),
    legend.title = ggplot2::element_text(
      face = "bold"
    ),
    legend.position = "right",
    panel.grid.minor = ggplot2::element_blank()
  )


# ------------------------------------------------------------------------------
# 11.1 Rendimientos de mayor magnitud
# ------------------------------------------------------------------------------

grafico_revision_rendimientos <- ggplot2::ggplot(
  rendimientos_long,
  ggplot2::aes(
    x = Fecha,
    y = Rendimiento,
    color = Empresa
  )
) +
  ggplot2::geom_point(
    alpha = 0.48,
    size = 0.75
  ) +
  ggplot2::geom_point(
    data = tabla_rendimientos_extremos,
    ggplot2::aes(
      x = Fecha,
      y = Rendimiento
    ),
    inherit.aes = FALSE,
    shape = 1,
    color = "black",
    size = 2.7,
    stroke = 0.9
  ) +
  ggplot2::scale_x_date(
    date_breaks = "4 months",
    labels = etiquetas_fecha_es
  ) +
  ggplot2::labs(
    title = "Revisión de los rendimientos de mayor magnitud",
    x = "Fecha",
    y = "Rendimiento",
    color = "Empresa"
  ) +
  tema_revision_extremos

ggplot2::ggsave(
  filename = "revisionrendimientos.png",
  plot = grafico_revision_rendimientos,
  width = 10,
  height = 5.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# ------------------------------------------------------------------------------
# 11.2 Volúmenes observados más altos
# ------------------------------------------------------------------------------

grafico_revision_volumen_alto <- ggplot2::ggplot(
  volumen_observado_long %>%
    dplyr::filter(
      is.finite(Volumen),
      Volumen >= 0
    ),
  ggplot2::aes(
    x = Fecha,
    y = log1p(Volumen),
    color = Empresa
  )
) +
  ggplot2::geom_point(
    alpha = 0.48,
    size = 0.75
  ) +
  ggplot2::geom_point(
    data = tabla_volumen_alto,
    ggplot2::aes(
      x = Fecha,
      y = Volumen_log
    ),
    inherit.aes = FALSE,
    shape = 1,
    color = "black",
    size = 2.7,
    stroke = 0.9
  ) +
  ggplot2::scale_x_date(
    date_breaks = "4 months",
    labels = etiquetas_fecha_es
  ) +
  ggplot2::labs(
    title = "Revisión de los volúmenes observados más altos",
    x = "Fecha",
    y = "log(1 + Volumen)",
    color = "Empresa"
  ) +
  tema_revision_extremos

ggplot2::ggsave(
  filename = "volumenesaltos.png",
  plot = grafico_revision_volumen_alto,
  width = 10,
  height = 5.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# ------------------------------------------------------------------------------
# 11.3 Volúmenes observados positivos más bajos
# ------------------------------------------------------------------------------

grafico_revision_volumen_bajo <- ggplot2::ggplot(
  volumen_observado_long %>%
    dplyr::filter(
      is.finite(Volumen),
      Volumen >= 0
    ),
  ggplot2::aes(
    x = Fecha,
    y = log1p(Volumen),
    color = Empresa
  )
) +
  ggplot2::geom_point(
    alpha = 0.48,
    size = 0.75
  ) +
  ggplot2::geom_point(
    data = tabla_volumen_bajo,
    ggplot2::aes(
      x = Fecha,
      y = Volumen_log
    ),
    inherit.aes = FALSE,
    shape = 1,
    color = "black",
    size = 2.7,
    stroke = 0.9
  ) +
  ggplot2::scale_x_date(
    date_breaks = "4 months",
    labels = etiquetas_fecha_es
  ) +
  ggplot2::labs(
    title = "Revisión de los volúmenes observados más bajos",
    x = "Fecha",
    y = "log(1 + Volumen)",
    color = "Empresa"
  ) +
  tema_revision_extremos

ggplot2::ggsave(
  filename = "volumenesbajos.png",
  plot = grafico_revision_volumen_bajo,
  width = 10,
  height = 5.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# ==============================================================================
# 12. VALIDACIÓN OPCIONAL CONTRA EL EXCEL ORIGINAL
# ==============================================================================

# Este bloque NO interviene en la selección. Solo comprueba que, si el archivo
# original está disponible, los resultados reconstruidos coincidan con él.
#
# Puede cambiar la ruta si el archivo se encuentra en otra carpeta.

archivo_referencia <- file.path("datos", "control", "revision_observaciones_extremas.xlsx")

normalizar_fecha_excel <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }

  as.Date(x)
}

comparar_hoja_revision <- function(
  nombre_hoja,
  tabla_generada,
  columnas,
  tolerancia = 1e-10
) {
  referencia <- readxl::read_excel(
    archivo_referencia,
    sheet = nombre_hoja
  )

  referencia <- as.data.frame(referencia)

  # Conservar únicamente las columnas que forman parte de la validación.
  referencia <- referencia[, columnas, drop = FALSE]
  generada <- tabla_generada[, columnas, drop = FALSE]

  # Caso especial importante:
  # si ambas tablas no tienen filas, se consideran equivalentes siempre que
  # tengan los mismos nombres de columna. Esto evita falsos "REVISAR" por la
  # inferencia de tipos que hace readxl en hojas que contienen solo encabezados.
  if (nrow(referencia) == 0 && nrow(generada) == 0) {
    resultado <- identical(
      names(generada),
      names(referencia)
    )

    cat(
      sprintf(
        "%-23s : %s\n",
        nombre_hoja,
        ifelse(resultado, "COINCIDE", "REVISAR")
      )
    )

    return(invisible(resultado))
  }

  if ("Fecha" %in% columnas) {
    referencia$Fecha <- normalizar_fecha_excel(
      referencia$Fecha
    )
    generada$Fecha <- normalizar_fecha_excel(
      generada$Fecha
    )
  }

  resultado <- isTRUE(
    all.equal(
      generada,
      referencia,
      check.attributes = FALSE,
      tolerance = tolerancia
    )
  )

  cat(
    sprintf(
      "%-23s : %s\n",
      nombre_hoja,
      ifelse(resultado, "COINCIDE", "REVISAR")
    )
  )

  invisible(resultado)
}

if (file.exists(archivo_referencia)) {

  if (!requireNamespace("readxl", quietly = TRUE)) {
    install.packages("readxl")
  }

  cat("\n============================================================\n")
  cat("VALIDACIÓN CONTRA EL EXCEL ORIGINAL\n")
  cat("============================================================\n")

  validaciones <- c(
    comparar_hoja_revision(
      "Rendimientos",
      tabla_rendimientos_extremos,
      c(
        "Fecha",
        "Empresa",
        "Puesto",
        "Rendimiento",
        "Magnitud_absoluta",
        "Direccion",
        "Decision"
      )
    ),
    comparar_hoja_revision(
      "Volumen_alto",
      tabla_volumen_alto,
      c(
        "Fecha",
        "Empresa",
        "Puesto",
        "Volumen",
        "Volumen_log",
        "Tipo_revision",
        "Decision"
      )
    ),
    comparar_hoja_revision(
      "Volumen_bajo",
      tabla_volumen_bajo,
      c(
        "Fecha",
        "Empresa",
        "Puesto",
        "Volumen",
        "Volumen_log",
        "Tipo_revision",
        "Decision"
      )
    ),
    comparar_hoja_revision(
      "Resumen_ceros",
      tabla_resumen_ceros,
      c(
        "Empresa",
        "Numero_ceros",
        "Porcentaje_ceros"
      )
    ),
    comparar_hoja_revision(
      "Fechas_ceros",
      tabla_fechas_ceros,
      c(
        "Fecha",
        "Empresa",
        "Volumen"
      )
    ),
    comparar_hoja_revision(
      "Fechas_rendimientos",
      tabla_fechas_rendimientos,
      c(
        "Fecha",
        "Numero_empresas",
        "Empresas"
      )
    ),
    comparar_hoja_revision(
      "Fechas_volumen_alto",
      tabla_fechas_volumen_alto,
      c(
        "Fecha",
        "Numero_empresas",
        "Empresas"
      )
    ),
    comparar_hoja_revision(
      "Fechas_volumen_bajo",
      tabla_fechas_volumen_bajo,
      c(
        "Fecha",
        "Numero_empresas",
        "Empresas"
      )
    )
  )

  cat("------------------------------------------------------------\n")

  if (all(validaciones)) {
    cat("RESULTADO: las ocho hojas coinciden con el archivo original.\n")
  } else {
    cat(
      "RESULTADO: al menos una hoja difiere. ",
      "Revise la(s) hoja(s) marcada(s) como REVISAR.\n",
      sep = ""
    )
  }

  cat("============================================================\n\n")

} else {

  cat(
    "\nNo se ejecutó la validación contra el Excel original porque no se encontró:\n",
    archivo_referencia,
    "\n"
  )
}


# ==============================================================================
# 13. OBJETOS FINALES DISPONIBLES EN EL ENTORNO
# ==============================================================================

# tabla_rendimientos_extremos
# tabla_volumen_alto
# tabla_volumen_bajo
# tabla_resumen_ceros
# tabla_fechas_ceros
# tabla_fechas_rendimientos
# tabla_fechas_volumen_alto
# tabla_fechas_volumen_bajo
#
# grafico_revision_rendimientos
# grafico_revision_volumen_alto
# grafico_revision_volumen_bajo
#
# Archivo Excel:
# revision_observaciones_extremas_reproducida.xlsx
#
# Figuras:
# revisionrendimientos.png
# volumenesaltos.png
# volumenesbajos.png
# ==============================================================================

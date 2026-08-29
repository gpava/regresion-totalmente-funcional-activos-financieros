# ==============================================================================
# 00_VERIFICAR_ARCHIVOS.R
# Verifica que el repositorio tenga los insumos mínimos antes de ejecutar.
# ==============================================================================

archivos_datos <- c(
  "Bancolombia.csv",
  "Bancodebogota.csv",
  "Bolsavaloresdecolombia.csv",
  "CementosArgos.csv",
  "CelsiaSA.csv",
  "Ecopetrol.csv",
  "Corporacionfinancieradecolombia.csv",
  "Grupobolivar.csv",
  "Mineros SA (MAS).csv",
  "Nutresa (NCH).csv",
  "Organizacion Terpel SA (TPL).csv",
  "Promigas (PMG).csv",
  "Suramericana (SIS).csv"
)

rutas_datos <- file.path("datos", "originales", archivos_datos)
faltantes <- rutas_datos[!file.exists(rutas_datos)]

if (length(faltantes) > 0) {
  stop(
    "Faltan archivos de datos:\n",
    paste(faltantes, collapse = "\n")
  )
}

archivo_control <- file.path(
  "datos", "control", "revision_observaciones_extremas.xlsx"
)

if (!file.exists(archivo_control)) {
  stop("Falta el archivo de control: ", archivo_control)
}

cat("Verificación de archivos: OK\n")
cat("13 archivos fuente encontrados.\n")
cat("Archivo de control de observaciones extremas encontrado.\n")

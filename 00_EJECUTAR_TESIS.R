# ==============================================================================
# 00_EJECUTAR_TESIS.R
# Ejecución reproducible de los análisis finales de la tesis.
# ==============================================================================

cat("============================================================\n")
cat("REPRODUCCIÓN DE LA TESIS\n")
cat("============================================================\n")

if (!file.exists("RegresionTotalmenteFuncional.Rproj")) {
  stop(
    "Ejecute este archivo desde la raíz del repositorio. ",
    "Abra RegresionTotalmenteFuncional.Rproj en RStudio."
  )
}

dir.create("resultados", showWarnings = FALSE, recursive = TRUE)

source("00_VERIFICAR_ARCHIVOS.R", encoding = "UTF-8")

cat("\n[1/4] Modelo FDA y regresión totalmente funcional\n")
source(
  file.path("codigo", "FDA_tesis_Version_Final.R"),
  encoding = "UTF-8"
)

cat("\n[2/4] Revisión de observaciones extremas\n")
source(
  file.path("codigo", "Observaciones_Extremas_Tesis.R"),
  encoding = "UTF-8"
)

cat("\n[3/4] Auditoría de la sección 5.1\n")
source(
  file.path("codigo", "AuditoriaTesis.R"),
  encoding = "UTF-8"
)

cat("\n[4/4] Modelos GARCH(p,q)\n")
source(
  file.path("codigo", "GARCH_pq_tesis.R"),
  encoding = "UTF-8"
)

# Guardar información del entorno al finalizar.
sink(file.path("resultados", "sessionInfo.txt"))
print(sessionInfo())
sink()

cat("\n============================================================\n")
cat("EJECUCIÓN FINALIZADA\n")
cat("Información del entorno: resultados/sessionInfo.txt\n")
cat("============================================================\n")

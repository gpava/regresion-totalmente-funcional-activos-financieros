# ==============================================================================
# 99_VERIFICAR_RESULTADOS_CLAVE.R
# Comprobaciones mínimas sobre resultados que fueron auditados durante la tesis.
# Ejecutar después de 00_EJECUTAR_TESIS.R.
# ==============================================================================

# R_i^2 definitivos del modelo totalmente funcional.
r2_esperado <- c(
  bancolombia  = 0.16952766,
  bogota       = 0.18300131,
  bvc          = 0.23660914,
  ceargos      = 0.13241692,
  celsia       = 0.04736844,
  ecopetrol    = 0.44374655,
  corpcolombia = 0.20724600,
  bolivar      = 0.12743778,
  mineros      = 0.16323342,
  nutresa      = 0.16058473,
  terpel       = 0.09303103,
  promigas     = 0.12916549,
  suramericana = 0.15164518
)

if (!exists("R2_serie_fd")) {
  stop("No existe R2_serie_fd. Ejecute primero 00_EJECUTAR_TESIS.R.")
}

r2_obtenido <- R2_serie_fd[names(r2_esperado)]

if (!isTRUE(all.equal(
  unname(r2_obtenido),
  unname(r2_esperado),
  tolerance = 1e-6,
  check.attributes = FALSE
))) {
  stop("Los R_i^2 no coinciden con los valores finales auditados.")
}

if (exists("tabla_rendimientos_extremos")) {
  stopifnot(nrow(tabla_rendimientos_extremos) == 39)
}
if (exists("tabla_volumen_alto")) {
  stopifnot(nrow(tabla_volumen_alto) == 39)
}
if (exists("tabla_volumen_bajo")) {
  stopifnot(nrow(tabla_volumen_bajo) == 39)
}
if (exists("tabla_resumen_ceros")) {
  stopifnot(sum(tabla_resumen_ceros$Numero_ceros) == 0)
}

cat("Resultados clave auditados: OK\n")

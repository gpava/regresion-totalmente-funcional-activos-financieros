# Datos

## `originales/`

Contiene los trece archivos CSV utilizados por los scripts de la tesis. Los nombres se conservan porque el script principal los referencia de forma explícita.

Cada CSV contiene, en el archivo fuente, las columnas `Fecha`, `Último`, `Apertura`, `Máximo`, `Mínimo`, `Vol.` y `% var.` (en uno de los archivos la última etiqueta aparece como `%var.`). El análisis utiliza principalmente fecha, precio de cierre (`Último`) y volumen (`Vol.`).

Los archivos se incluyen con fines de **investigación y reproducibilidad**. De acuerdo con la información suministrada por el autor, su redistribución para estos fines está permitida por la fuente original. Estos archivos no quedan relicenciados por la licencia MIT aplicable al código del repositorio y conservan las condiciones y atribuciones de su fuente original.

La integridad de los archivos utilizados en la reproducción final puede comprobarse con `documentacion/MANIFIESTO_SHA256.md`.

## `control/`

Contiene `revision_observaciones_extremas.xlsx`, utilizado únicamente para comprobar que la reconstrucción de la Sección 5.1.1 coincide con el archivo de control preservado durante la investigación.

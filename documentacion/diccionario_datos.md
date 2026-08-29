# Diccionario mínimo de datos

Los archivos fuente utilizan formato CSV separado por punto y coma.

| Campo fuente | Uso en el código | Descripción operativa |
|---|---|---|
| `Fecha` | Sí | Fecha de la observación; se convierte con formato día-mes-año. |
| `Último` | Sí | Precio de cierre utilizado para construir los rendimientos logarítmicos. |
| `Apertura` | No en el modelo final | Precio de apertura presente en el archivo fuente. |
| `Máximo` | No en el modelo final | Máximo diario presente en el archivo fuente. |
| `Mínimo` | No en el modelo final | Mínimo diario presente en el archivo fuente. |
| `Vol.` | Sí | Volumen de negociación; admite sufijos K, M y B. |
| `% var.` / `%var.` | No en el modelo final | Variación porcentual informativa del archivo fuente. |

El script `leer_datos()` conserva tres variables para el análisis: `Fecha`, `Cierre` y `Volumen`.

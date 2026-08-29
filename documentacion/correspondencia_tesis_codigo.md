# Correspondencia entre la tesis y el código

Esta tabla facilita la trazabilidad de los principales resultados.

| Componente de la tesis | Script principal | Salidas o verificaciones |
|---|---|---|
| Preparación de precios y rendimientos | `codigo/FDA_tesis_Version_Final.R` | malla común, LOCF y rendimientos logarítmicos |
| Revisión de observaciones extremas (Sección 5.1.1) | `codigo/Observaciones_Extremas_Tesis.R` | 39 rendimientos, 39 volúmenes altos, 39 volúmenes bajos positivos; Excel y tres figuras |
| FDA del volumen | `codigo/FDA_tesis_Version_Final.R` | selección GCV, suavizado y covarianza funcional |
| FDA de los rendimientos | `codigo/FDA_tesis_Version_Final.R` | selección GCV, suavizado y covarianza funcional |
| Auditoría de la Sección 5.1 | `codigo/AuditoriaTesis.R` | `Auditoria_Seccion_5_1.xlsx` |
| Modelo de regresión totalmente funcional | `codigo/FDA_tesis_Version_Final.R` | superficie estimada, predicciones, residuales, `R^2(t)` y `R_i^2` |
| Diagnósticos residuales | `codigo/FDA_tesis_Version_Final.R` | linealidad, media residual, dispersión, correlación, QQ-plot, histograma y boxplot funcional |
| Comparación GARCH(p,q) | `codigo/GARCH_pq_tesis.R` | selección de órdenes, parámetros, tablas y comparación con volumen |

## Resultados clave auditados

Los coeficientes individuales finales `R_i^2` se verifican en `99_VERIFICAR_RESULTADOS_CLAVE.R`. La revisión de observaciones extremas se contrasta además contra `datos/control/revision_observaciones_extremas.xlsx`.

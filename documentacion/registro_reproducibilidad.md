# Registro de reproducibilidad

## Objetivo

Documentar la comprobación final de reproducibilidad computacional realizada antes de publicar el repositorio asociado a la tesis.

## Entorno validado

La ejecución final se realizó con **R 4.4.3** sobre macOS ARM. Las versiones completas de los paquetes se encuentran en `resultados/sessionInfo.txt` y en `renv.lock`.

## Procedimiento de comprobación

1. Se ejecutó el flujo completo desde la raíz del proyecto.
2. Se comprobó que `99_VERIFICAR_RESULTADOS_CLAVE.R` finalizara con `Resultados clave auditados: OK`.
3. Se inicializó `renv` y se generó `renv.lock` a partir de las versiones reales utilizadas en la ejecución.
4. Se verificó que `renv::status()` informara que el proyecto se encontraba en un estado consistente.
5. Se creó una copia independiente del proyecto.
6. En la copia se eliminó la biblioteca local `renv/library`.
7. El entorno fue reconstruido mediante `renv::restore()`.
8. Se corrigió la única diferencia detectada entre biblioteca y `lockfile` restaurando `Rcpp` a la versión 1.0.14 registrada en la ejecución validada.
9. `renv::status()` confirmó nuevamente un estado consistente.
10. Se ejecutaron `00_VERIFICAR_ARCHIVOS.R`, `00_EJECUTAR_TESIS.R` y `99_VERIFICAR_RESULTADOS_CLAVE.R` en la copia independiente.
11. La verificación final produjo nuevamente:

```text
Resultados clave auditados: OK
```

## Integridad de los datos

Los valores SHA-256 de los trece archivos fuente se registran en `MANIFIESTO_SHA256.md`, lo que permite comprobar que los insumos utilizados son exactamente los mismos que los preservados durante la auditoría.

## Alcance

Esta comprobación respalda la reproducibilidad computacional de los principales resultados generados por los scripts del repositorio en el entorno documentado. No constituye una garantía de identidad bit a bit de representaciones gráficas entre sistemas operativos distintos, ya que fuentes, dispositivos gráficos y bibliotecas del sistema pueden producir diferencias visuales menores.

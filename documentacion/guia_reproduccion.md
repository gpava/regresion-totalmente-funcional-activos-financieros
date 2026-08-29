# Guía de reproducción

## 1. Preparación

1. Instale R y RStudio.
2. Abra `RegresionTotalmenteFuncional.Rproj`.
3. No cambie manualmente el directorio de trabajo a una carpeta externa.
4. Si `renv` no está instalado, ejecute:

```r
install.packages("renv")
```

5. Reconstruya el entorno computacional registrado en `renv.lock`:

```r
renv::restore()
```

6. Compruebe el estado del entorno:

```r
renv::status()
```

Una instalación correctamente restaurada debe indicar que el proyecto está sincronizado con el `lockfile`.

7. Compruebe los insumos:

```r
source("00_VERIFICAR_ARCHIVOS.R", encoding = "UTF-8")
```

La comprobación debe informar que existen los 13 archivos fuente y el archivo de control.

## 2. Ejecución completa

Ejecute:

```r
source("00_EJECUTAR_TESIS.R", encoding = "UTF-8")
```

El script maestro ejecuta, en orden, el modelo FDA y la regresión totalmente funcional, la revisión de observaciones extremas, la auditoría de la Sección 5.1 y los modelos GARCH(p,q).

La ejecución debe realizarse preferiblemente en una sesión nueva de R. El flujo está diseñado para trabajar desde la raíz del proyecto y utiliza rutas relativas.

## 3. Verificación de resultados clave

Después de completar todos los scripts, ejecute:

```r
source("99_VERIFICAR_RESULTADOS_CLAVE.R", encoding = "UTF-8")
```

La comprobación valida los coeficientes individuales `R_i^2` finales y los conteos de la revisión de observaciones extremas.

Una reproducción satisfactoria debe finalizar con:

```text
Resultados clave auditados: OK
```

## 4. Captura del entorno

`00_EJECUTAR_TESIS.R` guarda automáticamente `sessionInfo()` en:

```text
resultados/sessionInfo.txt
```

Este archivo debe conservarse junto con la versión o *release* del repositorio a la que corresponda.

## 5. Entorno computacional con `renv`

El repositorio incluye un archivo `renv.lock` generado a partir del entorno utilizado en la reproducción final de la tesis. La reproducción validada se realizó con R 4.4.3.

En un equipo nuevo, después de clonar o descargar el repositorio, instale `renv` si es necesario:

```r
install.packages("renv")
```

y reconstruya las versiones registradas mediante:

```r
renv::restore()
```

Después puede comprobar el estado del entorno con:

```r
renv::status()
```

No se recomienda ejecutar `renv::snapshot()` antes de reproducir el estudio, ya que el objetivo es restaurar el entorno validado, no actualizarlo.

## 6. Prueba independiente realizada

La reproducibilidad fue comprobada en una copia independiente del proyecto mediante el siguiente procedimiento:

1. crear una copia del repositorio;
2. eliminar la biblioteca local `renv/library` de la copia;
3. reconstruir el entorno con `renv::restore()`;
4. comprobar que `renv::status()` no reportara inconsistencias;
5. ejecutar `00_VERIFICAR_ARCHIVOS.R`;
6. ejecutar `00_EJECUTAR_TESIS.R`;
7. ejecutar `99_VERIFICAR_RESULTADOS_CLAVE.R`.

La verificación final produjo:

```text
Resultados clave auditados: OK
```

Esta prueba confirma que los principales resultados computacionales pueden reconstruirse desde los insumos, scripts y entorno documentados en el repositorio.

## 7. Qué significa reproducir este estudio

La reproducción debe iniciarse desde una sesión limpia y producir nuevamente las tablas, archivos de auditoría, métricas y figuras sin crear objetos manualmente en el `Global Environment`.

Los resultados generados pueden compararse con los archivos de referencia conservados en el repositorio.

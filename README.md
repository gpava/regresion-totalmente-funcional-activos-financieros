# Modelo de Regresión Totalmente Funcional en Activos Financieros

Repositorio de reproducibilidad de la tesis de **Maestría en Ciencias - Matemática Aplicada** titulada *Modelo de Regresión Totalmente Funcional en Activos Financieros*, desarrollada por **Gustavo Andrés Pava Parra**.

**Repositorio:** https://github.com/gpava/regresion-totalmente-funcional-activos-financieros

**Versión reproducible:** v1.0.0

El repositorio contiene los archivos fuente de las trece series estudiadas, los scripts de preprocesamiento y análisis funcional, la revisión de observaciones extremas, la auditoría de la Sección 5.1, la comparación mediante modelos GARCH(p,q), los resultados de referencia y la configuración del entorno computacional utilizada en la reproducción final.

## Objetivo del repositorio

Permitir que una persona interesada pueda reconstruir, desde los archivos fuente, los principales resultados computacionales reportados en la tesis utilizando R, sin depender de rutas personales ni de objetos creados manualmente en el entorno de trabajo.

## Estado de reproducibilidad

La reproducción completa fue validada en una copia independiente del proyecto. En dicha prueba se eliminó la biblioteca local de `renv`, se reconstruyó el entorno mediante `renv::restore()`, se ejecutó nuevamente el flujo completo y la comprobación automática final produjo:

```text
Resultados clave auditados: OK
```

El registro de esta comprobación se documenta en `documentacion/registro_reproducibilidad.md`.

## Estructura

```text
.
├── 00_EJECUTAR_TESIS.R
├── 00_VERIFICAR_ARCHIVOS.R
├── 99_VERIFICAR_RESULTADOS_CLAVE.R
├── RegresionTotalmenteFuncional.Rproj
├── README.md
├── LICENSE
├── LICENCIA.md
├── CITATION.cff
├── renv.lock
├── .Rprofile
├── renv/
│   ├── activate.R
│   ├── settings.json
│   └── .gitignore
├── codigo/
│   ├── FDA_tesis_Version_Final.R
│   ├── Observaciones_Extremas_Tesis.R
│   ├── AuditoriaTesis.R
│   └── GARCH_pq_tesis.R
├── datos/
│   ├── originales/
│   ├── control/
│   └── README.md
├── documentacion/
│   ├── guia_reproduccion.md
│   ├── diccionario_datos.md
│   ├── correspondencia_tesis_codigo.md
│   ├── registro_reproducibilidad.md
│   └── MANIFIESTO_SHA256.md
└── resultados/
    ├── README.md
    └── sessionInfo.txt
```

Además, el flujo genera en la raíz del proyecto las figuras, tablas CSV y archivos Excel utilizados como resultados de referencia y auditoría.

## Requisitos

La reproducción validada se realizó con **R 4.4.3**. El repositorio incorpora un archivo `renv.lock` con las versiones de los paquetes utilizadas durante la reproducción final.

Para reconstruir el entorno computacional se utiliza el paquete [`renv`](https://rstudio.github.io/renv/). No se recomienda actualizar manualmente los paquetes antes de ejecutar el análisis, ya que esto modificaría el entorno validado.

La información detallada de la sesión utilizada durante la ejecución se conserva en `resultados/sessionInfo.txt`.

## Reproducción rápida

1. Descargar o clonar el repositorio.
2. Abrir `RegresionTotalmenteFuncional.Rproj` en RStudio.
3. Confirmar que el directorio de trabajo corresponde a la raíz del repositorio.
4. Si `renv` no está instalado, ejecutar:

```r
install.packages("renv")
```

5. Restaurar el entorno computacional registrado:

```r
renv::restore()
```

6. Verificar que estén disponibles los archivos requeridos:

```r
source("00_VERIFICAR_ARCHIVOS.R", encoding = "UTF-8")
```

7. Ejecutar el análisis completo:

```r
source("00_EJECUTAR_TESIS.R", encoding = "UTF-8")
```

8. Verificar los resultados clave auditados:

```r
source("99_VERIFICAR_RESULTADOS_CLAVE.R", encoding = "UTF-8")
```

Una reproducción satisfactoria debe finalizar con:

```text
Resultados clave auditados: OK
```

El archivo `00_EJECUTAR_TESIS.R` ejecuta los análisis en este orden:

1. modelo FDA y regresión totalmente funcional;
2. revisión de observaciones extremas;
3. auditoría de la Sección 5.1;
4. modelos GARCH(p,q).

## Datos

Los 13 archivos CSV utilizados por el análisis se encuentran en `datos/originales/`. El script principal accede a ellos mediante rutas relativas, por lo que el repositorio no depende de carpetas personales del equipo del autor.

La carpeta `datos/control/` contiene el archivo utilizado para verificar la reconstrucción de la revisión de observaciones extremas.

Los archivos fuente se incluyen con fines de investigación y reproducibilidad. De acuerdo con la información suministrada por el autor, su redistribución para estos fines está permitida por la fuente original. La licencia MIT de este repositorio se aplica al **código fuente**, no relicencia los datos de terceros; los datos conservan las condiciones y atribuciones de su fuente original.

La integridad de los 13 archivos fuente puede comprobarse mediante los valores SHA-256 registrados en `documentacion/MANIFIESTO_SHA256.md`.

## Resultados y trazabilidad

La relación entre las secciones de la tesis y los scripts correspondientes se documenta en `documentacion/correspondencia_tesis_codigo.md`.

Durante la ejecución se generan archivos de auditoría, tablas CSV, archivos Excel y figuras. Se conservan en el repositorio como resultados de referencia para facilitar la comparación con una nueva ejecución.

## Reproducibilidad de observaciones extremas

La Sección 5.1.1 selecciona, para cada una de las trece empresas:

- los tres rendimientos logarítmicos de mayor magnitud absoluta;
- los tres volúmenes observados más altos;
- los tres volúmenes observados positivos más bajos.

Esto produce 117 registros de revisión. El script `codigo/Observaciones_Extremas_Tesis.R` reproduce las ocho hojas del archivo de control y verifica su coincidencia.

## Citación

Si se reutiliza el código o la estructura del análisis, se recomienda citar la tesis y este repositorio. El archivo `CITATION.cff` permite que GitHub muestre la información de citación del software.

## Licencia

El código fuente de este repositorio se distribuye bajo la **licencia MIT**. Véanse `LICENSE` y `LICENCIA.md` para el alcance de la licencia y la distinción entre código, datos y texto de la tesis.

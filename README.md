# Site Planning for a Network of Government-operated Weather Stations in the Dominican Republic Using Zonal Statistics from Geospatial Sources, Multi-Criteria Decision-Making, and Neighborhood Analysis. *Selección de sitios para una red de estaciones meteoclimáticas en República Dominicana mediante estadística zonal de fuentes geoespaciales, análisis multicriterio y de vecindad*

## English

This repository compiles manuscripts and slide presentations related to the research conducted by José Ramón Martínez Batlle and Michela Izzo Gioiosa on site selection for establishing a network of weather and climate stations in the Dominican Republic in 2023.

### Manuscripts, Preprints, Articles

A preprint titled "Site Planning for a Network of Government-operated Weather Stations in the Dominican Republic Using Zonal Statistics from Geospatial Sources, Multi-Criteria Decision-Making, and Neighborhood Analysis" is currently available in this repo as [manuscrito.pdf](manuscrito.pdf) and on [EarthArXiv](https://doi.org/10.31223/X5B14C).

To reproduce the results of this research, follow these steps:

1. Clone this repository.

2. Download the `*.Rdata` files (serialized representations of R objects) from [this Zenodo repository](https://doi.org/10.5281/zenodo.14574177).

3. Extract the `*.Rdata` files and place them in the `datos` directory of the cloned repository.

4. Attempt to knit the [manuscrito.Rmd](manuscrito.Rmd) file into a PDF.

Notes:

- Before knitting, you will need to install the packages listed in the code chunk named `pkgs` within the `manuscrito.Rmd` file. Additional packages not listed in this chunk may also be required.

- The `lastchunk_sessionInfo` code chunk in [manuscrito.Rmd](manuscrito.Rmd) (not included in the PDF output) provides information about the R session used.

- The version of R used was 4.3.0, running on a machine with Ubuntu 20.04.3 LTS.

- There is an analysis repository (in Spanish) associated with this study, where the `*.Rdata` files were generated, available [here](https://github.com/geofis/datos-meteoclimaticos-escenarios-cc).


### Slide Presentations

- [Slide presentation](https://geofis.github.io/seleccion-sitios-estaciones-meteoclimaticas-rd/presentaciones/III-Congreso-IDI-XXIII-JIC-nov24.html). III International Congress on Research, Development, and Innovation (R&D&I). XXIII Scientific Research Conference. Universidad Autónoma de Santo Domingo (UASD). November 19–21, 2024. Santo Domingo, Dominican Republic.

## Español. 

Este repositorio recopila manuscritos y presentaciones de diapositivas sobre la investigación realizada por José Ramón Martínez Batlle y Michela Izzo Gioiosa sobre la selección de sitios para el establecimiento de red estaciones meteoclimáticas en República Dominicana en 2023.

### Manuscritos, preprints, artículos

Actualmente, se dispone de un preprint (en inglés) titulado "Site Planning for a Network of Government-operated Weather Stations in the Dominican Republic Using Zonal Statistics from Geospatial Sources, Multi-Criteria Decision-Making, and Neighborhood Analysis", disponible en este repo como [manuscrito.pdf](manuscrito.pdf) y en [EarthArXiv](https://doi.org/10.31223/X5B14C)

Para reproducir los resultados de esta investigación, se recomienda seguir los siguientes pasos:

1. Clonar este repositorio.

2. Descargar los archivos `*.Rdata` (imágenes serializadas de objetos de R) desde [este repositorio de Zenodo](https://doi.org/10.5281/zenodo.14574177).

3. Descomprimir los archivos `*.Rdata` y colocarlos en el directorio `datos` de la versión clonada de este repo.

4. Intentar tejer el archivo [manuscrito.Rmd](manuscrito.Rmd) a PDF.

Notas:

- Antes de intentar el tejido, necesitará instalar los paquetes listados en el bloque de código nombrado `pkgs` del archivo `manuscrito.Rmd`. Es probable que necesite otros paquetes no listados en el referido bloque.

- El bloque de código `lastchunk_sessionInfo` del archivo [manuscrito.Rmd](manuscrito.Rmd) (no impreso en la salidad PDF) muestra la información de la sesión de R usada.

- La versión de R usada fue la 4.3.0 en una máquina bajo sistema Ubuntu 20.04.3 LTS.

- Existe un repositorio de análisis (en español) asociado a este estudio, desde donde se generaron las imágenes `*.Rdata`, disponible [aquí](https://github.com/geofis/datos-meteoclimaticos-escenarios-cc).

### Presentaciones de diapositivas.

- [Presentación de diapositivas](https://geofis.github.io/seleccion-sitios-estaciones-meteoclimaticas-rd/presentaciones/III-Congreso-IDI-XXIII-JIC-nov24.html). III Congreso Internacional de Investigación, Desarrollo e Innovación (I+D+I). XXIII Jornada de Investigación Científica. Universidad Autónoma de Santo Domingo (UASD). 19 al 21 de noviembre, 2024. Santo Domingo, República Dominicana.
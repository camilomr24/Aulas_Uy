AULASUY
La reforma escolar en Uruguay: cifras, mapas e historias

AUTOR
Camilo Martínez Rodríguez
Programa de Historia Económica y Social
Facultad de Ciencias Sociales, Universidad de la República
Contacto: camilo.martinez@cienciassociales.edu.uy

DESCRIPCIÓN
Aplicación desarrollada en R Shiny para explorar la transformación
de la educación primaria en Uruguay a partir de la reforma escolar
de 1877.

Incluye series nacionales de presupuesto escolar, escuelas,
docentes y matrícula para 1879–1913; mapas e indicadores
departamentales; testimonios históricos; y una sección de datos,
fuentes y decisiones metodológicas.

CONTENIDO DE LA CARPETA
- app.R: código de la aplicación. Incluye la preparación de los
  datos, el diseño, la interfaz y el servidor.
- Aulas_Uy.Rproj: proyecto de RStudio.
- datos/Escuelas-1877.xlsx: base nacional.
- datos/educacion_departamentos.xlsx: indicadores departamentales.
- datos/educacion_departamentos_unificada.xlsx: panel departamental.
- datos/departamentos_uruguay.geojson: cartografía departamental.
- datos/AulasUY_imagenes_editadas: imágenes complementarias.
  Esta versión de la aplicación no las muestra en pantalla.
- manifest.json: información de dependencias para la publicación.
- README.txt: descripción e instrucciones de ejecución.

FUENTES DE LOS DATOS
Las bases fueron elaboradas por el autor a partir de las Memorias
de la Dirección General de Instrucción Pública y de los trabajos
académicos identificados en la sección "Datos y fuentes".

Las fuentes históricas pueden consultarse en el Archivo General
de la Nación y en el Museo Pedagógico del Uruguay.

Los archivos Excel incluyen documentación adicional sobre los
indicadores y su construcción.

La matriculación neta departamental de 1910 se incorpora
directamente en app.R a partir de Martínez Rodríguez (2023),
figura 7.

Los mapas utilizan límites departamentales actuales como soporte
visual para los datos históricos.

REQUISITOS
Tener R y, para seguir estas instrucciones, RStudio instalados.

Paquetes necesarios:
shiny, readxl, bslib, plotly, leaflet, sf y htmlwidgets.

Para instalar los paquetes que falten, ejecutar en la consola de R:

install.packages(c("shiny", "readxl", "bslib", "plotly",
                   "leaflet", "sf", "htmlwidgets"))

INSTRUCCIONES PARA EJECUTAR
1. Descomprimir la carpeta completa.
2. Abrir Aulas_Uy.Rproj con RStudio.
3. Abrir app.R.
4. Hacer clic en "Run App".

También se puede ejecutar desde la consola del proyecto:

shiny::runApp(".")

Mantener la carpeta datos junto a app.R y conservar los nombres
de los archivos. La aplicación utiliza la carpeta de trabajo
para localizar los datos y no requiere cargar un archivo .RData.

NOTAS DE INTERPRETACIÓN
Los importes monetarios se expresan en pesos uruguayos corrientes.
Sus variaciones nominales no equivalen a variaciones reales.

La matrícula y la asistencia media son indicadores diferentes.
Las definiciones, fuentes y limitaciones se explican en la
sección "Datos y fuentes" de la aplicación.

REPOSITORIO DE GITHUB
https://github.com/camilomr24/Aulas_Uy

APLICACIÓN PUBLICADA
https://01a209c6-30dc-aae8-25f9-742e40aa6ca0.share.connect.posit.cloud/

# 1. CARGAR LOS PAQUETES -------------------------------------------------------

library(shiny)
library(readxl)
library(bslib)
library(plotly)
library(leaflet)

# 2. CARGAR Y PREPARAR LAS BASES -----------------------------------------------

# 2.1. Carpeta del proyecto y archivos requeridos

carpeta_app <- getwd()

if (!dir.exists(carpeta_app)) {
  stop("No se encontró la carpeta: ", carpeta_app, call. = FALSE)
}

setwd(carpeta_app)
carpeta_datos <- file.path(carpeta_app, "datos")

planillas <- c(
  nacional = "Escuelas-1877.xlsx",
  departamental = "educacion_departamentos.xlsx",
  anual = "educacion_departamentos_unificada.xlsx"
)

faltantes <- planillas[!file.exists(file.path(carpeta_datos, planillas))]

if (length(faltantes) > 0L) {
  stop(
    "Faltan estas planillas en ", carpeta_datos, ":\n",
    paste(faltantes, collapse = "\n"),
    call. = FALSE
  )
}

# 2.2. Cargar las tres bases desde Excel

escuelas <- read_excel(file.path(carpeta_datos, planillas[["nacional"]]))
educacion_departamental <- read_excel(
  file.path(carpeta_datos, planillas[["departamental"]]),
  sheet = "departamentos"
)
educacion_anual <- read_excel(
  file.path(carpeta_datos, planillas[["anual"]]),
  sheet = "datos"
)

# 2.3. Preparar la serie nacional y el período de análisis

escuelas_1913 <- escuelas[
  !is.na(escuelas$Año) & escuelas$Año >= 1879 & escuelas$Año <= 1913,
]
escuelas_1913 <- escuelas_1913[order(escuelas_1913$Año), ]

# La matrícula total requiere datos públicos y privados del mismo año.
escuelas_1913$matricula_total <-
  escuelas_1913$`Matriculados publicos` +
  escuelas_1913$`Matriculados privados`

if (nrow(escuelas_1913) < 2L) {
  stop(
    "La base nacional debe contener al menos dos observaciones entre 1879 y 1913.",
    call. = FALSE
  )
}

escuelas_1913$presupuesto_por_matriculado <- NA_real_
filas_validas <- (
  is.finite(escuelas_1913$`Presupuesto escolar`) &
    is.finite(escuelas_1913$`Matriculados publicos`) &
    escuelas_1913$`Matriculados publicos` > 0
)
escuelas_1913$presupuesto_por_matriculado[filas_validas] <-
  escuelas_1913$`Presupuesto escolar`[filas_validas] /
  escuelas_1913$`Matriculados publicos`[filas_validas]

# 2.4. Preparar las bases departamentales

# Cociente departamental compartido por el mapa y la ficha.
educacion_anual$gasto_por_matriculado <- NA_real_
matriculas_validas <- (
  is.finite(educacion_anual$gasto) & educacion_anual$gasto >= 0 &
    is.finite(educacion_anual$matricula) & educacion_anual$matricula > 0
)
educacion_anual$gasto_por_matriculado[matriculas_validas] <-
  educacion_anual$gasto[matriculas_validas] /
  educacion_anual$matricula[matriculas_validas]

educacion_departamental$departamento <- sub(
  "^Rio Negro$", "Río Negro", educacion_departamental$departamento
)
educacion_anual$departamento <- sub(
  "^Rio Negro$", "Río Negro", educacion_anual$departamento
)

# Mostrar las bases preparadas en el Environment de RStudio.
if (interactive()) {
  list2env(
    list(
      escuelas = escuelas,
      escuelas_1913 = escuelas_1913,
      educacion_departamental = educacion_departamental,
      educacion_anual = educacion_anual
    ),
    envir = .GlobalEnv
  )
}

departamentos <- sort(unique(na.omit(educacion_departamental$departamento)))

# Martínez Rodríguez (2023), figura 7. Tasas en puntos porcentuales.
matriculacion_neta_1910 <- c(
  "Montevideo" = 60.13, "Salto" = 53.74, "Artigas" = 44.10,
  "San José" = 42.82, "Paysandú" = 42.26, "Rocha" = 41.71,
  "Flores" = 40.58, "Río Negro" = 39.16, "Soriano" = 38.46,
  "Treinta y Tres" = 36.06, "Colonia" = 35.36, "Maldonado" = 34.72,
  "Cerro Largo" = 31.25, "Durazno" = 30.81, "Canelones" = 30.58,
  "Rivera" = 30.42, "Minas" = 28.95, "Florida" = 26.53,
  "Tacuarembó" = 26.43
)

anios_gasto <- sort(unique(
  educacion_anual$anio[is.finite(educacion_anual$anio) & is.finite(educacion_anual$gasto)]
))

# Clave compartida para vincular las bases con la cartografía.
clave_departamento <- function(nombre) {
  clave <- chartr(
    "ÁÉÍÓÚÜÑáéíóúüñ", "AEIOUUNaeiouun",
    trimws(enc2utf8(nombre))
  )
  sub("^lavalleja$", "minas", tolower(clave))
}

tasas_netas <- setNames(
  matriculacion_neta_1910, clave_departamento(names(matriculacion_neta_1910))
)

# 2.5. Textos de los testimonios históricos ------------------------------------

cronologia_pasajes <- list(
  list(
    periodo = "1877–1878", categoria = "Estado", tema = "Escuela rural",
    titulo = "La escuela pública en el territorio rural",
    contexto = "En campaña, la presencia pública era especialmente importante: la enseñanza privada difícilmente podía reemplazar una escuela ausente.",
    cita = "los niños en los distritos rurales no tienen ni pueden tener otro medio de educación que la escuela pública",
    fuente = "José Pedro Varela, Inspector Nacional de Instrucción Pública, Memoria de 1877–1878, tomo I, p. XCVII."
  ),
  list(
    periodo = "1877–1878", categoria = "Dificultades", tema = "Reforma",
    titulo = "La puesta en marcha de la reforma",
    contexto = "La ley de agosto de 1877 reorganizó las autoridades escolares, pero la provisión de recursos y las resistencias locales demoraron su aplicación.",
    cita = "el Gobierno no afectó rentas para ese servicio hasta el mes de Febrero de 1878",
    fuente = "José Pedro Varela, Inspector Nacional de Instrucción Pública, Memoria de 1877–1878, tomo I, p. LXXXVI."
  ),
  list(
    periodo = "1883–1884", categoria = "Avances", tema = "Escuela rural",
    titulo = "La escuela comienza a llegar a la campaña",
    contexto = "La expansión escolar representaba una presencia institucional más extensa en localidades rurales, aunque todavía desigual.",
    cita = "entrando en el corazón de nuestros campos, civilizando con la escuela al antes desamparado habitante rural",
    fuente = "Jacobo A. Varela, Inspector Nacional de Instrucción Pública, Memoria de 1883–1884, p. 30."
  ),
  list(
    periodo = "1883–1884", categoria = "Dificultades", tema = "Infraestructura",
    titulo = "Más alumnos que espacio disponible",
    contexto = "El aumento de la demanda convivía con edificios insuficientes. Algunas escuelas tuvieron que cerrar nuevas inscripciones.",
    cita = "el número de niños es excesivo con relación al local y han debido cerrarse las matriculas de inscripción",
    fuente = "Jacobo A. Varela, Inspector Nacional de Instrucción Pública, Memoria de 1883–1884, p. 42."
  ),
  list(
    periodo = "1883–1884", categoria = "Dificultades", tema = "Maestras",
    titulo = "El magisterio y las desigualdades del campo",
    contexto = "La creciente presencia femenina en la docencia no eliminó los obstáculos para que las maestras trabajaran en distritos rurales.",
    cita = "en la campaña propiamente dicha, en los distritos rurales, es muy difícil y lento el acceso de la mujer como maestra",
    fuente = "Jacobo A. Varela, Inspector Nacional de Instrucción Pública, Memoria de 1883–1884, p. 15."
  ),
  list(
    periodo = "1883–1884", categoria = "Logros", tema = "Escuela rural",
    titulo = "Los vecinos participan en la expansión",
    contexto = "En San José, las comunidades cedieron terrenos y ayudaron a construir locales, articulando iniciativas locales y administración escolar.",
    cita = "Durante el año que alude el presente Informe se han creado cuatro Escuelas rurales, las que se abrirán al inaugurarse el curso escolar de 1884",
    fuente = "Orestes Araujo, informe departamental de San José, Memoria de 1883–1884, p. 74."
  ),
  list(
    periodo = "1901", categoria = "Estado", tema = "Escuela rural",
    titulo = "Qué escuela necesitaba la campaña",
    contexto = "Abel J. Pérez defendió una educación rural que no fuera una versión recortada de la enseñanza urbana.",
    cita = "Sinceramente creo que la escuela rural, debe difundirse sin decapitarla",
    fuente = "Abel J. Pérez, Inspector Nacional de Instrucción Pública, Memoria de 1901, sección Escuelas Rurales, pp. 60–61."
  ),
  list(
    periodo = "1901", categoria = "Dificultades", tema = "Asistencia",
    titulo = "La escuela y el calendario agrícola",
    contexto = "Las labores agrícolas, las lluvias y los caminos difíciles afectaban la asistencia a las escuelas de campaña.",
    cita = "En los distritos agrícolas, la asistencia media disminuye inevitablemente en los periodos de las siembras y de las cosechas",
    fuente = "Abel J. Pérez, Inspector Nacional de Instrucción Pública, Memoria de 1901, p. 16."
  ),
  list(
    periodo = "1904–1905", categoria = "Dificultades", tema = "Guerra civil",
    titulo = "La guerra interrumpe las clases",
    contexto = "En Treinta y Tres, la guerra civil de 1904 paralizó la actividad de numerosas escuelas rurales.",
    cita = "Los nueve meses de lucha fueron para la mayoría de las escuelas rurales, nueve meses de asueto",
    fuente = "Santiago E. Mussio, informe departamental de Treinta y Tres, Memoria de 1904–1905, p. 297."
  ),
  list(
    periodo = "1904–1905", categoria = "Logros", tema = "Maestras",
    titulo = "La maestra de Mansevillagra",
    contexto = "Durante la guerra, la escuela rural n.º 24 de Mansevillagra, Florida, se utilizó como hospital de campaña. La maestra atendió heridos.",
    cita = "una cantidad de heridos que fueron atendidos por esta señora mientras no los recogió la Comisión de la Cruz Roja",
    fuente = "José B. Miranda, informe departamental de Florida, Memoria de 1904–1905, p. 173."
  ),
  list(
    periodo = "1909–1910", categoria = "Estado", tema = "Fronteras",
    titulo = "La escuela fronteriza como presencia nacional",
    contexto = "En Rivera, las autoridades adjudicaron a las escuelas de frontera un papel de difusión del castellano y afirmación de la identidad nacional. Ese lenguaje expresa las ideas políticas de la época.",
    cita = "En Rivera, las Escuelas fronterizas representan el papel de centinelas de avanzada encargadas de impedir la invasión del idioma extranjero",
    fuente = "Informe departamental de Rivera, Memoria de 1909–1910, p. 999."
  ),
  list(
    periodo = "1909–1910", categoria = "Dificultades", tema = "Infraestructura",
    titulo = "Sostener las escuelas abiertas",
    contexto = "En Colonia, el inspector registró dificultades de edificios, licencias docentes y enfermedades, además de la necesidad de auxiliares en escuelas rurales muy concurridas.",
    cita = "Ayudantes a aquellas rurales cuya numerosa inscripción lo reclama",
    fuente = "Juan Pontet, informe departamental de Colonia, Memoria de 1909–1910, pp. 847 y 849–850."
  )
)

# 2.6. Publicaciones académicas ------------------------------------------------

publicaciones_aulas <- list(
  list(
    referencia = "Palomeque, Agapo L. (2010). La “Sociedad de Amigos de la Educación Popular”, primera etapa del proceso reformista de la escuela pública. Anales del Instituto de Profesores “Artigas”, (4), 241–247.",
    url = "https://repositorio.cfe.edu.uy/xmlui/handle/123456789/634"
  ),
  list(
    referencia = "Bralich, Jorge (2011). José Pedro Varela y la gestación de la escuela uruguaya. Revista Historia de la Educación Latinoamericana, 13(17), 43–70.",
    url = "https://www.scielo.org.co/scielo.php?pid=S0122-72382011000200003&script=sci_arttext"
  ),
  list(
    referencia = "Luongo Roca, Carolina (2023). Debates sobre educación en la prensa uruguaya (1877). Libertad de enseñanza y papel del Estado. Claves. Revista de Historia, 9(16), 1–28.",
    url = "https://doi.org/10.25032/crh.v9i16.6"
  ),
  list(
    referencia = "Monreal, Susana (2016). Libertad de enseñanza en Uruguay. Cuestionamientos y debates (1868–1888). Ariadna Histórica, (5), 127–150.",
    url = "https://doi.org/10.1387/ariadna.16076"
  ),
  list(
    referencia = "Martínez Rodríguez, Camilo (2023). Escuelas, maestras y territorios: capacidades estatales y capital humano en Uruguay (1877–1910). Investigaciones de Historia Económica / Economic History Research, 19(1), 17–30.",
    url = "https://doi.org/10.33231/j.ihe.2022.12.001"
  ),
  list(
    referencia = "Oroño, Mariela (2021). Políticas lingüísticas y contacto de lenguas: la escuela vareliana en la nacionalización de inmigrantes en el Uruguay de fines del siglo XIX. Revista Digital de Políticas Lingüísticas, (15), 28–48.",
    url = "https://revistas.unc.edu.ar/index.php/RDPL/issue/view/2423"
  ),
  list(
    referencia = "Acevedo, Fernando; Nossar, Karina; y Viera Duarte, Patricia (comps.) (2016). Miradas sobre educación y cambio. Udelar / CFE.",
    url = "https://repositorio.cfe.edu.uy/handle/123456789/289"
  ),
  list(
    referencia = "Carreras, Sandra (2001). La reforma educativa de José Pedro Varela: ¿una política de fomento a la mujer en el Uruguay de 1877? En Mujeres y naciones en América Latina: problemas de inclusión y exclusión (pp. 93–113).",
    url = "https://publications.iai.spk-berlin.de/servlets/MCRFileNodeServlet/Document_derivate_00002447/BIA_081_093_113.PDF"
  ),
  list(
    referencia = "Martínez Rodríguez, Camilo (2024). Anónimas, ilustradas y valientes: las primeras generaciones de maestras de la reforma vareliana (ca. 1877–1910).",
    url = "https://www.researchgate.net/publication/387183019_Anonimas_ilustradas_y_valientes_las_primeras_generaciones_de_maestras_de_la_reforma_vareliana_ca_1877-1910"
  ),
  list(
    referencia = "Fiorucci, Flavia; Pérez Navarro, Camila; Batista, Pía; Espinoza, G. Antonio; y Goetschel, Ana María (2022). Trayectorias de feminización del magisterio en Sudamérica. Revista de Historia de América, (163), 85–133.",
    url = "https://doi.org/10.35424/rha.163.2022.1145"
  ),
  list(
    referencia = "Clavero White, Carolina (2022). Abel J. Pérez. Significados para el género femenino en un proyecto político para la educación rural de Uruguay (1900–1918). Revista Mexicana de Historia de la Educación, 10(20), 1–19.",
    url = "https://doi.org/10.29351/rmhe.v10i20.381"
  ),
  list(
    referencia = "Espiga, Silvana (2014). El maestro higienista: nueva sanidad y moral en la escuela pública uruguaya (1885–1918). Tesis de maestría.",
    url = "https://repositorio.cfe.edu.uy/handle/123456789/3301"
  ),
  list(
    referencia = "Espiga, Silvana (2022). La edición, circulación y usos político-pedagógicos de los manuales escolares a principios del siglo XX en Uruguay. Claves. Revista de Historia, 8(15), 151–195.",
    url = "https://doi.org/10.25032/crh.v8i15.7"
  ),
  list(
    referencia = "Espiga, Silvana, y Alpini, Alfredo (coords.) (2025). Memorias, experiencias, imágenes y narrativas en la historia de la educación. Cuadernos de la SUHE, 3(3).",
    url = "https://www.aacademica.org/silvana.espiga.dorado/27"
  )
)

# 2.7. Fuentes primarias -------------------------------------------------------

fuentes_primarias_aulas <- list(
  list(
    tipo = "Legislación escolar",
    referencia = "Alonso Criado, Matías. Colección legislativa de la República Oriental del Uruguay. Tomo IV: 1873 a 1878. Montevideo: Imprenta Rural, 1879."
  ),
  list(
    tipo = "Legislación escolar",
    referencia = "Armand Ugón, Eduardo, Luis Arcos Ferrand, Julio Cerdeiras Alonso y Carlos Goldaracena. Compilación de leyes y decretos. Tomo XII: 1879-1880. Montevideo: Imprenta Nacional Colorada, 1930a."
  ),
  list(
    tipo = "Legislación escolar",
    referencia = "Armand Ugón, Eduardo, Luis Arcos Ferrand, Julio Cerdeiras Alonso y Carlos Goldaracena. Compilación de leyes y decretos. Tomo XIV: 1883-1884. Montevideo: Imprenta Nacional Colorada, 1930b."
  ),
  list(
    tipo = "Legislación escolar",
    referencia = "Armand Ugón, Eduardo, Luis Arcos Ferrand, Julio Cerdeiras Alonso y Carlos Goldaracena. Compilación de leyes y decretos. Tomo XVIII: 1889-1890. Montevideo: Imprenta Nacional Colorada, 1930c."
  ),
  list(
    tipo = "Legislación escolar",
    referencia = "Armand Ugón, Eduardo, Luis Arcos Ferrand, Julio Cerdeiras Alonso y Carlos Goldaracena. Compilación de leyes y decretos. Tomo XXVIII: 1908. Montevideo: Imprenta Nacional Colorada, 1930d."
  ),
  list(
    tipo = "Informes y memorias",
    referencia = "Consejo Nacional de Educación y Consejo de Educación Primaria. Informe Palomeque: 1855. Montevideo: Consejo Nacional de Educación, 1974."
  ),
  list(
    tipo = "Informes y memorias",
    referencia = "Dirección General de Instrucción Pública. Memoria correspondiente al periodo transcurrido desde el 24 de agosto de 1877 hasta el 31 de diciembre de 1878 por José Pedro Varela. Tomos I y II. Montevideo: Imprenta La Tribuna, 1879. Archivo General de la Nación."
  ),
  list(
    tipo = "Informes y memorias",
    referencia = "Dirección General de Instrucción Pública. Memoria correspondiente a los años 1883 y 1884 presentada a la Dirección General de Instrucción Pública por el Inspector Nacional de Instrucción Pública Jacobo A. Varela. Montevideo, 1885. Archivo General de la Nación."
  ),
  list(
    tipo = "Informes y memorias",
    referencia = "Dirección General de Instrucción Pública. Informe correspondiente al año 1887 presentado a la Dirección General de Instrucción Pública por Jacobo A. Varela. Montevideo: Imprenta El Siglo Ilustrado, 1888. Archivo General de la Nación."
  ),
  list(
    tipo = "Informes y memorias",
    referencia = "Dirección General de Instrucción Pública. Memoria correspondiente a los años 1890 y 1891 presentada a la Dirección General de Instrucción Pública por Urbano Chucarro. Montevideo: Imprenta La Nación, 1892. Archivo General de la Nación."
  ),
  list(
    tipo = "Informes y memorias",
    referencia = "Dirección General de Instrucción Pública. Memoria correspondiente al año 1901 presentada a la Dirección General de Instrucción Pública por Abel J. Pérez. Montevideo: Talleres de Barreiro y Ramos, 1902. Archivo General de la Nación."
  ),
  list(
    tipo = "Informes y memorias",
    referencia = "Dirección General de Instrucción Pública. Memoria correspondiente al año 1906 presentada a la Dirección General de Instrucción Pública por Abel J. Pérez. Montevideo: Talleres de Barreiro y Ramos, 1907. Archivo General de la Nación."
  ),
  list(
    tipo = "Informes y memorias",
    referencia = "Dirección General de Instrucción Pública. Memoria correspondiente a los años 1910 a 1914, inclusive, presentada a la Dirección General de Instrucción Pública y al Ministerio de Instrucción Pública por Abel J. Pérez. Montevideo: Talleres de Barreiro y Ramos, 1916. Archivo General de la Nación."
  ),
  list(
    tipo = "Obras pedagógicas",
    referencia = "Varela, José Pedro. La educación del pueblo. 2 vols. Montevideo: Tipografía de La Democracia, 1874."
  ),
  list(
    tipo = "Obras pedagógicas",
    referencia = "Varela, José Pedro. De la legislación escolar: consideraciones teórico-prácticas sobre la organización de la instrucción pública, precedidas de un estudio sobre nuestro estado actual y sus causas, seguidas de un proyecto de Ley de Educación Común para la República Oriental del Uruguay. Montevideo: Comisión de Instrucción Pública, 1876."
  ),
  list(
    tipo = "Obras pedagógicas",
    referencia = "Varela, José Pedro. Obras pedagógicas: La Educación del Pueblo. Tomo I. Montevideo: Ministerio de Instrucción Pública y Previsión Social, 1964."
  )
)

# 3. DEFINIR EL ESTILO DE LA APLICACIÓN ----------------------------------------



# 3.1. Título, colores, fuentes y tema bslib -----------------------------------

TITULO <- paste(
  "La reforma escolar en Uruguay:",
  "cifras, mapas e historias (1877-1913)"
)

COLOR_PRINCIPAL <- "#235C2A"
COLOR_SECUNDARIO <- "#1A8831"
COLOR_DOCENTES_TOTAL <- "#46271B"
COLOR_MAESTROS <- "#A58059"
COLOR_FONDO <- "#FFFFFF"
COLOR_TEXTO <- "#1E293B"
ALTURA_MAPA <- "580px"

tema_aulas <- bs_theme(
  version = 5,
  bg = COLOR_FONDO,
  fg = COLOR_TEXTO,
  primary = COLOR_PRINCIPAL,
  secondary = COLOR_SECUNDARIO,
  base_font = "Arial",
  heading_font = "Georgia"
)

# 3.2. Formato de cantidades y porcentajes -------------------------------------

mostrar_numero <- function(valor, decimales = 2) {
  ifelse(
    is.na(valor), "Sin dato",
    formatC(valor, format = "f", digits = decimales,
            big.mark = ".", decimal.mark = ",")
  )
}

mostrar_porcentaje <- function(valor) {
  ifelse(is.na(valor), "Sin dato", paste0(mostrar_numero(valor * 100), " %"))
}

# 3.3. Estilos CSS -------------------------------------------------------------

estilos_aulas <- "
body {
  background-color: #FFFFFF;
}
.introduccion {
  max-width: 950px;
  margin-top: 10px;
  margin-bottom: 30px;
  line-height: 1.5;
}
.introduccion p {
  text-align: justify;
  margin-bottom: 18px;
}
.introduccion h4 {
  line-height: 1.3;
  margin-bottom: 20px;
}
.nav-tabs {
  border-bottom: 1px solid #DCE5EF;
  gap: 4px;
  margin-top: 20px;
}
.nav-tabs .nav-link {
  color: #526579;
  background-color: #FFFFFF;
  border: none;
  border-bottom: 3px solid transparent;
  border-radius: 0;
  padding: 12px 18px;
}
.nav-tabs .nav-link:hover {
  color: var(--bs-secondary, #1A8831);
  border-bottom-color: var(--bs-secondary, #1A8831);
}
.nav-tabs .nav-link.active {
  color: var(--bs-secondary, #1A8831);
  background-color: #FFFFFF;
  border-bottom-color: var(--bs-secondary, #1A8831);
}
.tab-content {
  background-color: #FFFFFF;
  padding-top: 25px;
}
.contenido-seccion {
  padding-bottom: 30px;
}
.descripcion-seccion {
  max-width: 950px;
  line-height: 1.6;
  color: #526579;
  margin-bottom: 22px;
}
.resumen-nacional {
  border-left: 3px solid #235C2A;
  padding: 8px 0 8px 18px;
  margin: 22px 0;
  max-width: 950px;
  line-height: 1.6;
}
.nota-grafico {
  color: #526579;
  font-size: 13px;
  max-width: 950px;
  line-height: 1.5;
}
.consulta-datos {
  margin-top: 20px;
}
.consulta-datos summary {
  color: #235C2A;
  padding: 12px 0;
  cursor: pointer;
}
.grafico-series {
  border: 1px solid #E5ECE7;
  border-radius: 12px;
  padding: 14px 10px 4px;
  margin-top: 16px;
  background: #FFFFFF;
  box-shadow: 0 3px 14px rgba(35, 92, 42, 0.035);
}
.grafico-series h4 {
  font-family: Georgia, serif;
  font-size: 1.15rem;
  margin: 8px 16px 0;
  color: #33473A;
}
.filtro-periodo {
  max-width: 650px;
  margin: 8px auto 18px;
  padding: 0 12px;
}
.filtro-periodo label {
  font-size: 12px;
  font-weight: 400;
  color: #64748B;
  letter-spacing: 0.3px;
}
.filtro-periodo .irs--shiny .irs-bar {
  background: #235C2A;
  border-color: #235C2A;
}
.filtro-periodo .irs--shiny .irs-from,
.filtro-periodo .irs--shiny .irs-to,
.filtro-periodo .irs--shiny .irs-single {
  background: #235C2A;
  font-size: 11px;
}
.filtro-periodo .irs--shiny .irs-handle {
  background: #FFFFFF;
  border-color: #235C2A;
}
.filtro-periodo .irs--shiny .irs-line {
  background: #E8EEE9;
  border-color: #E8EEE9;
}
.selector-departamental {
  margin-top: 20px;
}
.mapa-departamental {
  border: 1px solid #DCE5EF;
  border-radius: 10px;
  overflow: hidden;
  margin-bottom: 12px;
}
.mapa-departamental .leaflet-container {
  background: #FFFFFF;
}
.mapa-departamental .leaflet-control-zoom {
  border: 1px solid #DCE5DF;
  border-radius: 8px;
  overflow: hidden;
  box-shadow: 0 2px 8px rgba(35, 92, 42, 0.08);
}
.mapa-departamental .leaflet-control-zoom a {
  color: #235C2A;
  background: #FFFFFF;
}
.mapa-departamental .leaflet-control-zoom a:hover {
  background: #F3F8F3;
}
.ficha-mapa {
  background: #F3F8F3;
  border-left: 3px solid var(--bs-secondary, #1A8831);
  border-radius: 0 8px 8px 0;
  padding: 16px;
  margin-bottom: 14px;
}
.ficha-mapa .dato-mapa {
  font-size: 1.5rem;
  font-weight: 600;
  color: var(--bs-primary, #235C2A);
}
.ficha-mapa .gasto-por-alumno {
  font-size: 1.1rem;
  font-weight: 400;
  line-height: 1.5;
}
.ficha-mapa p:last-child {
  margin-bottom: 0;
}
.testimonios-historicos {
  max-width: 1440px;
}
.testimonios-historicos .descripcion-seccion,
.testimonios-historicos .frase-central,
.testimonios-historicos .cronologia-pasajes,
.testimonios-historicos > .nota-grafico {
  max-width: none;
}
.frase-central {
  max-width: 960px;
  padding: 1.3rem 1.6rem;
  margin: 1.1rem 0 1.5rem;
  border-left: 4px solid #235C2A;
  background: #F3F8F3;
  border-radius: 0 8px 8px 0;
}
.frase-central blockquote {
  font: italic 1.25rem/1.5 Georgia, serif;
  color: #234C2A;
  margin: 0.65rem 0;
}
.cronologia-pasajes {
  max-width: 1000px;
  margin: 1.5rem 0 0 0.45rem;
  padding: 0 0 0 1.3rem;
  border-left: 2px solid #BBD0B8;
  list-style: none;
}
.pasaje-episodio {
  position: relative;
  margin-bottom: 1rem;
  padding: 1.2rem 1.5rem;
  border: 1px solid #DCE5DB;
  border-radius: 8px;
  background: #FFFFFF;
}
.pasaje-episodio::before {
  position: absolute;
  top: 1.5rem;
  left: calc(-1.3rem - 7px);
  width: 12px;
  height: 12px;
  border-radius: 50%;
  background: #235C2A;
  content: '';
}
.pasaje-cabecera {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
  align-items: center;
}
.pasaje-periodo {
  color: #235C2A;
  font-weight: 700;
  margin-right: 0.4rem;
}
.pasaje-categoria, .pasaje-tema {
  padding: 0.15rem 0.55rem;
  border-radius: 12px;
  background: #EAF2EA;
  color: #31583A;
  font-size: 0.78rem;
}
.pasaje-tema {
  background: #F1F5F9;
  color: #526579;
}
.pasaje-texto h4 {
  font-family: Georgia, serif;
  margin: 0.55rem 0 0.8rem;
}
.pasaje-texto blockquote {
  margin: 0.8rem 0 0.5rem;
  padding: 0.5rem 0 0.5rem 1rem;
  border-left: 2px solid #BBD0B8;
  font-family: Georgia, serif;
  font-style: italic;
}
.pasaje-texto blockquote p, .pasaje-fuente {
  margin-bottom: 0;
}
.pasaje-fuente {
  color: #526579;
  font-size: 0.85rem;
}
@media (max-width: 767px) {
  .pasaje-episodio {
    padding: 0.95rem;
  }
  .frase-central {
    padding: 1rem;
  }
  .grafico-series {
    padding-left: 0;
    padding-right: 0;
  }
}
.recurso-fuente {
  max-width: 950px;
  padding: 1rem 1.1rem;
  margin: 0.8rem 0 1.1rem;
  border: 1px solid #DCE5DB;
  border-radius: 8px;
  background: #FFFFFF;
}
.recurso-fuente h5 {
  color: #235C2A;
}
.recurso-fuente .btn-descarga {
  margin: 0.25rem 0.5rem 0.25rem 0;
}
.definiciones-aulas {
  max-width: 950px;
}
.definiciones-aulas dt {
  margin-top: 0.75rem;
  color: #235C2A;
}
.definiciones-aulas dd {
  margin-bottom: 0.7rem;
}
.fuentes-tabs {
  max-width: 1150px;
  margin-top: 1.2rem;
}
.fuentes-tabs > .nav-tabs, .fuentes-tabs .nav-tabs {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
  border-bottom: 1px solid #DCE5DB;
  margin-bottom: 1rem;
}
.fuentes-tabs .nav-tabs .nav-link {
  display: inline-flex;
  align-items: center;
  gap: 0.45rem;
  color: #235C2A;
  background: #F3F8F3;
  border: 1px solid #DCE5DB;
  border-radius: 0.55rem;
  padding: 0.75rem 0.95rem;
  font-weight: 600;
  white-space: normal;
  text-align: left;
}
.fuentes-tabs .nav-tabs .nav-link i,
.fuentes-tabs .nav-tabs .nav-link svg {
  color: #1A8831;
}
.fuentes-tabs .nav-tabs .nav-link:hover {
  color: #1A8831;
  background: #EAF5EA;
  border-color: #9ECCA4;
}
.fuentes-tabs .nav-tabs .nav-link.active {
  color: #FFFFFF;
  background: #235C2A;
  border-color: #235C2A;
}
.fuentes-tabs .nav-tabs .nav-link.active i,
.fuentes-tabs .nav-tabs .nav-link.active svg {
  color: #FFFFFF;
}
.fuentes-tabs .tab-content {
  padding-top: 0.65rem;
}
.fuentes-panel {
  max-width: 1050px;
  padding-bottom: 1.5rem;
}
.fuentes-panel .recurso-fuente {
  margin-bottom: 0.85rem;
}
.fuentes-panel details > summary {
  cursor: pointer;
}
@media (max-width: 767px) {
  .fuentes-tabs .nav-tabs .nav-link {
    font-size: 0.89rem;
    padding: 0.65rem 0.8rem;
  }
}
"

# 4. CREAR LA INTERFAZ DE USUARIO (UI) -----------------------------------------


# 4.1. Funciones para construir contenidos de la interfaz ----------------------

ficha_pasaje <- function(episodio) {
  tags$li(
    class = "pasaje-episodio",
    div(
      class = "pasaje-texto",
      div(
        class = "pasaje-cabecera",
        span(class = "pasaje-periodo", episodio$periodo),
        span(class = "pasaje-categoria", episodio$categoria),
        span(class = "pasaje-tema", episodio$tema)
      ),
      h4(episodio$titulo),
      p(episodio$contexto),
      tags$blockquote(p(paste0("«", episodio$cita, "»"))),
      p(class = "pasaje-fuente", episodio$fuente)
    )
  )
}

item_publicacion <- function(item) {
  tags$li(
    class = "mb-2",
    item$referencia,
    " ",
    tags$a("Consultar", href = item$url, target = "_blank", rel = "noopener noreferrer")
  )
}

lista_fuentes_primarias <- function(fuentes) {
  tipos <- vapply(fuentes, function(item) item$tipo, character(1))
  
  tagList(lapply(unique(tipos), function(tipo) {
    items <- lapply(fuentes[tipos == tipo], function(item) {
      tags$li(class = "mb-2", item$referencia)
    })
    tagList(h5(tipo), tags$ul(items))
  }))
}

ficha_descarga <- function(titulo, id, descripcion) {
  prefijo <- paste0("descargar_", id)
  div(
    class = "recurso-fuente",
    h5(titulo),
    p(descripcion),
    downloadButton(paste0(prefijo, "_xlsx"), "Descargar Excel", class = "btn-descarga"),
    downloadButton(paste0(prefijo, "_csv"), "Descargar CSV", class = "btn-descarga")
  )
}

nota_metodologica <- function(titulo, ...) {
  tags$details(class = "recurso-fuente", tags$summary(strong(titulo)), ...)
}

# 4.2. Pestañas, inputs y outputs ----------------------------------------------

# Los inputs recogen las selecciones de quien usa la aplicación.
# Los outputs reservan espacios que el servidor completa.
# Cada output usa el mismo identificador en la UI y en el servidor.

ui <- fluidPage(
  theme = tema_aulas,
  tags$head(tags$style(HTML(estilos_aulas))),
  titlePanel(span(TITULO, style = paste0("color: ", COLOR_PRINCIPAL, ";"))),
  tabsetPanel(
    id = "pantallas", type = "tabs", selected = "inicio",
    
    tabPanel(
      title = "Inicio", value = "inicio",
      div(
        class = "introduccion",
        h4("Una historia de escuelas, personas y territorios"),
        p(
          "Esta aplicación invita a explorar la transformación de la
          educación primaria en Uruguay a partir de la reforma escolar
          iniciada en 1877. A través de cifras nacionales y departamentales,
          permite conocer la expansión de la enseñanza primaria y sus diferencias
          territoriales."
        ),
        p(
          "La información reúne dos dimensiones: la oferta escolar,
          reflejada en la evolución del número de escuelas, del número
          de docentes y la expansión de la inversión pública en la educación primaria. En segundo lugar,
          aborda la demanda social de la instrucción primaria a nivel departamental, analizada a través de la matrícula y la asistencia media."
        ),
        p(
          "Esta visualización se basa en un trabajo de recolección y
          análisis de fuentes primarias correspondientes a las Memorias
          de la Dirección General de Instrucción Pública, organismo
          rector de la instrucción primaria en el Uruguay moderno.
          Todas las fuentes utilizadas y otras se encuentran disponibles
          en el Archivo General de la Nación (en la colección de Memorias
          Ministeriales) y en el Museo Pedagógico del Uruguay."
        ),
        p(
          "La edición de esta aplicación está a cargo de Camilo Martínez Rodríguez,
          docente del Programa de Historia Económica y Social de la Facultad de
          Ciencias Sociales, Universidad de la República. Asistente de investigación
          en régimen de Dedicación Total, licenciado en Desarrollo, magíster y doctor
          en Historia Económica por la Universidad de la República.
          Parte de estos datos fueron publicados en artículos académicos y en su
          tesis doctoral."
        ),
        p(
          "Contacto: ",
          tags$a(
            href = "mailto:camilo.martinez@cienciassociales.edu.uy",
            "camilo.martinez@cienciassociales.edu.uy"
          ),
          tags$br(),
          "Más información en su ",
          tags$a(
            href = "https://sites.google.com/cienciassociales.edu.uy/camilomartinezrodriguez/inicio",
            target = "_blank",
            rel = "noopener noreferrer",
            "página personal"
          ),
          "."
        ),
        p(
          "Muchas gracias por visitar esta página, nos alegra que estés aquí.
          Elegí una pestaña para comenzar a explorar."
        )
      )
    ),
    
    # Inputs nacionales y outputs asociados al indicador elegido.
    tabPanel(
      title = "Evolución nacional", value = "nacional",
      div(
        class = "contenido-seccion",
        h3("La expansión de la enseñanza pública"),
        p(
          class = "descripcion-seccion",
          "Explorá la evolución del presupuesto escolar, las escuelas públicas,
          el personal docente y la matrícula pública."
        ),
        # INPUT: indicador que se muestra en el gráfico y en la tabla.
        selectInput(
          "indicador_nacional", "¿Qué querés explorar?",
          choices = c(
            "Presupuesto escolar" = "presupuesto",
            "Escuelas" = "escuelas",
            "Docentes" = "docentes",
            "Escolarización" = "escolarizacion"
          ),
          selected = "presupuesto", width = "320px"
        ),
        conditionalPanel(
          condition = "input.indicador_nacional == 'presupuesto'",
          # INPUT: presupuesto total o presupuesto por matriculado.
          radioButtons(
            "medida_presupuesto", "Forma de presentación:",
            choices = c(
              "Presupuesto total" = "total",
              "Presupuesto por matriculado" = "por_matriculado"
            ),
            selected = "total", inline = TRUE
          )
        ),
        div(
          class = "grafico-series",
          # OUTPUTS: título y gráfico construidos por el servidor.
          h4(textOutput("titulo_grafico_nacional", inline = TRUE)),
          plotlyOutput("grafico_nacional", height = "470px"),
          div(
            class = "filtro-periodo",
            # INPUT: período que filtra los datos reactivos.
            sliderInput(
              "periodo_nacional", "Período",
              min = min(escuelas_1913$Año, na.rm = TRUE),
              max = max(escuelas_1913$Año, na.rm = TRUE),
              value = range(escuelas_1913$Año, na.rm = TRUE),
              step = 1, sep = "", ticks = FALSE, width = "100%"
            )
          )
        ),
        p(
          class = "nota-grafico mt-3",
          "Recorré el gráfico para consultar los valores de cada año.
          Arrastrá sobre un tramo para ampliar y hacé doble clic para
          recuperar la vista completa. En Docentes y Escolarización, usá la leyenda
          para mostrar u ocultar series."
        ),
        # OUTPUTS: resumen, nota y tabla del indicador seleccionado.
        div(class = "resumen-nacional", textOutput("resumen_nacional")),
        div(class = "nota-grafico", textOutput("nota_nacional")),
        tags$details(
          class = "consulta-datos",
          tags$summary("Consultar rápidamente los datos del indicador"),
          div(style = "overflow-x: auto;", tableOutput("tabla_nacional"))
        )
      )
    ),
    
    tabPanel(
      title = "Departamentos", value = "departamentos",
      div(
        class = "contenido-seccion",
        h3("La transformación escolar por departamento"),
        navset_tab(
          id = "seccion_departamental", selected = "inversion",
          header = div(
            class = "selector-departamental",
            # INPUT: departamento compartido por los mapas y las fichas.
            selectInput("depto", "Elegí un departamento:",
                        choices = departamentos, selected = "Montevideo")
          ),
          nav_panel(
            title = "Inversión pública", value = "inversion",
            h5("Gasto escolar por matriculado y departamento"),
            # INPUT: año del mapa y de la ficha de inversión pública.
            selectInput(
              "anio_mapa_gasto", "Año del mapa:", choices = anios_gasto,
              selected = tail(anios_gasto, 1), width = "160px"
            ),
            fluidRow(
              column(
                width = 8,
                div(class = "mapa-departamental", leafletOutput("mapa_gasto", height = ALTURA_MAPA)),
                p(
                  class = "nota-grafico",
                  "Pesos uruguayos corrientes por matriculado en escuelas públicas.
                  Las cinco categorías se calculan con los datos de 1887, 1904 y 1910 juntos.
                  Los cortes son comunes a los tres años. Hacé clic en el mapa para elegir un departamento.
                  Se usan límites departamentales actuales simplificados."
                )
              ),
              column(
                width = 4,
                uiOutput("ficha_mapa_gasto"),
                div(
                  class = "ficha-mapa",
                  h6("Indicadores del departamento"),
                  div(
                    class = "nota-grafico",
                    tableOutput("ficha"),
                    textOutput("texto_escuelas"),
                    p(
                      class = "mt-2",
                      "Las tasas corresponden al crecimiento anual
                      acumulativo entre 1887 y 1914."
                    )
                  )
                )
              )
            )
          ),
          nav_panel(
            title = "Escolarización", value = "escolarizacion",
            h5("Matriculación neta por departamento, 1910"),
            fluidRow(
              column(
                width = 8,
                div(
                  class = "mapa-departamental",
                  leafletOutput("mapa_matriculacion_neta", height = ALTURA_MAPA)
                ),
                p(
                  class = "nota-grafico",
                  "Porcentaje de la población en edad escolar inscripta en
                  establecimientos públicos o privados. Las cinco categorías agrupan
                  cantidades similares de departamentos según las tasas de 1910. Hacé clic en el mapa
                  para elegir un departamento. Se usan límites departamentales
                  actuales simplificados."
                )
              ),
              column(
                width = 4,
                uiOutput("ficha_mapa_matriculacion"),
                div(
                  class = "ficha-mapa",
                  h6("Matrícula y asistencia por año en escuelas públicas"),
                  div(
                    class = "nota-grafico",
                    div(class = "table-responsive", tableOutput("tabla_escolarizacion_depto")),
                    p("El porcentaje indica la asistencia media sobre la matrícula.")
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    tabPanel(
      title = "Testimonios históricos", value = "fragmentos",
      div(
        class = "contenido-seccion testimonios-historicos",
        h3("La escuela y el territorio: expansión, dificultades y construcción estatal en Uruguay (1877–1910)"),
        p(
          class = "descripcion-seccion",
          "Las memorias de los inspectores cuentan qué hacía falta para
          abrir una escuela, llegar a clase y mantenerla en funcionamiento."
        ),
        div(
          class = "frase-central",
          strong("La expansión de la escuela pública"),
          tags$blockquote(
            "«Hay que multiplicar la Escuela, llevándola con empeño a los más apartados extremos de nuestro territorio»"
          ),
          p(
            class = "pasaje-fuente",
            "Abel J. Pérez, Inspector Nacional de Instrucción Pública, Memoria de Instrucción Pública, 1901, p. 11. Fragmento de la cita original."
          )
        ),
        tags$ol(class = "cronologia-pasajes", lapply(cronologia_pasajes, ficha_pasaje)),
        p(class = "nota-grafico", "Las expresiones históricas se reproducen como testimonios de época.")
      )
    ),
    
    tabPanel(
      title = "Datos y fuentes", value = "fuentes",
      div(
        class = "contenido-seccion",
        h3("Datos y fuentes"),
        p(
          class = "descripcion-seccion",
          "Consultá investigaciones, descargá bases de datos, revisá las fuentes históricas y conocé las decisiones metodológicas de AulasUY. Seleccioná una pestaña para explorar cada apartado."
        ),
        div(
          class = "fuentes-tabs",
          navset_tab(
            id = "fuentes_seccion", selected = "publicaciones",
            nav_panel(
              title = tagList(icon("book"), " Publicaciones académicas"),
              value = "publicaciones",
              div(
                class = "fuentes-panel",
                p(
                  class = "descripcion-seccion",
                  "Selección bibliográfica sobre la reforma, el territorio, las maestras, las fronteras y las prácticas escolares. Los enlaces conducen a editores, repositorios, DOI o páginas de consulta."
                ),
                tags$ul(lapply(publicaciones_aulas, item_publicacion)),
                p(
                  class = "nota-grafico",
                  "Algunas publicaciones amplían el período estudiado por AulasUY. Los enlaces externos pueden requerir acceso institucional; se priorizaron referencias persistentes cuando estaban disponibles."
                )
              )
            ),
            nav_panel(
              title = tagList(icon("database"), " Bases de datos"),
              value = "bases",
              div(
                class = "fuentes-panel",
                ficha_descarga(
                  "1. Serie nacional de educación", "nacional",
                  "Escuelas-1877.xlsx · 1879–1914 · Presupuesto escolar, escuelas públicas, matrícula, maestras, maestros, docentes y variables de educación privada cuando están disponibles. Los gráficos nacionales de AulasUY se acotan a 1913."
                ),
                ficha_descarga(
                  "2. Indicadores departamentales", "deptos",
                  "educacion_departamentos.xlsx · 19 departamentos · Gasto por alumno en 1914 y tasas de crecimiento anual acumulativo del gasto por alumno, docentes y escuelas entre 1887 y 1914. El Excel incorpora hojas de promedio informado y definiciones."
                ),
                ficha_descarga(
                  "3. Panel departamental histórico", "anual",
                  "educacion_departamentos_unificada.xlsx · 19 departamentos, años 1887, 1904 y 1910 · Gasto, escuelas, escuelas rurales, docentes, maestras, matrícula y asistencia. El Excel incluye totales de fuente y diccionario de variables."
                ),
                p(
                  class = "nota-grafico",
                  "Los CSV contienen únicamente la hoja principal de cada libro (Hoja1, departamentos o datos). Para consultar las definiciones, promedios y totales adicionales, descargá los Excel completos."
                )
              )
            ),
            nav_panel(
              title = tagList(icon("archive"), " Fuentes históricas"),
              value = "historicas",
              div(
                class = "fuentes-panel",
                p(
                  class = "descripcion-seccion",
                  "Referencias de colecciones legislativas, memorias oficiales y obras pedagógicas utilizadas para contextualizar la reforma escolar y el desarrollo de la enseñanza pública. Se indica la localización archivística cuando figura en la referencia. No se presupone disponibilidad digital de cada obra."
                ),
                lista_fuentes_primarias(fuentes_primarias_aulas),
                h5("Fuentes históricas y cartografía"),
                p(
                  "Las Memorias de Instrucción Pública sostienen la cronología de testimonios y parte de los indicadores históricos. Se recomienda consultar sus ediciones originales para verificar contexto, paginación y terminología."
                ),
                p(
                  "La aplicación utiliza límites departamentales actuales simplificados; no representan necesariamente los límites históricos de cada año. Cartografía: ",
                  tags$a(
                    "Infraestructura de Datos Espaciales y Servicio Geográfico Militar",
                    href = "https://catalogodatos.gub.uy/dataset/ide-limites-departamentales",
                    target = "_blank", rel = "noopener noreferrer"
                  ),
                  ". Licencia de Datos Abiertos de Gobierno de Uruguay."
                ),
                p(
                  class = "nota-grafico",
                  "Antes de publicar o redistribuir las bases, es conveniente completar sus referencias originales, autoría, procedimientos de elaboración y condiciones de reutilización."
                )
              )
            ),
            nav_panel(
              title = tagList(icon("cogs"), " Decisiones metodológicas"),
              value = "metodologia",
              div(
                class = "fuentes-panel",
                p(
                  class = "descripcion-seccion",
                  "AulasUY combina registros históricos con indicadores calculados para facilitar su exploración. Se distingue entre los cálculos realizados por la aplicación, los indicadores recibidos ya elaborados y las limitaciones de comparabilidad de las fuentes."
                ),
                nota_metodologica(
                  "1. Cobertura temporal y unidades de análisis",
                  p("La serie nacional contiene observaciones de 1879 a 1914, pero los gráficos interactivos se limitan a 1879–1913. Los indicadores por departamento se presentan para 19 unidades territoriales; el panel departamental reúne 1887, 1904 y 1910. Las tasas de crecimiento de la ficha departamental abarcan 1887–1914 y proceden de otra planilla."),
                  p("Se conserva la denominación histórica Minas. Los nombres de los departamentos se homogeneizan para relacionar bases y cartografía; por ejemplo, Rio Negro se presenta como Río Negro. No se interpolan años faltantes.")
                ),
                nota_metodologica(
                  "2. Presupuesto escolar y gasto por matriculado",
                  p("El gráfico nacional puede mostrar el presupuesto escolar total o el presupuesto dividido entre la matrícula pública del mismo año:"),
                  tags$code("Presupuesto por matriculado = presupuesto escolar / matriculados públicos"),
                  p("Solo se calcula cuando el presupuesto y la matrícula son datos válidos y la matrícula es mayor que cero. No equivale necesariamente al gasto ejecutado por alumno ni a una medida de calidad educativa."),
                  p("Las series nacionales de presupuesto se expresan en importes corrientes, sin deflactar. La moneda, cobertura contable y base de precios de ciertos campos departamentales aún requieren corroboración en las fuentes originales; por tanto, no se deben equiparar automáticamente a la serie nacional.")
                ),
                nota_metodologica(
                  "3. Variación total y tasa anual acumulativa",
                  p("En el resumen nacional, para los años inicial y final elegidos, la aplicación calcula:"),
                  tags$code("Variación total (%) = (valor final / valor inicial - 1) × 100"),
                  tags$br(),
                  tags$code("Tasa anual acumulativa (%) = ((valor final / valor inicial)^(1 / número de años) - 1) × 100"),
                  p("La tasa anual acumulativa se calcula cuando ambos valores son positivos y el año final es posterior al inicial. Se emplea la distancia temporal real entre observaciones, aunque no existan datos para todos los años intermedios."),
                  p("En la ficha departamental, las tasas de crecimiento anual acumulativo del gasto por alumno, docentes y escuelas para 1887–1914 se leen de educacion_departamentos.xlsx: la aplicación las presenta como porcentajes, pero no las recalcula. Las tasas están almacenadas como proporciones: por ejemplo, 0,0596 se muestra como 5,96 %.")
                ),
                nota_metodologica(
                  "4. Comparaciones de gasto y escuelas por departamento",
                  p("El mapa de inversión pública muestra el gasto por matriculado del año elegido. Sus cinco categorías se definen por quintiles de las observaciones válidas de 1887, 1904 y 1910 en conjunto. Los mismos cortes se aplican a cada año. El mapa de matriculación neta usa quintiles de las 19 tasas de 1910. Los valores empatados se mantienen juntos y las leyendas muestran los intervalos de cada categoría. El cociente requiere gasto válido y matrícula positiva. La participación del departamento se calcula como gasto departamental dividido por la suma de los gastos departamentales de ese año. El denominador es la suma de las filas disponibles en la base, no un total publicado independiente."),
                  tags$code("Participación departamental (%) = gasto del departamento / suma del gasto departamental × 100"),
                  p("En la ficha del mapa, el gasto por matriculado se obtiene dividiendo el gasto del departamento entre su matrícula, cuando ambos son válidos y la matrícula es positiva. El puesto del departamento se calcula ordenando ese cociente de mayor a menor: los empates comparten puesto."),
                  p("La variación de escuelas entre 1887 y 1910 usa el cambio absoluto (escuelas de 1910 menos escuelas de 1887) y la variación porcentual respecto de 1887. Se trata de crecimiento total del intervalo, no de tasa anual acumulativa.")
                ),
                nota_metodologica(
                  "5. Escolarización, asistencia y magisterio",
                  p("La matrícula representa inscripciones; la asistencia media representa concurrencia efectiva. Para cada departamento y año con matrícula positiva, la aplicación calcula:"),
                  tags$code("Asistencia relativa (%) = asistencia media / matrícula × 100"),
                  p("Esta proporción no es una tasa de matriculación. El mapa de matriculación neta de 1910 utiliza porcentajes ya elaborados de Martínez Rodríguez (2023), figura 7, a partir de la Memoria de 1911, e incluye enseñanza pública y privada."),
                  p("El gráfico nacional de escolarización muestra la matrícula pública, la privada y su suma. El total solo se calcula cuando ambas matrículas están disponibles para el mismo año. En el gráfico de docentes, las series de docentes totales, maestras y maestros se muestran según los valores de la base. La aplicación no estima su distribución rural o urbana, ni interpreta necesariamente los docentes como personas distintas de cargos.")
                ),
                nota_metodologica(
                  "6. Datos faltantes, redondeos y cartografía",
                  p("Los valores faltantes se omiten de los cálculos que requieren datos válidos. Los gráficos no completan las discontinuidades con interpolaciones. Los totales e indicadores publicados que figuran en hojas auxiliares de los Excel se conservan para consulta, incluso cuando no coinciden exactamente con sumas de filas debido a cobertura o redondeos."),
                  p("Los mapas usan límites departamentales actuales simplificados (IDE/SGM) como soporte visual para indicadores históricos. Por tanto, las fronteras representadas no deben interpretarse como una reconstrucción de la división territorial de cada año."),
                  p("Las cifras del presupuesto, gasto, escuelas y matrícula requieren lectura conjunta con las fuentes y las notas de las bases; las magnitudes absolutas no equivalen por sí solas a cobertura, acceso efectivo o calidad educativa.")
                ),
                p(
                  class = "nota-grafico",
                  "Documentación adicional: descargá los Excel completos y consultá las hojas 'definiciones' (indicadores departamentales), 'diccionario' y 'totales_fuente' (panel departamental). Los CSV contienen únicamente las hojas de datos principales."
                )
              )
            )
          )
        )
      )
    )
  )
)

# 5. CREAR EL SERVIDOR ---------------------------------------------------------

server <- function(input, output, session) {
  
  # 5.1. Motor reactivo: preparar datos según los inputs -------------------------
  
  # indicador_nacional y medida_presupuesto -> configuracion_nacional()
  # configuracion_nacional() y periodo_nacional -> serie_nacional()
  # anio_mapa_gasto -> datos_gasto_anual()
  # depto -> escolarizacion_depto()
  
  configuracion_nacional <- reactive({
    req(input$indicador_nacional)
    
    if (input$indicador_nacional == "presupuesto") {
      req(input$medida_presupuesto)
      if (input$medida_presupuesto == "por_matriculado") {
        list(
          columna = "presupuesto_por_matriculado",
          nombre = "Presupuesto escolar por matriculado",
          frase = "el presupuesto escolar por matriculado",
          unidad = "Pesos corrientes por matriculado", decimales = 2
        )
      } else {
        list(
          columna = "Presupuesto escolar", nombre = "Presupuesto escolar total",
          frase = "el presupuesto escolar total",
          unidad = "Pesos uruguayos corrientes", decimales = 0
        )
      }
    } else if (input$indicador_nacional == "escuelas") {
      list(
        columna = "Escuelas publicas", nombre = "Escuelas públicas",
        frase = "el número de escuelas públicas",
        unidad = "Número de escuelas", decimales = 0
      )
    } else if (input$indicador_nacional == "escolarizacion") {
      list(
        columna = "matricula_total", nombre = "Matrícula total, pública y privada",
        frase = "la matrícula total", unidad = "Alumnos matriculados", decimales = 0
      )
    } else {
      list(
        columna = "Docentes", nombre = "Personal docente",
        frase = "el número total de docentes", unidad = "Número de docentes", decimales = 0
      )
    }
  })
  
  serie_nacional <- reactive({
    configuracion <- configuracion_nacional()
    req(input$periodo_nacional)
    validate(need(
      configuracion$columna %in% names(escuelas_1913),
      "El indicador seleccionado no está en la base."
    ))
    
    serie <- data.frame(anio = escuelas_1913$Año, valor = escuelas_1913[[configuracion$columna]])
    serie <- serie[
      is.finite(serie$anio) &
        (is.finite(serie$valor) | input$indicador_nacional == "escolarizacion") &
        serie$anio >= input$periodo_nacional[1] &
        serie$anio <= input$periodo_nacional[2],
      , drop = FALSE
    ]
    
    validate(need(nrow(serie) >= 2, "Ampliá el período para incluir al menos dos observaciones."))
    serie[order(serie$anio), , drop = FALSE]
  })
  
  geografia_departamental <- reactive({
    ruta <- file.path(carpeta_datos, "departamentos_uruguay.geojson")
    validate(
      need(requireNamespace("sf", quietly = TRUE), "Instalá el paquete sf para abrir el mapa."),
      need(file.exists(ruta), paste("No se encontró la cartografía en", ruta))
    )
    
    mapa <- tryCatch(sf::st_read(ruta, quiet = TRUE), error = identity)
    if (inherits(mapa, "error")) {
      validate(need(FALSE, paste("No se pudo abrir el GeoJSON.", conditionMessage(mapa))))
    }
    
    validate(need(
      inherits(mapa, "sf") && "departamento" %in% names(mapa),
      "El GeoJSON debe tener geometrías y una columna llamada departamento."
    ))
    validate(need(!is.na(sf::st_crs(mapa)), "La cartografía no tiene un sistema de coordenadas."))
    
    mapa <- sf::st_transform(mapa, 4326)
    mapa$clave <- clave_departamento(mapa$departamento)
    mapa$departamento <- departamentos[match(mapa$clave, clave_departamento(departamentos))]
    
    validate(need(
      nrow(mapa) == 19 && !anyNA(mapa$departamento) && anyDuplicated(mapa$departamento) == 0,
      "Los nombres de los departamentos no coinciden con la cartografía."
    ))
    mapa
  })
  
  datos_gasto_anual <- reactive({
    req(input$anio_mapa_gasto)
    anio <- as.integer(input$anio_mapa_gasto)
    filas <- which(educacion_anual$anio == anio)
    datos <- educacion_anual[filas, , drop = FALSE]
    claves <- clave_departamento(departamentos)
    filas_departamentos <- match(claves, clave_departamento(datos$departamento))
    
    data.frame(
      clave = claves,
      gasto = datos$gasto[filas_departamentos],
      matricula = datos$matricula[filas_departamentos],
      gasto_por_matriculado = datos$gasto_por_matriculado[filas_departamentos]
    )
  })
  
  escolarizacion_depto <- reactive({
    req(input$depto)
    datos <- educacion_anual[
      which(educacion_anual$departamento == input$depto & is.finite(educacion_anual$anio)),
      c("anio", "matricula", "asistencia_media"), drop = FALSE
    ]
    
    validate(need(nrow(datos) > 0, "No hay datos de escolarización para este departamento."))
    datos <- datos[order(datos$anio), , drop = FALSE]
    datos$anio <- as.character(datos$anio)
    datos$proporcion_asistencia <- NA_real_
    validas <- is.finite(datos$matricula) & datos$matricula > 0 & is.finite(datos$asistencia_media)
    datos$proporcion_asistencia[validas] <- datos$asistencia_media[validas] / datos$matricula[validas]
    datos
  })
  
  # 5.2. Función compartida para dibujar los mapas -------------------------------
  
  dibujar_mapa_departamental <- function(valores, dominio, titulo_leyenda, sufijo, decimales) {
    req(input$depto)
    mapa <- geografia_departamental()
    valor <- unname(valores[mapa$clave])
    seleccionado <- mapa$departamento == input$depto
    
    # Quintiles del conjunto de referencia, con cortes comunes entre años.
    referencia <- dominio[is.finite(dominio)]
    validate(need(length(referencia) > 0, "No hay datos para clasificar el mapa."))
    cortes <- unique(as.numeric(quantile(
      referencia, probs = c(0.2, 0.4, 0.6, 0.8), names = FALSE
    )))
    categorias <- c("Bajo", "Medio bajo", "Medio", "Medio alto", "Alto")
    # Si coinciden cortes, los valores empatados permanecen en una categoría.
    posiciones <- round(seq(1, 5, length.out = length(cortes) + 1L))
    categorias <- categorias[posiciones]
    colores <- c("#EDF4D5", "#BDD99D", "#78B67B", "#36865A", "#145239")[posiciones]
    categoria <- cut(
      valor, breaks = c(-Inf, cortes, Inf),
      labels = categorias, right = TRUE, ordered_result = TRUE
    )
    paleta <- colorFactor(
      palette = colores, levels = categorias, ordered = TRUE,
      na.color = "#DCE5EF"
    )
    etiqueta <- paste0(
      mapa$departamento, " · ",
      ifelse(
        is.finite(valor),
        paste0(mostrar_numero(valor, decimales), sufijo, " · ", as.character(categoria)),
        "Sin dato"
      )
    )
    
    grafico <- leaflet(
      mapa,
      options = leafletOptions(attributionControl = FALSE, zoomControl = FALSE, zoomSnap = 0.25)
    )
    grafico <- addPolygons(
      grafico,
      layerId = mapa$departamento, fillColor = paleta(categoria), fillOpacity = 0.95,
      color = ifelse(seleccionado, COLOR_DOCENTES_TOTAL, "#FFFFFF"),
      weight = ifelse(seleccionado, 3, 1), label = etiqueta,
      highlightOptions = highlightOptions(
        color = COLOR_DOCENTES_TOTAL, weight = 3, bringToFront = TRUE
      )
    )
    limites_inferiores <- c(min(referencia), cortes)
    limites_superiores <- c(cortes, max(referencia))
    intervalos <- paste0(
      ifelse(seq_along(categorias) == 1L, "", "&gt; "),
      mostrar_numero(limites_inferiores, decimales), " a ",
      mostrar_numero(limites_superiores, decimales), sufijo
    )
    grafico <- addLegend(
      grafico, position = "bottomright",
      colors = rev(colores),
      labels = rev(paste0(categorias, " · ", intervalos)),
      title = titulo_leyenda, opacity = 1
    )
    if (any(!is.finite(valor))) {
      grafico <- addLegend(
        grafico, position = "bottomright",
        colors = "#DCE5EF", labels = "Sin dato", opacity = 1
      )
    }
    
    limites <- sf::st_bbox(mapa)
    grafico <- fitBounds(
      grafico,
      lng1 = unname(limites["xmin"]), lat1 = unname(limites["ymin"]),
      lng2 = unname(limites["xmax"]), lat2 = unname(limites["ymax"])
    )
    htmlwidgets::onRender(
      grafico,
      "function(el, x) {
        L.control.zoom({
          position: 'topright'
        }).addTo(this);
      }"
    )
  }
  
  # 5.3. Outputs de Evolución nacional -------------------------------------------
  
  # Gráficos interactivos construidos directamente con Plotly.
  # plotlyOutput("grafico_nacional") recibe output$grafico_nacional.
  
  output$titulo_grafico_nacional <- renderText({
    configuracion <- configuracion_nacional()
    serie <- serie_nacional()
    paste0(configuracion$nombre, ", ", min(serie$anio), "–", max(serie$anio))
  })
  
  output$grafico_nacional <- renderPlotly({
    configuracion <- configuracion_nacional()
    serie <- serie_nacional()
    es_docentes <- input$indicador_nacional == "docentes"
    varias_series <- input$indicador_nacional %in% c("docentes", "escolarizacion")
    
    if (es_docentes) {
      filas <- match(serie$anio, escuelas_1913$Año)
      series <- list(
        list(nombre = "Total de docentes", valores = escuelas_1913$Docentes[filas],
             color = COLOR_DOCENTES_TOTAL, trazo = "dash"),
        list(nombre = "Maestras", valores = escuelas_1913$Maestras[filas],
             color = COLOR_PRINCIPAL, trazo = "solid"),
        list(nombre = "Maestros", valores = escuelas_1913$Maestros[filas],
             color = COLOR_MAESTROS, trazo = "solid")
      )
    } else if (input$indicador_nacional == "escolarizacion") {
      filas <- match(serie$anio, escuelas_1913$Año)
      series <- list(
        list(nombre = "Matrícula total", valores = escuelas_1913$matricula_total[filas],
             color = COLOR_DOCENTES_TOTAL, trazo = "dash"),
        list(nombre = "Matrícula pública", valores = escuelas_1913$`Matriculados publicos`[filas],
             color = COLOR_PRINCIPAL, trazo = "solid"),
        list(nombre = "Matrícula privada", valores = escuelas_1913$`Matriculados privados`[filas],
             color = COLOR_MAESTROS, trazo = "solid")
      )
    } else {
      series <- list(
        list(nombre = configuracion$nombre, valores = serie$valor,
             color = COLOR_PRINCIPAL, trazo = "solid")
      )
    }
    
    todos_valores <- unlist(lapply(series, function(s) s$valores))
    todos_valores <- todos_valores[is.finite(todos_valores)]
    validate(need(length(todos_valores) > 0, "No hay valores disponibles."))
    
    techo <- max(c(0, todos_valores))
    piso <- min(c(0, todos_valores))
    amplitud <- techo - piso
    if (amplitud == 0) amplitud <- 1
    
    limite_y <- c(if (piso < 0) piso - amplitud * 0.05 else 0, techo + amplitud * 0.12)
    marcas_y <- pretty(limite_y, n = 5)
    marcas_y <- marcas_y[marcas_y >= limite_y[1] & marcas_y <= limite_y[2]]
    
    limites_x <- range(serie$anio)
    marcas_x <- pretty(limites_x, n = 7)
    marcas_x <- sort(unique(c(
      limites_x,
      marcas_x[marcas_x > limites_x[1] & marcas_x < limites_x[2] &
                 marcas_x == floor(marcas_x)]
    )))
    
    grafico <- plot_ly()
    
    for (s in series) {
      grafico <- add_trace(
        grafico,
        x = serie$anio, y = s$valores, type = "scatter", mode = "lines+markers",
        name = s$nombre, connectgaps = FALSE,
        line = list(color = s$color, width = 2.8, dash = s$trazo),
        marker = list(color = s$color, size = 5, line = list(color = "#FFFFFF", width = 1)),
        fill = if (varias_series) "none" else "tozeroy",
        fillcolor = "rgba(35,92,42,0.055)",
        customdata = mostrar_numero(s$valores, configuracion$decimales),
        hovertemplate = "%{customdata}<extra></extra>",
        showlegend = varias_series
      )
    }
    
    grafico <- plotly::layout(
      grafico,
      paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
      font = list(family = "Arial", size = 12, color = COLOR_TEXTO),
      margin = list(l = 85, r = 25, t = 78, b = if (varias_series) 100 else 60),
      hovermode = "x unified", hoverdistance = -1, spikedistance = -1, dragmode = "zoom",
      hoverlabel = list(
        bgcolor = "#FFFFFF", bordercolor = "#CBD5CE",
        font = list(family = "Arial", size = 13, color = COLOR_TEXTO)
      ),
      legend = list(
        orientation = "h", x = 0, y = -0.22, xanchor = "left", yanchor = "top",
        font = list(size = 12, color = "#475569"),
        itemclick = "toggle", itemdoubleclick = "toggleothers"
      ),
      xaxis = list(
        title = list(text = "Año", font = list(size = 11, color = "#64748B")),
        tickmode = "array", tickvals = marcas_x, ticktext = as.character(marcas_x),
        range = limites_x + c(-0.3, 0.3), hoverformat = "d",
        showgrid = FALSE, zeroline = FALSE, showline = TRUE, linecolor = "#DCE5DF",
        tickfont = list(color = "#64748B"),
        showspikes = TRUE, spikemode = "across", spikesnap = "cursor",
        spikecolor = "#94A89A", spikethickness = 1, spikedash = "dot"
      ),
      yaxis = list(
        title = list(text = configuracion$unidad, font = list(size = 11, color = "#64748B")),
        range = limite_y, tickmode = "array", tickvals = marcas_y,
        ticktext = mostrar_numero(marcas_y, configuracion$decimales),
        tickfont = list(color = "#64748B"),
        showgrid = TRUE, gridcolor = "#EDF1EE", gridwidth = 1,
        zeroline = FALSE, automargin = TRUE
      ),
      updatemenus = list(list(
        type = "buttons", direction = "right", x = 1, xanchor = "right",
        y = 1.22, yanchor = "top", active = 0, showactive = TRUE,
        bgcolor = "#FFFFFF", bordercolor = "#DCE5DF", borderwidth = 1,
        font = list(size = 11, color = "#475569"),
        buttons = list(
          list(label = "Líneas + puntos", method = "restyle",
               args = list(list(mode = "lines+markers"))),
          list(label = "Solo líneas", method = "restyle",
               args = list(list(mode = "lines"))),
          list(label = "Solo puntos", method = "restyle",
               args = list(list(mode = "markers")))
        )
      ))
    )
    
    plotly::config(
      grafico,
      locale = "es", displaylogo = FALSE, displayModeBar = TRUE,
      responsive = TRUE, scrollZoom = FALSE, doubleClick = "reset",
      modeBarButtonsToRemove = c("select2d", "lasso2d"),
      toImageButtonOptions = list(
        format = "png", filename = "AulasUY_evolucion_nacional",
        width = 1400, height = 750, scale = 2
      )
    )
  })
  
  output$resumen_nacional <- renderText({
    configuracion <- configuracion_nacional()
    serie <- serie_nacional()
    serie <- serie[is.finite(serie$valor), , drop = FALSE]
    if (nrow(serie) < 2L) {
      return("No hay dos observaciones completas para calcular el cambio en el período.")
    }
    primero <- serie[1, ]
    ultimo <- serie[nrow(serie), ]
    
    unidad_resumen <- if (input$indicador_nacional == "presupuesto") {
      paste0(" ", tolower(configuracion$unidad))
    } else {
      ""
    }
    
    texto <- paste0(
      "Entre ", primero$anio, " y ", ultimo$anio, ", ", configuracion$frase,
      " pasó de ", mostrar_numero(primero$valor, configuracion$decimales),
      " a ", mostrar_numero(ultimo$valor, configuracion$decimales), unidad_resumen, "."
    )
    
    if (primero$valor > 0 && ultimo$valor > 0 && ultimo$anio > primero$anio) {
      variacion_total <- (ultimo$valor / primero$valor - 1) * 100
      tasa_anual <- ((ultimo$valor / primero$valor)^(1 / (ultimo$anio - primero$anio)) - 1) * 100
      texto <- paste0(
        texto, " La variación total fue de ", mostrar_numero(variacion_total, 1),
        " %, equivalente a una tasa anual acumulativa de ", mostrar_numero(tasa_anual, 2), " %."
      )
    }
    
    texto
  })
  
  output$nota_nacional <- renderText({
    req(input$indicador_nacional)
    if (input$indicador_nacional != "presupuesto") {
      if (input$indicador_nacional == "escolarizacion") {
        return(paste(
          "La matrícula total suma las matrículas pública y privada del mismo año.",
          "Cuando falta una de ellas, el total se presenta como sin dato."
        ))
      }
      return("")
    }
    
    req(input$medida_presupuesto)
    if (input$medida_presupuesto == "por_matriculado") {
      paste(
        "Presupuesto por matriculado = presupuesto escolar total",
        "/ matrícula pública del mismo año.",
        "Los importes se expresan en pesos uruguayos corrientes.",
        "Sus variaciones no descuentan los cambios en los precios."
      )
    } else {
      paste(
        "Presupuesto escolar expresado en pesos uruguayos corrientes.",
        "Sus variaciones no descuentan los cambios en los precios."
      )
    }
  })
  
  output$tabla_nacional <- renderTable({
    req(input$indicador_nacional)
    configuracion <- configuracion_nacional()
    serie <- serie_nacional()
    
    if (input$indicador_nacional == "docentes") {
      docentes <- escuelas_1913[match(serie$anio, escuelas_1913$Año), , drop = FALSE]
      return(data.frame(
        Año = as.character(docentes$Año),
        "Total de docentes" = mostrar_numero(docentes$Docentes, 0),
        Maestras = mostrar_numero(docentes$Maestras, 0),
        Maestros = mostrar_numero(docentes$Maestros, 0),
        check.names = FALSE
      ))
    }
    
    if (input$indicador_nacional == "escolarizacion") {
      matriculas <- escuelas_1913[match(serie$anio, escuelas_1913$Año), , drop = FALSE]
      return(data.frame(
        Año = as.character(matriculas$Año),
        "Matrícula total" = mostrar_numero(matriculas$matricula_total, 0),
        "Matrícula pública" = mostrar_numero(matriculas$`Matriculados publicos`, 0),
        "Matrícula privada" = mostrar_numero(matriculas$`Matriculados privados`, 0),
        check.names = FALSE
      ))
    }
    
    tabla <- data.frame(
      Año = as.character(serie$anio),
      Valor = mostrar_numero(serie$valor, configuracion$decimales),
      check.names = FALSE
    )
    names(tabla)[2] <- paste0(configuracion$nombre, " (", configuracion$unidad, ")")
    tabla
  }, rownames = FALSE, striped = TRUE)
  
  # 5.4. Outputs de Departamentos: inversión pública -----------------------------
  
  output$mapa_gasto <- renderLeaflet({
    datos <- datos_gasto_anual()
    valores <- setNames(datos$gasto_por_matriculado, datos$clave)
    validate(need(any(is.finite(valores)), "No hay datos de gasto por matriculado para este año."))
    
    dibujar_mapa_departamental(
      valores, educacion_anual$gasto_por_matriculado,
      "Gasto por matriculado (pesos)", "", 2
    )
  })
  
  output$ficha_mapa_gasto <- renderUI({
    req(input$depto, input$anio_mapa_gasto)
    anio <- as.integer(input$anio_mapa_gasto)
    clave <- clave_departamento(input$depto)
    datos <- datos_gasto_anual()
    fila <- match(clave, datos$clave)
    gasto <- datos$gasto[fila]
    gasto_total <- sum(datos$gasto)
    resumen <- paste0(
      "El gasto escolar total fue de ", mostrar_numero(gasto, 0),
      " pesos uruguayos corrientes."
    )
    gastos_validos <- is.finite(datos$gasto) & datos$gasto >= 0
    if (is.finite(gasto) && is.finite(gasto_total) && gasto_total > 0) {
      puesto_participacion <- 1 + sum(datos$gasto > gasto, na.rm = TRUE)
      resumen <- paste0(
        resumen, " ", input$depto, " representó el ",
        mostrar_porcentaje(gasto / gasto_total),
        " del gasto escolar total (ranking ", puesto_participacion,
        " en ", sum(gastos_validos), " departamentos)."
      )
    }
    gasto_por_alumno <- datos$gasto_por_matriculado
    validos <- is.finite(gasto_por_alumno)
    gasto_alumno_depto <- gasto_por_alumno[fila]
    ranking_matriculado <- NULL
    
    if (is.finite(gasto_alumno_depto)) {
      puesto <- 1 + sum(gasto_por_alumno > gasto_alumno_depto, na.rm = TRUE)
      ranking_matriculado <- p(
        class = "mb-0",
        paste0(
          input$depto, " ocupó el puesto ", puesto,
          " de ", sum(validos), " según el gasto por matriculado."
        )
      )
    }
    
    div(
      class = "ficha-mapa",
      h5(paste(input$depto, "·", anio)),
      p(
        class = "dato-mapa gasto-por-alumno mb-2",
        paste0(
          mostrar_numero(gasto_alumno_depto),
          " pesos uruguayos corrientes por alumno."
        )
      ),
      p(class = "mb-2", resumen),
      ranking_matriculado
    )
  })
  
  # 5.5. Outputs de Departamentos: escolarización --------------------------------
  
  output$mapa_matriculacion_neta <- renderLeaflet({
    dibujar_mapa_departamental(tasas_netas, tasas_netas, "Matriculación neta", " %", 2)
  })
  
  output$ficha_mapa_matriculacion <- renderUI({
    req(input$depto)
    tasa <- unname(tasas_netas[clave_departamento(input$depto)])
    
    div(
      class = "ficha-mapa",
      h5(paste(input$depto, "· 1910")),
      p(class = "dato-mapa", mostrar_porcentaje(tasa / 100)),
      p("De la población en edad escolar, en establecimientos públicos y privados.")
    )
  })
  
  output$tabla_escolarizacion_depto <- renderTable({
    datos <- escolarizacion_depto()
    data.frame(
      Año = datos$anio,
      Matrícula = mostrar_numero(datos$matricula, 0),
      "Asistencia media" = mostrar_numero(datos$asistencia_media, 0),
      "% de asistencia media" = mostrar_porcentaje(datos$proporcion_asistencia),
      check.names = FALSE
    )
  }, rownames = FALSE, striped = TRUE)
  
  # 5.6. Outputs de indicadores y evolución departamental ------------------------
  
  output$ficha <- renderTable({
    req(input$depto)
    fila <- educacion_departamental[
      !is.na(educacion_departamental$departamento) &
        educacion_departamental$departamento == input$depto,
      , drop = FALSE
    ]
    validate(need(nrow(fila) == 1, "Debe existir una fila para el departamento seleccionado."))
    
    data.frame(
      Indicador = c(
        "Gasto por alumno, 1914 (pesos corrientes)",
        "Crecimiento anual del gasto por alumno, 1887–1914",
        "Crecimiento anual de docentes, 1887–1914",
        "Crecimiento anual de escuelas, 1887–1914"
      ),
      Valor = c(
        mostrar_numero(fila$gasto_alumno_1914),
        mostrar_porcentaje(fila$crec_gasto_alumno),
        mostrar_porcentaje(fila$crec_docentes),
        mostrar_porcentaje(fila$crec_escuelas)
      )
    )
  }, rownames = FALSE, striped = TRUE)
  
  output$texto_escuelas <- renderText({
    req(input$depto)
    filas_departamento <- (
      !is.na(educacion_anual$departamento) & !is.na(educacion_anual$anio) &
        educacion_anual$departamento == input$depto
    )
    
    inicio <- educacion_anual[filas_departamento & educacion_anual$anio == 1887, , drop = FALSE]
    fin <- educacion_anual[filas_departamento & educacion_anual$anio == 1910, , drop = FALSE]
    validate(need(nrow(inicio) == 1 && nrow(fin) == 1, "Debe existir una fila para 1887 y otra para 1910."))
    
    inicial <- inicio$escuelas
    final <- fin$escuelas
    validate(need(
      !is.na(inicial) && !is.na(final) && inicial > 0 && final >= 0,
      "No se puede calcular la variación con estos datos."
    ))
    
    diferencia <- final - inicial
    variacion <- (final / inicial - 1) * 100
    
    cambio <- if (diferencia > 0) {
      paste0(
        "aumentó en ", mostrar_numero(diferencia, 0), " escuelas (",
        mostrar_numero(variacion, 1), " % de crecimiento total del período)"
      )
    } else if (diferencia < 0) {
      paste0(
        "disminuyó en ", mostrar_numero(abs(diferencia), 0), " escuelas (",
        mostrar_numero(abs(variacion), 1), " % de reducción total del período)"
      )
    } else {
      "se mantuvo sin cambios"
    }
    
    paste0(
      "Entre 1887 y 1910, en el departamento de ", input$depto,
      ", el número de escuelas ", cambio, ": pasó de ",
      mostrar_numero(inicial, 0), " a ", mostrar_numero(final, 0), "."
    )
  })
  
  # 5.7. Eventos: seleccionar departamentos desde los mapas ----------------------
  
  seleccionar_departamento <- function(evento) {
    elegido <- evento$id
    req(length(elegido) == 1, elegido %in% departamentos)
    updateSelectInput(session, "depto", selected = elegido)
  }
  
  observeEvent(input$mapa_gasto_shape_click, {
    seleccionar_departamento(input$mapa_gasto_shape_click)
  })
  
  observeEvent(input$mapa_matriculacion_neta_shape_click, {
    seleccionar_departamento(input$mapa_matriculacion_neta_shape_click)
  })
  
  # 5.8. Descargas de bases en Excel y CSV ---------------------------------------
  
  crear_descarga_excel <- function(nombre_archivo) {
    downloadHandler(
      filename = function() nombre_archivo,
      content = function(file) {
        origen <- file.path(carpeta_datos, nombre_archivo)
        if (!file.exists(origen) || !file.copy(origen, file, overwrite = TRUE)) {
          stop("No se pudo copiar el archivo: ", origen)
        }
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
  }
  
  crear_descarga_csv <- function(datos, nombre_archivo) {
    downloadHandler(
      filename = function() nombre_archivo,
      content = function(file) {
        utils::write.csv(datos, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
      },
      contentType = "text/csv; charset=UTF-8"
    )
  }
  
  output$descargar_nacional_xlsx <- crear_descarga_excel(planillas[["nacional"]])
  
  output$descargar_deptos_xlsx <- crear_descarga_excel(planillas[["departamental"]])
  
  output$descargar_anual_xlsx <- crear_descarga_excel(planillas[["anual"]])
  
  output$descargar_nacional_csv <- crear_descarga_csv(escuelas, "aulasuy_serie_nacional.csv")
  
  output$descargar_deptos_csv <- crear_descarga_csv(
    educacion_departamental, "aulasuy_indicadores_departamentales.csv"
  )
  
  output$descargar_anual_csv <- crear_descarga_csv(
    educacion_anual, "aulasuy_panel_departamental.csv"
  )
  
}

# 6. EJECUTAR LA APLICACIÓN ----------------------------------------------------

shinyApp(ui = ui, server = server)
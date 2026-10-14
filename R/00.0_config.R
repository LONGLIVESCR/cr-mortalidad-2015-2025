# =============================================================================
# Configuración común del pipeline
# Costa Rica, mortalidad 2015-2025
# =============================================================================
#
# Todos los scripts de R/ empiezan con:
#
#   source("R/00_config.R")
#
# y se corren con el directorio de trabajo en la raíz del repositorio
# (abriendo el .Rproj, o con `Rscript R/01_...R` desde la raíz).
#
# Aquí viven:
#   - las carpetas de datos (rutas relativas, nunca rutas de una máquina)
#   - los nombres de todos los archivos de entrada y salida
#   - las constantes compartidas (provincias, sexos, grupos de edad)
#   - funciones pequeñas que usan varios scripts
#
# Si los datos están en otra carpeta, no edite los scripts: defina las
# variables de entorno CR_MORT_DATA_RAW y CR_MORT_PROCESSED (por ejemplo en
# un archivo .Renviron que no se sube al repositorio).
# =============================================================================

options(scipen = 999)


# -----------------------------------------------------------------------------
# Carpetas
# -----------------------------------------------------------------------------

# Excel originales del INEC (no se suben al repositorio; ver data-raw/README.md)
dir_raw <- Sys.getenv("CR_MORT_DATA_RAW", unset = "data-raw")

# CSV generados por el pipeline (no se suben al repositorio)
dir_procesados <- Sys.getenv("CR_MORT_PROCESSED", unset = file.path("data", "processed"))

if (!dir.exists(dir_raw)) {
  warning("No existe la carpeta de datos crudos: ", normalizePath(dir_raw, mustWork = FALSE),
          "\nRevise data-raw/README.md o defina CR_MORT_DATA_RAW.")
}
dir.create(dir_procesados, recursive = TRUE, showWarnings = FALSE)


# -----------------------------------------------------------------------------
# Archivos de entrada (en dir_raw)
# -----------------------------------------------------------------------------

# Estimaciones de población del INEC, hoja C10 (el doble punto es el nombre
# con que el archivo se usa en el pipeline; verifíquelo contra su descarga).
archivo_poblacion <- file.path(dir_raw, "poblacion_costarica_inec_2000-2050..xlsx")

# Defunciones por sexo × edad simple × provincia, un Excel por año (script 01)
patron_defunciones_edad_simple <- "^Defunciones_edadessimples_\\d{4}\\.xlsx$"

# Defunciones por causa (causa4dig) × grupo de edad, por provincia y sexo,
# un Excel por año (script 03)
patron_defunciones_causa <- "^Defunciones_\\d{4}\\.xlsx$"

# Listas oficiales del INEC: código REDATAM de 5 dígitos -> CIE-10 (script 05)
listas_inec <- data.frame(
  archivo = c(
    "Causa de muerte CIE-10 Ed2008 utilizada para período 2000-2016.xlsx",
    "Causa de muerte CIE-10 Ed2015 utilizada del 2017-2020.xlsx",
    "Grupos Causa de Muerte 2021 en adelante.xlsx"
  ),
  edicion  = c("Ed2008", "Ed2015", "2021 en adelante"),
  anio_min = c(2015L, 2017L, 2021L),   # Ed2008 cubre 2000-2016; aquí solo se usa desde 2015
  anio_max = c(2016L, 2020L, 2025L),
  stringsAsFactors = FALSE
)


# -----------------------------------------------------------------------------
# Archivos de salida (en dir_procesados)
# -----------------------------------------------------------------------------

salida <- list(
  # 01: edad simple (entrada de 02 y 02.1)
  edad_simple            = file.path(dir_procesados, "mortalidad_mx_1000_2015_2025.csv"),
  edad_simple_anio       = function(anio) file.path(dir_procesados, paste0("mortalidad_mx_1000_", anio, ".csv")),
  # 02: grupos quinquenales
  quinquenal             = file.path(dir_procesados, "mortalidad_mx_1000_quinquenal_2015_2025.csv"),
  quinquenal_anio        = function(anio) file.path(dir_procesados, paste0("mortalidad_mx_1000_quinquenal_", anio, ".csv")),
  # 02.1: verificación
  verif_quinquenal       = file.path(dir_procesados, "verificacion_quinquenal_fallos.csv"),
  # 03: defunciones por causa (INEC_4)
  cie10                  = file.path(dir_procesados, "mortalidad_cie10_2015_2025.csv"),
  # 04: causas + exposición
  cie10_mx               = file.path(dir_procesados, "mortalidad_cie10_mx_2015_2025.csv"),
  # 05: diccionario INEC -> CIE-10
  diccionario            = file.path(dir_procesados, "diccionario_inec_cie10_por_anio.csv"),
  # 06: base unificada con CIE10_4
  unificada              = file.path(dir_procesados, "mortalidad_inec_cie10_mx_2015_2025.csv"),
  # 08: principales causas
  top10                  = file.path(dir_procesados, "top10_causas_2015_2025.csv"),
  top10_sexo             = file.path(dir_procesados, "top10_causas_sexo_2015_2025.csv"),
  oficial_top10_manual   = file.path(dir_procesados, "oficial_top10_manual.csv")
)


# -----------------------------------------------------------------------------
# Constantes compartidas (convenciones INEC)
# -----------------------------------------------------------------------------

# Provincias en el orden oficial del INEC (códigos 1 a 7)
orden_provincias <- c(
  "San José", "Alajuela", "Cartago", "Heredia",
  "Guanacaste", "Puntarenas", "Limón"
)
codigos_provincia <- setNames(orden_provincias, as.character(1:7))

orden_sexos <- c("Hombre", "Mujer", "Total")

# Grupos de edad con exposición. Cada script agrega al final las categorías
# que le corresponden ("Ignorada", "Edad ignorada", "Total").
grupos_edad_quinquenal <- c(
  "0", "1-4",
  "5-9", "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54",
  "55-59", "60-64", "65-69", "70-74", "75-79",
  "80-84", "85-89",
  "90 años y más"
)

# Encabezados de edad de los Excel de causa (Defunciones_YYYY.xlsx), en orden
edades_originales_causa <- c(
  "Menores de 1 año", "1-4 años", "5-9 años", "10-14 años", "15-19 años",
  "20-24 años", "25-29 años", "30-34 años", "35-39 años", "40-44 años",
  "45-49 años", "50-54 años", "55-59 años", "60-64 años", "65-69 años",
  "70-74 años", "75-79 años", "80-84 años", "85-89 años", "90-94 años",
  "95-99 años", "100 y más años", "Edad ignorada", "Total"
)


# -----------------------------------------------------------------------------
# Funciones compartidas
# -----------------------------------------------------------------------------

# Tasas en decimal fijo para el CSV (evita 1.76e-4).
# options(scipen) NO afecta a readr::write_csv(), por eso se formatea aquí.
decimal_fijo <- function(x, decimales = 10) {
  ifelse(is.na(x), NA_character_, sprintf(paste0("%.", decimales, "f"), x))
}

# Edad simple -> grupo quinquenal (0 | 1-4 | 5-9 ... 85-89 | 90 años y más)
grupo_edad <- function(edad) {
  edad     <- as.character(edad)
  edad_num <- suppressWarnings(as.integer(edad))
  dplyr::case_when(
    edad == "Ignorada"      ~ "Ignorada",
    edad == "90 años y más" ~ "90 años y más",
    edad == "0"             ~ "0",
    edad_num %in% 1:4       ~ "1-4",
    !is.na(edad_num) & edad_num >= 5 & edad_num <= 89 ~
      paste0(5 * (edad_num %/% 5), "-", 5 * (edad_num %/% 5) + 4),
    TRUE ~ edad
  )
}

# Regla validada: causa4dig (INEC_4) -> categoría CIE-10 de 3 caracteres.
# Las dos primeras cifras son la posición de la letra en el alfabeto
# (01 = A ... 26 = Z) y las dos últimas son los dígitos de la categoría.
#   104 -> A04 | 921 -> I21 | 1044 -> J44 | 2107 -> U07
# Solo da la CATEGORÍA. El cuarto carácter (subcategoría) no existe en
# causa4dig y no se reconstruye.
inec4_a_cie10 <- function(inec4) {
  n <- as.integer(inec4)
  posicion <- n %/% 100
  fuera <- is.na(n) | n < 0 | n > 9999 | !(posicion %in% 1:26)
  if (any(fuera)) {
    stop("INEC_4 fuera de rango (la letra debe ser 01-26): ",
         paste(unique(inec4[fuera]), collapse = ", "))
  }
  paste0(LETTERS[posicion], sprintf("%02d", n %% 100))
}

# Excepciones conocidas: códigos de la base que NO están en la lista INEC de
# su año. Solo estos se completan con la regla; cualquier otro detiene el
# script 06. Agregar una fila aquí exige documentarla en el README.
excepciones_inec4 <- data.frame(
  anio   = c(2015L, 2016L),
  INEC_4 = c(1009L, 1009L),
  motivo = "J09 (influenza pandémica) aparece en la base pero no existe en la lista Ed2008",
  stringsAsFactors = FALSE
)

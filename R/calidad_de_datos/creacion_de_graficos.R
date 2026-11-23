# =============================================================================
# graficos_registro_tardio_por_edad.R
#
# Porcentaje de defunciones con registro tardío por grupo quinquenal de edad,
# sexo, provincia y año de ocurrencia (2015-2025), a partir de los 52
# tabulados de REDATAM (bases anuales de defunciones 2000-2025, hombres y
# mujeres).
#
#   Defunción tardía: la que se declaró en un año posterior al año en que
#   ocurrió (anio_declaracion > anio_defuncion).
#   Porcentaje = defunciones tardías / todas las defunciones ocurridas ese
#   año x 100 (declaradas en cualquier año hasta el último disponible).
#
# Uso:
#   source("graficos_registro_tardio_por_edad.R", encoding = "UTF-8")
#
# Resultado, en la carpeta graficos_registro_tardio (junto a la de datos):
#   registro_tardio_00_todas_las_edades.png   todas las edades, por provincia
#   registro_tardio_00_resumen_por_edad.png   Costa Rica, un panel por edad
#   registro_tardio_01_menores_de_1_ano.png ... registro_tardio_20_90_y_mas.png
#   porcentaje_registro_tardio_por_edad.csv   los números de los gráficos
#   lectura_archivos_redatam.csv              control de lectura por archivo
#
# La lectura se basa en la lógica de leer_redatam() del script del profesor,
# pero ubica cada columna de edad por su etiqueta y no por su posición: por
# eso funciona igual con 26 o 27 columnas y con 2012 mujeres sin "Edad
# ignorada". Cada archivo se valida contra los totales que trae el propio
# REDATAM (columna Total y sección RESUMEN).
# =============================================================================

# -----------------------------------------------------------------------------
# 0. PAQUETES
# -----------------------------------------------------------------------------

paquetes <- c("tidyverse", "readxl", "purrr")
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]


suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, tidyr, stringr, ggplot2, readr
  library(readxl)
  library(purrr)
})

# -----------------------------------------------------------------------------
# 1. PARÁMETROS (lo único que normalmente habría que tocar)
# -----------------------------------------------------------------------------

# Carpeta con los 52 archivos Excel de REDATAM
ruta_datos <- "/Users/jose/Desktop/UNA Proyecto/Registro_tardio"

# Carpeta donde se guardan los gráficos (se crea junto a la carpeta de datos)
carpeta_graficos <- file.path(dirname(ruta_datos), "graficos_registro_tardio")

# Años de ocurrencia que se grafican
anios_ocurrencia <- 2015:2025

# Último año de declaración que se toma en cuenta. NA = el último que traigan
# los archivos (2025). Si se prefiere no usar 2025 por ser preliminar, poner 2024.
anio_corte <- NA

# Años de seguimiento a partir de los cuales el porcentaje se considera
# consolidado (mismo criterio del script del profesor: menos de 5 años es
# "seguimiento incompleto", porque todavía pueden llegar declaraciones tardías).
anios_consolidado <- 5

# Cómo se combinan los 52 archivos (bases anuales o "universos"):
#   "todos"       suma todos los registros. Cada base anual contiene defunciones
#                 distintas, así que no hay doble conteo.
#   "declaracion" solo los registros cuyo año de declaración coincide con el
#                 año de la base (criterio del script del profesor).
criterio_universos <- "todos"

# Si la suma de las provincias no coincide con el RESUMEN de un archivo, el
# script se detiene (la lectura no sería confiable). FALSE = solo avisa.
detener_si_no_cuadra <- TRUE

# Colores por sexo (azul y naranja, distinguibles con daltonismo)
colores_sexo <- c(Hombre = "#2a78d6", Mujer = "#eb6834")

# -----------------------------------------------------------------------------
# 2. CATEGORÍAS
# -----------------------------------------------------------------------------

# Las 23 categorías de edad que usa REDATAM, en orden
edades_redatam <- c(
  "Menores de 1 año", "1-4 años", "5-9 años", "10-14 años", "15-19 años",
  "20-24 años", "25-29 años", "30-34 años", "35-39 años", "40-44 años",
  "45-49 años", "50-54 años", "55-59 años", "60-64 años", "65-69 años",
  "70-74 años", "75-79 años", "80-84 años", "85-89 años", "90-94 años",
  "95-99 años", "100 y más años", "Edad ignorada"
)

# Recodificación pedida: se unen las tres últimas edades y se excluye la
# edad ignorada. Quedan 20 grupos.
edades_90_y_mas <- c("90-94 años", "95-99 años", "100 y más años")
grupos_edad <- c(edades_redatam[1:19], "90 y más")

provincias <- c("San José", "Alajuela", "Cartago", "Heredia",
                "Guanacaste", "Puntarenas", "Limón")

# -----------------------------------------------------------------------------
# 3. FUNCIONES AUXILIARES
# -----------------------------------------------------------------------------

# Texto en minúsculas, sin tildes y sin espacios repetidos, para comparar
# etiquetas sin depender de mayúsculas o acentos.
normalizar <- function(x) {
  chartr("áéíóúüñ", "aeiouun", str_squish(str_to_lower(x)))
}

# Clave de una etiqueta de edad: "1 - 4 años" -> "1-4", "100 y más años" ->
# "100 y mas", "Menores de 1 año" -> "menores de 1".
clave_edad <- function(x) {
  x <- normalizar(x)
  x <- str_replace_all(x, "\\banos?\\b", " ")
  x <- str_replace_all(x, "(\\d)\\s*(-|a)\\s*(\\d)", "\\1-\\3")
  x <- str_remove(str_squish(x), "^de ")
  str_squish(x)
}

# Diccionario clave -> categoría (con variantes de escritura por si acaso)
diccionario_edad <- c(
  set_names(edades_redatam, clave_edad(edades_redatam)),
  "menor de 1" = "Menores de 1 año", "menos de 1" = "Menores de 1 año",
  "100 o mas" = "100 y más años", "100+" = "100 y más años",
  "90 y mas" = "90 y más", "90 o mas" = "90 y más",
  "ignorada" = "Edad ignorada", "ignorado" = "Edad ignorada",
  "edad ignorado" = "Edad ignorada",
  "total" = "Total"
)

# Etiqueta de columna -> categoría de edad ("Total" o NA si no se reconoce)
etiqueta_edad <- function(x) unname(diccionario_edad[clave_edad(x)])

# Celda de REDATAM -> número. "-" es cero; admite separadores de miles.
# Se detiene si encuentra algo que no sea un conteo entero, en lugar de
# convertirlo en NA sin avisar.
a_numero <- function(x, contexto) {
  x <- str_squish(x)
  x[is.na(x) | str_detect(x, "^-+$")] <- "0"
  x <- str_remove_all(x, "[\\s,]")
  con_punto_de_miles <- str_detect(x, "^\\d{1,3}(\\.\\d{3})+$")
  x[con_punto_de_miles] <- str_remove_all(x[con_punto_de_miles], "\\.")
  y <- suppressWarnings(as.numeric(x))
  malos <- is.na(y) | y < 0 | y != round(y)
  if (any(malos)) {
    stop(contexto, ": hay celdas que no son conteos enteros: ",
         paste(head(unique(x[malos]), 5), collapse = ", "), call. = FALSE)
  }
  y
}

# Nombre de archivo seguro a partir de una etiqueta ("1-4 años" -> "1_4_anos")
nombre_seguro <- function(x) {
  str_remove_all(str_replace_all(normalizar(x), "[^a-z0-9]+", "_"), "^_|_$")
}

# -----------------------------------------------------------------------------
# 4. LECTURA DE UN ARCHIVO REDATAM
# -----------------------------------------------------------------------------
# Estructura esperada (la misma que lee leer_redatam() del profesor):
#   - encabezado con VARIABLE.SEXO=1 o =2 (celda A8);
#   - un bloque por área: fila "AREA # n" (columna B) con el código en C;
#   - dentro de cada área, un sub-bloque por año de declaración: fila de
#     encabezado con el año en la columna B y las edades desde la columna D;
#     debajo, una fila por año de defunción (columna C) y una fila "Total";
#   - al final de cada área, un sub-bloque "Total" (todas las declaraciones);
#   - al final del archivo, la sección RESUMEN (país), que se excluye de los
#     datos pero se usa para validar la lectura.
# Nada se lee por posición fija: las áreas, los encabezados y las columnas de
# edad se buscan por su contenido.

leer_redatam <- function(archivo) {
  nombre <- basename(archivo)

  # 4.1 Toda la primera hoja como texto, sin suponer cuántas filas de
  #     encabezado ni cuántas columnas trae (26, 27 u otra cantidad).
  crudo <- read_excel(archivo, sheet = 1, col_names = FALSE,
                      col_types = "text", .name_repair = "minimal")
  if (nrow(crudo) == 0 || ncol(crudo) < 4) {
    stop(nombre, ": la hoja está vacía o no tiene la forma de un tabulado REDATAM",
         call. = FALSE)
  }
  m <- as.matrix(crudo)
  m[] <- str_squish(m)
  mn <- m
  mn[] <- normalizar(m)
  n <- nrow(m)
  cabecera <- seq_len(min(n, 30))

  # 4.2 Sexo: filtro VARIABLE.SEXO=1 (hombres) o =2 (mujeres) del encabezado.
  #     Si no aparece, se toma del nombre del archivo.
  codigos <- str_match(m[cabecera, , drop = FALSE],
                       regex("SEXO\\s*=\\s*([12])", ignore_case = TRUE))[, 2]
  codigos <- unique(na.omit(codigos))
  sexo_por_nombre <- case_when(
    str_detect(normalizar(nombre), "hombre|masculin") ~ "Hombre",
    str_detect(normalizar(nombre), "mujer|femenin") ~ "Mujer",
    TRUE ~ NA_character_
  )
  if (length(codigos) > 1) {
    stop(nombre, ": el encabezado trae más de un filtro de sexo", call. = FALSE)
  }
  sexo_archivo <- if (length(codigos) == 1) {
    c("1" = "Hombre", "2" = "Mujer")[[codigos]]
  } else {
    sexo_por_nombre
  }
  if (is.na(sexo_archivo)) {
    stop(nombre, ": no se encontró VARIABLE.SEXO=1 o =2 en el encabezado ni ",
         "'hombre'/'mujer' en el nombre del archivo", call. = FALSE)
  }
  if (!is.na(sexo_por_nombre) && sexo_por_nombre != sexo_archivo) {
    warning(nombre, ": el nombre dice ", sexo_por_nombre, " pero el filtro de ",
            "REDATAM dice ", sexo_archivo, ". Se usa el filtro.", call. = FALSE)
  }

  # 4.3 Universo (año de la base): el que REDATAM escribe debajo de
  #     "Universo"; si no está, el primer año que aparezca en el nombre.
  universo_nombre <- as.integer(str_extract(nombre, "(19|20)\\d{2}"))
  universo_celda <- NA_integer_
  pos_u <- which(mn[cabecera, , drop = FALSE] == "universo", arr.ind = TRUE)
  if (nrow(pos_u) > 0 && pos_u[1, "row"] < n) {
    universo_celda <- as.integer(
      str_extract(m[pos_u[1, "row"] + 1, pos_u[1, "col"]], "^(19|20)\\d{2}$"))
  }
  universo_archivo <- coalesce(universo_celda, universo_nombre)
  if (is.na(universo_archivo)) {
    stop(nombre, ": no se pudo identificar el año (universo) del archivo", call. = FALSE)
  }
  if (!is.na(universo_celda) && !is.na(universo_nombre) && universo_celda != universo_nombre) {
    warning(nombre, ": el nombre dice ", universo_nombre, " pero REDATAM dice ",
            "Universo ", universo_celda, ". Se usa ", universo_celda, ".", call. = FALSE)
  }

  # 4.4 Áreas: celdas "AREA # n". Su columna es la de los años de
  #     declaración; la siguiente, la de los años de defunción; las demás, las
  #     de edad.
  es_area <- matrix(!is.na(m) & str_detect(m, regex("^AREA\\s*#\\s*\\d+$", ignore_case = TRUE)),
                    nrow = n)
  pos_area <- which(es_area, arr.ind = TRUE)
  if (nrow(pos_area) == 0) {
    stop(nombre, ": no se encontraron las áreas ('AREA # 1', 'AREA # 2', ...)", call. = FALSE)
  }
  col_bloque <- unique(pos_area[, "col"])
  if (length(col_bloque) > 1) {
    stop(nombre, ": las filas 'AREA #' no están todas en la misma columna", call. = FALSE)
  }
  col_fila <- col_bloque + 1
  if (col_fila >= ncol(m)) {
    stop(nombre, ": no hay columnas de edad a la derecha de las áreas", call. = FALSE)
  }
  cols_valor <- seq.int(col_fila + 1, ncol(m))
  filas_area <- pos_area[, "row"]
  # Código del área: la celda a la derecha de "AREA # n" (como en el script
  # del profesor); si no es un número, el n del rótulo.
  codigo_area <- coalesce(
    suppressWarnings(as.integer(m[filas_area, col_fila])),
    as.integer(str_extract(m[filas_area, col_bloque], "\\d+"))
  )

  # 4.5 RESUMEN: desde la fila "RESUMEN" hasta el final.
  fila_resumen <- which(rowSums(mn[, seq_len(col_bloque), drop = FALSE] == "resumen",
                                na.rm = TRUE) > 0)
  fila_resumen <- fila_resumen[fila_resumen > min(filas_area)]
  inicio_resumen <- if (length(fila_resumen) > 0) min(fila_resumen) else n + 1L
  if (any(filas_area > inicio_resumen)) {
    stop(nombre, ": hay filas 'AREA #' después del RESUMEN", call. = FALSE)
  }

  # 4.6 Encabezados de edad: filas con al menos dos etiquetas de edad
  #     reconocidas a la derecha de la columna de años de defunción.
  etiquetas <- matrix(etiqueta_edad(m[, cols_valor, drop = FALSE]), nrow = n)
  es_encabezado <- rowSums(!is.na(etiquetas) & etiquetas != "Total") >= 2

  # 4.7 Cada fila "AREA #" o "RESUMEN" abre una sección; cada encabezado de
  #     edades abre un bloque. El año de declaración sale del encabezado del
  #     propio bloque (nunca se arrastra del bloque anterior).
  marcador <- seq_len(n) %in% c(filas_area, inicio_resumen)
  area_fila <- rep(NA_integer_, n)
  area_fila[filas_area] <- codigo_area
  if (inicio_resumen <= n) area_fila[inicio_resumen] <- 0L

  filas <- tibble(
    fila = seq_len(n),
    marcador = marcador,
    es_encabezado = es_encabezado & !marcador,
    area_id = area_fila,
    etiqueta_bloque = m[, col_bloque],
    etiqueta_fila = m[, col_fila],
    parte = if_else(seq_len(n) >= inicio_resumen, "resumen", "provincias")
  ) %>%
    mutate(seccion = cumsum(marcador),
           bloque = cumsum(marcador | es_encabezado)) %>%
    group_by(seccion) %>%
    mutate(area_id = first(area_id)) %>%
    group_by(bloque) %>%
    mutate(con_encabezado = first(es_encabezado),
           etiqueta_bloque = first(etiqueta_bloque)) %>%
    ungroup() %>%
    mutate(tipo_bloque = case_when(
      str_detect(etiqueta_bloque, "^(19|20)\\d{2}$") ~ "declaracion",
      normalizar(etiqueta_bloque) == "total" ~ "total",
      TRUE ~ "otro"
    ))

  # 4.8 Mapa de columnas de cada bloque, según las etiquetas de su encabezado
  mapa <- filas %>%
    filter(es_encabezado) %>%
    select(bloque, fila_encabezado = fila) %>%
    expand_grid(col = cols_valor) %>%
    mutate(etiqueta_col = m[cbind(fila_encabezado, col)],
           edad = etiquetas[cbind(fila_encabezado, col - col_fila)]) %>%
    filter(!is.na(etiqueta_col))

  # 4.9 Celdas de datos: filas dentro de un bloque con encabezado y con algo
  #     escrito en la columna de años de defunción.
  candidatas <- filas %>%
    filter(seccion > 0, con_encabezado, !es_encabezado, !marcador,
           !is.na(etiqueta_fila)) %>%
    mutate(tipo_fila = case_when(
      str_detect(etiqueta_fila, "^(19|20)\\d{2}$") ~ "anio",
      normalizar(etiqueta_fila) == "total" ~ "total",
      TRUE ~ "otra"
    ))

  celdas <- candidatas %>%
    select(fila, bloque, parte, area_id, tipo_bloque, etiqueta_bloque,
           tipo_fila, etiqueta_fila) %>%
    expand_grid(col = cols_valor) %>%
    inner_join(select(mapa, bloque, col, etiqueta_col, edad), by = c("bloque", "col")) %>%
    mutate(texto = m[cbind(fila, col)],
           es_numero = is.na(texto) | str_detect(texto, "^(-+|\\d[\\d.,\\s]*)$"))

  # Filas con texto donde debería haber números: si la fila es un año o un
  # total, la lectura está mal y se detiene; si no, es una nota y se ignora.
  revision <- celdas %>%
    group_by(fila, tipo_fila, etiqueta_fila) %>%
    summarise(numerica = all(es_numero), con_valores = any(!is.na(texto)),
              .groups = "drop")
  malas <- filter(revision, !numerica, tipo_fila != "otra")
  if (nrow(malas) > 0) {
    stop(nombre, ": la fila ", malas$fila[1], " (", malas$etiqueta_fila[1],
         ") tiene texto en columnas de conteos", call. = FALSE)
  }
  celdas <- celdas %>%
    semi_join(filter(revision, numerica, con_valores), by = "fila") %>%
    mutate(valor = a_numero(texto, nombre))

  # 4.10 Validaciones contra los totales del propio REDATAM
  # a) Una columna con etiqueta desconocida no puede tener defunciones
  desconocidas <- filter(celdas, is.na(edad), valor > 0)
  if (nrow(desconocidas) > 0) {
    stop(nombre, ": hay defunciones en columnas que no son edades conocidas: ",
         paste(unique(desconocidas$etiqueta_col), collapse = ", "), call. = FALSE)
  }
  # b) En cada fila, la suma de las edades debe ser igual a la columna Total
  if (any(mapa$edad %in% "Total")) {
    descuadres <- celdas %>%
      filter(!is.na(edad)) %>%
      group_by(fila) %>%
      summarise(tiene_total = any(edad == "Total"),
                suma_edades = sum(valor[edad != "Total"]),
                total = sum(valor[edad == "Total"]), .groups = "drop") %>%
      filter(tiene_total, suma_edades != total)
    if (nrow(descuadres) > 0) {
      stop(nombre, ": en ", nrow(descuadres), " fila(s) la suma de las edades no ",
           "coincide con la columna Total (primera: fila ", descuadres$fila[1],
           " de la hoja)", call. = FALSE)
    }
  } else {
    warning(nombre, ": no tiene columna Total; no se pudo validar fila por fila",
            call. = FALSE)
  }
  # c) La suma de las áreas debe ser igual al RESUMEN (país), celda por celda
  #    (bloque x fila), sin contar las filas Total.
  por_parte <- celdas %>%
    filter(edad %in% edades_redatam, tipo_fila != "total") %>%
    group_by(parte, bloque = normalizar(etiqueta_bloque), fila = normalizar(etiqueta_fila)) %>%
    summarise(v = sum(valor), .groups = "drop")
  if (any(por_parte$parte == "resumen")) {
    comparacion <- full_join(filter(por_parte, parte == "provincias"),
                             filter(por_parte, parte == "resumen"),
                             by = c("bloque", "fila"), suffix = c("_areas", "_resumen"))
    diferencia_resumen <- sum(abs(coalesce(comparacion$v_areas, 0) -
                                    coalesce(comparacion$v_resumen, 0)))
    estado_resumen <- if (diferencia_resumen == 0) "cuadra" else "no cuadra"
  } else {
    diferencia_resumen <- NA_real_
    estado_resumen <- "sin RESUMEN"
  }

  # 4.11 Datos: bloques de un año de declaración, filas de un año de
  #      defunción, columnas de edad (sin la columna Total ni el RESUMEN).
  datos <- celdas %>%
    filter(parte == "provincias", tipo_bloque == "declaracion",
           tipo_fila == "anio", edad %in% edades_redatam) %>%
    group_by(area_id,
             anio_declaracion = as.integer(etiqueta_bloque),
             anio_defuncion = as.integer(etiqueta_fila),
             edad) %>%
    summarise(defunciones = sum(valor), .groups = "drop") %>%
    mutate(archivo = nombre, universo = universo_archivo, sexo = sexo_archivo,
           .before = 1)

  # Lo que no se puede ubicar en un año de ocurrencia (p. ej. una fila
  # "Ignorado") se excluye y se informa.
  excluidas <- celdas %>%
    filter(parte == "provincias", edad %in% edades_redatam, tipo_fila != "total",
           tipo_bloque != "total",
           tipo_fila == "otra" | tipo_bloque == "otro")
  edades_halladas <- intersect(edades_redatam, mapa$edad)

  diagnostico <- tibble(
    archivo = nombre,
    universo = universo_archivo,
    sexo = sexo_archivo,
    columnas = ncol(m),
    columnas_edad = length(edades_halladas),
    edades_ausentes = paste(setdiff(edades_redatam, edades_halladas), collapse = "; "),
    areas = paste(sort(unique(codigo_area)), collapse = ","),
    defunciones_leidas = sum(datos$defunciones),
    excluidas_sin_anio = sum(excluidas$valor),
    etiquetas_excluidas = paste(unique(c(excluidas$etiqueta_fila[excluidas$tipo_fila == "otra"],
                                         excluidas$etiqueta_bloque[excluidas$tipo_bloque == "otro"])),
                                collapse = "; "),
    resumen = estado_resumen,
    diferencia_resumen = diferencia_resumen
  )

  list(datos = datos, diagnostico = diagnostico)
}

# -----------------------------------------------------------------------------
# 5. LEER LOS 52 ARCHIVOS
# -----------------------------------------------------------------------------

if (!dir.exists(ruta_datos)) {
  stop("No existe la carpeta de datos: ", ruta_datos, call. = FALSE)
}

archivos <- list.files(ruta_datos, pattern = "\\.xlsx?$", ignore.case = TRUE,
                       full.names = TRUE)
# Se ignoran los temporales que crea Excel cuando un archivo está abierto
archivos <- archivos[!startsWith(basename(archivos), "~$")]
if (length(archivos) == 0) {
  stop("No hay archivos Excel en ", ruta_datos, call. = FALSE)
}
if (length(archivos) != 52) {
  message("Aviso: se esperaban 52 archivos y hay ", length(archivos), ".")
}

cat("Leyendo", length(archivos), "archivos de", ruta_datos, "...\n")
lecturas <- map(archivos, function(a) tryCatch(leer_redatam(a), error = function(e) e))

# Si algún archivo falla, se informa de todos a la vez
con_error <- map_lgl(lecturas, inherits, what = "error")
if (any(con_error)) {
  stop("No se pudieron leer ", sum(con_error), " archivo(s):\n",
       paste0("  - ", map_chr(lecturas[con_error], conditionMessage), collapse = "\n"),
       call. = FALSE)
}

datos <- bind_rows(map(lecturas, "datos"))
lectura <- bind_rows(map(lecturas, "diagnostico")) %>% arrange(universo, sexo)

# Cada universo y sexo debe aparecer una sola vez (si no, se contaría doble)
repetidos <- lectura %>% count(universo, sexo) %>% filter(n > 1)
if (nrow(repetidos) > 0) {
  stop("Hay archivos repetidos para el mismo año y sexo: ",
       paste(repetidos$universo, repetidos$sexo, collapse = "; "), call. = FALSE)
}

# Año de corte: último año de declaración disponible (o el fijado arriba)
corte <- if (is.na(anio_corte)) max(datos$anio_declaracion) else as.integer(anio_corte)

# Deben estar todas las bases desde 2015 hasta el corte, para los dos sexos
necesarios <- expand_grid(universo = anios_ocurrencia[anios_ocurrencia <= corte],
                          sexo = c("Hombre", "Mujer"))
faltan <- anti_join(necesarios, lectura, by = c("universo", "sexo"))
if (nrow(faltan) > 0) {
  stop("Faltan los archivos de: ", paste(faltan$universo, faltan$sexo, collapse = "; "),
       call. = FALSE)
}

# Validación contra el RESUMEN de cada archivo
no_cuadran <- filter(lectura, resumen == "no cuadra")
if (nrow(no_cuadran) > 0) {
  texto <- paste0("La suma de las provincias no coincide con el RESUMEN en: ",
                  paste0(no_cuadran$archivo, " (diferencia ", no_cuadran$diferencia_resumen, ")",
                         collapse = "; "))
  if (detener_si_no_cuadra) stop(texto, call. = FALSE) else warning(texto, call. = FALSE)
}

# Informe de la lectura
cat("\nLECTURA DE LOS ARCHIVOS\n")
cat("  Archivos leídos:", nrow(lectura), "| universos",
    min(lectura$universo), "a", max(lectura$universo), "\n")
cat("  Columnas por archivo:",
    paste0(names(table(lectura$columnas)), " columnas (", table(lectura$columnas), " archivo(s))",
           collapse = ", "), "\n")
cat("  Validación con el RESUMEN:", sum(lectura$resumen == "cuadra"), "cuadran,",
    sum(lectura$resumen == "no cuadra"), "no cuadran,",
    sum(lectura$resumen == "sin RESUMEN"), "sin RESUMEN\n")
con_ausentes <- filter(lectura, edades_ausentes != "")
if (nrow(con_ausentes) > 0) {
  cat("  Edades que REDATAM no imprimió (sin casos, se toman como 0):\n")
  cat(paste0("    ", con_ausentes$archivo, ": ", con_ausentes$edades_ausentes, "\n"), sep = "")
}
if (sum(lectura$excluidas_sin_anio) > 0) {
  cat("  Defunciones excluidas por no tener año de defunción o de declaración (",
      paste(unique(unlist(strsplit(lectura$etiquetas_excluidas[lectura$etiquetas_excluidas != ""], "; "))),
            collapse = ", "), "): ", sum(lectura$excluidas_sin_anio), "\n", sep = "")
}
otras_areas <- setdiff(unique(datos$area_id), 1:7)
if (length(otras_areas) > 0) {
  cat("  Áreas fuera de las 7 provincias (solo cuentan en Costa Rica):",
      paste(otras_areas, collapse = ", "), "\n")
}

# -----------------------------------------------------------------------------
# 6. PORCENTAJE DE DEFUNCIONES TARDÍAS
# -----------------------------------------------------------------------------

if (criterio_universos == "declaracion") {
  datos <- filter(datos, anio_declaracion == universo)
} else if (criterio_universos != "todos") {
  stop("criterio_universos debe ser \"todos\" o \"declaracion\"", call. = FALSE)
}

base <- datos %>%
  filter(anio_declaracion <= corte,
         anio_defuncion %in% anios_ocurrencia,
         edad != "Edad ignorada") %>%
  mutate(
    edad_grupo = if_else(edad %in% edades_90_y_mas, "90 y más", edad),
    tardia = anio_declaracion > anio_defuncion,
    # declarada antes de ocurrir: error de fecha. Cuenta en el total pero no
    # como tardía; se informa abajo.
    anomala = anio_declaracion < anio_defuncion
  )

# Provincias (1 a 7) y el país completo; cada edad y todas las edades juntas
por_provincia <- base %>%
  filter(area_id %in% 1:7) %>%
  mutate(provincia = provincias[area_id])
pais <- mutate(base, provincia = "Costa Rica")
todo <- bind_rows(por_provincia, pais)
todo <- bind_rows(todo, mutate(todo, edad_grupo = "Todas las edades"))

etiqueta_consolidado <- paste0("Consolidado (", anios_consolidado, " años o más de seguimiento)")
etiqueta_incompleto <- paste0("Seguimiento incompleto (menos de ", anios_consolidado, " años)")

porcentajes <- todo %>%
  group_by(provincia, sexo, edad_grupo, anio_ocurrencia = anio_defuncion) %>%
  summarise(total_defunciones = sum(defunciones),
            defunciones_tardias = sum(defunciones[tardia]),
            declaradas_antes_de_ocurrir = sum(defunciones[anomala]),
            .groups = "drop") %>%
  complete(provincia, sexo, edad_grupo, anio_ocurrencia = anios_ocurrencia,
           fill = list(total_defunciones = 0, defunciones_tardias = 0,
                       declaradas_antes_de_ocurrir = 0)) %>%
  mutate(
    anios_seguimiento = corte - anio_ocurrencia,
    # Sin seguimiento posterior (2025) o sin defunciones no hay porcentaje
    pct_tardias = if_else(total_defunciones > 0 & anios_seguimiento > 0,
                          100 * defunciones_tardias / total_defunciones, NA_real_),
    estado = case_when(
      anios_seguimiento <= 0 ~ "Sin seguimiento posterior",
      anios_seguimiento < anios_consolidado ~ etiqueta_incompleto,
      TRUE ~ etiqueta_consolidado
    ),
    provincia = factor(provincia, levels = c(provincias, "Costa Rica")),
    sexo = factor(sexo, levels = c("Hombre", "Mujer")),
    edad_grupo = factor(edad_grupo, levels = c("Todas las edades", grupos_edad))
  ) %>%
  arrange(edad_grupo, provincia, sexo, anio_ocurrencia)

# Comprobación: el total nacional de todas las edades es la suma de las
# 20 edades, y cada provincia aporta lo suyo.
control <- porcentajes %>%
  filter(provincia == "Costa Rica") %>%
  group_by(es_total = edad_grupo == "Todas las edades") %>%
  summarise(v = sum(total_defunciones), .groups = "drop")
stopifnot(nrow(control) == 2, control$v[1] == control$v[2],
          sum(base$defunciones) == control$v[1])

# -----------------------------------------------------------------------------
# 7. GRÁFICOS
# -----------------------------------------------------------------------------

dir.create(carpeta_graficos, showWarnings = FALSE, recursive = TRUE)

rango_anios <- paste0(min(anios_ocurrencia), "-", max(anios_ocurrencia))
nota <- paste0(
  "Defunción tardía: declarada en un año posterior al de su ocurrencia. Porcentaje = tardías / todas las defunciones ocurridas ese año.\n",
  "Círculo vacío: menos de ", anios_consolidado, " años de seguimiento; el porcentaje todavía puede subir. ",
  max(anios_ocurrencia), " no tiene valor: aún no hay declaraciones posteriores.\n",
  "Con pocas defunciones (menores de 15 años, provincias pequeñas) un solo caso tardío mueve mucho el porcentaje; ",
  "los totales están en el CSV.\n",
  "Se excluyen las defunciones con edad ignorada. Fuente: INEC, tabulados REDATAM de las bases de defunciones ",
  min(lectura$universo), "-", max(lectura$universo), " (declaraciones hasta ", corte, ")",
  if (criterio_universos == "declaracion") ", solo registros declarados en el año de su base." else "."
)

formas_estado <- set_names(c(16, 21), c(etiqueta_consolidado, etiqueta_incompleto))

# Facetas; los ejes x en todos los paneles solo existen desde ggplot2 3.5.0
facetas <- function(variable, ncol) {
  argumentos <- list(facets = reformulate(variable), ncol = ncol)
  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    argumentos <- c(argumentos, axes = "all_x", axis.labels = "all_x")
  }
  do.call(facet_wrap, argumentos)
}

tema <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0, size = 8, color = "grey35"),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.key.width = unit(2, "lines")
  )

# Líneas por sexo, un punto por año (vacío si el seguimiento es incompleto)
graficar <- function(d, titulo, subtitulo, variable_faceta, ncol, cortes_x) {
  ggplot(d, aes(x = anio_ocurrencia, y = pct_tardias,
                color = sexo, linetype = sexo, group = sexo)) +
    geom_line(linewidth = 0.6, na.rm = TRUE) +
    geom_point(data = filter(d, !is.na(pct_tardias)),
               aes(shape = factor(estado, levels = names(formas_estado))),
               size = 2.2, stroke = 0.9, fill = "white") +
    scale_color_manual(values = colores_sexo, name = "Sexo", drop = FALSE) +
    scale_linetype_manual(values = c(Hombre = "solid", Mujer = "22"), name = "Sexo",
                          drop = FALSE) +
    scale_shape_manual(values = formas_estado, name = "Seguimiento", drop = FALSE) +
    scale_x_continuous(breaks = cortes_x, limits = range(anios_ocurrencia)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    # que los puntos en 0 % se vean enteros y no cortados por el borde del panel
    coord_cartesian(clip = "off") +
    facetas(variable_faceta, ncol) +
    guides(color = guide_legend(order = 1), linetype = guide_legend(order = 1),
           shape = guide_legend(order = 2, override.aes = list(color = "grey25"))) +
    labs(title = titulo, subtitle = subtitulo, x = "Año de ocurrencia",
         y = "Defunciones con registro tardío (%)", caption = nota) +
    tema
}

titulo_edad <- function(g) {
  case_when(g == "Menores de 1 año" ~ "menores de 1 año",
            g == "90 y más" ~ "90 años y más",
            g == "Todas las edades" ~ "todas las edades",
            TRUE ~ g)
}

guardados <- character(0)
guardar <- function(grafico, archivo, ancho, alto) {
  ruta <- file.path(carpeta_graficos, archivo)
  ggsave(ruta, grafico, width = ancho, height = alto, units = "in", dpi = 300)
  guardados <<- c(guardados, archivo)
}

subtitulo_provincias <- paste0(
  "Porcentaje de defunciones declaradas después del año en que ocurrieron, por sexo y provincia, ",
  rango_anios)

# 7.1 Resumen: todas las edades juntas, por provincia
guardar(
  graficar(filter(porcentajes, edad_grupo == "Todas las edades"),
           "Registro tardío de defunciones: todas las edades",
           subtitulo_provincias, "provincia", 2, anios_ocurrencia),
  "registro_tardio_00_todas_las_edades.png", 10, 11)

# 7.2 Resumen: Costa Rica, un panel por grupo de edad
guardar(
  graficar(filter(porcentajes, provincia == "Costa Rica", edad_grupo != "Todas las edades"),
           "Registro tardío de defunciones por grupo de edad, Costa Rica",
           paste0("Porcentaje de defunciones declaradas después del año en que ocurrieron, por sexo, ",
                  rango_anios),
           "edad_grupo", 4, seq(min(anios_ocurrencia), max(anios_ocurrencia), by = 2)),
  "registro_tardio_00_resumen_por_edad.png", 12, 14)

# 7.3 Un gráfico por grupo de edad, por provincia
for (i in seq_along(grupos_edad)) {
  g <- grupos_edad[i]
  guardar(
    graficar(filter(porcentajes, edad_grupo == g),
             paste0("Registro tardío de defunciones: ", titulo_edad(g)),
             subtitulo_provincias, "provincia", 2, anios_ocurrencia),
    sprintf("registro_tardio_%02d_%s.png", i, nombre_seguro(g)), 10, 11)
}

# -----------------------------------------------------------------------------
# 8. TABLAS Y RESUMEN EN CONSOLA
# -----------------------------------------------------------------------------

write_excel_csv(
  porcentajes %>%
    mutate(pct_tardias = round(pct_tardias, 3)) %>%
    select(anio_ocurrencia, sexo, provincia, edad_grupo, total_defunciones,
           defunciones_tardias, pct_tardias, anios_seguimiento, estado,
           declaradas_antes_de_ocurrir),
  file.path(carpeta_graficos, "porcentaje_registro_tardio_por_edad.csv"))
write_excel_csv(lectura, file.path(carpeta_graficos, "lectura_archivos_redatam.csv"))

anomalas <- sum(porcentajes$declaradas_antes_de_ocurrir[
  porcentajes$provincia == "Costa Rica" & porcentajes$edad_grupo == "Todas las edades"])
if (anomalas > 0) {
  cat("\nAviso: defunciones de ", rango_anios, " declaradas antes de ocurrir (error de fecha): ",
      anomalas, ". Se cuentan en el total, no como tardías.\n", sep = "")
}

cat("\nPORCENTAJE DE DEFUNCIONES TARDÍAS, COSTA RICA, TODAS LAS EDADES\n")
porcentajes %>%
  filter(provincia == "Costa Rica", edad_grupo == "Todas las edades") %>%
  select(anio_ocurrencia, sexo, pct_tardias) %>%
  mutate(pct_tardias = round(pct_tardias, 2)) %>%
  pivot_wider(names_from = sexo, values_from = pct_tardias) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\nListo:", length(guardados), "gráficos y 2 tablas en", normalizePath(carpeta_graficos), "\n")
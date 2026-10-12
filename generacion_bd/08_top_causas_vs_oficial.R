# =============================================================================
# Principales causas de muerte (categoría CIE-10) por año y comparación con
# cifras oficiales del INEC.
#
# Entrada:  bd/bdProcces/mortalidad_inec_cie10_mx_2015_2025.csv
# Salida:   imprime las comparaciones y guarda bd/bdProcces/top10_causas_2015_2025.csv
#
# Cifras oficiales usadas como control:
#   - Total de defunciones 2017-2024: casos de los microdatos públicos del INEC
#     (ANDA, catálogos 298 y 377). 2015: total de la exportación REDATAM 2015.
#   - Top 10 de 2015 y 2019: calculado directamente de las exportaciones REDATAM
#     del INEC (Defunciones_2015.xlsx y Defunciones_2019.xlsx).
#   - Otros años: llena oficial_top10_manual.csv con el Cuadro 3.27 / Figura 3.2
#     de "Estadísticas Vitales" de cada año (INEC usa "categorías", el mismo
#     nivel CIE-10 de 3 caracteres que esta base).
#   - El INEC registró de verdad a una mujer con el código 363 (C63, órganos 
#     genitales masculinos)
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)

carpeta <- "ruta de la carpeta con los datos"
base <- read_csv(file.path(carpeta, "mortalidad_inec_cie10_mx_2015_2025.csv"),
                 col_types = cols(.default = col_character())) |>
  mutate(anio = as.integer(anio), mortalidad = as.numeric(mortalidad))

# Títulos oficiales CIE-10 (OMS/OPS) de las categorías más frecuentes.
# Los Excel del INEC solo traen nombres de subcategorías, por eso se ponen aquí.
nombres_cat <- tribble(
  ~CIE10_4, ~nombre,
  "I21", "Infarto agudo del miocardio",
  "I25", "Enfermedad isquémica crónica del corazón",
  "J44", "Otras enfermedades pulmonares obstructivas crónicas (EPOC)",
  "J18", "Neumonía, organismo no especificado",
  "C16", "Tumor maligno del estómago",
  "V89", "Accidente de vehículo, tipo no especificado",
  "E14", "Diabetes mellitus, no especificada",
  "E11", "Diabetes mellitus no insulinodependiente (tipo 2)",
  "I67", "Otras enfermedades cerebrovasculares",
  "C61", "Tumor maligno de la próstata",
  "N18", "Enfermedad renal crónica",
  "I11", "Enfermedad cardíaca hipertensiva",
  "U07", "COVID-19 (y trastorno por vapeo, U07.0)",
  "C34", "Tumor maligno de los bronquios y del pulmón",
  "C18", "Tumor maligno del colon",
  "C50", "Tumor maligno de la mama",
  "C22", "Tumor maligno del hígado y vías biliares intrahepáticas",
  "X95", "Agresión con disparo de otras armas de fuego",
  "I10", "Hipertensión esencial (primaria)",
  "I64", "Accidente vascular encefálico, no especificado",
  "I61", "Hemorragia intraencefálica",
  "G30", "Enfermedad de Alzheimer",
  "K74", "Fibrosis y cirrosis del hígado",
  "J96", "Insuficiencia respiratoria, no clasificada en otra parte",
  "A41", "Otras sepsis",
  "F03", "Demencia, no especificada",
  "X59", "Exposición a factores no especificados",
  "V29", "Motociclista lesionado en otros accidentes de transporte y los no especificados",
  "C53", "Tumor maligno del cuello del útero",
  "C54", "Tumor maligno del cuerpo del útero",
  "C56", "Tumor maligno del ovario",
  "C25", "Tumor maligno del páncreas",
  "X70", "Lesión autoinfligida intencionalmente por ahorcamiento",
  "N39", "Otros trastornos del sistema urinario"
)

# ---- 1. Totales por año -----------------------------------------------------
# Se suman las edades (sin la fila "Total" para no contar doble).
por_anio <- base |> filter(edad != "Total") |>
  group_by(anio) |> summarise(defunciones_base = sum(mortalidad, na.rm = TRUE), .groups = "drop")

oficial_total <- tribble(
  ~anio, ~defunciones_oficial, ~fuente,
  2015L, 21038, "REDATAM 2015 (Defunciones_2015.xlsx)",
  2017L, 23246, "Microdatos INEC (ANDA 298)",
  2018L, 23762, "Microdatos INEC (ANDA 298)",
  2019L, 24292, "Microdatos INEC (ANDA 298)",
  2020L, 26205, "Microdatos INEC (ANDA 298)",
  2021L, 31093, "Microdatos INEC (ANDA 298)",
  2022L, 28931, "Microdatos INEC (ANDA 377)",
  2023L, 29185, "Microdatos INEC (ANDA 377)",
  2024L, 30179, "Microdatos INEC (ANDA 377)"
)

cat("\n== 1. Total de defunciones por año: base vs oficial\n")
print(por_anio |> left_join(oficial_total, by = "anio") |>
        mutate(diferencia = defunciones_base - defunciones_oficial,
               estado = case_when(is.na(defunciones_oficial) ~ "sin dato oficial",
                                  diferencia == 0 ~ "OK",
                                  TRUE ~ "REVISAR")), n = 20)
# 2016 sin dato oficial aquí; 2025 son datos preliminares.

# ---- 2. Top 10 por año ------------------------------------------------------
top10 <- base |> filter(edad != "Total") |>
  group_by(anio, INEC_4, CIE10_4) |>
  summarise(defunciones = sum(mortalidad, na.rm = TRUE), .groups = "drop") |>
  group_by(anio) |>
  mutate(pct = round(100 * defunciones / sum(defunciones), 1)) |>
  slice_max(defunciones, n = 10, with_ties = FALSE) |>
  mutate(puesto = row_number()) |>
  ungroup() |>
  left_join(nombres_cat, by = "CIE10_4") |>
  select(anio, puesto, INEC_4, CIE10_4, nombre, defunciones, pct)

cat("\n== 2. Diez categorías más frecuentes por año\n")
for (y in sort(unique(top10$anio))) {
  cat("\n--", y, "\n"); print(filter(top10, anio == y) |> select(-anio), n = 10)
}

# ---- 3. Comparación con el top 10 oficial -----------------------------------
oficial_top10 <- tribble(
  ~anio, ~CIE10_4, ~defunciones_oficial,
  2015L, "I21", 1408, 2015L, "I25", 988, 2015L, "J44", 725, 2015L, "J18", 678,
  2015L, "C16", 660,  2015L, "V89", 622, 2015L, "E14", 510, 2015L, "E11", 457,
  2015L, "I67", 439,  2015L, "C61", 391,
  2019L, "I21", 1486, 2019L, "J44", 910, 2019L, "I25", 742, 2019L, "E11", 713,
  2019L, "E14", 701,  2019L, "J18", 668, 2019L, "C16", 651, 2019L, "N18", 558,
  2019L, "I67", 505,  2019L, "I11", 453
)

# Opcional: valores copiados de "Estadísticas Vitales" (Cuadro 3.27 / Figura 3.2).
# Columnas: anio,CIE10_4,defunciones_oficial
manual <- file.path(carpeta, "oficial_top10_manual.csv")
if (file.exists(manual)) {
  oficial_top10 <- bind_rows(oficial_top10,
                             read_csv(manual, col_types = cols(anio = "i", CIE10_4 = "c", defunciones_oficial = "d")))
}

base_cat <- base |> filter(edad != "Total") |>
  group_by(anio, CIE10_4) |> summarise(defunciones_base = sum(mortalidad, na.rm = TRUE), .groups = "drop")

comp <- oficial_top10 |>
  left_join(base_cat, by = c("anio", "CIE10_4")) |>
  left_join(nombres_cat, by = "CIE10_4") |>
  mutate(diferencia = defunciones_base - defunciones_oficial,
         estado = if_else(!is.na(diferencia) & diferencia == 0, "OK", "REVISAR"))

cat("\n== 3. Top 10 oficial vs base\n")
print(comp |> select(anio, CIE10_4, nombre, defunciones_oficial, defunciones_base, diferencia, estado), n = 100)
cat("\nResumen:", sum(comp$estado == "OK"), "OK |", sum(comp$estado == "REVISAR"), "REVISAR\n")

# ---- 4. Análisis por sexo ---------------------------------------------------
porsexo <- base |> filter(edad != "Total") |>
  group_by(anio, sexo, INEC_4, CIE10_4) |>
  summarise(defunciones = sum(mortalidad, na.rm = TRUE), .groups = "drop")

# 4a. Totales por sexo vs oficial (REDATAM 2015 y 2019)
oficial_sexo <- tribble(
  ~anio, ~sexo, ~defunciones_oficial,
  2015L, "Hombre", 12111, 2015L, "Mujer", 8927,
  2019L, "Hombre", 13584, 2019L, "Mujer", 10708
)
cat("\n== 4a. Total de defunciones por sexo\n")
print(porsexo |> group_by(anio, sexo) |> summarise(defunciones_base = sum(defunciones), .groups = "drop") |>
        left_join(oficial_sexo, by = c("anio", "sexo")) |>
        mutate(estado = case_when(is.na(defunciones_oficial) ~ "",
                                  defunciones_base == defunciones_oficial ~ "OK", TRUE ~ "REVISAR")), n = 30)

# 4b. Top 10 por sexo y año
top10_sexo <- porsexo |>
  group_by(anio, sexo) |>
  mutate(pct = round(100 * defunciones / sum(defunciones), 1)) |>
  slice_max(defunciones, n = 10, with_ties = FALSE) |>
  mutate(puesto = row_number()) |> ungroup() |>
  left_join(nombres_cat, by = "CIE10_4") |>
  select(anio, sexo, puesto, INEC_4, CIE10_4, nombre, defunciones, pct)

cat("\n== 4b. Diez categorías más frecuentes por sexo (lado a lado)\n")
for (y in sort(unique(top10_sexo$anio))) {
  cat("\n--", y, "\n")
  print(top10_sexo |> filter(anio == y) |>
          mutate(causa = paste0(CIE10_4, " (", defunciones, ")")) |>
          select(puesto, sexo, causa) |>
          pivot_wider(names_from = sexo, values_from = causa), n = 10)
}

# 4c. Top 10 por sexo oficial (REDATAM 2015 y 2019) vs base
oficial_top10_sexo <- tribble(
  ~anio, ~sexo, ~CIE10_4, ~defunciones_oficial,
  2015L,"Hombre","I21",892, 2015L,"Hombre","I25",584, 2015L,"Hombre","V89",536, 2015L,"Hombre","C16",434,
  2015L,"Hombre","J18",395, 2015L,"Hombre","C61",391, 2015L,"Hombre","J44",380, 2015L,"Hombre","X95",355,
  2015L,"Hombre","E14",257, 2015L,"Hombre","E11",213,
  2015L,"Mujer","I21",516,  2015L,"Mujer","I25",404,  2015L,"Mujer","J44",345,  2015L,"Mujer","C50",320,
  2015L,"Mujer","J18",283,  2015L,"Mujer","E14",253,  2015L,"Mujer","E11",244,  2015L,"Mujer","I67",233,
  2015L,"Mujer","C16",226,  2015L,"Mujer","C18",195,
  2019L,"Hombre","I21",919, 2019L,"Hombre","J44",479, 2019L,"Hombre","I25",469, 2019L,"Hombre","C61",428,
  2019L,"Hombre","C16",417, 2019L,"Hombre","J18",384, 2019L,"Hombre","X95",358, 2019L,"Hombre","E11",339,
  2019L,"Hombre","E14",328, 2019L,"Hombre","V29",327,
  2019L,"Mujer","I21",567,  2019L,"Mujer","J44",431,  2019L,"Mujer","C50",389,  2019L,"Mujer","E11",374,
  2019L,"Mujer","E14",373,  2019L,"Mujer","J18",284,  2019L,"Mujer","I25",273,  2019L,"Mujer","I11",253,
  2019L,"Mujer","I67",247,  2019L,"Mujer","C18",237
)
comp_sexo <- oficial_top10_sexo |>
  left_join(porsexo |> select(anio, sexo, CIE10_4, defunciones_base = defunciones),
            by = c("anio", "sexo", "CIE10_4")) |>
  mutate(estado = if_else(!is.na(defunciones_base) & defunciones_base == defunciones_oficial, "OK", "REVISAR"))
cat("\n== 4c. Top 10 por sexo oficial vs base:", sum(comp_sexo$estado == "OK"), "OK |",
    sum(comp_sexo$estado == "REVISAR"), "REVISAR\n")
if (any(comp_sexo$estado == "REVISAR")) print(filter(comp_sexo, estado == "REVISAR"))

# 4d. ¿El reparto por sexo tiene sentido? (% de hombres por causa, todos los años)
esperado <- tribble(
  ~CIE10_4, ~regla,               ~min_h, ~max_h,
  "C61", "solo hombres",           100,    100,
  "C53", "solo mujeres",           0,      0,
  "C54", "solo mujeres",           0,      0,
  "C56", "solo mujeres",           0,      0,
  "C50", "casi solo mujeres",      0,      5,
  "X95", "mayoría hombres",        80,     100,
  "X70", "mayoría hombres",        70,     100,
  "V89", "mayoría hombres",        70,     100,
  "V29", "mayoría hombres",        80,     100,
  "I21", "más hombres",            50,     80,
  "C16", "más hombres",            50,     80,
  "F03", "más mujeres",            20,     50,
  "G30", "más mujeres",            20,     50
)
reparto <- porsexo |> filter(CIE10_4 %in% esperado$CIE10_4) |>
  group_by(anio, CIE10_4) |>
  summarise(hombres = sum(defunciones[sexo == "Hombre"]), mujeres = sum(defunciones[sexo == "Mujer"]),
            .groups = "drop") |>
  filter(hombres + mujeres > 0) |>
  mutate(pct_hombres = round(100 * hombres / (hombres + mujeres), 1)) |>
  left_join(esperado, by = "CIE10_4") |>
  mutate(estado = if_else(pct_hombres >= min_h & pct_hombres <= max_h, "OK", "REVISAR"))

cat("\n== 4d. Reparto por sexo (% hombres) por causa y año\n")
print(reparto |> select(CIE10_4, regla, anio, pct_hombres) |>
        pivot_wider(names_from = anio, values_from = pct_hombres), n = 30, width = Inf)
cat("\nCasos fuera de lo esperado:\n")
print(reparto |> filter(estado == "REVISAR") |> select(anio, CIE10_4, regla, hombres, mujeres, pct_hombres), n = 50)

# ---- 5. Guardar -------------------------------------------------------------
write_csv(top10, file.path(carpeta, "top10_causas_2015_2025.csv"))
write_csv(top10_sexo, file.path(carpeta, "top10_causas_sexo_2015_2025.csv"))
cat("\nGuardado:\n  top10_causas_2015_2025.csv\n  top10_causas_sexo_2015_2025.csv\n")
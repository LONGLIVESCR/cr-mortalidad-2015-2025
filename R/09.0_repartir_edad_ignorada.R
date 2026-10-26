# =============================================================================
# 09.0 Repartir la edad ignorada en las bases de edad simple y quinquenal
#      (HMD, sección 4.1)
# =============================================================================
#
# Corre al final, después de 01 → 08, y no cambia ningún archivo anterior.
# Lee las dos bases de mortalidad y escribe dos bases nuevas en las que las
# defunciones de edad ignorada se reparten entre las edades conocidas, en
# proporción a las defunciones de cada edad (Methods Protocol v6, 4.1):
#
#   D*(x) = D(x) + D_ign · D(x) / Σ D(x)
#
# Entrada (salidas de 01 y 02)                  Salida (nueva)
#   mortalidad_mx_1000_2015_2025.csv              ..._edad_repartida.csv
#   mortalidad_mx_1000_quinquenal_2015_2025.csv   ..._edad_repartida.csv
#
# Reglas
#   - Hombre y Mujer se reparten dentro de cada sexo, año y provincia, y el
#     Total se rehace como Hombre + Mujer. Si en un año y provincia el Total
#     del INEC no es Hombre + Mujer (por ejemplo, por sexo ignorado), ese
#     Total se reparte con sus propios datos y se avisa.
#   - Se agrega mortalidad_observada con el conteo original del INEC y se
#     recalculan mx (por persona) y mx_1000 (por 1 000), con el mismo
#     formato en las dos bases, aunque la base de entrada traiga otra cosa.
#   - Después de repartir y verificar, se eliminan las filas Ignorada (ya
#     con 0 defunciones). Por eso, en las bases nuevas, mortalidad_observada
#     suma menos que mortalidad: la diferencia son las defunciones repartidas.
#   - Las defunciones dejan de ser enteras, como en la HMD.
# =============================================================================

library(dplyr)
library(readr)

source("R/00.0_config.R")

entrada <- c(
  simple     = salida$edad_simple,
  quinquenal = salida$quinquenal
)

faltan <- entrada[!file.exists(entrada)]
if (length(faltan) > 0) {
  stop("Faltan las bases: ", paste(faltan, collapse = ", "), ". Corra antes 01 y 02.")
}

nombre_repartida <- function(ruta) sub("\\.csv$", "_edad_repartida.csv", ruta)

decimal_fijo <- function(x, decimales = 10) {
  if_else(is.na(x), NA_character_, sprintf(paste0("%.", decimales, "f"), x))
}

calcular_tasas <- function(datos) {
  datos %>%
    mutate(
      mx = if_else(
        is.na(exposicion) | exposicion == 0,
        NA_real_,
        mortalidad / exposicion
      ),
      mx_1000 = mx * 1000
    )
}

# Solo se necesitan estas columnas; mx y mx_1000 se recalculan.
leer_base <- function(ruta) {
  base <- read_csv(ruta, col_types = cols(.default = col_character()))
  faltan_col <- setdiff(c("edad", "sexo", "anio", "region", "exposicion", "mortalidad"), names(base))
  if (length(faltan_col) > 0) {
    stop(ruta, " no tiene las columnas: ", paste(faltan_col, collapse = ", "))
  }
  if ("mortalidad_observada" %in% names(base)) {
    stop(ruta, " ya trae la edad ignorada repartida.")
  }
  base %>%
    mutate(
      anio       = as.integer(anio),
      exposicion = as.numeric(exposicion),
      mortalidad = as.numeric(mortalidad),
      orden_fila = row_number()
    )
}


# =============================================================================
# PASO 1. Funciones de reparto
# =============================================================================

# Reparte la fila Ignorada dentro de cada grupo de `llaves`.
repartir <- function(datos, llaves, nombre = NULL) {

  datos <- datos %>%
    mutate(
      mortalidad_observada = mortalidad,
      .ignorada = edad == "Ignorada",
      .peso     = if_else(.ignorada, 0, mortalidad)
    ) %>%
    group_by(across(all_of(llaves))) %>%
    mutate(
      .d_ignorada = sum(mortalidad[.ignorada], na.rm = TRUE),
      .suma_peso  = sum(.peso, na.rm = TRUE),
      mortalidad  = case_when(
        .d_ignorada == 0 | .suma_peso == 0 ~ mortalidad,
        .ignorada ~ 0,
        TRUE      ~ mortalidad + .d_ignorada * .peso / .suma_peso
      )
    ) %>%
    ungroup()

  if (!is.null(nombre)) {
    ign <- datos$.ignorada
    cat(nombre, ": defunciones de edad ignorada ", sum(datos$mortalidad_observada[ign], na.rm = TRUE),
        " | repartidas ", sum(datos$mortalidad_observada[ign] - datos$mortalidad[ign], na.rm = TRUE),
        " | sin repartir ", sum(datos$mortalidad[ign], na.rm = TRUE), "\n", sep = "")
  }

  select(datos, -starts_with("."))
}

# Hombre y Mujer por separado; Total = Hombre + Mujer.
repartir_por_sexo <- function(datos, nombre) {

  hombre_mujer <- datos %>%
    filter(sexo != "Total") %>%
    repartir(llaves = c("sexo", "anio", "region"), nombre = nombre)

  suma_sexos <- hombre_mujer %>%
    group_by(edad, anio, region) %>%
    summarise(
      hm     = sum(mortalidad),
      hm_obs = sum(mortalidad_observada),
      .groups = "drop"
    )

  total <- datos %>%
    filter(sexo == "Total") %>%
    repartir(llaves = c("sexo", "anio", "region")) %>%
    left_join(suma_sexos, by = c("edad", "anio", "region")) %>%
    group_by(anio, region) %>%
    mutate(cuadra = all(!is.na(hm_obs) & mortalidad_observada == hm_obs)) %>%
    ungroup()

  no_cuadran <- total %>% filter(!cuadra) %>% distinct(anio, region)
  if (nrow(no_cuadran) > 0) {
    warning(nombre, ": el Total del INEC no es igual a Hombre + Mujer en ",
            paste(no_cuadran$anio, no_cuadran$region, collapse = "; "),
            ". Ahí el Total se repartió con sus propios datos.")
  }

  total <- total %>%
    mutate(mortalidad = if_else(cuadra, hm, mortalidad)) %>%
    select(-hm, -hm_obs, -cuadra)

  bind_rows(hombre_mujer, total) %>% arrange(orden_fila)
}


# =============================================================================
# PASO 2. Repartir las dos bases
# =============================================================================

simple <- leer_base(entrada["simple"]) %>%
  repartir_por_sexo("Edad simple") %>%
  calcular_tasas() %>%
  select(edad, sexo, anio, region, exposicion, mortalidad, mortalidad_observada, mx, mx_1000)

quinquenal <- leer_base(entrada["quinquenal"]) %>%
  repartir_por_sexo("Quinquenal") %>%
  calcular_tasas() %>%
  select(edad, sexo, anio, region, exposicion, mortalidad, mortalidad_observada, mx, mx_1000)


# =============================================================================
# PASO 3. Verificar
# =============================================================================

tolerancia <- 1e-6

# Cada sexo, año y provincia conserva sus defunciones
conserva <- function(datos) {
  datos %>%
    group_by(sexo, anio, region) %>%
    summarise(d = abs(sum(mortalidad) - sum(mortalidad_observada)), .groups = "drop") %>%
    pull(d) %>%
    max()
}

# La quinquenal repartida es la suma de la edad simple repartida
grupo_quinquenal <- function(edad) {
  e <- suppressWarnings(as.integer(edad))
  case_when(
    is.na(e) ~ edad,
    e == 0   ~ "0",
    e <= 4   ~ "1-4",
    TRUE     ~ paste0(5 * (e %/% 5), "-", 5 * (e %/% 5) + 4)
  )
}

dif_quinquenal <- simple %>%
  mutate(edad = grupo_quinquenal(edad)) %>%
  group_by(edad, sexo, anio, region) %>%
  summarise(desde_simple = sum(mortalidad), .groups = "drop") %>%
  full_join(quinquenal, by = c("edad", "sexo", "anio", "region")) %>%
  mutate(d = abs(desde_simple - mortalidad)) %>%
  pull(d)

verificaciones <- tibble(
  prueba = c(
    "Edad simple: cada sexo, año y provincia conserva sus defunciones",
    "Quinquenal: cada sexo, año y provincia conserva sus defunciones",
    "Quinquenal repartida = suma de la edad simple repartida",
    "Ninguna base quedó con defunciones vacías",
    "Todas las filas Ignorada quedaron en 0 (se eliminan sin perder defunciones)"
  ),
  ok = c(
    conserva(simple) < tolerancia,
    conserva(quinquenal) < tolerancia,
    !anyNA(dif_quinquenal) && max(dif_quinquenal) < tolerancia,
    !anyNA(simple$mortalidad) && !anyNA(quinquenal$mortalidad),
    all(c(simple$mortalidad[simple$edad == "Ignorada"],
          quinquenal$mortalidad[quinquenal$edad == "Ignorada"]) == 0)
  )
)

cat("\n")
for (i in seq_len(nrow(verificaciones))) {
  cat(if (verificaciones$ok[i]) "[OK]   " else "[FALLA]", verificaciones$prueba[i], "\n")
}
if (!all(verificaciones$ok)) stop("Alguna verificación falló; no se guardaron las bases.")


# =============================================================================
# PASO 4. Eliminar las filas Ignorada y guardar
# =============================================================================

filas_antes <- c(nrow(simple), nrow(quinquenal))

simple     <- filter(simple,     edad != "Ignorada")
quinquenal <- filter(quinquenal, edad != "Ignorada")

cat("\nFilas Ignorada eliminadas (edad simple, quinquenal):",
    paste(filas_antes - c(nrow(simple), nrow(quinquenal)), collapse = ", "), "\n")

# Las dos bases salen con las mismas columnas: mx por persona y mx_1000 por
# 1 000 habitantes, ambas con 10 decimales.
write_csv(
  mutate(simple, mx = decimal_fijo(mx), mx_1000 = decimal_fijo(mx_1000)),
  nombre_repartida(entrada["simple"])
)

write_csv(
  mutate(quinquenal, mx = decimal_fijo(mx), mx_1000 = decimal_fijo(mx_1000)),
  nombre_repartida(entrada["quinquenal"])
)

cat("\nGuardado:\n")
cat(paste0("  ", nombre_repartida(entrada), "\n"), sep = "")

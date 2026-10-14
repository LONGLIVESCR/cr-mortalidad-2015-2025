# =============================================================================
# Verificar la base unificada mortalidad_inec_cie10_mx_2015_2025.csv
# Solo lee e imprime. 
#
# Compara contra la base original (mortalidad_cie10_mx_2015_2025.csv) y el
# diccionario (diccionario_inec_cie10_por_anio.csv).
# =============================================================================

library(dplyr)
library(readr)

carpeta <- "dirección de la carpeta con los datos procesados"
orig <- read_csv(file.path(carpeta, "mortalidad_cie10_mx_2015_2025.csv"),
                 col_types = cols(.default = col_character()))
unif <- read_csv(file.path(carpeta, "mortalidad_inec_cie10_mx_2015_2025.csv"),
                 col_types = cols(.default = col_character()))
dic  <- read_csv(file.path(carpeta, "diccionario_inec_cie10_por_anio.csv"),
                 col_types = cols(.default = col_character()))

resultados <- list()
revisar <- function(nombre, ok, detalle = NULL) {
  resultados[[length(resultados) + 1]] <<- data.frame(prueba = nombre, resultado = if (ok) "OK" else "FALLA")
  cat(if (ok) "[OK]    " else "[FALLA] ", nombre, "\n", sep = "")
  if (!ok && !is.null(detalle)) print(head(detalle, 20))
}

regla <- function(inec4) { n <- as.integer(inec4)
  paste0(LETTERS[n %/% 100], sprintf("%02d", n %% 100)) }

# ---- 1. Estructura ----------------------------------------------------------
cat("\n== 1. Estructura\n")
esperadas <- c("edad","sexo","anio","region","INEC_4","CIE10_4","mortalidad","exposicion","mx","mx_1000")
revisar("Columnas y orden correctos", identical(names(unif), esperadas),
        data.frame(tiene = paste(names(unif), collapse = ", ")))
revisar("Mismo número de filas que la original", nrow(unif) == nrow(orig),
        data.frame(original = nrow(orig), unificada = nrow(unif)))

# ---- 2. Nada cambió respecto a la original ----------------------------------
cat("\n== 2. Valores iguales a la original (fila por fila)\n")
if (nrow(unif) == nrow(orig)) {
  revisar("INEC_4 = CIE10_4 de la original", all(unif$INEC_4 == orig$CIE10_4),
          which(unif$INEC_4 != orig$CIE10_4))
  for (col in c("edad","sexo","anio","region","mortalidad","exposicion","mx","mx_1000")) {
    a <- unif[[col]]; b <- orig[[col]]
    iguales <- (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
    revisar(paste0("'", col, "' idéntica (mismo texto)"), all(iguales),
            data.frame(fila = which(!iguales), original = b[!iguales], unificada = a[!iguales]))
  }
}

# ---- 3. CIE10_4 correcta ----------------------------------------------------
cat("\n== 3. CIE10_4\n")
revisar("Sin CIE10_4 vacías", !any(is.na(unif$CIE10_4)), filter(unif, is.na(CIE10_4)))
mal <- unif |> distinct(INEC_4, CIE10_4) |> filter(CIE10_4 != regla(INEC_4))
revisar("CIE10_4 cumple la regla (01=A ... 26=Z)", nrow(mal) == 0, mal)
varias <- unif |> distinct(INEC_4, CIE10_4) |> count(INEC_4) |> filter(n > 1)
revisar("Cada INEC_4 tiene una sola CIE10_4", nrow(varias) == 0, varias)

en_dic <- dic |> distinct(anio, INEC_4, CIE10_4)
fuera <- unif |> distinct(anio, INEC_4, CIE10_4) |> anti_join(en_dic, by = c("anio","INEC_4","CIE10_4"))
esperado_fuera <- fuera |> filter(INEC_4 == "1009", anio %in% c("2015","2016"))
revisar("Todas las CIE10_4 existen en el diccionario de su año (salvo J09 en 2015-2016)",
        nrow(fuera) == nrow(esperado_fuera), anti_join(fuera, esperado_fuera, by = names(fuera)))
if (nrow(esperado_fuera) > 0) cat("        (J09 en 2015-2016 fuera de Ed2008: esperado)\n")

# ---- 4. mx y mx_1000 --------------------------------------------------------
cat("\n== 4. Tasas (mx = mortalidad / exposicion; mx_1000 = mx * 1000; 10 decimales)\n")
t <- unif |> mutate(m = as.numeric(mortalidad), e = as.numeric(exposicion),
                    mx_calc   = if_else(is.na(e) | e == 0, NA_character_, sprintf("%.10f", m / e)),
                    mx1k_calc = if_else(is.na(e) | e == 0, NA_character_, sprintf("%.10f", m / e * 1000)))
d_mx  <- t |> filter(!((is.na(mx) & is.na(mx_calc)) | (!is.na(mx) & !is.na(mx_calc) & mx == mx_calc)))
d_mx1 <- t |> filter(!((is.na(mx_1000) & is.na(mx1k_calc)) | (!is.na(mx_1000) & !is.na(mx1k_calc) & mx_1000 == mx1k_calc)))
revisar("mx recalculado coincide", nrow(d_mx) == 0, select(d_mx, edad, sexo, anio, region, INEC_4, mortalidad, exposicion, mx, mx_calc))
revisar("mx_1000 recalculado coincide", nrow(d_mx1) == 0, select(d_mx1, edad, sexo, anio, region, INEC_4, mx_1000, mx1k_calc))

# ---- 5. Coherencia de la fila Total -----------------------------------------
cat("\n== 5. Fila 'Total'\n")
grp <- c("sexo","anio","region","INEC_4")
suma_m <- unif |> filter(edad != "Total") |> group_by(across(all_of(grp))) |>
  summarise(suma = sum(as.numeric(mortalidad), na.rm = TRUE), .groups = "drop")
tot_m <- unif |> filter(edad == "Total") |> transmute(across(all_of(grp)), total = as.numeric(mortalidad)) |>
  inner_join(suma_m, by = grp) |> filter(total != suma)
revisar("mortalidad Total = suma de todas las edades (con Ignorada)", nrow(tot_m) == 0, tot_m)

suma_e <- unif |> filter(!edad %in% c("Total","Ignorada")) |> group_by(across(all_of(grp))) |>
  summarise(suma = sum(as.numeric(exposicion), na.rm = TRUE), .groups = "drop")
tot_e <- unif |> filter(edad == "Total") |> transmute(across(all_of(grp)), total = as.numeric(exposicion)) |>
  inner_join(suma_e, by = grp) |> filter(abs(total - suma) > 0.5)
revisar("exposicion Total = suma de edades sin Ignorada", nrow(tot_e) == 0, tot_e)

# ---- 6. Coherencia médica de las causas -------------------------------------
cat("\n== 6. Causas con sentido (sexo, edad, año)\n")
sinT <- unif |> filter(edad != "Total") |> mutate(m = as.numeric(mortalidad))
solo_h <- c("C61","C62","C63","N40")
solo_m <- c("C51","C52","C53","C54","C55","C56","C57","C58")
h_en_m <- sinT |> filter(sexo == "Mujer",  CIE10_4 %in% solo_h, m > 0) |> count(CIE10_4, wt = m)
m_en_h <- sinT |> filter(sexo == "Hombre", (CIE10_4 %in% solo_m | substr(CIE10_4,1,1) == "O"), m > 0) |> count(CIE10_4, wt = m)
revisar("Causas solo masculinas (próstata, testículo) sin mujeres", nrow(h_en_m) == 0, h_en_m)
revisar("Causas solo femeninas (útero, ovario, embarazo) sin hombres", nrow(m_en_h) == 0, m_en_h)

per <- sinT |> filter(substr(CIE10_4,1,1) == "P", m > 0) |>
  summarise(pct_menor1 = round(100 * sum(m[edad == "0"]) / sum(m), 1))
cat("        Perinatales (P) en menores de 1 año:", per$pct_menor1, "%\n")
revisar("Perinatales (P) casi todas en menores de 1 año (>= 90%)", isTRUE(per$pct_menor1 >= 90), per)

covid <- sinT |> filter(INEC_4 == "2107") |> group_by(anio) |> summarise(defunciones = sum(m, na.rm = TRUE))
cat("        U07 (2107) por año:\n"); print(covid)
revisar("U07 (covid/vapeo) sin defunciones antes de 2020",
        all(covid$defunciones[as.integer(covid$anio) < 2020] == 0), covid)

# ---- Resumen ----------------------------------------------------------------
res <- bind_rows(resultados)
cat("\n== RESUMEN:", sum(res$resultado == "OK"), "OK |", sum(res$resultado == "FALLA"), "FALLA\n")
if (any(res$resultado == "FALLA")) print(filter(res, resultado == "FALLA"))

library(dplyr)
library(readr)
library(ggplot2)

setwd("ruta de la carpeta con los datos")

datos <- read_csv("mortalidad_mx_2015_2025.csv", show_col_types = FALSE)

edad_num <- function(x) {
  dplyr::case_when(
    x == "Ignorada" ~ NA_real_,
    grepl("90", x) ~ 90,
    TRUE ~ suppressWarnings(as.numeric(x))
  )
}

por_decenio <- datos %>%
  filter(sexo == "Total") %>%
  mutate(
    e = edad_num(edad),
    grupo = case_when(
      is.na(e) ~ NA_character_,
      e >= 90 ~ "90+",
      TRUE ~ paste0(10 * (e %/% 10), "-", 10 * (e %/% 10) + 9)
    )
  ) %>%
  filter(!is.na(grupo)) %>%
  group_by(anio, grupo) %>%
  summarise(defunciones = sum(mortalidad, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    grupo = factor(
      grupo,
      levels = c(
        "0-9", "10-19", "20-29", "30-39", "40-49",
        "50-59", "60-69", "70-79", "80-89", "90+"
      )
    )
  )

p <- ggplot(por_decenio, aes(anio, defunciones, color = grupo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(por_decenio$anio))) +
  scale_color_brewer(palette = "Paired") +
  labs(
    title = "Defunciones en Costa Rica por grupo de edad",
    subtitle = "Sexo Total  ·  grupos de 10 años  ·  2015–2025",
    x = NULL,
    y = "Defunciones",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

png("defunciones_por_decenio.png", width = 1500, height = 900, res = 150)
print(p)
dev.off()

system("open defunciones_por_decenio.png")
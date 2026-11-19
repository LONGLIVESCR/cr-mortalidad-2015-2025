# =============================================================================
# COSTA RICA | DEFUNCIONES POR CAUSA Y EDAD SIMPLE | 2015-2025
# =============================================================================
# Script principal: solo configura y orquesta. Las funciones están en
# funciones_edad_simple.R, en esta misma carpeta.
#
# CÓMO CORRERLO (desde la raíz del repositorio):
#   Rscript generacion_base_de_datos_edades_simples/script_completo_corregido_2015_2025.R \
#           --entrada=RUTA/defunciones_redatam_sin_procesar
#   Opcionales: --salida=CARPETA  --referencia=ARCHIVO.csv
#   En vez de argumentos se pueden usar las variables de entorno
#   CR_MORT_EDAD_SIMPLE_ENTRADA, CR_MORT_EDAD_SIMPLE_SALIDA y
#   CR_MORT_EDAD_SIMPLE_REFERENCIA (p. ej. en un archivo .Renviron).
#   Desde RStudio: defina la variable de entorno o cambie el valor por defecto
#   de 'carpeta_entrada' abajo, y use 'Source'.
#
# ENTRADAS, juntas en la carpeta de entrada:
#   def_2015.xlsx ... def_2025.xlsx       [matrices REDATAM: sexo, edad, causa]
#   mortalidad.csv                        [exposición por edad, sexo, año, región]
#   diccionario_inec_cie10_por_anio.csv   [INEC_4 -> CIE10_4 según año]
#   mortalidad_mx_1000_2015_2025_edad_repartida.csv
#                                         [referencia para repartir 'Ignorada';
#                                          también se acepta el nombre original
#                                          '...edad_repartida(in) (1).csv']
#
# SALIDAS, en la carpeta de salida (por defecto 'salida_edad_simple' dentro
# de la carpeta de entrada):
#   mortalidad_inec_cie10_mx_edad_simple_2015_2025.csv (completa, con ceros)
#   mortalidad_inec_cie10_mx_edad_simple_YYYY.csv      (por año)
#   mortalidad_inec_cie10_mx_edad_simple_2015_2025_REPARTIDA.csv
#   control_totales_edad_simple_2015_2025.csv
#   control_diferencias_con_mortalidad_2.csv
#   control_diferencias_observadas_y_repartidas.csv
#   control_edades_ignoradas_sin_distribuir.csv
#   top10_causas_edad_simple_2015_2025.csv
#   top10_causas_sexo_edad_simple_2015_2025.csv
#   excepciones_diccionario_edad_simple.csv
#
# REGLAS:
#  * Edades 0..89 separadas; 90 y más agrupadas (exposición solo disponible 90+).
#  * Ignorada se conserva; Total se reconstruye incluyendo la edad ignorada.
#  * Se excluyen las filas 'Total' del Excel y el bloque de sexo 'Total' para
#    no duplicar defunciones; los totales de edad del Excel se usan para validar.
#  * En cada año/provincia/causa observada se completan TODAS las edades y
#    ambos sexos con 0 donde no hubo fallecimientos.
#  * La exposición viene SOLO del CSV. Se recalculan mx = muertes / exposición
#    y mx_1000 = mx * 1000; NO se importa mx del CSV (allí ya está por 1000).
#  * Claves de diccionario: anio + INEC_4; evita la relación many-to-many.
#  * La edad ignorada de cada causa se reparte con la distribución por edad de
#    todas las causas (método pendiente de confirmar con el profesor).
# =============================================================================


# ---- 0. CONFIGURACIÓN --------------------------------------------------------

# Carpeta de este script (sirve con Rscript y con source()/RStudio).
carpeta_script <- local({
  archivo <- NULL
  for (i in rev(seq_len(sys.nframe()))) {
    archivo <- sys.frame(i)$ofile
    if (!is.null(archivo)) break
  }
  if (is.null(archivo)) {
    arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
    if (length(arg)) archivo <- sub('^--file=', '', arg[1])
  }
  if (is.null(archivo)) 'generacion_base_de_datos_edades_simples'
  else dirname(normalizePath(archivo))
})
# Las etiquetas ('90 años y más', 'San José', 'Limón') necesitan una sesión
# que pueda representar tildes y eñes.
if (!isTRUE(l10n_info()[['UTF-8']]) && !isTRUE(l10n_info()[['Latin-1']]))
  stop('La sesion de R no usa UTF-8. En Linux o Mac corra con ',
       'LANG=es_CR.UTF-8 (o C.UTF-8); en Windows use R 4.2 o posterior.',
       call. = FALSE)
source(file.path(carpeta_script, 'funciones_edad_simple.R'), encoding = 'UTF-8')
cargar_paquetes(c('data.table', 'readxl'))

# fwrite usa esta opción: evita notación científica en las salidas.
options(scipen = 999)

CONFIG <- list(
  carpeta_entrada = opcion('entrada', 'CR_MORT_EDAD_SIMPLE_ENTRADA',
    por_defecto = file.path('data-raw', 'defunciones_redatam_sin_procesar')),
  # NULL = subcarpeta 'salida_edad_simple' dentro de la carpeta de entrada.
  carpeta_salida = opcion('salida', 'CR_MORT_EDAD_SIMPLE_SALIDA'),
  # NULL = busca el nombre por defecto dentro de la carpeta de entrada.
  archivo_referencia = opcion('referencia', 'CR_MORT_EDAD_SIMPLE_REFERENCIA'),
  anios = 2015:2025,
  # TRUE: conserva filas cero para todas las edades y sexos dentro de cada
  # causa que ocurra en una provincia-año (necesario para el reparto).
  completar_ceros = TRUE,
  # TRUE: además de la base principal, produce una base sin filas cero.
  guardar_version_compacta = FALSE,
  # TRUE: ejecuta la sección 6 (reparto de la edad ignorada por causa).
  repartir_ignoradas = TRUE
)

cfg <- preparar_configuracion(CONFIG)
verificar_entradas(cfg)
preparar_carpeta_salida(cfg$carpeta_salida)
message('Entrada: ', normalizePath(cfg$carpeta_entrada))
message('Salida:  ', normalizePath(cfg$carpeta_salida))


# ---- 1-2. EXPOSICIÓN, DICCIONARIO Y REFERENCIA -------------------------------
paso('1. Exposición, diccionario y referencia')
exposicion <- con_contexto('Exposición',
  leer_exposicion(cfg$entrada$exposicion, cfg))
insumos <- list(
  exposicion  = exposicion$exposicion,
  control_csv = exposicion$control_csv,
  diccionario = con_contexto('Diccionario',
                             leer_diccionario(cfg$entrada$diccionario)),
  excepciones = excepciones_diccionario()
)
# La referencia del reparto se valida aquí, antes de procesar los 11 años,
# para que un error en ella aparezca de inmediato.
if (cfg$repartir_ignoradas) {
  message('Referencia: ', basename(cfg$entrada$referencia))
  pesos <- con_contexto('Referencia',
    preparar_pesos_referencia(cfg$entrada$referencia, cfg))
}


# ---- 3-4. BASE POR CAUSA Y EDAD SIMPLE, UN AÑO A LA VEZ ---------------------
# Cada año se escribe al terminar, para no cargar los 11 años en memoria.
paso('2. Base por causa y edad simple')
borrar_si_existe(cfg$salida$consolidada)
if (cfg$guardar_version_compacta) borrar_si_existe(cfg$salida$compacta)

resultados_anio <- lapply(cfg$anios, function(anio) {
  con_contexto(paste('Año', anio), procesar_anio(anio, cfg, insumos))
})


# ---- 5. INFORMES Y TOP 10 ----------------------------------------------------
paso('3. Informes y top 10')
informe <- escribir_informes(resultados_anio, cfg, insumos$excepciones)

cat('\n================== RESUMEN FINAL ==================\n')
print(informe$resumen)
cat('\nTotal defunciones sin duplicar: ',
    sum(informe$resumen$muertes_sin_total), '\n')
cat('Discrepancias con ', basename(cfg$entrada$exposicion), ': ',
    informe$n_discrepancias, '\n', sep = '')
cat('Excepciones de diccionario registradas: ', informe$n_excepciones, '\n')
cat('Base consolidada: ', cfg$salida$consolidada, '\n')
cat('Archivos anuales y controles: ', cfg$carpeta_salida, '\n')


# ---- 6. REPARTO DE LA EDAD IGNORADA POR CAUSA --------------------------------
if (cfg$repartir_ignoradas) {
  paso('4. Reparto de la edad ignorada')
  borrar_si_existe(cfg$salida$repartida)
  resultados_reparto <- lapply(cfg$anios, function(anio) {
    res <- con_contexto(paste('Reparto, año', anio),
                        repartir_anio(anio, cfg, pesos))
    gc(verbose = FALSE)
    res
  })
  reparto <- escribir_controles_reparto(resultados_reparto, cfg)

  cat('\nBASE FINAL: ', cfg$salida$repartida, '\n', sep = '')
  cat('COMPARACION ENTRE FUENTES: ', cfg$salida$control_fuentes,
      ' (', reparto$n_control, ' grupos con diferencias)\n', sep = '')
  cat('IGNORADAS SIN REPARTIR: ', cfg$salida$sin_repartir,
      ' (', reparto$n_sin_repartir, ' grupos, ',
      miles(reparto$muertes_sin_repartir),
      ' defunciones)\n', sep = '')
}

# Para abrir la base final, siempre que la RAM permita cargar el CSV:
# base_final_repartida <- data.table::fread(cfg$salida$repartida)

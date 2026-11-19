# =============================================================================
# COSTA RICA | DEFUNCIONES POR CAUSA Y EDAD SIMPLE | 2015–2025
# Adaptación de codigo_defunciones_edad_quinquenal.R 
# =============================================================================
# ENTRADAS, juntas en una misma carpeta:
#   def_2015.xlsx ... def_2025.xlsx
#   mortalidad.csv                       [exposicion por edad, sexo, año, región]
#   diccionario_inec_cie10_por_anio.csv [INEC_4 -> CIE10_4 según año]
#
# SALIDAS, en subcarpeta 'salida_edad_simple':
#   mortalidad_inec_cie10_mx_edad_simple_2015_2025.csv (completa, con ceros)
#   mortalidad_inec_cie10_mx_edad_simple_YYYY.csv      (por año)
#   control_totales_edad_simple_2015_2025.csv
#   control_diferencias_con_mortalidad_2.csv
#   top10_causas_edad_simple_2015_2025.csv
#   top10_causas_sexo_edad_simple_2015_2025.csv
#   excepciones_diccionario_edad_simple.csv
#
# REGLAS:
#  * Edades 0..89 separadas; 90 y más agrupadas (exposición solo disponible 90+).
#  * Ignorada se conserva; Total se reconstruye incluyendo la edad ignorada.
#  * Se excluyen las filas 'Total' del Excel y el bloque de sexo 'Total' para
#    no duplicar defunciones; los totales del Excel se usan para validar.
#  * En cada año/provincia/causa observada se completan TODOS los grupos de
#    edad y ambos sexos con 0 donde no hubo fallecimientos, como el original.
#  * Exposición viene SOLO del CSV. Se recalculan mx = muertes / exposición
#    y mx_1000 = mx * 1000; NO se importa mx del CSV (allí ya está por 1000).
#  * Claves de diccionario: anio + INEC_4; evita la relación many-to-many.
# La edad ignorada de cada causa se reparte con la distribución por edad de todas las causas.
# =============================================================================

# ---- 0. INSTALAR UNA VEZ (si falta alguno): ----------------------------------
# install.packages(c('data.table', 'readxl'))

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
})
options(scipen = 999)

# MODIFICAR SOLAMENTE ESTA RUTA (Windows permite barras /):
carpeta <- 'C:/Users/User/Downloads/defunciones_redatam_sin_procesar'

# TRUE: conserva filas cero para todas las edades y sexos dentro de cada causa
# que ocurra en una provincia-año. Es la opción equivalente al script previo.
COMPLETAR_CEROS <- TRUE

# TRUE: además de la base principal, produce una base compacta sin filas cero.
GUARDAR_VERSION_COMPACTA <- FALSE

carpeta_salida <- file.path(carpeta, 'salida_edad_simple')
dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)

anios <- 2015:2025
provincias <- c('San José', 'Alajuela', 'Cartago', 'Heredia',
                'Guanacaste', 'Puntarenas', 'Limón')
sexos <- c('Hombre', 'Mujer')
edades <- c(as.character(0:89), '90 años y más', 'Ignorada', 'Total')
llaves <- c('anio', 'region', 'INEC_4', 'edad', 'sexo')

entrada_exposicion <- file.path(carpeta, 'mortalidad.csv')
entrada_diccionario <- file.path(carpeta, 'diccionario_inec_cie10_por_anio.csv')
salida_consolidada <- file.path(carpeta_salida,
  'mortalidad_inec_cie10_mx_edad_simple_2015_2025.csv')
salida_compacta <- file.path(carpeta_salida,
  'mortalidad_inec_cie10_mx_edad_simple_2015_2025_solo_positivas.csv')

faltantes <- c(file.path(carpeta, paste0('def_', anios, '.xlsx')),
               entrada_exposicion, entrada_diccionario)
faltantes <- faltantes[!file.exists(faltantes)]
if (length(faltantes)) stop('No se encuentran estos archivos:\n',
                            paste(faltantes, collapse = '\n'))

# Funciones de apoyo ----------------------------------------------------------
arrastrar <- function(x) {
  # Rellenar hacia abajo A (sexo) y B (edad), SIN alterar las columnas de causa.
  x <- trimws(as.character(x))
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  i <- seq_along(x)
  ultimo <- cummax(ifelse(!is.na(x), i, 0L))
  y <- rep(NA_character_, length(x))
  ok <- ultimo > 0L
  y[ok] <- x[ultimo[ok]]
  y
}

inec_a_cie10 <- function(inec4) {
  n <- as.integer(inec4)
  letra <- n %/% 100L
  if (anyNA(n) || any(!(letra %in% 1:26)))
    stop('INEC_4 fuera de rango: revise los códigos.')
  paste0(LETTERS[letra], sprintf('%02d', n %% 100L))
}

fijo <- function(x) {
  res <- rep(NA_character_, length(x))
  sel <- !is.na(x) & is.finite(x)
  res[sel] <- sprintf('%.10f', x[sel])
  res
}

valor_num <- function(x, etiqueta) {
  x <- trimws(as.character(x))
  x[is.na(x) | x %in% c('-', '')] <- '0'
  malo <- !grepl('^[0-9]+$', x)
  if (any(malo))
    stop('Conteo no numérico en ', etiqueta, ': ', paste(unique(x[malo]), collapse=', '))
  as.integer(x)
}

# ---- 1. EXPOSICIÓN -----------------------------------------------------------
exp_raw <- fread(entrada_exposicion, encoding='UTF-8', na.strings=c('NA', ''),
                 colClasses=list(character=c('edad', 'sexo', 'region')))
columnas_exp <- c('edad','sexo','anio','region','exposicion','mortalidad')
if (!all(columnas_exp %in% names(exp_raw)))
  stop('mortalidad.csv no contiene columnas esperadas.')

# Los totales de muertes y mx del CSV NO sustituyen los del Excel original.
control_mortalidad_csv <- exp_raw[sexo %in% sexos,
  .(edad, sexo, anio=as.integer(anio), region,
    mortalidad_csv=as.numeric(mortalidad))]
exp <- exp_raw[sexo %in% sexos,
  .(edad, sexo, anio=as.integer(anio), region,
    exposicion=as.numeric(exposicion))]
exp <- exp[anio %in% anios]

if (anyDuplicated(exp, by=c('edad','sexo','anio','region')))
  stop('Hay duplicados en las llaves edad/sexo/año/provincia de exposición.')
if (!setequal(unique(exp$edad), edades[edades != 'Total']))
  stop('Las edades de mortalidad(2).csv no corresponden a 0..89, 90+, Ignorada.')
if (!setequal(unique(exp$region), provincias))
  stop('Las provincias de exposición no son las siete esperadas.')
if (exp[edad != 'Ignorada', anyNA(exposicion)])
  stop('Exposiciones NA en edades conocidas. Revisar mortalidad(2).csv.')

# La población total por sexo/provincia/año suma 0..89 más 90+; sin Ignorada.
exp_total <- exp[edad != 'Ignorada',
  .(exposicion = sum(exposicion)), by=.(anio, region, sexo)]
exp_total[, edad := 'Total']
exp <- rbind(exp, exp_total[, names(exp), with=FALSE])
setkeyv(exp, c('edad','sexo','anio','region'))

# ---- 2. DICCIONARIO ----------------------------------------------------------
dic <- fread(entrada_diccionario, encoding='UTF-8',
             colClasses=list(character=c('CIE10_4', 'codigo_redatam')))
if (!all(c('anio','INEC_4','CIE10_4') %in% names(dic)))
  stop('El diccionario no tiene anio, INEC_4 y CIE10_4.')
dic[, `:=`(anio=as.integer(anio), INEC_4=as.integer(INEC_4))]
dic_cat <- unique(dic[, .(anio, INEC_4, CIE10_4)])
if (anyDuplicated(dic_cat, by=c('anio','INEC_4')))
  stop('Un INEC_4 del mismo año tiene CIE10_4 diferentes. Revisar diccionario.')
if (any(dic_cat$CIE10_4 != inec_a_cie10(dic_cat$INEC_4)))
  stop('Código CIE10_4 inconsistente con el código INEC_4.')
setkey(dic_cat, anio, INEC_4)

# Excepciones verificadas en los 11 Excel proporcionados.
excepciones <- data.table(
  anio=c(2015L, 2016L, 2024L, 2025L),
  INEC_4=c(1009L, 1009L, 2155L, 2155L),
  CIE10_4=c('J09','J09','U55','U55')
)
registro_excepciones <- list()

# ---- 3. LECTOR DE UNA MATRIZ REDATAM ANUAL ----------------------------------
leer_def <- function(anio) {
  archivo <- file.path(carpeta, paste0('def_', anio, '.xlsx'))
  message('Leyendo ', basename(archivo))
  x <- as.data.table(read_excel(archivo, col_names=FALSE,
              col_types='text', range=cell_cols('A:K'), .name_repair='minimal'))
  if (ncol(x) != 11L) stop('Estructura diferente de A:K en ', archivo)
  setnames(x, c('etiqueta_sexo','etiqueta_edad','codigo', provincias, 'total_fila'))

  # En las filas sin etiqueta se mantiene el sexo y la edad del bloque anterior.
  x[, etiqueta_sexo := fifelse(grepl('^1\\. Hombres', etiqueta_sexo), 'Hombre',
                         fifelse(grepl('^2\\. Mujeres', etiqueta_sexo), 'Mujer',
                           fifelse(etiqueta_sexo=='Total', 'Total', NA_character_)))]
  x[, sexo := arrastrar(etiqueta_sexo)]
  x[, edad_original := arrastrar(etiqueta_edad)]

  # Código de causa: SOLO filas numéricas (excluye encabezados y notas).
  x <- x[!is.na(codigo) & grepl('^[0-9]{1,4}$', trimws(codigo))]
  if (nrow(x) == 0L) stop('No se hallaron causas en ', archivo)
  x[, INEC_4 := as.integer(codigo)]
  if (anyNA(x$sexo) || anyNA(x$edad_original))
    stop('Falta sexo o edad en filas con causa, año ', anio)
  if (!all(x$sexo %in% c(sexos,'Total')))
    stop('Etiquetas de sexo desconocidas, año ', anio)

  # Validar totales horizontales de las siete provincias contra columna K.
  x[, (provincias) := lapply(.SD, valor_num, etiqueta=paste0(anio, ' provincia')),
    .SDcols=provincias]
  x[, total_col := valor_num(total_fila, paste0(anio, ' total fila'))]
  if (any(rowSums(as.matrix(x[, ..provincias])) != x$total_col))
    stop('Columnas provinciales no cuadran con Total de fila en ', anio)

  # Edad original 999 detiene la corrida. 90,91,... -> 90 años y más.
  edad_num <- suppressWarnings(as.integer(x$edad_original))
  valido <- x$edad_original == 'Total' |
            grepl('ignor', x$edad_original, ignore.case=TRUE) |
            (!is.na(edad_num) & edad_num >= 0L & edad_num <= 130L)
  if (!all(valido))
    stop('Edades inesperadas en ', anio, ': ',
         paste(unique(x$edad_original[!valido]), collapse=', '))
  x[, edad := fifelse(edad_original == 'Total', 'Total',
               fifelse(grepl('ignor', edad_original, ignore.case=TRUE),
                 'Ignorada', fifelse(!is.na(edad_num) & edad_num >= 90L,
                                    '90 años y más', edad_original)))]

  # Los bloques Total del Excel solo se usan para comprobaciones; no sumar.
  mujeres_hombres <- x[sexo %in% sexos]
  orig_totales <- mujeres_hombres[edad == 'Total']
  orig_detalle <- mujeres_hombres[edad != 'Total']

  # Transformar siete columnas provinciales en 7 filas por causa y edad.
  transformar <- function(z) {
    melt(z, id.vars=c('sexo','edad','INEC_4'), measure.vars=provincias,
         variable.name='region', value.name='mortalidad', variable.factor=FALSE)
  }
  detalle <- transformar(orig_detalle)
  totales_excel <- transformar(orig_totales)
  detalle[, anio := as.integer(anio)]
  totales_excel[, anio := as.integer(anio)]

  detalle <- detalle[, .(mortalidad=sum(mortalidad)),
    by=.(anio, region, INEC_4, edad, sexo)]
  totales_excel <- totales_excel[, .(mortalidad_excel=sum(mortalidad)),
    by=.(anio, region, INEC_4, sexo)]

  # Cotejar el Total del Excel (por código) frente a la suma de edades reales.
  calculados <- detalle[, .(mortalidad=sum(mortalidad)),
    by=.(anio, region, INEC_4, sexo)]
  cotejo <- merge(calculados, totales_excel,
                  by=c('anio','region','INEC_4','sexo'), all=TRUE)
  cotejo[is.na(mortalidad), mortalidad := 0]
  cotejo[is.na(mortalidad_excel), mortalidad_excel := 0]
  errores <- cotejo[mortalidad != mortalidad_excel]
  if (nrow(errores)) {
    print(head(errores, 20))
    stop('No coinciden los totales originales por causa/sexo/región en ', anio)
  }

  # Construir Total de edad, por CAUSA: incluye Ignorada; NO duplica sexo.
  totales <- copy(calculados)
  totales[, edad := 'Total']
  rbind(detalle, totales, use.names=TRUE)
}

# ---- 4. PROCESAR POR AÑO Y ESCRIBIR SIN CARGAR 11 AÑOS EN RAM -----------------
if (file.exists(salida_consolidada)) file.remove(salida_consolidada)
if (GUARDAR_VERSION_COMPACTA && file.exists(salida_compacta))
  file.remove(salida_compacta)

reporte_anual <- list()
reporte_diferencias <- list()
top_acumulado <- list()

afor <- function(y) {
  real <- leer_def(y)
  # Límite de causas presentes: solo se generan ceros para causas que tuvieron
  # al menos una defunción en esa misma provincia y año.
  ocurridas <- real[edad == 'Total',
    .(total=sum(mortalidad)), by=.(anio,region,INEC_4)][total > 0L,
       .(anio,region,INEC_4)]

  if (COMPLETAR_CEROS) {
    # 93 edades (0..89, 90+, Ignorada, Total) x Hombre/Mujer POR causa-provin.
    mapa <- ocurridas[, .(edad=rep(edades, each=length(sexos)),
                       sexo=rep(sexos, times=length(edades))),
                     by=.(anio,region,INEC_4)]
    d <- merge(mapa, real, by=llaves, all.x=TRUE, sort=FALSE)
    d[is.na(mortalidad), mortalidad := 0L]
  } else {
    d <- real[ocurridas, on=.(anio,region,INEC_4), nomatch=0L]
  }

  # CIE10_4 del diccionario según anio e INEC_4 (muchas subcategorías -> UNA).
  d <- merge(d, dic_cat, by=c('anio','INEC_4'), all.x=TRUE, sort=FALSE)
  sin_dic <- unique(d[is.na(CIE10_4), .(anio,INEC_4)])
  if (nrow(sin_dic)) {
    inesperado <- sin_dic[!excepciones, on=.(anio,INEC_4)]
    if (nrow(inesperado)) {
      print(inesperado)
      stop('Códigos sin pareja no documentados en el año ', y)
    }
    registro_excepciones[[as.character(y)]] <<- copy(sin_dic)
    d[is.na(CIE10_4), CIE10_4 := inec_a_cie10(INEC_4)]
  }
  if (anyNA(d$CIE10_4) || any(d$CIE10_4 != inec_a_cie10(d$INEC_4)))
    stop('CIE10_4 incompleta o inconsistente en ', y)

  n_before <- nrow(d)
  total_before <- d[edad != 'Total', sum(mortalidad)]
  d <- merge(d, exp, by=c('edad','sexo','anio','region'),
             all.x=TRUE, sort=FALSE)
  if (nrow(d) != n_before) stop('La unión con exposición multiplicó filas.')
  if (d[edad != 'Ignorada', anyNA(exposicion)])
    stop('Falta exposición para edades conocidas en el año ', y)
  d[, mx := fifelse(is.na(exposicion) | exposicion == 0,
                   NA_real_, mortalidad/exposicion)]
  d[, mx_1000 := mx * 1000]

  # Todos los totales de edad se conservan, a pesar del cambio de granularidad.
  total_from_detail <- d[edad != 'Total', sum(mortalidad)]
  total_from_total <- d[edad == 'Total', sum(mortalidad)]
  if (total_before != total_from_detail || total_from_detail != total_from_total)
    stop('Se perdieron o duplicaron defunciones en ', y)
  if (d[edad=='Ignorada' & !is.na(exposicion), .N] > 0)
    stop('La edad ignorada no debería tener exposición.')

  # Comparación NO destructiva con las muertes del CSV auxiliar: puede diferir.
  porgrupo <- d[edad != 'Total',
    .(mortalidad_excel=sum(mortalidad)), by=.(edad,sexo,anio,region)]
  control <- merge(porgrupo,
    control_mortalidad_csv[anio==y],
    by=c('edad','sexo','anio','region'), all=TRUE)
  control[, diferencia := mortalidad_excel - mortalidad_csv]
  reporte_diferencias[[as.character(y)]] <<- control[
    is.na(diferencia) | diferencia != 0,
    .(edad,sexo,anio,region,mortalidad_excel,mortalidad_csv,diferencia)]

  # Top 10 sin duplicar la fila de edad Total ni crear causas adicionales.
  top_acumulado[[as.character(y)]] <<- d[edad=='Total',
    .(defunciones=sum(mortalidad)), by=.(anio,sexo,INEC_4,CIE10_4)]

  # Orden idéntico al del script quinquenal: año, provincia, causa, edad, sexo.
  d[, `:=`(orden_region=match(region, provincias),
           orden_edad=match(edad, edades),
           orden_sexo=match(sexo, sexos))]
  if (anyNA(d[, .(orden_region,orden_edad,orden_sexo)]))
    stop('Etiquetas fuera de catálogo en el año ', y)
  setorder(d, anio, orden_region, INEC_4, orden_edad, orden_sexo)

  out <- d[, .(edad,sexo,anio,region,INEC_4,CIE10_4,
               mortalidad,exposicion,
               mx=fijo(mx),mx_1000=fijo(mx_1000))]
  archivo_anual <- file.path(carpeta_salida,
    paste0('mortalidad_inec_cie10_mx_edad_simple_',y,'.csv'))
  fwrite(out, archivo_anual, na='NA', bom=FALSE)
  fwrite(out, salida_consolidada, append=file.exists(salida_consolidada),
         col.names=!file.exists(salida_consolidada), na='NA', bom=FALSE)

  if (GUARDAR_VERSION_COMPACTA) {
    no_ceros <- out[mortalidad != 0]
    fwrite(no_ceros, salida_compacta, append=file.exists(salida_compacta),
           col.names=!file.exists(salida_compacta), na='NA', bom=FALSE)
  }

  reporte_anual[[as.character(y)]] <<- data.table(
    anio=y, filas=nrow(out), causas_provincia=nrow(ocurridas),
    muertes_sin_total=total_from_detail, muertes_filas_total=total_from_total,
    ignoradas=d[edad=='Ignorada',sum(mortalidad)],
    filas_sin_exposicion=d[is.na(exposicion), .N],
    grupos_diferentes_del_csv=nrow(reporte_diferencias[[as.character(y)]])
  )
  message('  ',y,': ',format(nrow(out),big.mark='.'),
          ' filas | ',format(total_from_detail,big.mark='.'),' defunciones | ',
          nrow(reporte_diferencias[[as.character(y)]]),' discrepancias con CSV')
  invisible(NULL)
}

for (y in anios) afor(y)

# ---- 5. INFORMES Y TOP 10 ----------------------------------------------------
resumen <- rbindlist(reporte_anual)
fwrite(resumen, file.path(carpeta_salida,
  'control_totales_edad_simple_2015_2025.csv'))

comparaciones <- rbindlist(reporte_diferencias, fill=TRUE)
fwrite(comparaciones, file.path(carpeta_salida,
  'control_diferencias_con_mortalidad_2.csv'), na='NA')

casos_excepcion <- rbindlist(registro_excepciones, fill=TRUE)
if (!ncol(casos_excepcion)) casos_excepcion <- data.table(anio=integer(),INEC_4=integer())
casos_excepcion <- merge(casos_excepcion, excepciones,
                          by=c('anio','INEC_4'), all.x=TRUE)
fwrite(casos_excepcion, file.path(carpeta_salida,
  'excepciones_diccionario_edad_simple.csv'))

causas_top <- rbindlist(top_acumulado)
total_top <- causas_top[, .(defunciones=sum(defunciones)),
                        by=.(anio,INEC_4,CIE10_4)]
setorder(total_top, anio, -defunciones, INEC_4)
total_top[, `:=`(puesto=seq_len(.N),
                   pct=round(100*defunciones/sum(defunciones),1)), by=anio]
fwrite(total_top[puesto<=10,.(anio,puesto,INEC_4,CIE10_4,defunciones,pct)],
  file.path(carpeta_salida,'top10_causas_edad_simple_2015_2025.csv'))

setorder(causas_top, anio, sexo, -defunciones, INEC_4)
causas_top[, `:=`(puesto=seq_len(.N),
                     pct=round(100*defunciones/sum(defunciones),1)),
           by=.(anio,sexo)]
fwrite(causas_top[puesto<=10,
   .(anio,sexo,puesto,INEC_4,CIE10_4,defunciones,pct)],
   file.path(carpeta_salida,'top10_causas_sexo_edad_simple_2015_2025.csv'))

cat('\n================== RESUMEN FINAL ==================\n')
print(resumen)
cat('\nTotal defunciones sin duplicar: ',sum(resumen$muertes_sin_total),'\n')
cat('Discrepancias con mortalidad(2).csv: ', nrow(comparaciones),'\n')
cat('Excepciones de diccionario registradas: ', nrow(casos_excepcion),'\n')
cat('Base consolidada: ',salida_consolidada,'\n')
cat('Archivos anuales y controles: ',carpeta_salida,'\n')

# =============================================================================
# 6. REDISTRIBUIR EDADES IGNORADAS POR CAUSA USANDO REFERENCIA AGREGADA
#    y reemplazar mortalidad, mx y mx_1000 (SIN forzar igualdad de fuentes)
# =============================================================================
# Consultar al profe para confirmar la aceptación de este metodo.
# Agregar DESPUES de la seccion 5 original.
# Cada causa conserva sus muertes observadas en mortalidad_observada.
# La referencia aporta la DISTRIBUCION por edad dentro de sexo/anio/provincia,
# no cifras para copiar directamente a cada causa.
# Si las bases fuente difieren, se registran discrepancias y se conservan los
# totales de la base por causa (sin hacer pasar discrepancias por coincidencias).

if (!isTRUE(COMPLETAR_CEROS)) {
  stop('La seccion 6 requiere COMPLETAR_CEROS <- TRUE en la seccion 0.')
}

archivo_ref <- file.path(
  carpeta, 'mortalidad_mx_1000_2015_2025_edad_repartida(in) (1).csv'
)
if (!file.exists(archivo_ref)) stop('No existe la referencia: ', archivo_ref)

ref_raw <- data.table::fread(
  archivo_ref,
  colClasses = list(character = c('edad', 'sexo', 'region'))
)
requeridas_ref <- c('edad', 'sexo', 'anio', 'region',
                    'mortalidad_observada', 'mortalidad')
if (!all(requeridas_ref %in% names(ref_raw))) {
  stop('A la referencia le faltan: ',
       paste(setdiff(requeridas_ref, names(ref_raw)), collapse = ', '))
}

ref <- ref_raw[sexo %in% sexos,
  .(edad = as.character(edad), sexo, anio = as.integer(anio), region,
    observada_ref = as.numeric(mortalidad_observada),
    repartida_ref = as.numeric(mortalidad))]
rm(ref_raw)

claves_edad <- c('edad', 'sexo', 'anio', 'region')
claves_causa <- c('anio', 'region', 'sexo', 'INEC_4')
claves_grupo <- c('anio', 'region', 'sexo')
edades_conocidas <- c(as.character(0:89), '90 años y más')

if (anyDuplicated(ref, by = claves_edad)) {
  stop('Hay registros duplicados por edad, sexo, anio y region en referencia.')
}
if (anyNA(ref[, .(observada_ref, repartida_ref)])) {
  stop('La referencia contiene mortalidad NA.')
}
if (!all(ref$edad %in% edades_conocidas)) {
  stop('La referencia tiene edades inesperadas (tras filtrar Hombre/Mujer).')
}

# Se utiliza la distribucion de incrementos de la referencia (HMD).
# Si el grupo no tiene edades ignoradas para repartir, se usan las
# proporciones observadas por edad, por si nuestra fuente si tiene ignoradas.
# Solo cuando la referencia carece de muertes observadas en todas las edades
# de un grupo, no se puede inferir pesos y se conserva 'Ignorada'.
ref[, incremento := repartida_ref - observada_ref]
if (ref[incremento < -0.000001, .N] > 0) {
  stop('La referencia presenta mortalidad repartida menor a la observada.')
}
ref[abs(incremento) < 0.00000001, incremento := 0]
ref[, `:=`(
  suma_incremento = sum(incremento),
  suma_observada = sum(observada_ref)
), by = claves_grupo]
ref[, peso_edad := fifelse(
  suma_incremento > 0.00000001,
  incremento / suma_incremento,
  fifelse(suma_observada > 0, observada_ref / suma_observada, NA_real_)
)]

pesos_grupo <- ref[, .(peso_disponible = any(!is.na(peso_edad))),
                    by = claves_grupo]

archivo_final_repartido <- file.path(
  carpeta_salida,
  'mortalidad_inec_cie10_mx_edad_simple_2015_2025_REPARTIDA.csv'
)
archivo_diferencias <- file.path(
  carpeta_salida, 'control_diferencias_observadas_y_repartidas.csv'
)
archivo_grupos_sin_pesos <- file.path(
  carpeta_salida, 'control_edades_ignoradas_sin_distribuir.csv'
)

if (file.exists(archivo_final_repartido)) file.remove(archivo_final_repartido)
control_fuentes <- list()
control_no_repartida <- list()

for (y in anios) {
  archivo_anual <- file.path(
    carpeta_salida,
    paste0('mortalidad_inec_cie10_mx_edad_simple_', y, '.csv')
  )
  if (!file.exists(archivo_anual)) stop('Falta archivo anual: ', archivo_anual)

  bd <- data.table::fread(
    archivo_anual,
    colClasses = list(character = c('edad', 'sexo', 'region', 'CIE10_4'))
  )
  if (anyDuplicated(bd, by = c(claves_causa, 'edad'))) {
    stop('Causas/edades duplicadas en ', y)
  }
  data.table::setnames(bd, 'mortalidad', 'mortalidad_observada')
  bd[, mortalidad_observada := as.numeric(mortalidad_observada)]
  bd[, fila_original := .I]
  n_original <- nrow(bd)

  # LEFT JOIN por edad, sexo, anio y region (sin perder filas).
  bd <- merge(
    bd,
    ref[anio == y, .(edad, sexo, anio, region,
                      observada_ref, repartida_ref, peso_edad)],
    by = claves_edad, all.x = TRUE, sort = FALSE
  )
  if (nrow(bd) != n_original) {
    stop('El left join altero la cantidad de filas para ', y)
  }
  if (bd[edad %in% edades_conocidas & is.na(observada_ref), .N] > 0) {
    stop('Faltan coincidencias de edad/sexo/anio/provincia para ', y)
  }

  # Las discrepancias se INFORMAN, no se fuerzan a desaparecer.
  # Si las fuentes no tienen iguales observadas, tampoco necesariamente
  # tendran iguales repartidas: no es un error de la union.
  bd[pesos_grupo[anio == y],
     peso_disponible := i.peso_disponible,
     on = .(anio, region, sexo)]

  ignoradas_causa <- bd[edad == 'Ignorada',
    .(ignorada_causa = sum(mortalidad_observada)), by = claves_causa]
  bd <- merge(bd, ignoradas_causa, by = claves_causa,
              all.x = TRUE, sort = FALSE)
  if (nrow(bd) != n_original) {
    stop('La union de edades ignoradas duplico filas en ', y)
  }
  bd[is.na(ignorada_causa), ignorada_causa := 0]

  # Reparticion por causa. La referencia aporta pesos, no totales por causa.
  bd[, mortalidad := mortalidad_observada]
  bd[edad %in% edades_conocidas & !is.na(peso_edad),
     mortalidad := mortalidad_observada + ignorada_causa * peso_edad]
  bd[edad == 'Ignorada' & peso_disponible == TRUE, mortalidad := 0]
  # Edad Total conserva el numero observado original de cada causa.
  # Si no hay pesos, 'Ignorada' conserva las muertes no redistribuibles.

  # Reemplazo de mx y mx_1000 por tasas calculadas con nueva mortalidad.
  bd[, mx := data.table::fifelse(
    is.na(exposicion) | exposicion == 0,
    NA_real_, mortalidad / exposicion
  )]
  bd[, mx_1000 := mx * 1000]

  # Control fundamental: conservar el total de defunciones POR CAUSA.
  total_causa <- bd[edad == 'Total',
    .(total_original = sum(mortalidad_observada)), by = claves_causa]
  detalle_causa <- bd[edad != 'Total',
    .(total_repartido = sum(mortalidad)), by = claves_causa]
  verificar <- merge(total_causa, detalle_causa,
                     by = claves_causa, all = TRUE)
  verificar[, diferencia := total_repartido - total_original]
  if (anyNA(verificar[, .(total_original, total_repartido)]) ||
      verificar[abs(diferencia) > 0.0001, .N] > 0) {
    print(head(verificar[is.na(diferencia) | abs(diferencia) > 0.0001], 15))
    stop('No se conservaron los totales por causa para ', y)
  }

  # Auditoria: compara las sumas por edad de ambas fuentes sin imponer igualdad.
  chequeo <- bd[edad %in% edades_conocidas,
    .(observada_causas = sum(mortalidad_observada),
      observada_ref = observada_ref[1],
      repartida_causas = sum(mortalidad),
      repartida_ref = repartida_ref[1]),
    by = claves_edad]
  chequeo[, `:=`(
    diferencia_observada = observada_causas - observada_ref,
    diferencia_repartida = repartida_causas - repartida_ref
  )]
  control_fuentes[[as.character(y)]] <- chequeo[
    abs(diferencia_observada) > 0.00001 |
    abs(diferencia_repartida) > 0.0001
  ]

  pendientes <- bd[edad == 'Ignorada' & mortalidad > 0,
    .(mortalidad_sin_repartir = sum(mortalidad)),
    by = claves_grupo]
  if (nrow(pendientes)) control_no_repartida[[as.character(y)]] <- pendientes

  # Mismo orden y variables principales; mx numericas con hasta 10 decimales.
  data.table::setorder(bd, fila_original)
  out <- bd[, .(edad, sexo, anio, region, INEC_4, CIE10_4,
                mortalidad_observada, mortalidad, exposicion,
                mx = fijo(mx), mx_1000 = fijo(mx_1000))]
  data.table::fwrite(
    out, archivo_final_repartido,
    append = file.exists(archivo_final_repartido),
    col.names = !file.exists(archivo_final_repartido),
    na = 'NA'
  )
  message(
    'Anio ', y, ': ', format(nrow(out), big.mark = '.'),
    ' filas; ', nrow(control_fuentes[[as.character(y)]]),
    ' grupos con diferencias respecto a la referencia.'
  )
  rm(bd, out)
  gc(verbose = FALSE)
}

# Guardar auditora, aunque no haya discrepancias.
control_completo <- data.table::rbindlist(control_fuentes, fill = TRUE)
if (ncol(control_completo) == 0) {
  control_completo <- data.table::data.table(
    edad = character(), sexo = character(), anio = integer(),
    region = character(), diferencia_observada = numeric(),
    diferencia_repartida = numeric()
  )
}
data.table::fwrite(control_completo, archivo_diferencias)

sin_repartir <- data.table::rbindlist(control_no_repartida, fill = TRUE)
if (ncol(sin_repartir) == 0) {
  sin_repartir <- data.table::data.table(
    anio = integer(), region = character(), sexo = character(),
    mortalidad_sin_repartir = numeric()
  )
}
data.table::fwrite(sin_repartir, archivo_grupos_sin_pesos)

cat('\nBASE FINAL: ', archivo_final_repartido, '\n', sep = '')
cat('COMPARACION ENTRE FUENTES: ', archivo_diferencias, '\n', sep = '')
cat('IGNORADAS SIN REPARTIR: ', archivo_grupos_sin_pesos, '\n', sep = '')
# Para abrir, siempre que la RAM permita cargar el CSV:
# base_final_repartida <- data.table::fread(archivo_final_repartido)
# View(base_final_repartida)

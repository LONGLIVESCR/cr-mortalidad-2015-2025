# LONGLIVESCR — Mortalidad Costa Rica 2015–2025

Código reproducible para el análisis de mortalidad de Costa Rica (2015–2025) a partir de tabulations REDATAM del Instituto Nacional de Estadística y Censos (INEC).

## Objetivo

Transformar las exportaciones de REDATAM y las estimaciones de población del INEC en bases de datos de análisis listas para usar, con trazabilidad completa de la codificación CIE-10.

## `causa4dig`

La variable REDATAM **“Causa básica de defunción numérica a 4 dígitos”** 

## Ediciones de los diccionarios INEC

| Años       | Edición INEC | Archivo Excel |
|------------|--------------|---------------|
| 2000–2016  | Ed2008       | bd INEC       |
| 2017–2020  | Ed2015       | bd INEC       |
| 2021+      | 2021 onwards | bd INEC       |

Cada prefijo de 4 dígitos mapea a exactamente una categoría CIE-10 en las tres ediciones. El código se deriva del número, no del texto de la etiqueta (existen errores menores de etiquetado en los Excel oficiales).

## Cómo obtener los datos crudos

...



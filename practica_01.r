library(data.table)

archivo <- "C:/Users/javi/Documents/Universidad/2019-2020/Estadistica/Notas.csv"

dfNotas <- read.csv(archivo,TRUE, sep = ';', dec = ',')
dtNotas <- data.table(dfNotas)

dtNotas <- setNames(dtNotas, c('Grupo','P1','P2','P3','TotalPracticas','EvaluacionParcial','ExamenJunio','NotaJunio','ExamenSeptiembre','NotaFinalSeptiembre'))

####TABLA DE FRECUENCIAS ABSOLUTAS####

## Unidimensional

table(dtNotas$Grupo)

## Bidimensional

t = table( cut(dtNotas$NotaJunio, breaks = 5*(0:2),labels = FALSE, right = FALSE), dbtNotas$Grupo,dnn = c('Calificacion','Grupo'))
rownames(t) <- c('Suspenso', 'Aprobado')

t

####GRAFICOS####

## Diagrama de Barras

barplot(dtNotas[Grupo = A])

## Diagrama de Sectores

## Histograma

## Diagrama de Caja

## Recta de Regresion Lineal sobre un Grafico de Puntos


####MEDIA ARITMETICA####

####MODA####

####MEDIANTA####

####ANALISIS DE LOS CUANTILES####

####VARIANZA Y DESVIACION TIPICA####

####COEFICIENTE DE VARIACION DE PEARSON####

####COEFICIENTE DE ASIMETRIA DE FISHER####

####COEFICIENTES DE CURTOSIS####

####COVARIANZA####

####ANALISIS DE REGRESION LINEAL DE DOS VARIABLES RELACIONADAS####


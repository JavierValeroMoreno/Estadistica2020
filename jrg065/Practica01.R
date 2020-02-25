##PRACTICA 01
#PAQUETES
  install.packages("data.table")
  install.packages("modes")

  library(data.table)
  library(modes)
#!PAQUETES
#SCRIPT
  #DATOS
  ##IMPORTAR DATOS
  notas <- "/home/jpramez/Documentos/ESTADISTICA/P01/Notas.csv"
  dfNotas <- read.csv(notas,sep = ';', dec = ',' ,header = TRUE, stringsAsFactors = FALSE)
  dtNotas <- data.table(dfNotas)
  ##PREPROCESAMIENTO
  ###NOMBRES DE COLUMNAS
  names(dtNotas)
  colnames(dtNotas) <- c("grupo","p1","p2","p3","totalPracticas","EParcial","EJunio","FJunio","ESeptiembre","FSeptiembre")
  ###FILTRAR DATOS BASURA
  #dtNotas[is.na(dtNotas)] <- -1
  dtNotas[dtNotas<0] <- NA
  dtNotas
  ##EXTRAER LOS NOMBRES DE LAS COLUMNAS A VARIABLES
  ##Extraemos los valores de las columnas en variables
  grupo <- dtNotas$grupo
  p01 <- dtNotas$p1
  p02 <- dtNotas$p2
  p03 <- dtNotas$p3
  totalP <- dtNotas$totalPracticas
  eParcial <- dtNotas$EParcial
  eJunio <- dtNotas$EJunio
  fJunio <- dtNotas$FJunio
  eSeptiembre <- dtNotas$ESeptiembre
  fSeptiembre <- dtNotas$FSeptiembre
  
  ##### MAIN #####
  #### TABLA DE FRECUENCIAS ABSOLUTAS ####
  ### UNIDIMENSIONAL
  
  #-- ES UN PROCESO DIRECTO. LA FRECUENCIA ABSOLUTA DE LA VARIABLE GRUPO
  freqGrupo <- table(grupo)
  freqGrupo
  
  ### BIDIMENSIONAL ###
  
  #-- EXTRAEMOS LOS VALORES DE, POR EJEMPLO, EL EXAMEN DE JUNIO, SIN PONDERAR
  eJunio10 <- dtNotas$EJunio
  eJunio10 <- eJunio10/0.6
  #-- LOS VALORES NA LOS SUSTITUIMOS POR UN VALOR A MEDIDA Y QUE NUNCA PODRÁ SER CONSECUENCIA DE UN PROCESO VALIDO (-1, POR EJEMPLO)
  eJunio10[is.na(eJunio10)] <- -1
  labels <- c("NO PRESENTADO","SUSPENSO","APROBADO","NOTABLE","SOBRESALIENTE")
  cutNotas <- cut(eJunio10, breaks = c(-1,0,5,7,9,10), right = F,include.lowest = T,labels = labels)
  freqGrupoJunio <- table(grupo, cutNotas, dnn = c("Grupo","Notas"))
  
  #### GRÁFICOS ####
  ### DIAGRAMA DE BARRAS  ###
  #-- BARPLOT DE NOTAS DEL GRUPO A EN JUNIO
  freqGrupoJunio[1,]
  png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/D_BARRAS_A_JUNIO.png")
  barplot(freqGrupoJunio[1,], xlab="Resultado",ylab = "Nº de alumnos", col = c("black","red","orange","yellow","green"), cex.lab = 1, cex.axis = 1, cex.names = 0.8)
  dev.off()
  #-- BARPLOT DE NOTAS DEL GRUPO B EN JUNIO
  freqGrupoJunio[1,]
  png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/D_BARRAS_B_JUNIO.png")
  barplot(freqGrupoJunio[2,], xlab="Resultado",ylab = "Nº de alumnos", col = c("black","red","orange","yellow","green"), cex.lab = 1, cex.axis = 1, cex.names = 0.8)
  dev.off()

  #-- BARPLOT DE NOTAS DE AMBOS GRUPOS EN JUNIO
  png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/D_BARRAS.png")
  barplot(freqGrupoJunio,beside = T, xlab="Resultado",ylab = "Nº de alumnos", col = c("cyan","lightgreen"), cex.lab = 1, cex.axis = 1, cex.names = 0.8)
  legend("topright", legend = c("GRUPO A", "GRUPO B"), fill = c("cyan", "lightgreen"))
  dev.off()
  
  ### DIAGRAMA DE SECTORES ###
  png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/D_SECTORES.png")
  labels <- freqGrupo
  pie(freqGrupo, label= freqGrupo , col= c("cyan","lightgreen"))
  legend("topright", legend = c("GRUPO A", "GRUPO B"), fill = c("cyan", "lightgreen"))
  dev.off()
  
  ### HISTOGRAMA ###

  png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/Histograma_Jun.png")
  h <- hist(eJunio10, col= c("white","gray"),main="Histograma de frecuencia",xlab="Notas de Junio",ylab = "Cantidad de alumnos", cex.lab =1.5)
  dev.off()
  
  #-- EXTRAEMOS LOS VALORES DE, POR EJEMPLO, EL EXAMEN DE JUNIO, SIN PONDERAR
  eSept10 <- dtNotas$ESeptiembre
  eSept10 <- eSept10/0.8
  #-- LOS VALORES NA LOS SUSTITUIMOS POR UN VALOR A MEDIDA Y QUE NUNCA PODRÁ SER CONSECUENCIA DE UN PROCESO VALIDO (-1, POR EJEMPLO)
  eSept10[is.na(eSept10)] <- -1
  eSept10 <- eSept10[eSept10>0]
  png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/Histograma_Sep.png")
  h <- hist(eSept10, col= c("white","gray"),main="Histograma de frecuencia",xlab="Notas de Septiembre",ylab = "Cantidad de alumnos", cex.lab =1.5)
  dev.off()
  
  ### DIAGRAMA DE CAJA ###
  png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/D_CAJA.png")
  boxplot(fJunio, col="cyan",xlab="Notas de Junio",ylab="Alumnos", cex.lab =1.5)
  dev.off()
  
  ### DIAGRAMA DE DISPERSIÓN ###
  #- MAL AJUSTE
  png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/Dispersion_Practicas_Junio.png")
  plot(totalP, fJunio, col="blue", xlab="Notas finales de practicas",ylab="Notas junio")
  abline(lm(fJunio~eJunio),col="black")
  dev.off()
  
  #- BUEN AJUSTE
  png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/Dispersion_Junio_FinalJunio.png")
  plot(eJunio, fJunio, col="blue", xlab="Notas finales de practicas",ylab="Notas junio")
  abline(lm(fJunio~eJunio),col="black")
  dev.off()

  #### CALCULOS ####
  fJunioDiscrete <- as.integer(fJunio)
  fSeptiembreDiscrete <- as.integer(fSeptiembre)
  ### MEDIA ARITMETICA ###
  mean(fJunio, na.rm = T)
  mean(fSeptiembre, na.rm = T)
  ### MEDIANA ###
  median(fJunio, na.rm = T)
  median(fSeptiembre, na.rm = T)
  ### MODA ###
  fJunio
  fJunioDiscrete
  table(fJunioDiscrete)
  modes(fJunioDiscrete)
  
  ### ANALISIS DE LOS CUANTILES ###
  quantile(fJunioDiscrete, na.rm = T)
  quantile(fSeptiembreDiscrete, na.rm = T)
  
  ### VARIANZA ###
  var(totalP, na.rm = T)
  var(fJunio, na.rm = T)
  var(fSeptiembre, na.rm = T)
  
  ### DESVIACION TIPICA ###
  sd(totalP, na.rm = T)
  sd(fJunio, na.rm = T)
  sd(fSeptiembre, na.rm = T)
  
  ### COEFICIENTE DE VARIACION DE PEARSON ###
  sd(fJunio)/mean(fJunio)
  
  ### COEFICIENTE DE ASIMETRIA DE FISHER ###
  myFisher <- function(data){
    suma <- sum(data)
    media <- mean(data)
    desvEstandar <- sd(data)
    tam <- length(data)
    
    result <- (suma-media)^(desvEstandar/media)/tam
    return(result)
  }
  myFisher(fJunio)
  
  ### COEFICIENTE DE CURTOSIS ###
  fJunio <- fJunio[]
  hist(fJunio)
  kurtosis(fJunio, finite = T)
  
  ### COVARIANZA ###
  cov(eJunio, fJunio)
  
  
  View(dtNotas)  
  
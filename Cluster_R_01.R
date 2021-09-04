# ANALISIS DE CLUSTER DATOS SIERRA DE QUILA


# Elimina una lista de objetos (eliminar todo el ambiente)
rm(list = ls(all.names = T))


# Asignacion del directorio de trabajo
setwd("G:/0/_Procesos_ARH/R_Stats_2")


# Llamando librerias de trabajo
library(readr)
library(cluster)
library(vegan)
library(factoextra)


# Lectura de datos de entrada
datos <- read.csv(file="datos.csv", header=TRUE,
                sep=",", dec=".", row.names=1)

# Revisando los datos
head(datos)
str(datos)

# Estadísticas generales de los datos
summary(datos)

# Matriz de correlaciones
correlaciones = cor(datos)
correlaciones

# Normalizando o no datos
datos_norm <- datos # No normalizar, solo copiar
# datos_norm <-  as.data.frame(scale(datos, center = TRUE, scale = TRUE)) # Si normalizar
head(datos_norm)

# Calculando distancias
dist1<- vegdist(datos_norm, method="euclidean")
dist1[1:10]

# Calculando cluster
cpro <- hclust(dist1, method="average")

#Calculando la matriz cofenética
cpro_coph <- cophenetic(cpro)
cor(cpro_coph, dist1)

fviz_nbclust(datos_norm, kmeans, method = "silhouette") # Estima el numero de cluster

# Dibujando dendrograma
plot(cpro, hang=-0.1, cex.axis=0.7, cex.lab=0.8, cex.main=0.8)
# Haciendo corte al número de cluster en el dendograma
rect.hclust(cpro, 3)

# Obteniendo la pertenencia de los grupos
grupo <- cutree(cpro, 3)
grupo[1:10]

# Haciendo una copia de los datos originales
datos_2 <- datos
head(datos_2)

# Agregando los grupos a la copia de datos
datos_2$GRUPO <- grupo
head(datos_2)

# Dibujado gráficas de cajas
boxplot(EVI~GRUPO,
        data=datos_2,
        main="CAJAS EVI",
        xlab="CLUSTER", 
        ylab="INDICE EVI")

boxplot(MDE~GRUPO,
        data=datos_2,
        main="CAJAS MDE",
        xlab="CLUSTER", 
        ylab="MDE msnm")

boxplot(HARB~GRUPO,
        data=datos_2,
        main="CAJAS ALTURA ARBOLES",
        xlab="CLUSTER", 
        ylab="ALTURA ARBOLES m")

boxplot(PMA~GRUPO,
        data=datos_2,
        main="CAJAS PRESIPITACION MEDIA ANUAL",
        xlab="CLUSTER", 
        ylab="PMA mm")

boxplot(TMEDPROM~GRUPO,
        data=datos_2,
        main="CAJAS TEMPERATURA MEDIA PROMEDIO",
        xlab="CLUSTER", 
        ylab="ALTURA TMED °c")

# Escribiendo los resultados en csv
write.csv(datos_2,"datos_clust.csv")







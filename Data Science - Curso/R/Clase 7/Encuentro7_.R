###############################################
### CERTIFICACIÃ“N UNIVERSITARIA EN DATA SCIENCE
###        ANALISIS DE DATOS II
###############################################


## 24 de Marzo de 2022 ##

datos <- read.csv2( "mydata.csv", header = TRUE,sep = "," , dec = ",")
str(datos)
summary(datos)
range(datos$EDAD)

datos$GRUPO <- cut(datos$EDAD, breaks = seq(20, 35, by = 3), 
                   include.lowest = T)

datos$GRUPO
class(datos$GRUPO)
table(datos$GRUPO,datos$SEXO)#construyo table de contigencia
addmargins(table(datos$GRUPO,datos$SEXO))#sumatoria marginales de filas y columnas
prop.table(table(datos$GRUPO,datos$SEXO))#tabla con valores porcentuales relativos a las filas y columnas
round(100*prop.table(table(datos$GRUPO,datos$SEXO),margin=1),2)#valores porcentuales relativos solo a las filas
round(100*prop.table(table(datos$GRUPO,datos$SEXO),margin=2),2)#valores porcentuales relativos solo a las columnas
tapply(datos$PESO,datos$SEXO,mean)
by(datos$PESO,datos$SEXO,mean)
aggregate(PESO ~ SEXO + GRUPO, data=datos,mean)
cov(datos$PESO,datos$EDAD) #calculo de covarianza cuyo indice me dice como es la relacion (inversa o directa)
cor(datos$PESO,datos$EDAD) #calculo de la correlacion (PEARSON por default)
cor.test(datos$PESO,datos$EDAD)




##otro ejemplo
tabla <- read_excel("Tabla3.xlsx")
summary(tabla)
#ho = la muestra proviene de una poblacion normalmente distribuida
#p-valor<0.05 rechazo H0
shapiro.test(tabla$hto)
shapiro.test(tabla$visc)
shapiro.test(tabla$proteínas)
#al tener las tres pruebas no rechazadas puedo decir que la muestra viene de una población normalmente distribuida
cor.test(tabla$visc,tabla$hto,alternative = "greater",method="pearson",conf.level=0.95)
#en este caso el p-valor nos da muy pequeño (significativo) y por lo tanto muy correlacionadas las variables
cor.test(tabla$visc,tabla$proteínas,alternative = "greater",method="pearson",conf.level=0.95)

#---Correlacion Spearman
#H0 = no hay asociacion lineal entre las variables

ala <- c(10.4,10.8,11.1,10.2,10.3,10.2,10.7,10.5,10.8,11.2,10.6,11.4)
cola <- c(7.4,7.6,7.9,7.2,7.4,7.1,7.4,7.2,7.8,7.7,7.8,8.3)
scatter <- data.frame(ala,cola)
ggplot(scatter,aes(ala,cola))+
  geom_point()+
  stat_ellipse()

shapiro.test(ala)
shapiro.test(cola)
corranalp<-cor.test(ala,cola,method="pearson")
corranalp

#ejemplo MASS-----
require(dplyr)
install.packages("MASS")
library(MASS)
library(ggplot2)
# data set con informaciÃ³n sobre diferentes autos. Se quiere estudiar si existe 
# una correlaciÃ³n entre el peso de un vehÃ­culo (Weight) y la potencia de su motor (Horsepower).

data(Cars93) # cargo el dataset

ggplot(data = Cars93, aes(x = Weight, y = Horsepower)) + 
  geom_point(colour = "red4") +
  ggtitle("Diagrama de dispersiÃ³n") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
par(mfrow=c(1,2))
hist(Cars93$Weight,breaks=10,main="",xlab="weight",border="darkred")
hist(Cars93$Horsepower,breaks=10,main="",xlab="Horsepower",border="blue")
qqnorm(Cars93&Weight,main="Weight",col="darkred")
qqline(Cars93$Weight)
qqnorm(Cars93&Horsepower,main="Horsepower",col="blue")
################################################
###__________________________________________###

rm(list=ls())# Borrar los datos de entorno cargados en memoria 

setwd("C:/ARRAYANES/MUNDO_E") # directorio de trabajo
require(tidyverse)
require(readxl)

## cualitativa vs cualitativa-----------
datos <- read.csv2( "mydata.csv", header = TRUE,sep = "," , dec = ",")

range(datos$EDAD)

datos$GRUPO <- cut(datos$EDAD, breaks = seq(20, 35, by = 3), 
                   include.lowest = T) # creo nueva variable en base a la variable edad

datos$GRUPO

class(datos$GRUPO)

class(datos$SEXO)
datos$SEXO<- as.factor(datos$SEXO)

table(datos$GRUPO, datos$SEXO) # contruyo tabla de contingencia

addmargins(table(datos$GRUPO, datos$SEXO)) # sumatorias marginales de filas y columnas

prop.table(table(datos$GRUPO, datos$SEXO)) # tabla con valores porcentuales

round(100*prop.table(table(datos$GRUPO, datos$SEXO), margin = 1),2) 
# margin=1, el denominador es es el total de la fila

round(100*prop.table(table(datos$GRUPO, datos$SEXO), margin = 2),2) 
# margin=2, el denominador es por columna

## cuantativa vs cualitativa-----------
tapply(datos$PESO, datos$SEXO,mean)

by(datos$PESO, datos$SEXO,mean) # lo mismo que tapply pero las salida es diferente

aggregate(PESO ~ SEXO, data = datos, FUN = mean)

# podemos incorporara otras variables categÃ³ricas

aggregate(PESO ~ GRUPO + SEXO, data = datos, mean)

aggregate(PESO ~ SEXO + GRUPO, data = datos, mean)

##

## cuantativa vs cuantativa-----------

# Prueba de hipotesis para correlacion
# correlacion de Pearson

# calculo de covarianza usando R base
cov(datos$PESO,datos$EDAD)

#calculo corficiente correlacion usando R base
cor(datos$PESO,datos$EDAD)

# para saber si al correlacion es significativa evalup el p-valor
# p-valor< 0,05 la correlacion es significativa

cor.test(datos$PESO,datos$EDAD)

# otro ejemplo-----------

Tabla3<- read_excel("Tabla3.xlsx")
summary(Tabla3) # resumen de la tabla

# evaluo la normalidad:
# H0= la muestra proviene de una poblacion normalmente distribuida
# p-valor < 0,05 rechazo H0
shapiro.test(Tabla3$hto) 
shapiro.test(Tabla3$visc) 
shapiro.test(Tabla3$proteÃ­nas)

# planteo analisis de correlacion

cor.test(Tabla3$visc,Tabla3$hto,alternative="greater",method="pearson",conf.level=0.95)

# correlacion alta entre avriables ( r) con alto significado estadistico (p-valor) 

cor.test(Tabla3$visc,Tabla3$proteÃ­nas,alternative="greater",method="pearson",conf.level=0.95)

##
# correlacion Spearman----------------

#H0 = no hay asociacion lineal entre las variables

ala <- c(10.4, 10.8,11.1, 10.2, 10.3, 10.2, 10.7, 10.5, 10.8, 11.2, 10.6,11.4 )

cola <- c(7.4,7.6,7.9,7.2,7.4,7.1,7.4,7.2,7.8,7.7,7.8,8.3)

scatter <- data.frame(ala,cola)
ggplot(scatter, aes(ala, cola)) +
  geom_point() +
  stat_ellipse()

# analizao la normalidad
shapiro.test(ala)
shapiro.test(cola)

# analizao la correlacion parametrica
corranalp <- cor.test(ala,cola, method = "pearson")
corranalp


# ejemplo MASS---------

require(dplyr)
library(MASS)
library(ggplot2)
# data set con informaciÃ³n sobre diferentes autos. Se quiere estudiar si existe 
# una correlaciÃ³n entre el peso de un vehÃ­culo (Weight) y la potencia de su motor (Horsepower).

data(Cars93) # cargo el dataset

ggplot(data = Cars93, aes(x = Weight, y = Horsepower)) + 
  geom_point(colour = "red4") +
  ggtitle("Diagrama de dispersiÃ³n") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# parece indicar una posible relaciÃ³n lineal positiva entre ambas variables.

# analisis de normalidad
par(mfrow = c(1, 2))
hist(Cars93$Weight, breaks = 10, main = "", xlab = "Weight", border = "darkred")
hist(Cars93$Horsepower, breaks = 10, main = "", xlab = "Horsepower",
     border = "blue")

qqnorm(Cars93$Weight, main = "Weight", col = "darkred")
qqline(Cars93$Weight)
qqnorm(Cars93$Horsepower, main = "Horsepower", col = "blue")
qqline(Cars93$Horsepower)
par(mfrow = c(1,1))

# Test de hipÃ³tesis para el anÃ¡lisis de normalidad
shapiro.test(Cars93$Weight)

shapiro.test(Cars93$Horsepower)

# para la variable Horsepower no se puede asumir normalidad (p< 0,05) y que la 
# variable Weight esta en  el lÃ­mite. 
# Siendo estrictos, este hecho excluye la posibilidad de utilizar 
# el coeficiente de Pearson, dejando como alternativas el de Spearman o Kendall. 
# Dado que la distribuciÃ³n no se aleja mucho de la normalidad y de que el 
# coeficiente de Pearson tiene cierta robustez, a fines prÃ¡cticos sÃ­ que se 
# podrÃ­a utilizar siempre y cuando se tenga en cuenta este hecho en los resultados. 
# Otra posibilidad es tratar de transformar las variables para mejorar su distribuciÃ³n.

dev.off() # Desactivamos todas las ventanas grÃ¡ficas o dispositivos

# Matriz de correlacion----------------

# ejemplo clientes

cliente=read.csv("https://raw.githubusercontent.com/VictorGuevaraP/Mineria-de-datos-2019-2/master/clientes.csv", sep = ";")
# 850 observaciones 9 variables

str(cliente)
head(cliente)
# selecciono las variables edad , educacacion , aÃ±os_empleo , Ingreso , 
# Tarjeta_credito , otra_tarjeta , ratio.ingreso.deuda

cliente_correlaciones=cliente[c(2:7,9)]

cor(cliente_correlaciones)

install.packages("corrplot")
library(corrplot)
# "corrplot" podemos tener un grafico que nos indica atravez de color la 
# correlacion que existe siendo : azul = correlacion positiva rojo = correlacion negativa

corrplot(cor(cliente_correlaciones))

corrplot(cor(cliente_correlaciones), method = "pie")

corrplot(cor(cliente_correlaciones), method = "ellipse")

corrplot(cor(cliente_correlaciones), method = "color")

corrplot(cor(cliente_correlaciones), method = "number")

corrplot(cor(cliente_correlaciones), method = "pie", type = "upper")

corrplot(cor(cliente_correlaciones), method = "pie", type = "lower")

corrplot(cor(cliente_correlaciones),order = "hclust")
# La matriz de correlaciÃ³n se puede reordenar segÃºn el coeficiente de correlaciÃ³n. 
# Importante para identificar la estructura y el patrÃ³n ocultos en la matriz

corrplot(cor(cliente_correlaciones), method = "number", type = "upper", order = "hclust")


corrplot(cor(cliente_correlaciones),type = "upper", order = "hclust",col=c("green","red"))

##########################################################################################
########################## Clase 7 repaso ################################################
##########################################################################################

datos <- read.table("mydata.csv",header=T,sep=",",dec=".")
range(datos$EDAD)
datos$GRUPO<-cut(datos$EDAD,breaks=seq(20,35,by=3),include.lowest = T)
datos$GRUPO
#creamos una tabla de contigencia con EDAD(agrupado) vs SEXO
table(datos$GRUPO,datos$SEXO)
addmargins(table(datos$GRUPO,datos$SEXO))
round(100*(prop.table(table(datos$GRUPO,datos$SEXO),margin=2)),2)
#creamos tablas de comparacion entre var.cualitativasy cuantitativas
datos$PESO <- as.numeric(as.factor(datos$PESO))
by(datos$PESO,datos$SEXO,mean)
tapply(datos$PESO,datos$SEXO,mean)
#apply(datos$PESO,FUN=mean)
aggregate(PESO~GRUPO+SEXO,data=datos,mean)

#Correlacion y covarianza
cov(datos$PESO,datos$EDAD)
cov(datos$EDAD,datos$PESO)
cor(datos$PESO,datos$EDAD)

#Ejercicio de test de correlacion con coeficiente de Pearson
cor.test(datos$PESO,datos$EDAD)

#Ejercicio con tabla3 de datos de hematocritos, viscosidad de la sangre y cantidad de proteínas.
library(readxl)
tabla3<-read_excel("Tabla3.xlsx")
str(tabla3)
summary(tabla3)
#Evaluamos normalidad para cada variable
shapiro.test(tabla3$hto)
shapiro.test(tabla3$visc)
shapiro.test(tabla3$proteínas)
#Evaluamos correlacion para las variables: hto y visc
cor.test(tabla3$hto,tabla3$visc)
#Evaluamos correlacion para las variables: proteínas y visc
cor.test(tabla3$proteínas,tabla3$visc)

#Ejercicio de evaluación de correlación de relación lineal sin especificasr ningun tipo de relación entre las variables
set.seed(123)
librospersona<-rnorm(2000,5,2)
librospersona[librospersona<0]<-0
psu <- 200 + 20*librospersona + rnorm(2000,150,45) 
plot(librospersona,psu)
abline(lm(psu~librospersona),col='red')
shapiro.test(librospersona)
shapiro.test(psu)
cor.test(librospersona,psu)
#Al resultar significativa la prueba nos dice que se rechaza la "no correlacion" entre las variables
#y se acepta que hay relación/correlacion, el signo del valor de correlacion nos dirá
#si es inversamente o directamente proporcional la correlacion entre variables.

ala <- c(10.4,10.8,11.1,10.2,10.3,10.2,10.7,10.5,10.8,11.2,10.6,11.4) 
cola <- c(7.4,7.6,7.9,7.2,7.4,7.1,7.4,7.2,7.8,7.7,7.8,8.3)
scatter <- data.frame(ala,cola)
str(scatter)
library(ggplot2)
ggplot(scatter,aes(ala,cola))+
  geom_point()+
  stat_ellipse()
#Analizamos la normalidad
shapiro.test(ala)
shapiro.test(cola)
cor.test(ala,cola,method="pearson")
#Ahora hacemos correlación no parametrica
corranalk <- cor.test(ala,cola, method = "kendall") 
#Usamos la correlacion de Spearman
cor.test(ala,cola,method="spearman")

###############################################
### CERTIFICACI脫N UNIVERSITARIA EN DATA SCIENCE
###        ANALISIS DE DATOS II
###############################################


## 18 de Marzo de 2022 ##

rm(list=ls())# Borrar los datos de entorno cargados en memoria 

setwd("C:/ARRAYANES/MUNDO_E") # directorio de trabajo


# Prueba de madias para una muestra ----------

# verificar si el proceso de llenado de bolsas de caf茅 con 500 gramos, en una f谩brica 
# cumple con las especificaciones de la envoltura, asumiendo una distribuci贸n normal con un nivel 
# de significancia de 5%. Se toman aleatoriamente  diez muestras cada cuatro horas. 

# H0= 500 gr
contenido <- c(510, 492, 494, 498, 492,496, 502, 491, 507, 496) 

t.test(contenido, alternative='two.sided',
       conf.level=0.999, mu=500)
# conclusion? 
#p-valor (0.3155)> 0.05, entonces ......

##
# pruebas de media para 1 medias-------

# Se espera que el nivel de colesterol en plasma de unos enfermos sometidos a un 
# determinado tratamiento se distribuya seg煤n una ley normal de media 220 mg/dl. 
# Se toma una muestra de 9 enfermos, y se miden sus niveles de colesterol. 

colesterol<- c(203, 229, 215, 220, 223, 233, 208, 228, 209,1090)

t.test(colesterol,mu=220,alternative="two.sided",conf.level=0.95)

# El valor-p del contraste es 0.71377 y el intervalo de confianza del 95% para el 
# nivel medio de colesterol  es: [210.5773713,226.7559621] No rechazar Ho. 
# No hay evidencia que nos permita rechazar la hip贸tesis nula H0:media=220 

##

#Prueba de hip贸tesis para varianza de una poblacion normal------
# Se realiza un estudio para comparar dos tratamientos que se aplicar谩n a frijoles 
# crudos con el objetivo de reducir el tiempo de cocci贸n. El tratamiento T1 es a 
# base de bicarbonato de sodio, el T2 es a base de cloruro de sodio. 
# La variable respuesta es el tiempo de cocci贸n en minutos. 
# 驴Son las varianzas de los tiempos iguales o diferentes? Usar  伪 = 0.05

# comparacion de varianzas--------------
T1 <- c(76, 85, 74,78, 82, 75, 82) 
T2 <- c(57, 67, 55, 64, 61, 63, 63)

# debo explorar si viene de una distribucion normal

q1 <- qqnorm(T1, plot.it=TRUE)
q2 <- qqnorm(T2, plot.it=TRUE)
plot(range(q1$x, q2$x), range(q1$y, q2$y), type="n", las=1,
     xlab='cuantiles te贸ricos', ylab='cuantiles de muestra')
points(q1, pch=19)
points(q2, col="red", pch=19)
qqline(T1, lty='dashed')
qqline(T2, col="red", lty="dashed")
legend('topleft', legend=c('T1', 'T2'), bty='n',
       col=c('black', 'red'), pch=19)

# topleft= arriba a la izquierda

var.test(T1, T2, null.value=1, alternative="two.sided",
         conf.level=0.95)
#p-valor(0.9897)> 0.05, concluyo que las varianzas son iguales

##

# concentraciones de ars茅nico en agua potable en partes por bill贸n (ppb) para diez 
# comunidades urbanas y diez comunidades rurales

urbana <- c(3, 7, 25, 10, 15, 6, 12, 25, 15, 7)
rural <- c(48, 44, 40, 38, 33, 21, 20, 12, 1, 18)

q1 <- qqnorm(urbana, plot.it=FALSE)
q2 <- qqnorm(rural, plot.it=FALSE)
plot(range(q1$x, q2$x), range(q1$y, q2$y), type="n", las=1,
     xlab='cuantiles te贸ricos', ylab='cuantiles de muestra')
points(q1, pch=19)
points(q2, col="red", pch=19)
qqline(urbana, lty='dashed')
qqline(rural, col="red", lty="dashed")
legend('topleft', legend=c('urb', 'rur'), bty='n',
       col=c('black', 'red'), pch=19)

var.test(urbana, rural, null.value=1, alternative="two.sided",
         conf.level=0.95)
# p-valor(0.04936) < 0.05
# las varianzas son iguales o no?

## G

# # Prueba de hip贸tesis para la diferencia de medias con varianzas diferentes

# volvemos al ejemplo del arsenico en agua

datos <- data.frame(Concentracion=c(urbana, rural),
                    Zona=rep(c('Urbana', 'Rural'), each=10))
str(datos)
boxplot(Concentracion ~ Zona, data=datos, las=1,
        xlab='Zona', ylab='Concentraci贸n ars茅nico (ppb)')

# planteo hipotesis
# Ho la diferencia de medias es igual a 0
# Hi la diferencia de medias es diferente de 0

t.test(x=urbana, y=rural, alternative="two.sided", mu=0, 
       paired=FALSE, var.equal=FALSE, conf.level=0.95)

# p-valor( 0,01583) < 0.05
# conclusion: hay diferencia significativa en las concentraciones de arsenico 
# entre las dos zonas. La zona rural presneta mayor concentracion media de As.(27.5)

## Fin del Script ##


iris
Titanic
cars

#-------------------------------------------------------------------------#
###########################################################################
###########################################################################
######################## PRACTICA #########################################
###########################################################################
#Ejercicio: queremos verigicar el proceso de llenado de bolsas de cafe con 500 gramos
#con un nivel de significa del 5%.
cafe <- c(510,492,494,498,492,496,502,491,507,496)
#H0: mu=500gr
#H1: mu!=500gr
t.test(cafe,alternative='two.sided',conf.level = 0.95,mu=500)
hist(cafe)

#Test de normalidad Shapiro-Wilk y Kolmogorov Smirnov
#Ejercicio con "estudiantes2.xlsx"
library(tidyverse)
library(readxl)
estudiantes <- read_excel("estudiantes2.xlsx")
hist(estudiantes$edad,freq=F)
lines(density(estudiantes$edad))

str(estudiantes$edad)
shapiro.test(estudiantes$edad)
shapiro.test(cafe)

#Ejercicio Dataframe "estudiantes.csv"
students <-read.csv2("estudiantes.csv",header=T,
                     sep=",",dec=".")
datos2a35 <-students[2:35,]
nrow(datos2a35)
dim(datos2a35)
str(datos2a35)
names(datos2a35)
p3 <-as.numeric(datos2a35$P3)
p3
xbarra <-mean(p3)
xbarra
v <-var(p3)
v

s <-sd(p3)
s
t.test(p3,alternative = "two.sided",
       mu=xbarra,conf.level = 0.95)
range(p3)
summary(datos2a35)
hist(p3)
shapiro.test(p3)
length(p3)
#tama駉 de la muestra n=34
#Distribuci髇 normal
#Grado de confianza 0.95
#Poblacion clasificacion del tercer parcial
#Tipo de prueba bilateral
#Parametro media
#H0: u=x
#H1: u!=x
#la poblaci髇 es >30 y tiene distribucion normal.
#se calcula z= (x-u)/(s/n^0.5)
mu<-3.5
media<-xbarra
n <- length(p3)
ES <- s/sqrt(n)
z <- (xbarra-mu)/ES
z
alfa <- 0.05
critico <- qnorm(1-(alfa/2))
critico
pnorm(z)

#Ejercicio de compa駃as de automoviles
#H0: u>=20000km/a駉
#H1: u<20000km/a駉
xbarra<-19500
s<-3900
n<-100
mu<-20000
est<-(xbarra-mu)/(s/sqrt(n))
alfa<-0.03
est
pnorm(est)

#Ejercicio para la varianza poblacional de dos muestras
t1<-c(76,85,74,78,82,75,82)
t2<-c(57,67,55,64,61,63,63)
q1<-qqnorm(t1,plot.it=F)
q2<-qqnorm(t2,plot.it=F)
plot(range(q1$x,q2$x),range(q1$y,q2$y),type="n",las=1,
     xlab='Cuantiles te髍icos',ylab='Cuantiles de muestra')
points(q1,pch=19)
points(q2,col='red',pch=19)
qqline(t1,lty='dashed')
qqline(t2,col="red",lty='dashed')
legend('topleft',legend=c('t1','t2'),
       bty='n',col=c('black','red'),pch=19)
#Al observar los puntos bastante alineados nos hace pensar que las muestras provienen de una poblaci髇 normal
var.test(t1,t2,null.value=1,alternative="two.sided",
         conf.level=0.95)

var(t1)
var(t2)

#Ejercicio para dos varianzas muestrales
ur<-c(3,7,25,10,15,6,12,25,15,7)
ru<-c(48,44,40,38,33,21,20,12,1,18)
length(ur)
length(ru)
var(ur)
var(ru)
q1<-qqnorm(ur,plot.it = F)
q2<-qqnorm(ru,plot.it = F)

plot(range(q1$x,q2$x),range(q1$y,q2$y),type="n",las=1,
     xlab='Cuantiles te髍icos',ylab='Cuantiles de muestra')
points(q1,pch=19)
points(q2,col='red',pch=19)
qqline(ur,lty='dashed')
qqline(ru,col="red",lty='dashed')
legend('topleft',legend=c('ur','ru'),
       bty='n',col=c('black','red'),pch=19)
var.test(ur,ru,null.value=1,alternative="two.sided",
         conf.level=0.95)

#Ejercicio de prueba de hipotesis para la diferencia entre muestras
#con varianzas iguales

datos<-data.frame(tiempo=c(t1,t2),trat=rep(1:2,each=7))
datos
boxplot(tiempo~trat,data=datos,las=1,
        xlab='Tratamiento',ylab='Tiempo (min)')
t.test(t1,t2,alternative="two.sided",mu=0,var.equal = T,
       conf.level =0.97 )


#Ejercicio de diferencia de muestras p/varianzas diferentes
datos<-data.frame(Concentracion=c(ur,ru),
                  Zona=rep(c('Urbana','Rural'),each=10))
datos
boxplot(Concentracion~Zona,data=datos,las=1,
        xlab='Zona',ylab='Concentracion Arsenico(ppb)')
mean(ur)
mean(ru)
t.test(ur,ru,alternative="two.sided",mu=0,
       paired=F,var.equal=F,conf.level=0.95)

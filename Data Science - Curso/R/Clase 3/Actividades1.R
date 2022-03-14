setwd("D:/Data Science - Curso/R/Clase 3")

ex2_1 <- sqrt(144)/27^(1/3)
# <- (sqrt((356-366)*3))^2

print(ex2_1)
#print(ex2_2)
#Ejercicio4----
#Variables
a <- 5
b <- 4
c <- a + b
a <- b * c
b <- (c - a)^2
c <- a * b
#Objetos
x <- c(1,3,5,7,9)
y <- c(2,3,5,7,11,13)
#Ejercicios
x+1
y+1
y*3
x+y
length(x)+length(y)

#Ejercicio5----
vector <- 1:10
vector_2 <- 10:1
vector_2
simpar <- seq(from=1,to=10,by=2)
simpar

ssix <- seq(length=6,from=1,to=10)
ssix

r1 <- rep(5,10)
r1

meses<-c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
meses
length(meses)
str(meses)

#Ejercicio 7----
ls <- list(lugar="Granja",
           nombre="La vaca Lola",
           numero_animales=6,
           animales=c("gallo","gallina","conejo","caballo","perro","vaca"),
           cantidad=c(1,8,6,4,2,1))
View(ls)
names(ls)
str(ls)
ls


getwd()

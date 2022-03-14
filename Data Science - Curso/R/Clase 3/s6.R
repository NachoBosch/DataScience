setwd("D:/Data Science - Curso/R/Clase 3")

ex2_1

matrix(1:8,nrow=4)

matrix(1:6,nrow=2,byrow=TRUE)

cbind(c(1,2,3),c(4,5,6))# cbind es para columnas
rbind(c(1,2,3),c(4,5,6))# rbind es para filas
x <- matrix(1:6, nrow=3)
x[1,2]
x[-1,]

# Asignar nombres a filas y columnas----
x
colnames(x) <- c('Data1','Data2')
rownames(x) <- c('Item1','Item2','Item3')
x
str(x)

#Structures of control----

#if sentences
y <- 0
if (y<10){
  y <- y +1
  print(y)
}else{
  print("hello")
}

#for sentences
for (i in 1:10){
  print(i)
}

#while sentences
i<-0
while(i<10){
  i<-i+1
  print(i)
}

#repeat sentences
n<-0
repeat{
  print(n<-n+1) 
  if (n==10) break}

#Hacer ejercicio para saber si un numero es par o impar
num = scan(nmax=1)

if (num%%2==0){
  print("Numero par")
}else{
  print("Numero impar")}










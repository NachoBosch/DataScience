x = 1+1
y = x**2
z = y/x

library(UsingR)
#lectura de un fichero texto

#x = read.table(file='dataset.csv',dec='.',sep = ',',header = TRUE)
#x = read.table(file='D:/Data Science - Curso/R/Clase 3/dataset.csv',,dec='.',sep = ',',header = TRUE)


datos = c(12.3,15.4,3,5,6,7)
#datos[2:5]
datos<=7
datos[c(2,4:6)]
datos[datos>5]
datos[datos>7 & datos<15.4]
nuevo_vector = datos[datos>5] 
nuevo_vector

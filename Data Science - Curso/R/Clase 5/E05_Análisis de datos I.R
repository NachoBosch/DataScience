###############################################
### CERTIFICACIÃN UNIVERSITARIA EN DATA SCIENCE
###   ANALISIS DE DATOS 1
###############################################


## Myrian Aguilar, 17 de MARZOde 2022 ##

# siempre comienza la sesion limpiando los paneles

rm(list=ls())# Borrar los datos de entorno cargados en memoria 

getwd()
#setwd("C:/ARRAYANES/MUNDO_E") # indica TU de  directorio de trabajo

mi_csv <- read.csv2( "./Clase 4/mydata.csv", header = TRUE, 
                     sep = "," , dec = ".")

names(mi_csv) # nombre de campos ( o variables o columnas, son sinonimos)
str(mi_csv) # veo la estructura y verifico si esta ok

names(mi_csv)
mi_csv$EDAD # llamo a la variable EDAD

sort(mi_csv$EDAD) # ordeno los datos de forma creciente

sort(mi_csv$EDAD, decreasing = TRUE) #Agregamos el parametro decreasing 
                                     #(ordeno en  forma decreciente

order(mi_csv$EDAD) #arroja el indice de los valor lo puedo aplicar a un dataframe

mi_csv$EDAD[20] # muestra la edad que esta en la posicion 20
mi_csv$EDAD[1]
mi_csv$EDAD[5]

mi_csv[order(mi_csv$EDAD),]
mi_csv[order(mi_csv$EDAD), c("EDAD","SEXO")]
mi_csv[order(mi_csv$EDAD), c(1,4)]


mi_csv$SEXO == "femenino"
names(mi_csv)[4]<-"TALLA"
names(mi_csv)

## Dividimos el conjunto de datos
mi_csv[mi_csv$SEXO == "femenino", ]

data_F <- mi_csv[mi_csv$SEXO == "femenino",] # pongo el condicionante en el lugar de las filas
data_M <- mi_csv[mi_csv$SEXO == "masculino",]


# Parametros de posicion
data_F$EDAD

mean(data_F$EDAD, na.rm = TRUE) #media
median(data_F$EDAD, na.rm = TRUE) #mediana

quantile(data_F$EDAD) # muestra todos
quantile(data_F$EDAD, probs = 0.50) # Q2 (mediana)
quantile(data_F$EDAD, probs = 0.99) # Percentil 99
quantile(data_F$EDAD, probs = seq(0, 1, by = 0.1)) # deciles

seq(0, 1, by = 0.1) #secuencia de 0 a 1 cada 0.1


# Parametros de dispersion
range(data_F$EDAD) #rango
range(data_F$EDAD)[1] # muestra el minimo del rango
range(data_F$EDAD)[2] # muestra el maximo del rango
min(data_F$EDAD)

var(data_F$EDAD, na.rm = TRUE) #varianza
sd(data_F$EDAD, na.rm = TRUE) #desv st

min(data_F$EDAD) #minimo
max(data_F$EDAD) #maximo


# Funciones de resumen
summary(data_F$EDAD)
fivenum(data_F$EDAD)

summary(data_F)
summary(mi_csv)


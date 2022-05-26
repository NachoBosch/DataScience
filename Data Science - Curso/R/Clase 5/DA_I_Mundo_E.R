

## Cargamos los datos
data <- read.csv("C:\\Users\\solre\\Downloads\\mydata.csv", dec = ",", sep = "," ) 


names(data) # nombre de campos
str(data) # ver si todo está ok

data$EDAD
sort(data$EDAD) #ordena
sort(data$EDAD, decreasing = TRUE) #Agregamos el paramétro decreasing

data$EDAD
order(data$EDAD) #arroja el indice de los valores

data[order(data$EDAD),]
data[order(data$EDAD), c("EDAD","SEXO")]
data[order(data$EDAD), c(1,3)]

data$SEXO == "femenino"

## Dividimos el conjunto de datos
data[data$SEXO == "femenino", ]

data_F <- data[data$SEXO == "femenino",]
data_M <- data[data$SEXO == "masculino",]


# Parametros de posicion
data_F$EDAD

mean(data_F$EDAD, na.rm = TRUE) #media
median(data_F$EDAD, na.rm = TRUE) #mediana

quantile(data_F$EDAD)
quantile(data_F$EDAD, probs = 0.99) # P99
quantile(data_F$EDAD, probs = seq(0, 1, by = 0.1)) # deciles

seq(0, 1, by = 0.1) #secuencia de 0 a 1 cada 0.1


# Parámetros de dispersion
range(data_F$EDAD) #rango
var(data_F$EDAD, na.rm = TRUE) #varianza
sd(data_F$EDAD, na.rm = TRUE) #desv st

min(data_F$EDAD) #minimo
max(data_F$EDAD) #maximo


# Funciones de resumen
summary(data_F$EDAD)
fivenum(data_F$EDAD)


summary(data_F)
summary(data)

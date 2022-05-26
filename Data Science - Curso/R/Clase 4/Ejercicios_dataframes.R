###############################################
### CERTIFICACIÓN UNIVERSITARIA EN DATA SCIENCE
###   MANIPULACIÒN DE DATOS EN R
###############################################


## Myrian Aguilar, 13 de Octubre de 2021 ##

# siempre comienza la sesion limpiando los paneles
rm(list=ls())# Borrar los datos de entorno cargados en memoria 

#setwd("C:/ARRAYANES/MUNDO_E") # indica TU de  directorio de trabajo

path = getwd()
print(path)
setwd(path)

#install.packages("tidyverse")
library(tidyverse)
tidyverse_packages()

#Importación de datos ----
#read.table()
#read.csv()
#read.csv2()
mi_csv<-read.table("./Clase 4/mydata.csv",header=TRUE,sep=",",dec=".")
print(head(mi_csv))#veo los primeros

#install.packages("readxl")
library(readxl)
#read_excel()
#read_xls()
#read_xlxs()
mi_excel <- read_excel("./Clase 4/mydata.xlsx",col_names=TRUE)
print(head(mi_excel))
names(mi_excel)
#write.table(mi_csv,file="mi_csv.csv",row.names=False,sep=',')
str(mi_excel)
print(ncol(mi_excel))

#Cambiar el nombre
names(mi_csv)
names(mi_csv)[4] <-"TALLA"
names(mi_csv)

mi_csv2 <- read.csv("./Clase 4/mydata.csv",header=TRUE,sep=",",dec=".")
names(mi_csv2)[4]<-"TALLA"
names(mi_csv2)

#Eliminar columna 2 [fila,columna]
mi_csv2[,2]<-NULL
names(mi_csv2)

#Recortar columnas [, x:y]
mi_csv3 <- mi_csv[,3:4]
names(mi_csv3)

#Reordenar variables (columnas)
mi_csv <- mi_csv[c(2,3,1,4)]
names(mi_csv)

#Guardar/exportar dataframe
write.table(mi_csv, file="./Clase 4/mi_csv.txt",sep=",",row.names=TRUE)


# Tratamiento de Valores faltantes en los datos----

datos <- read.table("./Clase 4/mydata.csv",header=TRUE,sep=",",dec=".")
names(datos)[4]<-"TALLA" 

#Para saber cantidad de NA
sum(is.na(datos))

#Para saber cantida de NA por columnas
colSums(is.na(datos))

#Para saber donde está ese valor NA en la columna
which(is.na(datos$TALLA)==TRUE)
datos$TALLA[16]

#Cambiar tipos de datos
str(datos)
class(datos$TALLA)
mean(datos$TALLA)#argument is not numeric or logical: returning NA
datos$TALLA<-as.numeric(datos$TALLA)
class(datos$TALLA)
mean(datos$TALLA)
mean(datos$TALLA,na.rm=TRUE) #calcula la media sin tener en cuenta los NA

class(datos$SEXO)
datos$SEXO <- as.factor(datos$SEXO) #Convertimos los datos strings en objetos
class(datos$SEXO)
levels(datos$SEXO)#Para saber cuales son las categorías de la variable SEXO
datos$SEXO <- factor(datos$SEXO, labels=c("M","H"))#cambio las categorías
levels(datos$SEXO)
class(datos$SEXO)

#---Dataset IRIS---#
data(iris)
names(iris)
levels(iris$Species)
str(iris)

#Tuberías %>%
iris%>%select(Sepal.Length,Sepal.Width)
select(iris,Sepal.Length,Sepal.Width)
iris%>%select(-Species)#selecciono todas las variables menos species
select(iris,1,3) #selecciono por numero de columna
iris%>%select("Species",everything()) #reordeno el data frame tomando como primera columan "Species"

#Renombrar variables
iris%>%rename("Especie"="Species","Largo Sepalo"="Sepal.Length","Ancho Sepalo"="Sepal.Width","Largo Petalo"="Petal.Length","Ancho Petalo"="Petal.Width")
#iris%>%rename("Largo Petalo"="Petal.Length","Ancho Petalo"="Petal.Width")
iris%>%filter(Species=="versicolor")#filtro especie versicolor
filter(iris,Species=="setosa")
filter(iris,Species=="setosa"|Species=="versicolor")
filter(iris,Species==c("setosa","versicolor"))

#Creación de nuevas variables
iris%>%
  mutate(Petal.Shape=Petal.Width/Petal.Length,
         Sepal.Shape=Sepal.Width/Sepal.Length)%>%
  select(Species,Petal.Shape,Sepal.Shape)

iris%>%
  mutate(Petal.Long=Petal.Length>5)%>%
  group_by(Species,Petal.Long)%>%
  summarise(Mean.Petal.Length=mean(Petal.Length),
             n.Petals=length(Petal.Length),
             sd.Petal.Length=sd(Petal.Length),
             SE.Petal.Length=sd(Petal.Length)/sqrt(length(Petal.Length)))

#Gestion de Factores
#----FORCATS----#
#factor()
#as_factor()
#fct_recode()
library(forcats)
datos_salud <- read_excel("./Clase 4/Datos_salud.xlsx",sheet=1,col_names=T)
str(datos_salud)
class(datos_salud$Enfermedad)
levels(datos_salud$Esalud)
datos_salud$Esalud <- as.factor(datos_salud$Esalud)
levels(datos_salud$Esalud)

datos_salud$Enfermedad<-factor(datos_salud$Enfermedad)
class(datos_salud$Enfermedad)
levels(datos_salud$Enfermedad)
datos_salud$Sexo<-fct_recode(datos_salud$Sexo,
                             Varon="Masculino",
                             Mujer="Femenino")
levels(datos_salud$Sexo)
fct_count(datos_salud$Sexo)
which(is.na(datos_salud$Civil)==TRUE)
datos_salud$Civil[8]

# trabajar con fechas---------
# convierto de character >>> Date
navidad <- as.Date("2018-12-25")
navidad
class(navidad)
fecha1 <- as.character(navidad)
class(fecha1)

Sys.Date()

dia1 <- as.Date("25/12/2017", format = "%d/%m/%Y")
dia2 <- as.Date("20/01/2019", format = "%d/%m/%Y")
dia2 -dia1
class(dia1)

difftime(dia2,dia1,units="weeks")
difftime(dia2,dia1,units="days")
difftime(dia2,dia1,units="hours")
difftime(dia2,dia1,units="mins")
difftime(dia2,dia1,units="secs")

seq(dia1,dia2,length=100)

fh1 <- as.POSIXct("01/10/1983 22:10:00",
                  format = "%d/%m/%Y %H:%M:%S",
                  tz = "America/Argentina/Buenos_Aires" )
fh1
fh2 <- as.POSIXct("04/12/1985 10:30:00",
                  format = "%d/%m/%Y %H:%M:%S",
                  tz = "America/Argentina/Buenos_Aires" )
fh2
difftime(fh2, fh1, units="days")/365

#Trabajar con fechas
##Por deafult R toma año/mes/día

fecha1 <- as.Date("1914-07-28")
fecha2 <- as.Date("1918-11-11")
as.character(fecha1,format="%A")
as.character(fecha2,format="%A")
difftime(fecha2,fecha1,units="days")
difftime(fecha2,fecha1,units="weeks")

fecha1 <- as.Date("1914/07/28")
fecha2 <- as.Date("1918/11/11")
as.character(fecha1,format="%A")
as.character(fecha2,format="%A")
difftime(fecha2,fecha1,units="days")
difftime(fecha2,fecha1,units="weeks")


#----------------------------------------------------------------------#
#----------------------------------------------------------------------#
#----------------------------------------------------------------------#
###

# usando otros formatos
# format() usa otros formatos para codificar fechas
as.Date("25/12/2013", format ="%d/%m/%Y" )# día en Nº, mes en Nº, año con 4 digitos 

as.Date("25 Diciembre 2013", format = "%d %B %Y" )

# Opera sobre vectores siempre que esten en codificados del mismo modo

dias <- c("1/10/2005", "2/2/2006", "3/4/2006", "8/11/2014" )
as.Date( dias, format = "%d/%m/%Y" ) # día en Nº, mes en Nº, año con 4 digitos 
                                     # devuelve yyyy/mm/dd

# convierto de Date >>> Character

f1_nav <-  as.character(navidad)
f2_nav <-  format(navidad)
c( f1_nav, f2_nav)

Sys.Date() # fecha actual

###
# operaciones con clase Date
dia1 <- as.Date("25/12/2017", format = "%d/%m/%Y")
dia2 <- as.Date("20/1/2018", format = "%d/%m/%Y")
dia2 - dia1

difftime(dia2, dia1, units = "weeks")
difftime(dia2, dia1, units = "days")
difftime(dia2, dia1, units = "hours")
difftime(dia2, dia1, units = "mins")
difftime(dia2, dia1, units = "secs")

# operaciones con fechas
seq (dia1, dia2, length = 10)
seq (dia1, dia2, by  =  "week")

###
# convierto de character >>> POSIXct

fh1 <- as.POSIXct( "01/10/1983", format = "%d/%m/%Y" )
class(fh1)

fh1 <- as.POSIXct("01/10/1983 22:10:00",
                    format = "%d/%m/%Y %H:%M:%S",
                    tz = "America/Argentina/Buenos_Aires" )
fh1
fh2 <- as.POSIXct("04/12/1985 10:30:00",
                    format = "%d/%m/%Y %H:%M:%S",
                    tz = "America/Argentina/Buenos_Aires" )
fh2
difftime(fh2, fh1, units="weeks")

# paquete lubridate-----------
install.packages("lubridate")
library(lubridate)

fh4 <- ymd_hms("2013-03-10 08:32:07")
fh4
class(fh4)

fh5 <- dmy_hm("1/10/1963 08:32")
fh5
class(fh5)

# extraer dia de la semana
fechitas <- c(my_birthday = as.Date("Febrero 02, 1973", format = "%B %d, %Y"),
              Luciano = as.Date("Diciembre 14, 2003", format = "%B %d, %Y"),
              Marta = as.Date("15ENERO1998", format = "%d%b%Y"),
              Paola = as.Date('1998-09-15'),
              today = as.Date("2021/10/13")) 
fechitas

weekdays(fechitas)

#extraer el mes
months(fechitas)

# ¿Qué día de la semana era el 20 de julio de 1969? (llegada del hombre a la luna)

luna <-  as.Date("1969-07-20")
as.character( luna, format="%A")

# ¿Qué día de la semana llegó Colón a América? (12 de octubre de 1492)
colon <-  as.Date("1492-10-12")
as.character( colon, format="%A")

# ¿En qué día de la semana naciste?
nacimiento<-as.Date("1973-02-02")
as.character(nacimiento,format="%A")

###
# ejercicio 1----------------

Inicio<-as.Date("1914-07-28") #fecha de incio de la Primera Guerra Mundial
Inicio
Fin<-as.Date("1918-11-11")    # fecha de finalizacion de la Primera Guerra Mundial
Fin
difftime(Fin, Inicio, units = "weeks") # casi 224 semanas 

# calculo fecha edad -----------------
FecNac <- as.Date("Febrero 02, 1973", format = "%B %d, %Y")
FecNac
Edad_cal<- (Sys.Date()- FecNac)/365
Edad_cal
Edad_cal<-round((Sys.Date()- FecNac)/365)
Edad_cal

### 

# instalando librerias opaquetes-------
#install.packages("tidyverse") # instalo el paquete

library(tidyverse)
require(tidyverse)
tidyverse_packages() # muestra todas las librerias incluidas en el paquete


# importar datos desde archivo csv------
# usando funcion read.table()

mis_datos <- read.table( "mydata.csv", header = TRUE,
                      sep = "," , dec = ".")
head(mis_datos)

#usando la funcion read.csv2
require(readr)
mi_csv <- read.csv2( "mydata.csv", header = TRUE, 
                     sep = "," , dec = ".") 

head(mi_csv)# veo los primeros registros


# desde excel-------------------
# install.packages("readxl")
library(readxl)
require(readxl)

mi_excel <- read_excel("mydata.xlsx", col_names = TRUE)
head(mi_excel)


# exploracion de Dataframe-------

# str()	Devuelve la estructura del dataframe
str(mi_csv) 
# ncol()	Devuelve el número de columnas (variables) del dataframe
ncol(mi_csv)
# nrow()	Devuelve el número de filas (observaciones) del dataframe
nrow(mi_csv)
# dim()	Devuelve la dimensión del dataframe (filas por columnas)
dim(mi_csv)
# names() devuelve el nombre de las variables
names(mi_csv)

# cambiar nombre a variable
names(mi_csv)[4] <- "TALLA"
names(mi_csv)

# eliminar variables o columnas
mi_csv[,2] <- NULL # elimino la columna 2( variable Peso)
names(mi_csv)

datos <- mi_csv[,2:3]
names(datos)

# reordenar variables
mi_csv <- mi_csv[c(2,3,1)]
names(mi_csv)

# guardar archivos------------

write.table(mi_csv, file = "mi_csv.txt", sep = ",",
              row.names = FALSE)

# datos ausentes--------------

datos <- read.table( "mydata.csv", header = TRUE,
                     sep = "," , dec = ".")

sum(is.na(datos))# cuantos NA tiene mi DF

colSums(is.na(datos))# cantidad de NA por columna

which(is.na(datos$TALLa) == TRUE) 
#La variable TALLa tiene un dato del tipo NA en la fila 16
 

mean(datos$TALLa) # calculo la media. Nos dice no disponible porque hay NA
# na.rm = TRUE que excluye el valor NA
mean(datos$TALLA, na.rm = TRUE)# no tiene en cuenta los NA

#complete.cases() nos devuelve un vector con las filas que se encuentren completas.
datos[complete.cases(datos$TALLa),] # observar resultados que no esta la fila 16
datos$TALLa

view(datos)
# Conversion de tipo de datos--------------
# as.numeric	Convierte a tipo numérico
# as.integer	Convierte a tipo entero
# as.character	Convierte a tipo carácter
# as.logical	Convierte a tipo lógico o booleano
# as.factor	Convierte a tipo factor
# as.ordered	Convierte a tipo factor ordenado

class(datos$SEXO)# pido la clase de objeto, su atributo
datos$SEXO <- as.factor(datos$SEXO) # transformo en factor
class(datos$SEXO) # verifico
levels(datos$SEXO) # etiquetas de los niveles 

datos$SEXO <- factor(datos$SEXO, labels = c("Mujer","Hombre")) # cambio etiquetas
class(datos$SEXO) # Verificamos el tipo convertido
levels(datos$SEXO) # verifico

# Dplyr-------
# Las funciones del paquete responden a las siguientes acciones (verbos):
  # seleccionar--> select(): devuelve un conjunto de columnas (variables)
# renombrar--> rename(): renombra variables en una conjunto de datos
# filtrar -->filter(): devuelve un conjunto de filas  según una o varias condiciones lógicas
# ordenar -->filas arrange(): reordena filas de un conjunto de datos
# añadir -->variables/columnas mutate(): añade nuevas variables/columnas o transforma variables existentes
# resumir--> summarise() / summarize(): genera resúmenes estadísticos de diferentes variables en el conjunto de datos.
# agrupar--> group_by(): agrupa un conjunto de filas seleccionado, en un conjunto de filas de resumen de acuerdo con los valores de una o más columnas o expresiones.

# dataset IRIS
# contiene las mediciones en centímetros de las variables longitud (Length) y 
# ancho (Width) de los pétalos (Petal) y sépalos (Sepal) de 50 flores de cada una 
# de las 3 especies (Species) del género Iris: Iris setosa, Iris versicolor e Iris virginica

data(iris)  # cargar el dataset en el workspace
names(iris) # nombre de las columnas
iris %>% select(Sepal.Length, Sepal.Width) # largo y ancho de sepalo
select(iris, Sepal.Length, Sepal.Width) #seleccionar las columnas "Sepal.Length" y "Sepal.Width"
iris %>% select(-Species) # selecciono todas variables menos Species
iris %>% select(1,3) # lo mismo pero seleccionando columna 1 y 3
iris %>% select("Species",everything()) # reordenar el Dataframe
iris %>% rename("Especie"= "Species", "Long Sepalo"="Sepal.Length", "Ancho Sepalo"= "Sepal.Width") 
# renombranos variables a español

iris %>% filter(Species == "versicolor") # filtro especie versicolor

# filtrar sólo especie setosa
filter(iris, Species == 'setosa')

# filrar especie setosa o virginica
filter(iris, Species == 'setosa' | Species == 'virginica')

# especie setosa con longitud de sépalo menor a 5 mm
filter(iris, Species == 'setosa', Sepal.Length < 5)

# ordenemos Iris por la variable longitud del pétalo (Petal.Length):
iris %>% arrange(Petal.Length) # ascendente por defecto
iris %>% arrange(desc(Petal.Length)) # descendente

# longitud de pétalo ascendente y ancho descendente
iris %>% arrange(Petal.Length,desc(Petal.Width) )

# creacion de variables
iris %>%
     mutate(Petal.Shape = Petal.Width / Petal.Length,
                	Sepal.Shape = Sepal.Width / Sepal.Length) %>%
   select(Species, Petal.Shape, Sepal.Shape)

# Observemos que la función realiza el cálculo e incorpora una nueva variable por 
# cada observación con el resultado. De esta forma, se pueden construir múltiples 
# variables en la misma expresión, solamente separadas por comas.

iris %>%
    group_by(Species) %>% # agrupo por especie
    summarise(Mean.Petal.Length = mean(Petal.Length)) # calculo media

# varios procesos juntos

iris %>%
     mutate(Petal.Long = Petal.Length > 5) %>%
     group_by(Species, Petal.Long) %>%
     summarise(Mean.Petal.Length = mean(Petal.Length),
                         	n.Petals = length(Petal.Length),
                         	sd.Petal.Length = sd(Petal.Length),
                         	SE.Petal.Length = sd(Petal.Length) / sqrt(length(Petal.Length)))

###
# gestion de factores------------

require(forcats)
Datos_salud <- read_excel("Datos_salud.xlsx",sheet = 1,col_names=T) # 19 observaciones 9 variables
str(Datos_salud)

Datos_salud$Enfermedad <- factor(Datos_salud$Enfermedad) # transformo a factor
levels(Datos_salud$Enfermedad) # vemos los niveles del factor

Datos_salud$Sexo <- as_factor(Datos_salud$Sexo)
levels(Datos_salud$Sexo)

Datos_salud$Sexo <- fct_recode(Datos_salud$Sexo,  # recodifico
                               Varon="Masculino",  
                               Mujer="Femenino")
levels(Datos_salud$Sexo) # verifico
fct_count(Datos_salud$Sexo) # cuento por nivel del factor

Datos_salud$Civil <- as_factor(Datos_salud$Civil)
levels(Datos_salud$Civil)

# mostrar dentro de una tabla de frecuencia la cantidad de valores perdidos o 
# desconocidos que tenemos de la variable Estado Civil
Datos_salud$Civil <- fct_explicit_na(Datos_salud$Civil,na_level = "Desconocido")
levels(Datos_salud$Civil)

Datos_salud$Esalud <- ordered(Datos_salud$Esalud) 
levels(Datos_salud$Esalud)

Datos_salud$Esalud <- fct_relevel(Datos_salud$Esalud,# ordeno la variable categorica ordinal
                                  "Muy buena", "Buena",
                                  "Regular","Mala", "Muy mala")
levels(Datos_salud$Esalud)


Datos_salud$Ciudad<- as_factor(Datos_salud$Ciudad)
fct_count(Datos_salud$Ciudad) # cuento cuantas por nivel

# hay varias categorías con poca frecuencia, es mejor agruparlas en un "otras/os". 
# Eso lo podemos con la función fct_other(). En keep especificamos las variables 
# que deseamos que se conserven y en other_level definimos cómo queremos que se llamen las otras.

Datos_salud$Ciudad <- fct_other(Datos_salud$Ciudad,
                                keep = "La Plata",
                                other_level = "Otras")
levels(Datos_salud$Ciudad)

Datos_salud$Comorbilidades<- as_factor(Datos_salud$Comorbilidades)
levels(Datos_salud$Comorbilidades)

Datos_salud$Comor_agrupadas <- fct_collapse(Datos_salud$Comorbilidades, # creo nueva variable y las agrupo por patologia
                                            Respiratoria = c("EPOC", "TBC",  
                                                             "Neumonia"),
                                            Digestiva = c("Hepatitis", "Gastritis"),
                                            Circulatorio = c("aterosclerosis", 
                                                             "Hipertensión"))
levels(Datos_salud$Comor_agrupadas)

### FIN del SCRIPT###

fh1 <- as.Date("1914/07/28")
fh2 <- as.Date("1918/11/11")

as.character(fh1,format="%A")
as.character(fh2,format="%A")
difftime(fh2,fh1,units = "days")
difftime(fh2,fh1,units="weeks")

birth <- as.Date("1992-10-05")
actual <- Sys.Date()

Edad_calc <-difftime(actual,birth,units="days")/365
Edad_calc



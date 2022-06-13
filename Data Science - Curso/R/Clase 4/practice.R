
### PRACTICA DATASET TITANIC ###

#Buscamos datasets para practicar --> ?data()
#Cargamos el dataset
titanic <- read.csv("./Clase 4/titanic/train.csv")
str(titanic)
names(titanic)#Nombre de las columnas
length(titanic)#Cantidad de columnas
head(titanic,n=1L)#Primeros 6L
dim(titanic)#dimension del dataset
length(names(titanic))
is.na(titanic)
sum(is.na(titanic))#Cantidad de valores NUll/na del dataset
which(is.na(titanic$Age)==TRUE)
age_mean <- mean(titanic$Age,na.rm = TRUE)
age_mean
summary(titanic)#sumario del dataset
sapply(titanic,class)

#Transformamos el tipo de dato de las columnas Survived y Sex para que sean un factor
titanic$Survived <- as.factor(titanic$Survived)
class(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
class(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)
class(titanic$Embarked)
titanic$Pclass <- as.factor(titanic$Pclass)
class(titanic$Pclass)

#Buscamos na y completamos el dataset por c/columna
sum(is.na(titanic$Sex))
sum(is.na(titanic$Age)) # Este tiene 177 na
sum(is.na(titanic$Embarked))
sum(is.na(titanic$Survived))
sum(is.na(titanic$Cabin))
sum(is.na(titanic$Ticket))
sum(is.na(titanic$Sex))

#Asignamos valor promedio de Age donde existan na valores
titanic$Age[is.na(titanic$Age)] <- age_mean
sum(is.na(titanic$Age))
titanic$Age


sum(is.na(titanic))# Ahora el dataset está completo

#Hacemos análisis exploratorio mediante gráficas
library(ggplot2)
library(patchwork)
a <- ggplot(titanic)
bar_sex<-a+geom_bar(aes(Sex))
bar_survived <- a+geom_bar(aes(Survived))
hist_age <- a+geom_histogram(aes(Age))
bar_sex/bar_survived/hist_age
a+geom_histogram(aes(titanic$Fare))

a+geom_count(aes(Survived,Age))
a+geom_boxplot(aes(Sex,Age),fill=Survived)
a+geom_point(aes(Sex,Age))
a+geom_bar(aes(Embarked))
a+geom_bar(aes(Pclass))
a+geom_point(aes(Age,Pclass))
a+geom_count(aes(Survived,Sex))
a+geom_count(aes(Survived,Pclass))

# Hacemos una exploración mediante estadísticos descriptivos
tabla<-table(titanic$Survived,titanic$Pclass)#construyo table de contigencia
library(MASS)
cov(tabla$)
tabla
range(titanic$Age)
min(titanic$Age)
range(titanic$Fare)


#Test de hipotesis
#Se desea contrastar la hipotesis con un nivel de significancia de 5% que los pasajeros con edad = 30 
#eran los que No iban a sobrevivir contra los demas si iban a sobrevivir
#H0: u=30
#H1: u!=30
install.packages("BSDA") 
library(BSDA)
age <- c(titanic$Age)
var(age)
range(age)
z.test(x=age,y=NULL,alternative="less",mu=30,sigma.x=sd(age),sigma.y=NULL,conf.level=0.95)

#Test de normalidad
shapiro.test(age)

#Test de hipotesis t-student
t.test(age,mu=30)

#Test de correlacion entre variables ordinales
titanic$Survived <- as.numeric(titanic$Survived)
titanic$Pclass <- as.numeric(titanic$Pclas)
survivor <- c(titanic$Survived)
cor.test(titanic$Pclass,survivor,method="spearman") #nos da un p valor menor a 0.05 lo cual rechazamos la H0 de que no existe correlacion
#existe correlacion negativa entre las clases y los sobrevivientes (esto se puede corroborar graficamente mas arriba)


x <-seq(from=-1000000,to=1000000)
z.test(x=x,y=NULL,alternative="two.sided",mu=0,sigma.x=sd(x),sigma.y=NULL,conf.level=0.95)

## Linear regression ##
library(dplyr)
library(GGally)
library(lmtest)
library(car)

datos_titanic <- select(titanic,c(Age,Fare))
str(datos_titanic)
ggpairs(datos_titanic, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")
plot(datos_titanic$Age,datos_titanic$Fare)
cor(datos_titanic$Age,datos_titanic$Fare)
cor.test(datos_titanic$Age,datos_titanic$Fare)
shapiro.test(datos_titanic$Age)
shapiro.test(datos_titanic$Fare)

modelolineal = lm(Age~Fare, data =datos_titanic)
summary(modelolineal)



##################################
######## Unployee Dataset ########
##################################

unp <- read.csv("https://raw.githubusercontent.com/amankharwal/Website-data/master/unemployment.csv")
names(unp)
str(unp)

##transform data
unp$Region <- as.factor(unp$Region)
unp$Date <- as.Date(unp$Date,format="%d-%m-%y")
unp$Frequency <- as.factor(unp$Frequency)
unp$Region.1 <- as.factor(unp$Region.1)
str(unp)

#EDA
a<-ggplot(data=unp)
a+geom_bar(aes(Region.1))
a+geom_count(aes(Region.1,longitude))
data_unp <- select(unp,-c(Region,Date,Frequency,Region.1))
ggpairs(data_unp, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

#Modelo regresion
plot(unp$latitude,unp$Estimated.Labour.Participation.Rate....)
shapiro.test(unp$latitude)
shapiro.test(unp$Estimated.Labour.Participation.Rate....)
cor.test(unp$latitude,unp$Estimated.Labour.Participation.Rate....,method = "spearman")
cor.test(unp$latitude,unp$Estimated.Labour.Participation.Rate....)

modelolineal = lm(unp$latitude~unp$Estimated.Labour.Participation.Rate....,data=unp)
summary(modelolineal)

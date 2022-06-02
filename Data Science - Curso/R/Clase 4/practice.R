
### PRACTICA DATASET TITANIC ###

#Buscamos datasets para practicar --> ?data()
#Cargamos el dataset
titanic <- read.csv("./titanic/train.csv",)
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


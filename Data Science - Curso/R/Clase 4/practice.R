#Buscamos datasets para practicar --> ?data()
#Cargamos el dataset
titanic <- read.csv("./titanic/train.csv")
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
a <- ggplot(titanic)
a+geom_bar(aes(Sex))
a+geom_bar(aes(Survived))


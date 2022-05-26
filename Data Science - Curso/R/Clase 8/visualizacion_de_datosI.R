#Autor: Ignacio Bosch
#Fecha 31/03/22
#Objetivo:
#         - Aprender a utilizar librerias graficas de R
#---------------------------------------------------------#

#Histogramas. hist()
data("ToothGrowth")?

str(ToothGrowth)
length(ToothGrowth$supp)
summary(ToothGrowth)
describe(ToothGrowth)
class(ToothGrowth)

hist(ToothGrowth$len,
     breaks=15,
     freq=T,
     col="#308E62",
     border='orange',
     main="Histograma de crecimiento de dientes")
#-----------------------------------------------------#
#GGPLOT

#ggplot(x) nos permite crear un "lienzo en blanco" donde se irá armando la imagen
library(ggplot2)
data(iris)
str(iris)
levels(iris$Species)
View(iris$Species)
ggplot(iris)
a<-ggplot(iris)
a+geom_histogram(aes(Sepal.Length),
                 binwidth=0.1)
a+geom_bar(aes(Species))
a+geom_point(aes(Sepal.Length,Sepal.Width))
a+geom_bar(aes(Species,Petal.Length),stat='identity')
a+geom_boxplot(aes(Species,Petal.Length))
a+geom_point(aes(Petal.Length,Petal.Width,
                 shape=Species,
                 color=Species))

a+geom_point(aes(Petal.Length,Petal.Width,
                 color=Species,
                 shape=Species))+
  scale_color_manual(values=c('red','orange','purple'))+
  scale_shape_manual(values=c(19))
boxplot(iris$Petal.Length ~ iris$Species) 


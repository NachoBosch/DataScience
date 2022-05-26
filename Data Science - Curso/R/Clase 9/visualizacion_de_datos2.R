#################################
### Visualizacion de Datos II ###
#################################


# Objetivos:
#          - Construccion de tableros usando patchwork y ggplot2
#          - Aplicacion de etiquetas y temas en ggplot
library(ggplot2);library(patchwork)
data(iris)
puntos<-ggplot(iris) + geom_point(aes(Petal.Width,
                                      Petal.Length,
                                      color=Species))
barras <- ggplot(iris)+geom_bar(aes(Sepal.Width))
barras

hist_sepalo <- ggplot(iris) +geom_histogram(aes(Sepal.Length,
                                                fill=Species))
hist_sepalo

boxplot<-ggplot(iris)+geom_boxplot(aes(Species,Sepal.Width))
boxplot

# TABLERO
# Sintaxis de patchwork
# () graficos de un mismo renglon
# / cambia de renglon
# | separamos columnas

(puntos+barras)/(hist_sepalo+boxplot)

puntos/hist_sepalo | barras/boxplot + plot_layout(ncol = 2, widths = c(1, 2))

# labs(title,subtitlte,tag,x,y,caption,...)

puntos<-puntos + labs(title='Relacion entre el ancho y largo de petalo',
              subtitle='gráfico de puntos',
              x='Largo de petalo (cm)',
              y='Ancho de petalo (cm)',
              caption='Fuente: dataset iris',
              tag='Figura 1') 

puntos + theme(axis.line=element_line(color='black'),
               panel.background = element_blank(),
               title=element_text(color = 'grey50'),
               plot.title=element_text(face='bold'),
               axis.title.x=element_text(hjust=1),
               axis.title.y=element_text(hjust=1),
               panel.grid=element_line(color='black'))

# R trae sus propios temas para editar los  graficos de forma mas linda
boxplot+theme_dark()


#Ejercicio practico en grupo----

#DATASETS:
# ChickWeight
# Orange
# PlantGrowth
# airquality
# rock
# trees
# cars


data("rock")
rock <- data(rock)

data("rock")
str(rock)
r <- ggplot(rock)
puntos <- r + geom_point(aes(perm,shape))
puntos
box <- r + geom_boxplot(aes(perm,shape))
box

puntos2 <- r + geom_point(aes(area,shape))
puntos2

rock$grupo <- cut(rock$perm,breaks=seq(from=0,to=1500,by=500))
rock

rock_hist <- ggplot(rock) + geom_histogram(aes(area,color=grupo))
rock_hist













###############################################
### CERTIFICACIÓN UNIVERSITARIA EN DATA SCIENCE
###   TEST DE HIPOTESIS
###############################################


## Myrian Aguilar, 22 de diciembre de 2021 ##


rm(list=ls())# Borrar los datos de entorno cargados en memoria 

setwd("C:/ARRAYANES/MUNDO_E") 
### Inferencias Basadas en una muestra.
# POBLACIÓN NORMAL CON DESVIACION ESTANDAR CONOCIDA.--------------

# Ejemplo: Se desea contrastar con un nivel de significancia del 5% la hipótesis 
# de que la estatura media de los hombres de 18 o más años de un país es igual a 175, 
# contra la alternativa que es menor que 175. Suponiendo que la desviación estándar es de 4.5. 
# Para dicha prueba se recolectaron los datos que constituyen una muestra de n=14 
# hombres seleccionados al azar, 
# cuyas alturas son: 167 167 168 168 168 169 171 172 173 175 175 175 177 182.

muestra <- c(167,167,168,168,168,169,171,172,173,175,175,175,177,182)
summary(muestra)
boxplot(muestra)
# Los Box plot (diagramas de caja y bigote) nos brindan una idea bastante precisa sobre cómo 
# se distribuyen los datos. Un gráfico box plot está construido por, la línea 
# intermedia que es la mediana de los datos (percentil 50), los límites inferior y superior de la caja 
# corresponden al primer y tercer cuartil (percentiles 25 y 75) respectivamente. Por defecto, 
# los bigotes se extenderán hasta 1.5 veces el rango intercuartílico (Q3-Q1) desde la parte superior 
# (inferior) de la caja hasta el dato más lejano dentro de esa distancia. Si hay algún dato mas allá
# de dicha distancia, serán representados mediante puntos individuales (outliers).

# debo verificar que la muestra es aproximadamente normal
# Luego debo definir la hipótesis nula y alternativa: Ho: µ= 175 contra Ha: µ < 175.

install.packages("BSDA") 
library(BSDA)

# prueba de hipótesis con la función z.test 

z.test(x=muestra,y=NULL,alternative="less",mu=175,sigma.x=sd(muestra),sigma.y=NULL,conf.level=0.95)

# Conclusión: rechazo Ho, ya que existe suficiente evidencia de que la estatura media de 
# los hombres en ese país es menor que 175, basado en un nivel de significancia del 5%, 
# que comparado con el valor p, (p<0,05) lo que nos indica que se trata de una caso poco 
# probable el hecho de que la estatura media sea igual a 175.

# MUESTRA GRANDE DESVIACION ESTANDAR DESCONOCIDA.-----------

# UNA POBLACIÓN NORMAL CON DESVIACION ESTANDAR DESCONOCIDA Y MUESTRA PEQUEÑA.------
# Una compañía ferroviaria canadiense afirma que sus trenes de mercancías no bloquean 
# los pasos a nivel durante más de 8 minutos, en promedio. Una muestra aleatoria de 
# 10 tiempos de bloqueo dio como resultado estos valores (en minutos): 
# 10.1, 9.5, 6.5, 8.0, 8.8, 12, 7.2, 10.5, 8.2, 9.3. Bajo un nivel de significancia del 0.05.

# como la muestra es pequeña (n = 10), usaremos la distribución t de Student para el cálculo del p-valor. 
# Llamamos µ al tiempo medio de bloqueo, planteamos nuestras hipótesis: Ho: µ= 8 contra Ha: µ > 8

datos=c(10.1, 9.5, 6.5, 8.0, 8.8, 12, 7.2, 10.5, 8.2, 9.3)
mu0=8
t.test(datos,mu=mu0,alternative="greater",conf.level = 0.95)

# Conclusion: rechazo Ho, ya que existe suficiente evidencia de que el tiempo de bloqueo de 
# los trenes es mayor que 8 minutos, bajo un nivel de significancia del 5%, que comparado 
# con el valor p, (p<0,05),lo que nos indica que se trata de un caso poco probable el hecho 
# de que la media del tiempo de bloqueo de los trenes sea igual a 8 minutos.

### Inferencias Basadas en dos muestras.-----------
# POBLACIONES NORMALES CON VARIANZAS CONOCIDAS.--------

# Un dueño de una fábrica tiene un pedido muy grande y para llevarlo a cabo necesita 
# conocer cuál de las dos máquinas para realizar el proceso trabaja con mayor velocidad, 
# el intuye que la maquina 2 es más efectiva y para ello hace una prueba, toman el tiempo 
# de fabricación en minutos de 10 productos en cada máquina. Los resultados son:

# Fabrica 1: (15, 16, 15, 14, 14, 15, 13, 15, 15,14) 
# Fabrica 2: (16, 15, 14, 17, 12, 17, 15, 16, 15,14)

# ¿Es posible que la maquina 2 tenga un mejor tiempo de producción que la maquina 1 
# usando un nivel de significancia de 0.05?   Hipótesis. Ho: ??1-??2=0 Ha: ??2>??1

# Primero verificaremos si ambas poblaciones son normales esto lo haremos con el 
# código qqnorm y qqline o también podríamos usar un diagrama de caja con la función Boxplot, 
# luego procederemos calcular la media y varianza de cada población.

maquina_1 <- c(14,13,15,14,17,16,15,16,13,17)
maquina_2 <- c(16,15,14,17,12,17,15,16,15,14)

summary(maquina_1)
summary(maquina_2)

sd(maquina_1) # calculo la desviacion estandar
sd(maquina_2)

mean(maquina_1) # calculo la media
mean(maquina_2)

# observo si son normales 
qqnorm(maquina_1,col="blue",xlab="Eje z",ylab="Tiempo", main="Maquina 1"); qqline(maquina_1,col="red") 
qqnorm(maquina_2,col="blue",xlab="Eje z",ylab="Tiempo", main="Maquina 2"); qqline(maquina_2,col="red")
# veo que cumplen con la normalidad

z.test(x=maquina_1,y=maquina_2,alternative="greater",mu=0,sigma.x=sd(maquina_1),sigma.y=sd(maquina_2),conf.level=0.95)

# conclusion: como p>0,05 acepto hipotesis alternativa.No rechazamos Ho,existe evidencia que la maquina 2 
# es más igual rápida que la maquina 1 por lo que se puede hacer uso tanto de la maquina 2 como de 
# la maquina 1 para un tiempo de producción menor y cumplir con la entrega todo esto a un nivel de 
# significancia de 0.05


# POBLACIONES NORMALES CON VARIANZAS DESCONOCIDAS PERO IGUALES.-----

# Dos ciclistas compiten para saber quién de los dos es más rápido y entrenan a diario de la misma forma,
# cada día recorren 15 kilómetros durante un mes. El dato promedio del tiempo para el ciclista 1 es de 122
# y para el ciclista 2 es 120. Varias personas afirman que el ciclista 2 ganara la carrera, a un nivel de significancia del 5% comprobar las especulaciones de las personas asimiento una varianza igual para ambos ciclistas.

# Ciclista1:(130,129,130,124,124,122,130,125,126,123,130,126,125,128,125,125,125,125,125,125,130,123,120,122,125,123,122,127,120,121) 
# Ciclista2:(128,130,125,125,127,123,130,125,124,123,130,125,125,129,125,125,125,124,125,125,130,122,121,121,125,125,122,128,121,125)

# Hipótesis. Ho: ??1-??2=0 Ha: ??2>??1

ciclista_1<-c(130,129,130,124,124,122,130,125,126,123,130,126,125,128,125,125,125,125,125,125,130,123,120,122,125,123,122,127,120,121)
ciclista_2 <- c(128,130,125,125,127,123,130,125,124,123,130,125,125,129,125,125,125,124,125,125,130,122,121,121,125,125,122,128,121,125)

# con un boxplot debemos de comprobar si nuestras poblaciones son normales y si presentan las misma varianzas
# luego aplicamos prueba t.student

boxplot(ciclista_1,ciclista_2,main="Comparacion de recorrido",ylab="Kilometros",col="Orange")

t.test(ciclista_1,ciclista_2,alternative="greater",mu=0,var.equal=TRUE,conf.level=0.95)
# conclusion:(p>0,05). No rechazar Ho,la prueba t.test nos dice que la diferencia de 
# media entre los ciclistas es aproximadamente igual, por ende, el ciclista 2 y ciclista 1 
# tienen las mismas opciones de ganar la Carrera basándonos en la estadística a un nivel de 
# significancia del 5%.

# POBLACIONES NORMALES CON VARIANZAS DESCONOCIDAS Y DIFERENTES.-------

# En un supermercado se evalúan los contenidos de cajas de cereal de dos compañías, 
# el encargado de tal tarea asegura que los contenidos son los mismos, pero a la queja de los 
# clientes se decide realizar una nueva evaluación tomando 30 cajas de cereal pesadas en gramos. 
# Los datos son:

# Marca1(600,605,604,603,597,604,602,600,600,600,600,599,598,597,597,597,600,600,600,602,600,598,597,605,605,605,600,604,603,603) 
# Marca2(605,602,600,597,598,604,601,604,600,598,600,600,598,605,597,597,604,600,602,602,597,600,597,600,605,597,600,604,600,600)
                                                                                                                                       
# Demostar si el contenido de ambas marcas es el mismo usando un nivel de confianza del 95%
# Hipótesis. Ho: ??1-??2=0 Ha: ??2?????1

marca_1<- c(600,605,604,603,597,604,602,600,600,600,600,599,598,597,597,597,600,600,600,602,600,598,597,605,605,605,600,604,603,603) 
marca_2<- c(605,602,600,597,598,604,601,604,600,598,600,600,598,605,597,597,604,600,602,602,597,600,597,600,605,597,600,604,600,600)

# realizo un boxplot para comprobar que la población sea normal y quela varianza sea diferente, 
# luego aplico un test para nuestra prueba de hipótesis con varianzas desconocidas y diferentes.

boxplot(marca_1,marca_2,col="yellow",ylab="Gramos",main="Diferencia de pesos")

t.test(marca_1,marca_2,alternative="two.sided",mu=0,var.equal=FALSE,conf.level=0.95)

# conclusion: (p>0,05). No rechazamos Ho,la prueba nos dice que la diferencia de media 
# de ambas marcas es aproximadamente la misma y por ende contienen los mismos contenidos, 
# dando la razón al empleador que realizo la prueba en un inicio con un nivel de significancia del 5%.

# Prueba t para observaciones pareadas.-------

# Una empresa afirma que el control para videojuegos que el produce tiene la misma duración 
# media que el control original. Se realiza una prueba para 5 marcas de baterías de la duración 
# del control en uso. Los datos son los siguientes:

datos<-matrix(c(4.1,3.7,3.8,4.1,4.5,3.9,3.6,3.8,4.2,4.0),
              nrow=5,byrow=T)
colnames(datos)<-c("Original","Generico")
datos

datos<-as.data.frame(datos) # trasnforma a dataframe
attach(datos) # La base de datos está adjunta a la ruta de búsqueda R. Significa que R 
# busca en la base de datos al evaluar una variable, por lo que se puede acceder a los 
# objetos de la base de datos simplemente dando sus nombres.

var.test(Original,Generico)
shapiro.test(Original)
shapiro.test(Generico)

t.test(Original,Generico,"two.sided",paired=TRUE, conf.level=0.95)
boxplot (Original,Generico)
# Conclusión: Se tiene de la prueba que el valor-P es de 0.4615, lo cual p>0,05
# por lo tanto, se acepta la hipótesis nula y se rechaza la hipótesis alterna. 
# La afirmación de la empresa es cierta, el control de videojuegos que fabrican 
# tiene una duración de batería media igual a la del original.

# Prueba para la diferencia de proporciones de dos muestras.------------
# Se desea conocer si la proporción de consumidores de un restaurante que son hombres y 
# consumen más de $5.00, es igual a la proporción de mujeres que visita el restaurante y 
# consume más de $5.00.

# Un día se recogió que de 120 hombres que se atendieron,78 consumió más de $5.00. 
# Mientras que el mismo día se recogió que de 69 mujeres que se atendieron el restaurante, 
# 28 consumió más de $5.00. 
  
# H0: La proporción de hombres que consumen más de $5.00 en el restaurante es igual 
# a la proporción de mujeres que consumen más de $5.00 en el restaurante.

# H1: La proporción de hombres que consumen más de $5.00 en el restaurante es diferente 
# a la proporción de mujeres que consumen más de $5.00 en el restaurante.

x <- c(78, 28)
n <- c(120, 69)
prop.test(x, n, conf.level = 0.95, correct = FALSE) # test para proporciones

# Conclusión: el valor-P es de 0.001126, (p<0,05)al nivel de significancia de ?? = 0.05 
# (al nivel de confianza del 95%), por lo tanto, se rechaza la hipótesis nula y 
# se acepta la hipótesis alterna. 
# Es decir, la proporción de hombres que consumen más de $5.00 en el restaurante 
# es diferente a la proporción de mujeres que consumen más de $5.00 en el restaurante.


# dataset Wages---------
install.packages("ISLR")
library(ISLR)
data("Wage") # set de datos incorporado en el paquete.3000 observaciones y 11 variables
str(Wage)

# Cómo varían los salarios a lo largo de los años con la edad, la educación y la clase de trabajo
require(ggplot2)
require(data.table)

Wage <- as.data.table(Wage)
Wage$year <- factor(Wage$year)

subWage <- Wage

g <- ggplot(data = subWage, aes(y=wage, x=age, color=education))
g <- g + ggtitle("Salarios por edad") + xlab("Años") + ylab("Salarios")
g <- g + facet_wrap(~jobclass)
g <- g + geom_point() + geom_smooth(se=FALSE)

print(g)


### leo el set de datos desde la web
wage_web<- read.csv("https://raw.githubusercontent.com/xkong100/IS607/master/Project2/Wages1.csv", stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
# tiene 3294 observaciones y 5 variables

# var.1 es la identificación del individuo,
# exper que representa la experiencia en años,
# sex es la variable dummy de sexo (1=hombre, 0=mujer),
# school que representa la escolaridad en años y
# wage que corresponde al salario por hora (en dólares).


#EDA
head(wage_web)

attach(wage_web) # Para poder usar libremente las variables contenidas en la base de datos 

# organicemos los datos para encontrar los salarios de diferentes géneros 
# cuya experiencia es de 10 años y el año de educación es 14
attach(wage_web)
require(tidyr)
library(dplyr)
library(lattice)
library(knitr)

wage_web <- data.frame(wage_web) # lo trasnformo en dataframe
wage1 <- wage_web %>% spread (sex, wage)
wage1
wage1$female[is.na(wage1$female)] <-0
wage1$male[is.na(wage1$male)] <- 0
wage2 <- wage1 %>% filter(exper == 10)
wage2
wage3 <- wage2 %>% filter (school == 14)
wage3
wage4 <- wage3 %>% arrange(exper)
wage4
colnames(wage4)[1]="Worker_id"
kable(head(wage4))

#  La kable() función devuelve una sola tabla para un solo objeto de datos y 
# devuelve una tabla que contiene varias tablas si el objeto de entrada es una 
# lista de objetos de datos. 


# obtengo un resumen

wage4 %>% summarise (Mean_femalewage_10yrs = mean(female), max_femalewage_10yrs = max(female),
                     mean_malewage_10yrs= mean(male), max_malewage_10yrs=max(male))

# queremos saber si la educación afectará la tasa salarial. 
# Estableceremos el año de experiencia = 10 años.

wage5 <- wage_web %>% filter(exper == 10)
wage6 <- wage5 %>% arrange(school)
kable(head(wage6))

xyplot(wage6$school ~ wage6$wage, data = wage6,
       xlab = "Escala logarítmica de años de educación",
       ylab = "Escala de registro de salarios",
       main = "Educacion y salarios"
)

# variable sexo
class(wage_web$sex)# veo la clase
wage_web$sex <-factor(wage_web$sex, labels = c("Mujer", "Hombre"))# pongo etiquetas  alos niveles
levels(wage_web$sex)# verifico

head(wage_web)

#box plot para las variables experiencia, escolaridad y salario según sexo
boxplot(wage_web$exper~wage_web$sex, xlab="Sexo", ylab="Experiencia (años)", names=c("Mujer", "Hombre"), main="Experiencia según el sexo")


#
boxplot(wage_web$school~wage_web$sex, xlab="Sexo", ylab="Escolaridad (años)", names=c("Mujer", "Hombre"), main="Escolaridad según el sexo")

boxplot(wage_web$wage~wage_web$sex, xlab="Sexo", ylab="Escolaridad (años)", names=c("Mujer", "Hombre"), main="Salarios según el sexo")

# Los gráficos anteriores nos hacen pensar que tanto la experiencia como el salario son mayores 
# para los hombres, mientras que la escolaridad parece ser mayor en las mujeres. Sin embargo, esta
# idea solo es aplicable a los datos muestrales.
# R provee múltiples opciones para realizar análisis exploratorios desde el punto de vista gráfico. 
# Un buen ejemplo de aquello es el paquete PASWR2 a través de la función eda. Esta función se aplica a
# una variable (idealmente continua) y entrega un sumario estadístico y cuatro gráficos que nos ayudan
# a entender la distribución de los datos. Los gráficos construidos son un histograma, un gráfico de 
# densidad, un box plot (horizontal) y un gráfico Q-Q (cuantil-cualtil).
install.packages("PASWR2")
library(PASWR2)

eda(wage)
# Los dos primeros gráficos generados nos sirven para verificar que la distribución de los datos 
# se ajusta o no a una campana de Gauss, el box plot nos ayuda a identificar datos extremos y, 
# finalmente, el gráfico Q-Q compara los cuantiles de los datos con los cuantiles hipotéticos de una
# distribución normal. Entre mas alineados se encuentren los puntos del gráfico con la línea de color 
# rojo, mas opciones tienen los datos de seguir una distribución normal.
# Los resultados obtenidos para la variable de salario sugieren que ésta no sigue una distribución normal,
# lo cual será verificado con la prueba estadística de Shapiro-Wilk.


wage_web %>% group_by(sex) %>% summarise(mean(exper), mean(school), 
                                      mean(wage),median(exper), median(school), 
                                      median(wage), sd(exper), sd(school), sd(wage))
# Conjeturas: Los hombre poseen mas experiencia que las mujeres,
# Las mujeres tienen una mayor escolaridad que los hombres,
# Y los hombres tienen un salario (por hora) mayor que las mujeres.

# Para sacar conclusiones hay que plantear test de hipotesis, pero primero verificar normalidad

# La prueba de Shapiro-Wilk es un test paramétrico. La H0 prueba plantea que los datos provienen de una 
# distribucion normal mientras que  H1 indica lo contrario

lapply(split(wage_web$exper,wage_web$sex),shapiro.test)

lapply(split(wage_web$school,wage_web$sex),shapiro.test)

lapply(split(wage_web$wage,wage_web$sex),shapiro.test)

# conclusion: como todos los p-valores son menores a 0,05  se rechaza el supuesto de normalidad para todas las variables analizadas.
#  por ende se debe recurrir a técnicas no paramétricas.

# Las pruebas paramétricas para la media de una variable o para la diferencia de medias entre 
# dos variables o grupos dependen del supuesto de normalidad y de la igualdad (o desigualdad) 
# de las varianzas poblacionales.

# Si bien hemos verificado que el supuesto de normalidad no se satisface para ninguna de las variables 
# de nuestra base de datos de ejemplo, de igual manera procederemos a aplicar los test paramétricos
# con el objetivo de ilustrar su implementación e interpretación.

library(psych)
describe(wage_web) # muestra los descriptivos

# segun esto podemos tomar la hipótesis de que el salario medio es de 5.76 dólares por hora
t.test(wage_web$wage, mu=5.76)

# como valor p es mayor que los niveles de significancia usuales, concluimos que no existe evidencia para rechazar la hipótesis nula; 
# es decir, podemos suponer que el salario medio poblacional es de 5.76 dólares hora.

t.test(wage_web$wage, mu=7)

# si mu=7, concluimos que existe suficiente evidencia (valor p es menor que las significancias usuales)
# para rechazar la hipótesis nula, y por ende, el salario medio real difiere de 7 dólares por hora.



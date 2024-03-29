###############################################
### CERTIFICACI�N UNIVERSITARIA EN DATA SCIENCE
###   TEST DE HIPOTESIS
###############################################


## Myrian Aguilar, 22 de diciembre de 2021 ##


rm(list=ls())# Borrar los datos de entorno cargados en memoria 

setwd("C:/ARRAYANES/MUNDO_E") 
### Inferencias Basadas en una muestra.
# POBLACI�N NORMAL CON DESVIACION ESTANDAR CONOCIDA.--------------

# Ejemplo: Se desea contrastar con un nivel de significancia del 5% la hip�tesis 
# de que la estatura media de los hombres de 18 o m�s a�os de un pa�s es igual a 175, 
# contra la alternativa que es menor que 175. Suponiendo que la desviaci�n est�ndar es de 4.5. 
# Para dicha prueba se recolectaron los datos que constituyen una muestra de n=14 
# hombres seleccionados al azar, 
# cuyas alturas son: 167 167 168 168 168 169 171 172 173 175 175 175 177 182.

muestra <- c(167,167,168,168,168,169,171,172,173,175,175,175,177,182)
summary(muestra)
boxplot(muestra)
# Los Box plot (diagramas de caja y bigote) nos brindan una idea bastante precisa sobre c�mo 
# se distribuyen los datos. Un gr�fico box plot est� construido por, la l�nea 
# intermedia que es la mediana de los datos (percentil 50), los l�mites inferior y superior de la caja 
# corresponden al primer y tercer cuartil (percentiles 25 y 75) respectivamente. Por defecto, 
# los bigotes se extender�n hasta 1.5 veces el rango intercuart�lico (Q3-Q1) desde la parte superior 
# (inferior) de la caja hasta el dato m�s lejano dentro de esa distancia. Si hay alg�n dato mas all�
# de dicha distancia, ser�n representados mediante puntos individuales (outliers).

# debo verificar que la muestra es aproximadamente normal
# Luego debo definir la hip�tesis nula y alternativa: Ho: �= 175 contra Ha: � < 175.

install.packages("BSDA") 
library(BSDA)

# prueba de hip�tesis con la funci�n z.test 

z.test(x=muestra,y=NULL,alternative="less",mu=175,sigma.x=sd(muestra),sigma.y=NULL,conf.level=0.95)

# Conclusi�n: rechazo Ho, ya que existe suficiente evidencia de que la estatura media de 
# los hombres en ese pa�s es menor que 175, basado en un nivel de significancia del 5%, 
# que comparado con el valor p, (p<0,05) lo que nos indica que se trata de una caso poco 
# probable el hecho de que la estatura media sea igual a 175.

# MUESTRA GRANDE DESVIACION ESTANDAR DESCONOCIDA.-----------

# UNA POBLACI�N NORMAL CON DESVIACION ESTANDAR DESCONOCIDA Y MUESTRA PEQUE�A.------
# Una compa��a ferroviaria canadiense afirma que sus trenes de mercanc�as no bloquean 
# los pasos a nivel durante m�s de 8 minutos, en promedio. Una muestra aleatoria de 
# 10 tiempos de bloqueo dio como resultado estos valores (en minutos): 
# 10.1, 9.5, 6.5, 8.0, 8.8, 12, 7.2, 10.5, 8.2, 9.3. Bajo un nivel de significancia del 0.05.

# como la muestra es peque�a (n = 10), usaremos la distribuci�n t de Student para el c�lculo del p-valor. 
# Llamamos � al tiempo medio de bloqueo, planteamos nuestras hip�tesis: Ho: �= 8 contra Ha: � > 8

datos=c(10.1, 9.5, 6.5, 8.0, 8.8, 12, 7.2, 10.5, 8.2, 9.3)
mu0=8
t.test(datos,mu=mu0,alternative="greater",conf.level = 0.95)

# Conclusion: rechazo Ho, ya que existe suficiente evidencia de que el tiempo de bloqueo de 
# los trenes es mayor que 8 minutos, bajo un nivel de significancia del 5%, que comparado 
# con el valor p, (p<0,05),lo que nos indica que se trata de un caso poco probable el hecho 
# de que la media del tiempo de bloqueo de los trenes sea igual a 8 minutos.

### Inferencias Basadas en dos muestras.-----------
# POBLACIONES NORMALES CON VARIANZAS CONOCIDAS.--------

# Un due�o de una f�brica tiene un pedido muy grande y para llevarlo a cabo necesita 
# conocer cu�l de las dos m�quinas para realizar el proceso trabaja con mayor velocidad, 
# el intuye que la maquina 2 es m�s efectiva y para ello hace una prueba, toman el tiempo 
# de fabricaci�n en minutos de 10 productos en cada m�quina. Los resultados son:

# Fabrica 1: (15, 16, 15, 14, 14, 15, 13, 15, 15,14) 
# Fabrica 2: (16, 15, 14, 17, 12, 17, 15, 16, 15,14)

# �Es posible que la maquina 2 tenga un mejor tiempo de producci�n que la maquina 1 
# usando un nivel de significancia de 0.05?   Hip�tesis. Ho: ??1-??2=0 Ha: ??2>??1

# Primero verificaremos si ambas poblaciones son normales esto lo haremos con el 
# c�digo qqnorm y qqline o tambi�n podr�amos usar un diagrama de caja con la funci�n Boxplot, 
# luego procederemos calcular la media y varianza de cada poblaci�n.

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
# es m�s igual r�pida que la maquina 1 por lo que se puede hacer uso tanto de la maquina 2 como de 
# la maquina 1 para un tiempo de producci�n menor y cumplir con la entrega todo esto a un nivel de 
# significancia de 0.05


# POBLACIONES NORMALES CON VARIANZAS DESCONOCIDAS PERO IGUALES.-----

# Dos ciclistas compiten para saber qui�n de los dos es m�s r�pido y entrenan a diario de la misma forma,
# cada d�a recorren 15 kil�metros durante un mes. El dato promedio del tiempo para el ciclista 1 es de 122
# y para el ciclista 2 es 120. Varias personas afirman que el ciclista 2 ganara la carrera, a un nivel de significancia del 5% comprobar las especulaciones de las personas asimiento una varianza igual para ambos ciclistas.

# Ciclista1:(130,129,130,124,124,122,130,125,126,123,130,126,125,128,125,125,125,125,125,125,130,123,120,122,125,123,122,127,120,121) 
# Ciclista2:(128,130,125,125,127,123,130,125,124,123,130,125,125,129,125,125,125,124,125,125,130,122,121,121,125,125,122,128,121,125)

# Hip�tesis. Ho: ??1-??2=0 Ha: ??2>??1

ciclista_1<-c(130,129,130,124,124,122,130,125,126,123,130,126,125,128,125,125,125,125,125,125,130,123,120,122,125,123,122,127,120,121)
ciclista_2 <- c(128,130,125,125,127,123,130,125,124,123,130,125,125,129,125,125,125,124,125,125,130,122,121,121,125,125,122,128,121,125)

# con un boxplot debemos de comprobar si nuestras poblaciones son normales y si presentan las misma varianzas
# luego aplicamos prueba t.student

boxplot(ciclista_1,ciclista_2,main="Comparacion de recorrido",ylab="Kilometros",col="Orange")

t.test(ciclista_1,ciclista_2,alternative="greater",mu=0,var.equal=TRUE,conf.level=0.95)
# conclusion:(p>0,05). No rechazar Ho,la prueba t.test nos dice que la diferencia de 
# media entre los ciclistas es aproximadamente igual, por ende, el ciclista 2 y ciclista 1 
# tienen las mismas opciones de ganar la Carrera bas�ndonos en la estad�stica a un nivel de 
# significancia del 5%.

# POBLACIONES NORMALES CON VARIANZAS DESCONOCIDAS Y DIFERENTES.-------

# En un supermercado se eval�an los contenidos de cajas de cereal de dos compa��as, 
# el encargado de tal tarea asegura que los contenidos son los mismos, pero a la queja de los 
# clientes se decide realizar una nueva evaluaci�n tomando 30 cajas de cereal pesadas en gramos. 
# Los datos son:

# Marca1(600,605,604,603,597,604,602,600,600,600,600,599,598,597,597,597,600,600,600,602,600,598,597,605,605,605,600,604,603,603) 
# Marca2(605,602,600,597,598,604,601,604,600,598,600,600,598,605,597,597,604,600,602,602,597,600,597,600,605,597,600,604,600,600)
                                                                                                                                       
# Demostar si el contenido de ambas marcas es el mismo usando un nivel de confianza del 95%
# Hip�tesis. Ho: ??1-??2=0 Ha: ??2?????1

marca_1<- c(600,605,604,603,597,604,602,600,600,600,600,599,598,597,597,597,600,600,600,602,600,598,597,605,605,605,600,604,603,603) 
marca_2<- c(605,602,600,597,598,604,601,604,600,598,600,600,598,605,597,597,604,600,602,602,597,600,597,600,605,597,600,604,600,600)

# realizo un boxplot para comprobar que la poblaci�n sea normal y quela varianza sea diferente, 
# luego aplico un test para nuestra prueba de hip�tesis con varianzas desconocidas y diferentes.

boxplot(marca_1,marca_2,col="yellow",ylab="Gramos",main="Diferencia de pesos")

t.test(marca_1,marca_2,alternative="two.sided",mu=0,var.equal=FALSE,conf.level=0.95)

# conclusion: (p>0,05). No rechazamos Ho,la prueba nos dice que la diferencia de media 
# de ambas marcas es aproximadamente la misma y por ende contienen los mismos contenidos, 
# dando la raz�n al empleador que realizo la prueba en un inicio con un nivel de significancia del 5%.

# Prueba t para observaciones pareadas.-------

# Una empresa afirma que el control para videojuegos que el produce tiene la misma duraci�n 
# media que el control original. Se realiza una prueba para 5 marcas de bater�as de la duraci�n 
# del control en uso. Los datos son los siguientes:

datos<-matrix(c(4.1,3.7,3.8,4.1,4.5,3.9,3.6,3.8,4.2,4.0),
              nrow=5,byrow=T)
colnames(datos)<-c("Original","Generico")
datos

datos<-as.data.frame(datos) # trasnforma a dataframe
attach(datos) # La base de datos est� adjunta a la ruta de b�squeda R. Significa que R 
# busca en la base de datos al evaluar una variable, por lo que se puede acceder a los 
# objetos de la base de datos simplemente dando sus nombres.

var.test(Original,Generico)
shapiro.test(Original)
shapiro.test(Generico)

t.test(Original,Generico,"two.sided",paired=TRUE, conf.level=0.95)
boxplot (Original,Generico)
# Conclusi�n: Se tiene de la prueba que el valor-P es de 0.4615, lo cual p>0,05
# por lo tanto, se acepta la hip�tesis nula y se rechaza la hip�tesis alterna. 
# La afirmaci�n de la empresa es cierta, el control de videojuegos que fabrican 
# tiene una duraci�n de bater�a media igual a la del original.

# Prueba para la diferencia de proporciones de dos muestras.------------
# Se desea conocer si la proporci�n de consumidores de un restaurante que son hombres y 
# consumen m�s de $5.00, es igual a la proporci�n de mujeres que visita el restaurante y 
# consume m�s de $5.00.

# Un d�a se recogi� que de 120 hombres que se atendieron,78 consumi� m�s de $5.00. 
# Mientras que el mismo d�a se recogi� que de 69 mujeres que se atendieron el restaurante, 
# 28 consumi� m�s de $5.00. 
  
# H0: La proporci�n de hombres que consumen m�s de $5.00 en el restaurante es igual 
# a la proporci�n de mujeres que consumen m�s de $5.00 en el restaurante.

# H1: La proporci�n de hombres que consumen m�s de $5.00 en el restaurante es diferente 
# a la proporci�n de mujeres que consumen m�s de $5.00 en el restaurante.

x <- c(78, 28)
n <- c(120, 69)
prop.test(x, n, conf.level = 0.95, correct = FALSE) # test para proporciones

# Conclusi�n: el valor-P es de 0.001126, (p<0,05)al nivel de significancia de ?? = 0.05 
# (al nivel de confianza del 95%), por lo tanto, se rechaza la hip�tesis nula y 
# se acepta la hip�tesis alterna. 
# Es decir, la proporci�n de hombres que consumen m�s de $5.00 en el restaurante 
# es diferente a la proporci�n de mujeres que consumen m�s de $5.00 en el restaurante.


# dataset Wages---------
install.packages("ISLR")
library(ISLR)
data("Wage") # set de datos incorporado en el paquete.3000 observaciones y 11 variables
str(Wage)

# C�mo var�an los salarios a lo largo de los a�os con la edad, la educaci�n y la clase de trabajo
require(ggplot2)
require(data.table)

Wage <- as.data.table(Wage)
Wage$year <- factor(Wage$year)

subWage <- Wage

g <- ggplot(data = subWage, aes(y=wage, x=age, color=education))
g <- g + ggtitle("Salarios por edad") + xlab("A�os") + ylab("Salarios")
g <- g + facet_wrap(~jobclass)
g <- g + geom_point() + geom_smooth(se=FALSE)

print(g)


### leo el set de datos desde la web
wage_web<- read.csv("https://raw.githubusercontent.com/xkong100/IS607/master/Project2/Wages1.csv", stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
# tiene 3294 observaciones y 5 variables

# var.1 es la identificaci�n del individuo,
# exper que representa la experiencia en a�os,
# sex es la variable dummy de sexo (1=hombre, 0=mujer),
# school que representa la escolaridad en a�os y
# wage que corresponde al salario por hora (en d�lares).


#EDA
head(wage_web)

attach(wage_web) # Para poder usar libremente las variables contenidas en la base de datos 

# organicemos los datos para encontrar los salarios de diferentes g�neros 
# cuya experiencia es de 10 a�os y el a�o de educaci�n es 14
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

#  La kable() funci�n devuelve una sola tabla para un solo objeto de datos y 
# devuelve una tabla que contiene varias tablas si el objeto de entrada es una 
# lista de objetos de datos. 


# obtengo un resumen

wage4 %>% summarise (Mean_femalewage_10yrs = mean(female), max_femalewage_10yrs = max(female),
                     mean_malewage_10yrs= mean(male), max_malewage_10yrs=max(male))

# queremos saber si la educaci�n afectar� la tasa salarial. 
# Estableceremos el a�o de experiencia = 10 a�os.

wage5 <- wage_web %>% filter(exper == 10)
wage6 <- wage5 %>% arrange(school)
kable(head(wage6))

xyplot(wage6$school ~ wage6$wage, data = wage6,
       xlab = "Escala logar�tmica de a�os de educaci�n",
       ylab = "Escala de registro de salarios",
       main = "Educacion y salarios"
)

# variable sexo
class(wage_web$sex)# veo la clase
wage_web$sex <-factor(wage_web$sex, labels = c("Mujer", "Hombre"))# pongo etiquetas  alos niveles
levels(wage_web$sex)# verifico

head(wage_web)

#box plot para las variables experiencia, escolaridad y salario seg�n sexo
boxplot(wage_web$exper~wage_web$sex, xlab="Sexo", ylab="Experiencia (a�os)", names=c("Mujer", "Hombre"), main="Experiencia seg�n el sexo")


#
boxplot(wage_web$school~wage_web$sex, xlab="Sexo", ylab="Escolaridad (a�os)", names=c("Mujer", "Hombre"), main="Escolaridad seg�n el sexo")

boxplot(wage_web$wage~wage_web$sex, xlab="Sexo", ylab="Escolaridad (a�os)", names=c("Mujer", "Hombre"), main="Salarios seg�n el sexo")

# Los gr�ficos anteriores nos hacen pensar que tanto la experiencia como el salario son mayores 
# para los hombres, mientras que la escolaridad parece ser mayor en las mujeres. Sin embargo, esta
# idea solo es aplicable a los datos muestrales.
# R provee m�ltiples opciones para realizar an�lisis exploratorios desde el punto de vista gr�fico. 
# Un buen ejemplo de aquello es el paquete PASWR2 a trav�s de la funci�n eda. Esta funci�n se aplica a
# una variable (idealmente continua) y entrega un sumario estad�stico y cuatro gr�ficos que nos ayudan
# a entender la distribuci�n de los datos. Los gr�ficos construidos son un histograma, un gr�fico de 
# densidad, un box plot (horizontal) y un gr�fico Q-Q (cuantil-cualtil).
install.packages("PASWR2")
library(PASWR2)

eda(wage)
# Los dos primeros gr�ficos generados nos sirven para verificar que la distribuci�n de los datos 
# se ajusta o no a una campana de Gauss, el box plot nos ayuda a identificar datos extremos y, 
# finalmente, el gr�fico Q-Q compara los cuantiles de los datos con los cuantiles hipot�ticos de una
# distribuci�n normal. Entre mas alineados se encuentren los puntos del gr�fico con la l�nea de color 
# rojo, mas opciones tienen los datos de seguir una distribuci�n normal.
# Los resultados obtenidos para la variable de salario sugieren que �sta no sigue una distribuci�n normal,
# lo cual ser� verificado con la prueba estad�stica de Shapiro-Wilk.


wage_web %>% group_by(sex) %>% summarise(mean(exper), mean(school), 
                                      mean(wage),median(exper), median(school), 
                                      median(wage), sd(exper), sd(school), sd(wage))
# Conjeturas: Los hombre poseen mas experiencia que las mujeres,
# Las mujeres tienen una mayor escolaridad que los hombres,
# Y los hombres tienen un salario (por hora) mayor que las mujeres.

# Para sacar conclusiones hay que plantear test de hipotesis, pero primero verificar normalidad

# La prueba de Shapiro-Wilk es un test param�trico. La H0 prueba plantea que los datos provienen de una 
# distribucion normal mientras que  H1 indica lo contrario

lapply(split(wage_web$exper,wage_web$sex),shapiro.test)

lapply(split(wage_web$school,wage_web$sex),shapiro.test)

lapply(split(wage_web$wage,wage_web$sex),shapiro.test)

# conclusion: como todos los p-valores son menores a 0,05  se rechaza el supuesto de normalidad para todas las variables analizadas.
#  por ende se debe recurrir a t�cnicas no param�tricas.

# Las pruebas param�tricas para la media de una variable o para la diferencia de medias entre 
# dos variables o grupos dependen del supuesto de normalidad y de la igualdad (o desigualdad) 
# de las varianzas poblacionales.

# Si bien hemos verificado que el supuesto de normalidad no se satisface para ninguna de las variables 
# de nuestra base de datos de ejemplo, de igual manera procederemos a aplicar los test param�tricos
# con el objetivo de ilustrar su implementaci�n e interpretaci�n.

library(psych)
describe(wage_web) # muestra los descriptivos

# segun esto podemos tomar la hip�tesis de que el salario medio es de 5.76 d�lares por hora
t.test(wage_web$wage, mu=5.76)

# como valor p es mayor que los niveles de significancia usuales, concluimos que no existe evidencia para rechazar la hip�tesis nula; 
# es decir, podemos suponer que el salario medio poblacional es de 5.76 d�lares hora.

t.test(wage_web$wage, mu=7)

# si mu=7, concluimos que existe suficiente evidencia (valor p es menor que las significancias usuales)
# para rechazar la hip�tesis nula, y por ende, el salario medio real difiere de 7 d�lares por hora.



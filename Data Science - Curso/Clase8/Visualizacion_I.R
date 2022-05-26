
# ENCUENTRO 8: Visualizacion de Datos I

# Autora: Corina Sanucci
# Fecha: 14/08/2021 - ultima modificacion 1/4/2022

# Objetivos:
#         - Aprender a construir gaficos con R Base
#         - Aprender a graficar con ggplot2

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Para ver una lista de los datasets incluidos en R:
data()

# Cargamos un dataset de nuestra eleccion:
data('ChickWeight')
# Veamos como esta estructurado el dataframe
str(ChickWeight)


# Graficos Unidimensionales con R Base #

### HISTOGRAMAS ###

# No correr:
hist(x, # datos para realizar el histograma: debe ser un VECTOR
     breaks = "Sturges", # cortes para los bloques (barras) del histograma
     freq = NULL, # logical: T o F. Si T, el histograma representa frecuencias, si F, representa densidad
     col = NULL, # color para rellenar las barras
     border = NULL, # color para el borde de las barras
     main = paste("Histogram of" , xname), # Titulo
     xlim = range(breaks), # rango de valores para los ejes, sólo se usa para la gráfica
     ylim = NULL,
     xlab = xname, ylab, # etiquetas de ejes, ylab = Frequency si freq = T
     axes = TRUE, # logical, si F los ejes no se dibujan en el gráfico
     plot = TRUE, # logical, si F, no se genera el gráfico
     labels = FALSE, # logical, si T agrega etiquetas en cada barra. Admite vector de caracteres.
     ...)
#

# hist() acepta muchos parametros, vamos a utilizar algunos:

#Para poder acceder a las variables directamente (sin llamar al data.frame antes) usamos el comando attach():
attach(ChickWeight)

hist(weight) # histograma con parametros graficos por default

hist(weight,
     freq = F) # histograma con la distribucion de probabilidad

# �Como guardar las graficas?:
# png(filename = 'direccion a la carpeta', width = ancho, height = alto)
png(filename = "D:/DOCENCIA/Data_Science/Histograma1.png", width = 800, height = 600)
# ejecuto el grafico a guardar:
hist(weight,
     breaks = 20,
     main = 'con breaks = 20')
# cierro y se guarda la imagen:
dev.off()

# Cortes irregulares para cada barra del histograma:
hist(weight,
     breaks = c(0, 15, 30, 45, 60, 120, 180, 240, 300, 360, 400))

# Modificando varios parametros:
hist(weight,
     breaks = 20,
     main = 'Peso de los pollos en distintas dietas',
     xlim = c(30, 400), # limites del rango de x
     ylim = c(0, 150), # limites del rango de y
     xlab = 'Peso',
     ylab = 'Frecuencia de pollos',
     axes = F, # no se dibujan los ejes
     col = 'grey50',
     border = NULL,
     labels = T) # etiquetas con la frecuencia de cada barra


### BOXPLOT ###

# no correr:
boxplot(x, # mis datos, vector numerico. Alternativa: formula + dataset
        range = 1.5, # el rango que abarcan los extremos de cada box ("bigotes")
        width = NULL, # ancho relativo de cada box, debe ser un vector (OJO)
        varwidth = FALSE, # si T, el ancho de cada box es proporcional a la cantidad de datos
        notch = FALSE, # si T, dibuja una muesca o corte en cada box
        outline = TRUE, # si F, no se dibujan los outliers
        names, # grupo de etiquetas para cada box
        border = par("fg"), # color para los bordes de cada box
        col = NULL, # color de relleno para cada box
        horizontal = FALSE, # Si T, los boxplot se grafican horizontalmente
        ...)
#

# por default:
boxplot(weight)

# agrupamos la variable de interes (weight) segun categorias (Diet):
boxplot(formula = weight ~ Diet, data = ChickWeight)

# Guardamos un ejemplo:
# abrimos un archivo PNG:
png('D:/DOCENCIA/Data_Science/boxplot.png')
# ejecutamos el grafico a guardar:
boxplot(formula = weight ~ Diet, data = ChickWeight,
        range = 0,
        width = c(1, 2, 1, 3)) # anchos de boxplot determinados manualmente: OJO, que implica hacer esto?
# cerramos grafico:
dev.off()


boxplot(formula = weight ~ Diet, data = ChickWeight,
        varwidth = T, # ancho de cajas proporcional al numero de datos
        notch = T) # Si T, se dibuja la muesca que me permite ver si la diferencia entre medianas es significativa
                    # Es significativa si las muescas entre boxplot no se solapan

boxplot(formula = weight ~ Diet, data = ChickWeight,
        varwidth = T,
        notch = T,
        outline = F, # no se dibujan los outliers
        names = c('dieta 1', 'dieta2', 'dieta 3', 'dieta 4'), # nombres de categorias
        border = c('red', 'orange', 'green', 'blue'), # colores de borde
        horizontal = T) # boxplot horizontales



# # # BARPLOT # # #

# no correr:
barplot(height, # vector o matriz con los datos
        width = 1, # ancho de las barras
        space = NULL, # espacio entre barras
        names.arg = NULL, # nombres para las categorias de las barras
        beside = FALSE, # Si F, se agrupan las columnas en height
        horiz = FALSE, # Si F, las barras son verticales, si T: horizontales
        col = NULL, 
        main = NULL, 
        sub = NULL, 
        xlab = NULL, 
        ylab = NULL,
        axes = TRUE, 
        ...)
#


barplot(c(5,14,75,34,5), # vector con datos inventados
        space = 1, # espacio entre barras
        names.arg = c('martes', 'miércoles', 'jueves', 'viernes', 'sábado'), # nombres de categorias/barras
        col = c('grey50', 'grey50', 'orangered', 'grey50', 'grey50'),
        border = NA, # barras sin borde
        main = 'Los platenses no aguantan al fin de semana', # titulo
        ylab = 'cantidad de cervezas en Antares')

# Ejemplo con matriz y barras aagrupadas por categorias

# genero matriz con datos inventados
m <- matrix(c(3,6,12,23,13,8,9,15,45,25), nrow = 2)

barplot(m, # cada columna es una categoria, las filas son las dos barras por categoria
        names.arg = c('enero', 'marzo', 'mayo', 'julio', 'septiembre'), # categorias
        beside = T, # barras adyascentes en lugar de apiladas
        col = c('blue', 'lightblue', 'pink'), # colores que se reciclan
        border = F)


# # # Funcion generica plot() # # #

# no correr:
plot(x, # variable independiente: eje horizontal
     y, # variable dependiente: eje vertical
     type = 'p', # tipo de grafico: p = puntos, l = lineas, b = ambos (hay mas opciones)
     main = NULL, # titulo
     sub = NULL, # subtitulo
     xlab, ylab = xname, yname, # etiquetas de ejes
     col = NULL, # color
     pch = 1, # forma
     cex = 1 # tama�o
)
#

data("ToothGrowth")

# grafico del data.frame completo:
plot(ToothGrowth)

# grafico de cada variable por separado
plot(ToothGrowth$len) # variable continua: no me sirve un grafico de dispersion para esto
plot(ToothGrowth$supp) # variable discreta --> tipo de dato: factor
plot(ToothGrowth$dose) # es una variable discreta, pero R no la reconoce porque es tipo de dato numerico

# transofmramos a factor para que R reconozca que es discreto
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
plot(ToothGrowth$dose) # ahora si, devuelve un grafico de barras

plot(ToothGrowth$len, ToothGrowth$dose) # plot(x, y) plot(continua, discreta): dispersion
plot(ToothGrowth$dose, ToothGrowth$len) # plot(discreta, continua): boxplot

plot(ToothGrowth$len, ToothGrowth$len) # plot(continua, continua) / ES LA MISMA VARIABLE, por eso forma linea recta a 45 grados
plot(ToothGrowth$dose, ToothGrowth$supp) # plot(discreta, discreta): grafico de mosaico



# # # GRAFICOS CON GGPLOT2 # # #
library(ggplot2)


data("iris")
str(iris)
levels(iris$Species) # niveles (categorias) del tipo de dato factor (variable cualitativa nominal)

a <- ggplot(iris) # creo el sistema de coordenadas o canvas

# Geometrias de ggplot2

# Graficos unidimensionales

# Variable CONTINUA

ggplot(iris) + geom_histogram(aes(Petal.Length)) # aes(x, y, group)
# Me tira un WARNING: `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
# Me esta diciendo que para generar los cortes de las barras en el histograma usa por default bins = 30 (numero de cortes aprox)
# Me recomienda que use 'binwidth' (ancho de cada barra) para elegir cortes que me convengan mas:
ggplot(iris, aes(Petal.Length)) + geom_histogram(binwidth = 0.5)


# grafico de densidad de probabilidad:
a + geom_density()


# Variable DISCRETA

b <- ggplot(iris, aes(Species))

b + geom_bar() # por default geom_bar me muestra el numero de observaciones (count) por categoria (Species)


# Graficos Bidimensionales

# CONTINUA vs CONTINUA

a <- ggplot(iris, aes(Petal.Length, Petal.Width))

# Grafico de dispersion
a + geom_point()

# Grafico de densidad 2D
a + geom_density2d_filled() # Recuerden: montania vista desde arriba. Hay dos picos, dos valores mas probables

# CONTINUA vs DISCRETA

b <- ggplot(iris, aes(Species, Petal.Length))

b + geom_bar(stat = 'identity') # con stat = 'identity' le pido a geom_bar() que use los valores de la variable que asigno a y
                                # para la altura de las barras 

# Si nos fijamos, los valores del eje y son raros
# no parecen los largos de los petalos reales
# Nos falta indicarle que estad�stico queremos ver en y
# quiza el maximo largo de petalo, o la media, o el minimo

# para ello primero debemos calcular ese estadistico
# Vamos con la media de largo de petalo
# no queremos una unica media para todas las especies,
# sino una media para cada una de ellas

# para ello podemos usar la libreria dplyr de tidyverse
library(dplyr)

# Agrupamos los datos por especies
PLength_bySpecies <- group_by(iris, Species)

# calculamos la media de largo de petalo para los datos agrupados
mean_PLength <- dplyr::summarise(PLength_bySpecies, 
                                 mean = mean(Petal.Length,
                                             na.rm = T))
# na.rm = T le dice que no utilize los NA (no Data)
# para el calculo de la media

# el resultado es un nuevo dataframe con solo 3 observaciones
mean_PLength  # nombre de especie y media de largo de petalos

# ahora si graficamos con stats = 'identity'
# usamos este nuevo dataframe y el nombre de sus variables
ggplot(mean_PLength) + geom_bar(aes(Species, mean),
                                stat = 'identity')

# podemos hacer lo mismo con otros estadisticos
max_PLength <- dplyr::summarise(PLength_bySpecies, 
                                 max = max(Petal.Length,
                                             na.rm = T))

ggplot(max_PLength) + geom_bar(aes(Species, max),
                               stat = 'identity')

# Boxplot de datos (Petal.Lenght) agrupados por categorias (Species):
b + geom_boxplot(aes(group = Species)) #(y = Petal.Lenght, lo defini cuando cree el objeto b)

# Grafico de violin:
# son como los boxplot + la densidad de probabilidad vista verticalmente
b + geom_violin() # la especie setosa parece acercarse mas a una distribucion normal


# ESCALAS y AGRUPAMIENTOS

a <- ggplot(iris, aes(Petal.Length, Petal.Width)) # voy a graficar Ancho de petalo vs Largo de petalo

a + geom_point(aes(color = Species)) # le pido que la variable Species me la represente con color


a + geom_point(aes(color = Species)) + 
  scale_color_manual(values = c('red', 'yellow', 'green')) # modifico los colores manualmente

a + geom_point(aes(shape = Species)) + # ahora le pido que la variable Species la represente con formas (shape)
  scale_shape_manual(values = c(0, 19, 8)) # elijo las formas manualmente con sus codigos numericos
              # busquen en google "shapes R" y van a ver las formas disponibles y el codigo numerico para usarlas


library(dplyr)
library(GGally)
library(lmtest)
library(car)

###########################################
###              Clase 1                ### 
###########################################

# Usamos el dataset mtcars ya incluido en R

datos <- mtcars

# Nombre de las columnas
names(datos)
# Eliminamos variables que no son útiles para el ejercicio (que no son contínuas)

datos <- select(datos, -c(am, vs, cyl, gear, carb))

# Nos quedan 6 variables: 1) millas por galón (consumo), 2) desplazamiento (
# una medida de potencia del motor), 3) hp (potenica), 4) wt (peso)
# 5) qsec (aceleración)

# Comenzamos analizando la correlación entre estas varibales continuas

ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

# En esta clase vamos a realizar un modelo de regresión lineal simple, con
# una sola varibale explicativa.

# El primer lugar, tenemos que definir la variable a explicar
#           Variable a explicar (objetivo) = mpg (Millas por galón de combustible)
# En segundo lugar, tenemos que elegir la variable explicativa
#           Variable explicativa = Wt (el peso)
# Elegimos esa varibale porque posee el coeficiente de correlación más alto


                
cor(datos$mpg,datos$wt)


# -0.867. Es una relación inversamente proporcional. Vamos a chequear si
# es estadísticamente significativa

cor.test(datos$mpg,datos$wt)

# Observamos un p value muy bajo, entonces podemos sostenar que el coeficiente 
# de correlación r es distinto de cero
# Analicemos si estas variables presentan distribución normal (uno de los su-
# puestos necesarios para el calculo de r)

shapiro.test(datos$mpg)
shapiro.test(datos$wt)

# En ningún caso podemos rechazar la hiótesis nula, por lo tanto, cumplimos con
# los supuestos

# Estamos en condiciones de realizar un modelo lineal, y analizar los resul-
# tados


modelolineal = lm(mpg~wt, data =datos)
summary(modelolineal)

# Validación del modelo
  # 1) Ambos coeficientes son significativos
  # 2) el modelo es globalmente significativo
  # 3) R2 es del 75%. El peso explica el 75% de la variabilidad de mpg

# Análisis de los residuos
  # Tenemos que ver si el modelo es válido, por lo tanto, necesitamos chequear
  # que el los residuos cumplan:
      # 1) que se distribuyan linealmente  y media cero:

qqnorm(modelolineal$residuals)
qqline(modelolineal$residuals)
plot(modelolineal$residuals, modelolineal$fitted.values, main="Res vs Pred",
     xlab="Residuos ", ylab="Valor predicho ", pch=19)
      # Los residuos se aproximan a la linealidad esperada
mean(modelolineal$residuals)
      # La media es muy cercana a 0

      # 2) Dist NormaNo podemos rechazar la hipótesis nula, 
      # hay evidencia de dist normal
shapiro.test(modelolineal$residuals)

      # 3) que la varianza sea constante (homocedasticidad)
bptest(modelolineal)
      # el test de Bausch Pagan nos dicen que no tenemos evidencia para
      # para sostener que la varianza no es homogenea

      # 4) Autocorrelación
dwt(modelolineal, alternative = "two.sided")
      # rechazamos H0, no hay autocorrelación entre los residuos.

# Una vez que sabemos que nuestro modelo y significativo y válido,
# Hagamos una predicción para cerrar

# ¿ Cuál es el consumo por combustible de autos nuevo que pesam 2.5 y 2.9 tn?


nuevos = data.frame ( wt = c(2.5, 2.9)) 
predict(modelolineal, nuevos)






###########################################
###              Clase 2                ### 
###########################################


library(mlbench)
library(MASS)

# Repasamos de la clase pasada, armamos el modelo con una variables y analizamos
# la salida


modelolineal = lm(mpg~wt, data =datos)
summary(modelolineal)

# Nos enfocamos en R2

summary(modelolineal)$r.squared
summary(modelolineal)$adj.r.squared

# Comenzamos con la regresión multiple
# Entrenamosun modelo que incluya a todas la variables:


modelolineal2 = lm(mpg~wt+disp+hp+drat+wt+qsec, data =datos)
summary(modelolineal2)

# Como vemos en esto modelo, la única variables significativa es el peso, por lo
# tanto, el modelo original es el mejor que tenemos

#Cambiamos de dataset

BostonHousing <- read.csv(file = "https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv")
dim(BostonHousing)
head(BostonHousing)

# Correlmos modelo para predecir la mediana del valor del metro cuad,
# con la tasa de crimen como variables explicativa

modelocasas = lm(medv~crim,data=BostonHousing)
summary(modelocasas)

# Corremos modelos con todas las variables


modelocasas2 = lm(medv~.,data=BostonHousing)
summary(modelocasas2)

# Como podemos saber cual modelo es mejor?
# Vamos a calcular R2ajust, AIC y BIC para los dos modelos y analizar

AIC(modelocasas)
AIC(modelocasas2)

BIC(modelocasas)
BIC(modelocasas2)

summary(modelocasas)$adj.r.squared
summary(modelocasas2)$adj.r.squared

# Utilizamos stepwise

modelo.pasos <- stepAIC(modelocasas2, direction = "both", 
                      trace = TRUE)
summary(modelo.pasos)

# Para practicar, analicemos el dataset swiss:

summary(swiss)

datos <- select(datos, -c(am, vs, cyl, gear, carb))
str(datos)
# Nos quedan 6 variables: 1) millas por galón (consumo), 2) desplazamiento (
# una medida de potencia del motor), 3) hp (potenica), 4) wt (peso)
# 5) qsec (aceleración)

### REGRESION LINEAL SIMPLE ###

s <- swiss
str(s)
ggpairs(s, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")
plot(s$Fertility,s$Education)
# Analizar correlación entre variables
shapiro.test(s$Fertility)
shapiro.test(s$Education)
cor(s$Fertility,s$Education)
cor.test(s$Fertility,s$Education)
# Realizar modelo con la variable con mayor correlación
modelolineal = lm(Fertility~Education, data = s)# hp explica 50% del qsec
summary(modelolineal)

### REGRESION LINEAL MULTIPLE ###
# Realizar modelo con todas las variables
modelolineal2 = lm(Fertility~Education+Catholic+Agriculture+Examination+Infant.Mortality, data = s)
summary(modelolineal2)

# Comparar modelos utilizando BIC
AIC(modelolineal)
AIC(modelolineal2)

BIC(modelolineal)
BIC(modelolineal2)

summary(modelolineal)$adj.r.squared
summary(modelolineal2)$adj.r.squared

# Aplicar stepwise
modelofinal <- stepAIC(modelolineal2, direction = "both", trace = TRUE)
summary(modelofinal)

############
modelolineal3 = lm(Fertility~Education+Catholic+Infant.Mortality, data = s)
summary(modelolineal3)



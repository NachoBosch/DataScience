library(dplyr)
library(GGally)
library(lmtest)
library(car)

###########################################
###              Clase 1                ### 
###########################################

# Usamos el dataset mtcars ya incluido en R
datos <- mtcars
str(datos)

# Nombre de las columnas
names(datos)

# Eliminamos variables que no son útiles para el ejercicio (que no son contínuas)
datos <- select(datos, -c(am, vs, cyl, gear, carb))
str(datos)
# Nos quedan 6 variables: 1) millas por galón (consumo), 2) desplazamiento (
# una medida de potencia del motor), 3) hp (potenica), 4) wt (peso)
# 5) qsec (aceleración)

# Comenzamos analizando la correlación entre estas varibales continuas

ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")


plot(mtcars$mpg,mtcars$wt)
plot(mtcars$qsec,mtcars$hp)
plot(mtcars$mpg,mtcars$hp)
plot(mtcars$hp,mtcars$qsec)


# En esta clase vamos a realizar un modelo de regresión lineal simple, con
# una sola varibale explicativa.

# El primer lugar, tenemos que definir la variable a explicar
#           Variable a explicar (objetivo) = mpg (Millas por galón de combustible)
# En segundo lugar, tenemos que elegir la variable explicativa
#           Variable explicativa = Wt (el peso)
# Elegimos esa varibale porque posee el coeficiente de correlación más alto


                
cor(datos$mpg,datos$wt)
cor(mtcars$qsec,mtcars$hp)
cor(mtcars$mpg,mtcars$hp)
cor(mtcars$hp,mtcars$qsec)

# -0.867. Es una relación inversamente proporcional. Vamos a chequear si
# es estadísticamente significativa

cor.test(datos$mpg,datos$wt)
cor.test(mtcars$qsec,mtcars$hp)
cor.test(mtcars$mpg,mtcars$hp)
cor.test(mtcars$hp,mtcars$qsec,method='pearson')
#Correlacion = 0--> H0=0 p<0.05 --> H1 si hay correlacion
# -1 (cor inversa) 0(no hay) +1(cor directa)
#asumiendo que ambos datos tienen dist normal
cor.test(mtcars$mpg,mtcars$hp,method = 'spearman')
cor.test(mtcars$mpg,mtcars$hp,method = 'kendall')
# Observamos un p value muy bajo, entonces podemos sostenar que el coeficiente 
# de correlación r es distinto de cero
# Analicemos si estas variables presentan distribución normal (uno de los su-
# puestos necesarios para el calculo de r)

shapiro.test(datos$mpg)
shapiro.test(datos$wt)

shapiro.test(mtcars$qsec)
shapiro.test(mtcars$hp)


shapiro.test(mtcars$mpg)
shapiro.test(mtcars$hp)

shapiro.test(mtcars$hp)#p<0.05 no es normal
shapiro.test(mtcars$qsec)# p>0.05 si es normal

# En ningún caso podemos rechazar la hiótesis nula, por lo tanto, cumplimos con
# los supuestos

# Estamos en condiciones de realizar un modelo lineal, y analizar los resul-
# tados


modelolineal = lm(hp~qsec, data =datos)# hp explica 50% del qsec
summary(modelolineal)


modelolineal = lm(hp~qsec, data =datos)
summary(modelolineal)

modelolineal = lm(hp~mpg, data =datos)
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
dwt(modelolineal, alternative = "two.sided")#Durbin Watson
      # rechazamos H0, no hay autocorrelación entre los residuos.
      # esto significa que no hay valor de residuo que condiciones a los residuos cercanos.

# Una vez que sabemos que nuestro modelo y significativo y válido,
# Hagamos una predicción para cerrar

# ¿ Cuál es el consumo por combustible de autos nuevo que pesam 2.5 y 2.9 tn?


nuevos = data.frame ( wt = c(2.5, 2.9)) 
predict(modelolineal, nuevos)

plot(mtcars$mpg,mtcars$wt)
grafico1 = ggplot(mtcars,aes(mpg,wt))
grafico1 + geom_point()+geom_smooth(method="lm",colour="red")





###########################################
###              Clase 2                ### 
###########################################

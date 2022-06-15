library(mlbench)
library(MASS)
library(dplyr)
library(GGally)
library(lmtest)
library(car)

########################################
# REGRESION LINEAL (SIMPLE AND MULTIPLE)

cars <- read.csv("https://raw.githubusercontent.com/amankharwal/Website-data/master/CarPrice.csv")
head(cars)

c <- select(cars,c(wheelbase,carlength,carwidth,carheight,curbweight,enginesize,boreratio,stroke,compressionratio,horsepower,peakrpm,citympg,highwaympg,price))
str(c)
sum(is.na(c))

ggpairs(c, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

# Price = fn(HorsePower)
plot(c$horsepower,c$price)
cor(c$horsepower,c$price)

shapiro.test(c$horsepower)
shapiro.test(c$price)

a <- ggplot(c)
a+geom_histogram(aes(price))
a+geom_histogram(aes(horsepower))

cor.test(c$horsepower,c$price,method="pearson")
cor.test(c$horsepower,c$price,method="spearman")#en realidad al no cumplirse normalidad
#se debería usar spearman

## MODEL REGRESION LINEAL SIMPLE ##
modelolineal = lm(price~horsepower, data = c)
summary(modelolineal)
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
AIC(modelolineal)
BIC(modelolineal)
summary(modelolineal)$adj.r.squared

nuevos = data.frame(horsepower = c(205)) 
predict(modelolineal, nuevos)
grafico1 = ggplot(c,aes(price,horsepower))
grafico1 + geom_point()+geom_smooth(method="lm",colour="red")


#####################################
## MODEL REGRESION LINEAL MULTIPLE ##
#####################################
modelotutti = lm(price~wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+highwaympg, data = c)
summary(modelotutti)

#Extraemos variables de más
modelolineal = lm(price~carwidth+enginesize+stroke+peakrpm, data = c)
summary(modelolineal)


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
grafico2 = ggplot(c,aes(price,carwidth,enginesize,stroke,peakrpm))
grafico2 + geom_point()+geom_smooth(method="lm",colour="red")

AIC(modelolineal)
BIC(modelolineal)
summary(modelolineal)$adj.r.squared

modelo.pasos <- stepAIC(modelotutti, direction = "both",trace = TRUE)
summary(modelo.pasos)

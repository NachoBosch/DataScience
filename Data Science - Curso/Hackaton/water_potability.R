library(mlbench)
library(MASS)
library(dplyr)
library(GGally)
library(lmtest)

##https://bookdown.org/dietrichson/metodos-cuantitativos/test-de-normalidad.html

###################### 
###### Hackaton ######
#####################

#Setear path hacia 'C:\Code\DataScience\Data Science - Curso\Hackaton'
setwd('C:/Code/DataScience/Data Science - Curso/Hackaton')
#Cargamos dataset
df <- read.csv('water_potability.csv')
#Visualizamos los 3 primeros filas
head(df,3)

#Rellenamos los nan
sum(is.na(df))
is.na(df)
str(df)
names(df)

#Limpiamos nan de las columnas ph, Sulfate, Trihalomethanes
ph_mean <- mean(df$ph,na.rm = TRUE)
df$ph[is.na(df$ph)] <- ph_mean
sum(is.na(df$ph))

sum(is.na(df$Sulfate))
sulfate_mean <- mean(df$Sulfate, na.rm=TRUE)
df$Sulfate[is.na(df$Sulfate)] <- sulfate_mean
sum(is.na(df$Sulfate))

sum(is.na(df$Trihalomethanes))
trihalo_mean <- mean(df$Trihalomethanes,na.rm = TRUE)
df$Trihalomethanes[is.na(df$Trihalomethanes)] <- trihalo_mean
sum(is.na(df$Trihalomethanes))

#Analizamos correlacion entre las variables numericas dejando de lado la potability
data_num <- select(df, -c(Potability))
data_num

ggpairs(data_num, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

#Analizamos correlacion con todas las variables 
ggpairs(df, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

#Analizamos normalidad de cada variable
shapiro.test(df$ph) #Se rechaza la Ho por lo que la muestra no se distribuyen de modo normal
shapiro.test(df$Hardness) #Se rechaza la Ho por lo que la muestra no se distribuyen de modo normal
shapiro.test(df$Solids) #Se rechaza la Ho por lo que la muestra no se distribuyen de modo normal
shapiro.test(df$Chloramines) #Se rechaza la Ho por lo que la muestra no se distribuyen de modo normal
shapiro.test(df$Sulfate) #Se rechaza la Ho por lo que la muestra no se distribuyen de modo normal
shapiro.test(df$Conductivity) #Se rechaza la Ho por lo que la muestra no se distribuyen de modo normal
shapiro.test(df$Organic_carbon) #NO se rechaza la Ho por lo que la muestra sigue una distribución normal
shapiro.test(df$Trihalomethanes) #Se rechaza la Ho por lo que la muestra no se distribuyen de modo normal
shapiro.test(df$Turbidity) #No se rechaza la Ho por lo que la muestra sigue una distribución normal
shapiro.test(df$Potability) #Se rechaza la Ho por lo que la muestra no se distribuyen de modo normal

#Test de correlacion
cor.test(df$Potability,df$ph,method='kendall')
cor.test(df$Potability,df$Hardness,method='kendall')
cor.test(df$Potability,df$Solids,method='kendall') #Se rechaza la H0=0 correlación y se acepta la H1= si hay corr aunque muy debil +
cor.test(df$Potability,df$Chloramines,method='kendall') #Se rechaza la H0 y se acepta la H1 debil +
cor.test(df$Potability,df$Sulfate,method='kendall') #Se rechaza la H0 y se acepta la H1 debil -
cor.test(df$Potability,df$Conductivity,method='kendall') #Se rechaza la H0 y se acepta la H1 debil +
cor.test(df$Potability,df$Organic_carbon,method='kendall') #Se rechaza la H0 y se acepta la H1 debil -
cor.test(df$Potability,df$Trihalomethanes,method='kendall') #Se rechaza la Ho y se acepta la H1 debil +
cor.test(df$Potability,df$Turbidity,method='kendall') #Se rechaza la Ho y se acepta la H1 debil +

#Plots
plot(df$Potability,df$Solids)
plot(df$Potability,df$Chloramines)
plot(df$Potability,df$Sulfate)
plot(df$Potability,df$Conductivity)
plot(df$Potability,df$Organic_carbon)
plot(df$Potability,df$Trihalomethanes)
plot(df$Potability,df$Turbidity)


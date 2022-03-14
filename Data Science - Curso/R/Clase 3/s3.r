library(datasets)
help(airquality)
attach(airquality)

# 
#Para mostrar varios datos
c(Ozone[1:5],Ozone[100])
# 
# 
# #Para buscar entre los datos aquellos que cumplen una condicion
# which(Month==5)
# Ozone[which(Month==5)]
# 
# 
# #Para contar la cantidad usamos length
# length(which(Month==5))#table(Month)

x <- c(0.32,0.36,0.24,0.11,0.11,0.44,2.79,2.99,3.47,0.23,0.55,3.21,4.02,0.23)
sort(x)
Fn = ecdf(x)
Fn(1)
median(x)
quantile(x,probs=0.27)
quantile(x,probs=0.8)

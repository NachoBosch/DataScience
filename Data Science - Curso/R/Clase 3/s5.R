familia <- list(padres=c("Estela","Enrique"),
                hijos = c("Federico","Mercedes","Ignacio"),
                edades = c(45,40,29))
names(familia)
familia[1]

#Dataframe----
id<-1:4
edad<-c(23,43,12,65)
sexo<-c("M","F","F","M")
trabaja<-c(T,T,F,F)
datos<-data.frame(id,edad,sexo,trabaja)
datos
View(datos)
class(datos)
str(datos)#str means structure
datos$sexo# signo $ para ver variables unicas en el dataframe
datos$id

estudia <- c(T,F,T,F)
datos_estudia <- cbind(datos,estudia)#cbind es para pegar una columna, rbind es para unir dos dataframes
View(datos_estudia)
head(datos_estudia)#head es para ver los primeros 6 datos del dataframe


#Operadores Artiméticos y Relacionales----
5==10/2
8>1
8<1
4%%2==0
1/3
1%/%3

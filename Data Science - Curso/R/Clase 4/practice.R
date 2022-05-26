#Buscamos datasets para practicar --> ?data()
#Cargamos el dataset
titanic <- read.csv("./titanic/train.csv")
str(titanic)
names(titanic)
length(titanic)
head(titanic,n=1L)
dim(titanic)
length(names(titanic))
is.na(titanic)
sum(is.na(titanic))
which(is.na(titanic$Age)==TRUE)
age_mean <- mean(titanic$Age,na.rm = TRUE)
age_mean

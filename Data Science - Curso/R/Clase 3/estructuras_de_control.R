## Myrian Aguilar, Febrero 2022 ##

###########################
### Estructuras de control 
###########################

rm(list=ls())
# asignando directorio----
setwd("C:/ARRAYANES/MUNDO_E/CLASE 3")

# Las estructuras de control nos permiten controlar el flujo de ejecución de una 
# secuencia de comandos.Así podremos aplicar, por ejemplo, una determinada acción 
# a los distintos elementos de una tabla o un vector.

# Las estructuras condicionales permiten hacer una bifurcación en la secuencia de 
# ejecución de un script.

# La estructura if-------
# Si la condición se cumple, es decir, es verdadera (TRUE), entonces se realizan las operaciones. 
# En caso contrario, no ocurre nada y el código con las operaciones no es ejecutado.

# Ejemplo 1, le pedimos a R que nos muestre el texto “Verdadero” si la condición se cumple.
# Se cumple la condición y se muestra "verdadero"

if(4 > 3) {
  "Verdadero"
}

#  si la condición no se cumple, no pasa nada
if(4 > 5) {
  "Verdadero"
}

# else complementa a if

# La estructura if ... else------------------  
# nos permite realizar diferentes acciones dependiendo  de si se cumple o no una determinada condición.
# Un if con else es la manera de decirle a R:
# SI esta condición es es cierta, ENTONCES haz estas operaciones, DE OTRO MODO haz estas otras operaciones.

# Ejemplo 2: Si se cumple la condición, se muestra "Verdadero"
if(4 > 3) {
  print("Verdadero")
} else {
  print("Falso")
}

if(4 > 5) {
  "Verdadero"
} else {
  "Falso"
}

# Ejemplo 3: Determinación del signo de un número: positivo, negativo o nulo.

x <- 5 # al objeto x le asigno el valor 5
if(x>0) {
  "el número, es positivo"
} else if (x<0) {
  "el número, es negativo"
} else {
  "el número, es nulo"
}

x<- -10  # al objeto x le asigno el valor -10
if(x>0) {
  "el número, es positivo"
} else if (x<0) {
  "el número, es negativo"
} else {
  "el número, es nulo"
}


# si queremos saber si un determinado número es par o impar. 

# La expresión %% nos devuelve el resto de una división. 
# Si el resto de dividir un número por 2 es igual a 0, eso nos quiere 
# decir que es par, por lo que lo vamos a tomar como condición en nuestra estructura 
# de control.

numero = 10
if (numero %% 2 == 0){
  print("El numero es par") # me devuelve el mensaje
}else{
  print("El numero es impar") # me devuelve el mensaje
}

numero = 121
if (numero %% 2 == 0){
  print("El numero es par") # me devuelve el mensaje
}else{
  print("El numero es impar") # me devuelve el mensaje
}


resultado <- function(num){
  if(num%%2==0){
    print(paste("El numero ",num," es par"))
  }else{
    print(paste("El numero ",num," es impar"))
  }
}
resultado(10)


# otra forma d devolvernos el resultado

numero = 10
if (numero %% 2 == 0){
  print(paste("El numero", numero, "es par"))
}else{
  print(paste("El numero", numero, "es impar"))
}

numero = 121
if (numero %% 2 == 0){
  print(paste("El numero", numero, "es par"))
}else{
  print(paste("El numero", numero, "es impar"))
}

# Ejemplo 4 : 
# definiremos una función que calcule el promedio de calificaciones de un estudiante y, 
# dependiendo de la calificación calculada, nos devuelva un mensaje específico ( aprobado o desaprobado)

promedio <- function(x){mean(x)}
promedio(c(6,7,8,9,8))




promedio <- function(calificaciones) {
  mean(calificaciones) # la funcion mean() pertenece al R base
}

promedio(c(6, 7, 8, 9, 8))

promedio(c(5, 8, 5, 6, 5))


# podemos decir que:SI el promedio de un estudiante es igual o mayor a 6, 
# ENTONCES mostrar “Aprobado”, DE OTRO MODO, mostrar “Reprobado”.

promedio <- function(calificaciones) {
  media <- mean(calificaciones)
  
  if(media >= 6) {
    print("Aprobado")
  } else {
    print("Reprobado")
  }
}

promedio(c(6, 7, 8, 9, 8))
promedio(c(5, 8, 5, 6, 5))

# podemos mejorar la presentacion de lso resultados

promedio <- function(calificaciones) {
  media <- mean(calificaciones)
  texto <- paste0("Calificación: ", media, ", ")
  
  if(media >= 6) {
    print(paste0(texto, "aprobado"))
  } else {
    print(paste0(texto, "reprobado"))
  }
}

promedio(c(6, 7, 8, 9, 8))
promedio(c(5, 8, 5, 6, 5))

# Los bucles son estructuras que permiten ejecutar partes del código de forma repetida 
# mientras se cumpla una condición.

# Las estructuras repetitivas o bucles son usados en programación para repetir 
# bloques específicos de código.

# Bucle for------

# Los bucles for toman una variable a la que se le asignan los elementos de un objeto 
# (en general, vectores o listas) en forma sucesiva a medida que se van recorriendo 
# los ciclos. Entonces, el bucle for es utilizado para iterar sobre un vector.


# Ejemplo 5:  obtener el cuadrado de cada uno de los elementos en un vector numérico del 1 al 6, 
# que representa las caras de un dado
dado <- 1:6

for(cara in dado) {
  sqrtdado <- dado ^ 2
}
print(sqrtdado)
# parece que no ha ocurrido nada pero sí se han realizado las operaciones,aunque R no ha devuelto sus resultados.
# Para ver los resultados es necesario pedirlos de manera explícita.La solucion es usar
# la función print(9, la cual sólo mostrará los resultados de las operaciones en la consola, 
# pero no los asignará a un objeto.

for(cara in dado) {
  print(cara ^ 2)
}

# Ejemplo 6: queremos saber cuánto vale la suma de los primeros 100 números naturales, 
# para eso tenemos que tener una variable que vaya acumulando el resultado de la suma 
# en cada iteracion (n),y otra variable que vaya recorriendo los números desde 1 hasta 100 (i)

n=0
for (i in 1:100){
  n=n+i
}
print(n)


# Ejemplo 7: queremos guardar en una variable la suma de los números pares desde 1 a 100 y en 
# otra variable la suma de los números impares. En este caso vamos a necesitar un for 
# para ir recorriendo los distintos números del 1 al 100 y un if ... else que me permita 
# evaluar si un número es par o impar.

pares   = 0
impares = 0

for (i in 1:100){
  if (i %% 2 == 0)
    pares = pares + i
  else{
    impares = impares +i
  }
}

print(pares)

print(impares)

# Puedo hacer lo mismo sin usar estructuras de control

pares = sum(seq(2, 100, by = 2))
print(pares)

impares = sum(seq(1, 100, by = 2))
print(impares)


# El bucle WHILE-------

# es utilizado para repetir la ejecución de un bloque de código mientras que se 
# cumpla determinada condición.

# Con esta sentencia se controla la condición antes de entrar en el bucle. 
# Si ésta no se cumple, el programa no entrará en el bucle.

# le decimos a R: MIENTRAS esta condición sea VERDADERA, haz estas operaciones.
# La condición generalmente es expresada como el resultado de una o varias operaciones de comparación, pero también puede ser el resultado de una función

# Si en el interior del bucle hay más de una sentencia, éstas deberán ir entre llaves 
# para que se ejecuten como un bloque.

# Necesitamos la función print() para mostrar los resultados en la consola

# Si ejecutamos un while con una condición que nunca será FALSE, este nunca se detendrá!!!

# Ejemplo 8: queremos sumar calificaciones, del 1 al 10 al azar, hasta llegar a un número que mayor o igual a 50.

conteo <-  0  # creo el objeto conteo
valor <- 0    # creo el objeto valor

# while 1°tomará un número al azar del 1 al 10, y lo sumará a "valor". 2°le sumará 1 a 
# "conteo", de esta manera sabremos cuántas iteraciones ocurrieron para llegar a un valor
# que no sea menor a 50.

while(valor < 50) {
  valor <- valor + sample(x = 1:10, size = 1) # sample sirve para generar valores al azar
  conteo <- conteo + 1
}

valor 

conteo


# break nos permite interrumpir un bucle, mientras que next nos deja avanzar a la 
# siguiente iteración del bucle, “saltándose” la actual. Ambas funcionan para for y while.


# Para interrumpir un bucle con break, necesitamos que se cumpla una condición

# Ejemplo 9: cuando i es igual a 3 interrumpo el for

for(i in 1:10) {
  if(i == 3) {
    break
  }
  print(i)
}

# Interrumpimos un while antes de se cumpla la condición

numero <- 20

while(numero > 5) {
  if(numero == 15) {
    break
  }
  numero <- numero - 1
}

numero

# Uso de NEXT:  Cuando la condición se cumple, esa iteración es omitida.

for(i in 1:4) {
  if(i == 3) {
    next
  }
  print(i)
}
# El bucle REPEAT-----------

# es usado para iterar sobre un bloque de codigo varias veces.No tiene una condición 
# para salir del bucle.
# Nosotros debemos definir cuando se debe salir del bucle mediante la sentencia break. 
# Caso contrario el bucle seguirar repitiendose indefinidamente.

# Ejemplo 10:

for (i in 1:50){
  if (i == 15) break
  print(i) # se detiene en i=15, así que solo imprime hasta 14
}

# Ejemplo 11:
x <- 1
repeat {
  print(x)
  x = x+1
  if (x == 6){
    break
  }
}

# Si no incluimos un break, el bucle se repetirá indefinidamente y sólo lo podremos 
# detener pulsando la tecla ESC

# Ejemplo 12:
# al objeto " valor" le sumará +1 a valor hasta que este sea igual a cinco, entonces se detendrá.

valor <-  0
mi_vector <- NULL

repeat{
  valor <- valor + 1
  print(valor)
  if(valor == 5) {
    break
  }
}
valor


## Fin del script ##
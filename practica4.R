# Con el italiano

### ANALISIS DE DATOS ###
load("datosDescriptiva.RData")

head(datos)

### TEST PARA UNA MEDIA ###

# !!! p-value es la probabilidad de que la hipótesis sea correcta

# el test t de student se aplica para contrastar hipotesis relativas a variables continuas. Partimos de la hipótesis de
# que el peso medio es de 2500 gramos

t.test(datos$peso.bebe, mu = 2500)

## Para comprobar si la hipotesis de que el peso medio es 2500 es cierta, tenemos que comprovar el p-valor
# Si el p-valor es menor que 0.05, rechazamos la hipotesis nula, ya que el intervalo de confianza establecido es de 95%

## t.test dice que el p-valor es de 1.73e-12, lo que es menor que 0.05, por lo que rechazamos la hipotesis
# de que el peso medio es 2500

## Además, el intervalo de confianza que devuelve t.test es de (2530.5, 2543.5). Este intervalo no incluye el valor
# 2500, por lo que rechazamos la hipotesis de nuevo

## Ambos datos (p value y intervalo dispar de 2500) suelen ir de la mano y en este caso ambos nos llevan a rechazar la
# hipotesis de que el peso medio es 2500

# Se recomienda emplear el intervalo de confianza para realizar estimaciones, ya que es mas confiable

# Lo ultimo que retorna t.etst es la media, que es de 3102.719


### TEST PARA UNA PROPORCION ###
# Para variables binarias, se emplea el test de proporciones que nos permite comprobar si la proporcion es igual o
# distinta a la propuesta por nuestra hipótesis (ej. 0.2)

table(datos$bajopeso)
prop.test(x = 23, n = 96, p = 0.2) # siendo x el numero de bajopeso, n el numero total de muestras y p la propuesta

# el p-value es mayor de 0.05 por lo que no podemos rechazar la hipótesis de que la proporción de bajo peso es 0.2
# (es decir, la hipótesis PUEDE ser cierta)

# El intervalo de confianza es de (0.16, 0.33), lo que incluye el valor 0.2, por lo que no podemos rechazar la hipótesis
# de que la proporción de bajo peso es 0.2

# En este caso, el p-value y el intervalo de confianza nos llevan a la misma conclusión

# Este test es valido para un minimo de 15 pruebas


### COMPQRACION DE DOS MEDIAS ###

# el test de student nos permite comprobar la media de dos grupos independientes

var.test(peso.bebe ~ fuma, data = datos) # para comparar dos medias, se emplea var.test

# var.test nos devuelve un p-value de 0.4 por lo que rechazamos la hipótesis de que las dos medias son iguales



t.test(peso.bebe ~ fuma, data = datos, var.equal = TRUE) # para comparar dos medias, se emplea t.test

### COMPARACION DE DOS PROPORCIONES ###

# obtenemos tabla de contingencia

tabla <- table(datos$bajopeso, datos$fuma)
tabla

fisher.test(tabla) # para comparar dos proporciones, se emplea fisher.test

# El p-value es mayor que 0.05, por lo que podemos concluir que las proporciones no son iguales

# También devuelve odds ratio, que es de 5.65, lo que significa que la probabilidad de que un bebe sea de bajo peso
# es 5,65 veces mayor si la madre fuma

# Por tanto podemos concluir que el peso de los bebes es distinto si la madre fuma o no


### COMPARACIÓN DE MEDIAS CON DATOS PAEADOS ###
# Misma pregunta, mismo individuo, situaciones diferentes

data("mice2", package = "datarium")
head(mice2)
boxplot(mice2[,-1], col = c("orange", "yellow"))

# En la grafica de cajas se observa como los dos conjuntos estan muy separados por lo que se puede pensar que  el
# tratamiento tiene un efecto significativo

t.test(mice2$after, mice2$before, paired = TRUE) # para comparar dos medias con datos pareados, se emplea t.test

# El p-value es menor que 0.05, por lo que rechazamos la hipótesis de que las medias son iguales. La diferencia de las
# medias es de casi 200 gramos

## ej placebo

# tabla de contingencia

M = as.table(rbind(c(67,76,57), c(48,73,79)))
dimnames(M) = list(c("trat", "placebo"), c("mejora", "igual", "empeora"))
barplot(M, beside = TRUE, legend = TRUE, args.legend = c(x=4.25, y = 80, border= NULL), col = c("orange", "yellow"))

chisq.test(M)
# El p-value es menor que 0.05, por lo que rechazamos la hipótesis de que las proporciones son iguales, es decir,
# que el tratamiento funciona

### EJERCICIO ENCUESTA NAVARRA ###

# 1000 hombres y 1000 mujeres en ambos medios rural y urbano
# medio urbano 473 hombres y 616 mujeres
# medio rural 325 hombres y 578 mujeres

# proporcion de hombres mayor que de mujeres en urbano

tablaUrbano <- matrix(c(473, 527, 616, 384), ncol = 2, dimnames = list(c("Hombres", "Mujeres"), c("Des", "Emp")))
tablaUrbano

fisher.test(tablaUrbano)

# p-value es menor que 0.05, por lo que rechazamos la hipótesis de que las proporciones son iguales
# el odds ratio es de 0.56, lo que significa que la probabilidad de que un hombre este desempleado en el medio urbano
# es 0.56 veces la probabilidad de que una mujer este desempleada en el medio urbano

# miles de personas participan en al dia del campo de reinosa
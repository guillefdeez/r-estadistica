### REGRESIÓN ###
# Buscar el parametro epsilon que minimize la distancia de los puntos a 0
# es decir, que epsilon tenga una distribucion normal con media 0
# # puede ser estimada con el método de los mínimos cuadrados


# El comité de admisiones de una universidad estatal
# estadounidense seleccionó al azar los registros de 200
# estudiantes de primer año. La nota media (GPA) del
# primer semestre y la nota del examen de acceso a la
# universidad (SAT), se almacenan en el conjunto de
# datos Grades del paquete PASWR.

# a) Haz un diagrama de dispersión para investigar la
# relación entre gpa y sat.

# b) Obtén las estimaciones de los parámetros del mo-
# delo de regresión que las relaciona.

# c) ¿Cuál es el cambio esperado en gpa cuando la nota
# de sat aumenta en 50 puntos?

head(Grades)

plot(Grades, pch = 19, main = "Relacion_entre_GPA_y_SAT")
cor.test(Grades$sat, Grades$gpa, alternative = "two.sided", method = "pearson")

# Como p-value es menor que 0.05, rechazamos la hipotesis nula de que no hay relacion entre las dos variables.
# El valor ro (va de -1 a 1) es de 0.75 por lo que hay una correlacion positiva entre las dos variables

modelo.notas <- lm(gpa ~ sat, data = Grades)
modelo.notas$coefficients

# coefficients es un vector con dos elementos, el primero es la ordenada en el origen y el segundo es la pendiente
# por lo que la ecuacion de la recta es gpa = -1,192 + 0,0031 * sat

# En resumen, graficamente se puede observar que hay una relacion positiva entre las dos variables, y el modelo de
# regresion lineal nos dice que por cada punto que aumenta la nota de SAT, la nota de GPA aumenta en 0.0031 puntos
# y además el estudio de correlación nos dice que hay una correlación positiva y alta entre las dos variables
# (ro = 0.75).

# Por tanto, afrimamos que hay correlación

summary(modelo.notas)

# R-sqared es el coeficiente de correlación. Cuanto más alto sea, más valores de y son explicados por x

### PRACTICA ###

load("./datosDescriptiva.RData")

r <- cor(datos$pinimadre, datos$peso.bebe)
r

mod.reg <- lm(peso.bebe ~ pinimadre, data = datos)
mod.reg

# Representamos la relación entre pinimadre y peso.bebe
plot(datos$pinimadre, datos$peso.bebe, xlab = "Peso inicial de la madre", ylab = "Peso del bebe", pch = 19)
abline(mod.reg, col = rgb(0.5,0,0.5), lwd = 2)

# Para el coeficiente de correlación
r^2 # vale 0,16

summary(mod.reg)

### EJERCICIOS ###

# Carga los datos iris del paquete datasets, con el comando data(iris).
# a) Realiza un gráfico que relacione el largo del sépalo con el largo del pétalo.
# b) ¿Crees que hay una relación lineal? Cuánto vale el coeficiente de correlación?
# c) Utiliza el código dado en el ejercicio 7 de la página 84 de los apuntes para obtener un nuevo gráfico.
# ¿Qué puedes decir ahora sobre la relación entre estas dos variables?

data(iris)

plot(iris$Sepal.Length, iris$Petal.Length, xlab = "Largo del sepalo", ylab = "Largo del petalo", pch = 19)

r.iris <- cor(iris$Sepal.Length, iris$Petal.Length)
r.iris^2

# r.iris vale 0,76 por lo que hay una correlación positiva y alta entre las dos variables

mod.reg.iris <- lm(iris$Petal.Length ~ iris$Sepal.Length)
mod.reg.iris
abline(mod.reg.iris, col = rgb(0.5,0,0.5), lwd = 2)

# En el nuevo gráfico se observa que la relación entre las dos variables es lineal, y el coeficiente de correlación
# es de 0.76, lo que indica que hay una correlación positiva y alta entre las dos variables

# En resumen, hay una relación lineal positiva y alta entre las dos variables

attach(iris)
color <- Sepal.Length
color[Species == "setosa"] <- 2
color[Species == "versicolor"] <- 3
color[Species == "virginica"] <- 4
plot(Petal.Length~Sepal.Length, col = color, pch =19)
legend(x = 4.2, y =7, c("setosa", "versicol.", "virgin."), col = 2:4, pch =19)


humedad <- c(0, 12, 29.5, 43, 53, 62.5, 75.5, 85, 93)
perd.peso <- c(8.98, 8.14, 6.67, 6.08, 5.9, 5.83, 4.68, 4.2, 3.72)

plot(humedad, perd.peso, xlab = "Humedad (%)", ylab = "Perdida de peso (%)", pch = 19)

# Se observa que cuanto mayor es la humedad, menor es la pérdida de peso
# Modelo de regresión:

r.hum <- cor(humedad, perd.peso)
r.hum
r.hum^2

# da 0,97, por lo que hay una correlación alta entre las dos variables
# como r.hum es < 0, la correlación es negativa


mod.reg.hum <- lm(perd.peso ~ humedad)
mod.reg.hum
abline(mod.reg.hum, col = rgb(0.5,0,0.5), lwd = 2)

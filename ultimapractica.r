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

# R-sqared es
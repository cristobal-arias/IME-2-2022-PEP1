library(ggpubr)
library(pwr)

# Puesto que hay preguntas en relaci?n a poder estad?stico de pruebas con medias
# unilterales (de una cola) y bilaterales (de dos colas), se consideran aqu?
# ambos escenarios.

# Se sabe que una m?quina que envasa detergentes industriales llena bidones con
# un volumen de producto que sigue una distribuci?n normal con desviaci?n
# est?ndar de 1 litro. Usando una muestra aleatoria de 100 botellas, el
# ingeniero a cargo de la planta requiere determinar si la m?quina est?
# llenando los bidones con una media de 10 litros.



################################################################################
# PODER ESTAD?STICO PARA PRUEBA BILATERAL
################################################################################

cat("PODER PARA PRUEBA BILATERAL\n")



################################################################################
# Si el ingeniero piensa rechazar la hip?tesis nula cuando la muestra presente
# una media menor a 9,9 litros o mayor a 10,1 litros, ?cu?l es la probabilidad
# de que cometa un error de tipo I?
################################################################################

cat("\nPregunta 1\n")

# Definimos los valores conocidos.
desviacion.estandar <- 1
tamano.muestra <- 100
cota.inferior <- 9.9
cota.superior <- 10.1

# Del enunciado se desprende que se trata de una prueba t para una muestra, con
# hipotesis bilateral:
# H0: El volumen medio de los bidones es de 10 litros (mu = 10 [L]).
# HA: El volumen medio de los bidones es distinto de 10 litros (mu != 10 [L]).

# La probabilidad de cometer un error tipo I corresponde al nivel de
# significaci?n, que es lo que se solicita.

# El nivel de significaci?n est? dado por el ?rea de la regi?n de rechazo de la
# distribuci?n que deber?an seguir las medias muestrales bajo la hip?tesis nula.
# Se asume que se trata de una prueba t para una muestra, aunque, en este caso, 
# dado que se conoce la desviaci?n est?ndar, se puede usar tambi?n la
# distribuci?n normal (prueba Z).

# Calculamos el error estándar.
error_estandar <- desviacion.estandar / sqrt(tamano.muestra)

# Generemos una distribuci?n normal en torno al valor nulo, con 5000 valores.
v_nulo <- 10
puntos <- 5000

x <- seq(v_nulo - 5.2 * error_estandar, v_nulo + 5.2 * error_estandar,
         length.out = puntos)

y <- dnorm(x, mean = v_nulo, sd = error_estandar)
distr <- data.frame(x, y)

# Graficar la distribuci?n.
# - Definir paleta de colores.
colores <- hcl(h = (seq(15, 255, length.out = 3)), c = 100, l = 65)

# - Comenzar por la cuadr?cula.
g.dist <- ggplot(data = distr, aes(x))

# - Agregar la distribuci?n normal.
g.dist <- g.dist + stat_function(fun = dnorm,
                                 args = list(mean = v_nulo,
                                             sd = error_estandar),
                                 colour = colores[1], size = 1)

# - Quitar etiquetas del eje y.
g.dist <- g.dist + ylab("")

# - Quitar marcas del y.
g.dist <- g.dist + scale_y_continuous(breaks = NULL)

# - Agregar marcas y etiquetas al eje x.
g.dist <- g.dist + scale_x_continuous(name = "Volumen [L]",
                                      breaks = seq(cota.inferior, cota.superior,
                                                   0.2))

# - Dar formato con fondo blanco.
g.dist <- g.dist + theme_pubr()

# - Rotar etiquetas del eje x.
g.dist <- g.dist + theme(axis.text.x = element_text(angle = 30, size = 10))

# - Agregar la media bajo la hip?tesis nula.
g.dist <- g.dist + geom_vline(xintercept = v_nulo,
                              colour = colores[1], linetype = "longdash")

# - Agregar t?tulo y mostrar el gr?fico.
g.dist <- g.dist + ggtitle("Distribuci?n de las medias muestrales bajo H0")
print(g1)

# - Marcar las regiones de rechazo definidas por el ingeniero.
g.1.bilateral <- g.dist + geom_area(data = subset(distr, x < cota.inferior),
                                    aes(y = y), colour = colores[1],
                                    fill = colores[1], alpha = 0.5)

g.1.bilateral <- g.1.bilateral + geom_area(data = subset(distr,
                                                         x > cota.superior),
                                           aes(y = y), colour = colores[1],
                                           fill = colores[1], alpha = 0.5)

g.1.bilateral <- g.1.bilateral + ggtitle("Pregunta 1 - hip?tesis bilateral")
print(g.1.bilateral)

# Calcular la probablidad que suman las regiones de rechazo.
alfa_izquierdo <- pnorm(cota.inferior, mean = v_nulo, sd = error_estandar,
                        lower.tail = TRUE)

alfa_derecho <- pnorm(cota.superior, mean = v_nulo, sd = error_estandar,
                      lower.tail = FALSE)

alfa.1.bilateral <- alfa_izquierdo + alfa_derecho

cat("La probabilidad de cometer un error tipo I es alfa =", alfa.1.bilateral,
    "\n\n")

cat("=======================================================================\n")



################################################################################
# Si el verdadero volumen medio de los bidones fuera de 9,95 litros, ?cu?l
# ser?a la probabilidad de que el ingeniero, que obviamente no conoce este
# dato, cometa un error de tipo II?
################################################################################

cat("\nPregunta 2\n")

# Construimos un gr?fico de la verdadera distribuci?n y lo superponemos al de la
# hip?tesis nula.

media.bilateral <- 9.95

x1 <- seq(media.bilateral - 5.2 * error_estandar,
          media.bilateral + 5.2 * error_estandar, length.out = puntos)

y1 <- dnorm(x1, mean = media.bilateral, sd = error_estandar)
distr1 <- data.frame(x = x1, y = y1)

g.2.bilateral <- g.1.bilateral + stat_function(fun = dnorm, n = puntos,
                                               args = list(mean = media.bilateral,
                                                           sd = error_estandar),
                                               colour = colores[3], size = 1)

g.2.bilateral <- g.2.bilateral + geom_vline(xintercept = media.bilateral,
                                            colour = colores[3],
                                            linetype = "longdash")

# El error tipo II significa no rechazar la hip?tesis nula cuando esta es
# falsa: en este caso, no rechazar la idea de que la media de la poblaci?n es
# 10 [L], siendo que en realidad es 9,95 [L]. Este tipo de error ocurre si la
# media muestral cae fuera de las regiones cr?ticas definidas por el ingeniero.

# Sombrear ?rea de la curva "verdadera" que cae fuera de las regiones de rechazo
# de la curva correspondiente a la hip?tesis nula.
g.2.bilateral <- g.2.bilateral + geom_area(
  data = subset(distr1, x >= cota.inferior & x <= cota.superior),
  aes(y = y), colour = colores[3], fill = colores[3], alpha = 0.5)

g.2.bilateral <- g.2.bilateral + ggtitle("Pregunta 2 - hip?tesis bilateral")
print(g.2.bilateral)

# Calcular la probablidad de esta regi?n (beta)
beta.superior <- pnorm(cota.superior, mean = media.bilateral,
                       sd = error_estandar, lower.tail = TRUE)

beta.inferior <- pnorm(cota.inferior, mean = media.bilateral,
                       sd = error_estandar, lower.tail = TRUE)

beta.bilateral <- beta.superior - beta.inferior
cat("La probabilidad de cometer un error tipo II es beta =", beta.bilateral,
    "\n\n")

cat("=======================================================================\n")



################################################################################
# Como no se conoce el verdadero volumen medio, genere un gr?fico del poder
# estad?stico con las condiciones anteriores, pero suponiendo que el verdadero
# volumen medio podr?a variar de 9,75 a 10,25 litros.
################################################################################

cat("\nPregunta 3\n")

# Aqu? se pregunta por el poder estad?stico, la probabilidad de detectar que H0
# es falsa cuando realmente lo es, y se pide una curva de poder para diferentes
# valores de la verdadera media.

# Creamos una funci?n que calcule el poder a partir del razonamiento hecho en la
# pregunta anterior (considerando adem?s el caso de hip?tesis unilaterales).

poder <- function(media, error_estandar, limite_inf = NULL, limite_sup = NULL) {
  poder_inf <- 0
  poder_sup <- 1
  
  if(!is.null(limite_inf)) {
    poder_inf <- pnorm(limite_inf, mean = media, sd = error_estandar,
                       lower.tail = TRUE)
  }
  
  if(!is.null(limite_sup)) {
    poder_sup <- pnorm(limite_sup, mean = media, sd = error_estandar,
                       lower.tail = FALSE)
  }
  
  poder <- poder_inf + poder_sup
  return(poder)
}

# Generamos algunos puntos en el rango dado para poder graficar.
x3 <- seq(9.75, 10.25, 0.01)

y3 <- sapply(x3, poder, error_estandar = error_estandar, limite_inf = 9.75,
             limite_sup = 10.25)

distr3 <- data.frame(x = x3, y = y3)
g.3.bilateral <- ggplot(distr3, aes(x, y))
g.3.bilateral <- g.3.bilateral + geom_line(colour = colores[2])
g.3.bilateral <- g.3.bilateral + ylab("Poder estad?stico")
g.3.bilateral <- g.3.bilateral + xlab("Volumen media verdadero [L]")
g.3.bilateral <- g.3.bilateral + theme_pubr()

g.3.bilateral <- g.3.bilateral + theme(
  axis.text.x = element_text(angle = 30, size = 10))

g.3.bilateral <- g.3.bilateral + ggtitle("Pregunta 3 - hip?tesis bilateral")
print(g.3.bilateral)
cat("Ver gr?fico\n\n")

# En el gr?fico se puede ver la curva de poder, la cual se acerca a uno a medida
# que la verdadera media se aleja del valor de la hip?tesis nula, mientras que
# disminuye a medida que se aceca a este valor, donde muestra su valor m?nimo.
# Dicho valor m?nimo corresponde a la probabilidad de rechazar H0 cuando H0 es,
# despu?s de todo, verdadera. Es decir, es la probabilidad de cometer un error
# de tipo I. As?, el valor m?nimo corresponde al nivel de significaci?n.

cat("=======================================================================\n")



################################################################################
# Considerando un volumen medio de 10 litros, ?cu?ntos bidones deber?an
# revisarse para conseguir un poder estad?stico de 0,9 y un nivel de
# significaci?n de 0,05?
###############################################################################

cat("\nPregunta 4\n")

# Aqu? se pregunta por el tama?o de la muestra para conseguir los
# valores estipulados para los factores de la prueba: alfa = 0,05 y
# poder = 0,9.

# Calcular tama?o del efecto (d de Cohen).
efecto.bilateral <- (media.bilateral - v_nulo) / desviacion.estandar

# En caso de tratarse de una prueba z, se puede usar la funci?n pwr.norm.test()
# del paquete pwr() para calcular el tama?o de la muestra.
poder.z.bilateral <- pwr.norm.test(d = efecto.bilateral, sig.level = 0.05,
                                   power = .9, alternative = "two.sided")

cat("Resultado de la llamada a pwr.norm.test():\n")
print(poder.z.bilateral)

tamano.z.bilateral <- ceiling(poder.z.bilateral[["n"]])

cat("El tama?o de la muestra para una prueba z debe ser n =",
    tamano.z.bilateral, "\n\n")

# En caso de tratarse de una prueba t, se puede usar la funci?n pwr.t.test().
poder.t1.bilateral <- pwr.t.test(d = efecto.bilateral, sig.level = 0.05,
                                 power = 0.9, type = "one.sample",
                                 alternative = "two.sided")

cat("Resultado de la llamada a pwr.t.test():\n")
print(poder.t1.bilateral)

tamano.t1.bilateral <- ceiling(poder.t1.bilateral[["n"]])

cat("El tama?o de la muestra para una prueba t debe ser n =",
    tamano.t1.bilateral, "\n\n")

# Otra alternativa es usar la funci?n power.t.test(). Esta alternativa considera
# el tama?o del efecto en la escala de la variable.

# Calcular tama?o del efecto (verdadera diferencia).
diferencia.bilateral <- media.bilateral - v_nulo

poder.t2.bilateral <- power.t.test(delta = diferencia.bilateral,
                                   sd = desviacion.estandar,
                                   sig.level = 0.05, power = .9,
                                   type = "one.sample",
                                   alternative = "two.sided")

cat("Tama?o de la muestra para una prueba t, calculado con power.t.test():\n")
print(poder.t2.bilateral)

tamano.t2.bilateral <- ceiling(poder.t2.bilateral[["n"]])

cat("El tama?o de la muestra para una prueba t debe ser n =",
    tamano.t2.bilateral, "\n\n")

cat("=======================================================================\n")



################################################################################
# ?Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de
# cometer un error de tipo I a un 1% solamente?
################################################################################


cat("\nPregunta 5\n")

# Es la misma pregunta anterior, pero ahora con alfa = 0,01.

# Prueba Z.
poder.z.bilateral <- pwr.norm.test(d = efecto.bilateral, sig.level = 0.01,
                                   power = .9, alternative = "two.sided")

cat("Resultado de la llamada a pwr.norm.test():\n")
print(poder.z.bilateral)

tamano.z.bilateral <- ceiling(poder.z.bilateral[["n"]])

cat("El tama?o de la muestra para una prueba z debe ser n =",
    tamano.z.bilateral, "\n\n")

# Prueba t con pwr.t.test().
poder.t1.bilateral <- pwr.t.test(d = efecto.bilateral, sig.level = 0.01,
                                 power = 0.9, type = "one.sample",
                                 alternative = "two.sided")

cat("Resultado de la llamada a pwr.t.test():\n")
print(poder.t1.bilateral)

tamano.t1.bilateral <- ceiling(poder.t1.bilateral[["n"]])

cat("El tama?o de la muestra para una prueba t debe ser n =",
    tamano.t1.bilateral, "\n\n")

# Prueba t con power.t.test().
poder.t2.bilateral <- power.t.test(delta = diferencia.bilateral,
                                   sd = desviacion.estandar,
                                   sig.level = 0.01, power = .9,
                                   type = "one.sample",
                                   alternative = "two.sided")

cat("Tama?o de la muestra para una prueba t, calculado con power.t.test():\n")
print(poder.t2.bilateral)

tamano.t2.bilateral <- ceiling(poder.t2.bilateral[["n"]])

cat("El tama?o de la muestra para una prueba t debe ser n =",
    tamano.t2.bilateral, "\n\n")

cat("=======================================================================\n")



################################################################################
# PODER ESTAD?STICO PARA PRUEBA BILATERAL
################################################################################

cat("\n\n\n\nPODER PARA PRUEBA UNILATERAL\n")



################################################################################
# Si el ingeniero est? seguro de que el verdadero volumen medio no puede ser
# inferior a 10 litros y piensa rechazar la hip?tesis nula cuando la muestra
# presente una media mayor a 10,1 litros, ?cu?l es la probabilidad de que cometa
# un error de tipo I?
################################################################################

cat("\nPregunta 1\n")

# Del enunciado se desprende que se trata de una prueba t para una muestra, con
# hipotesis unilateral:
# H0: El volumen medio de los bidones es de 10 litros (mu = 10 [L]).
# HA: El volumen medio de los bidones es mayor a 10 litros (mu > 10 [L]).

# La probabilidad de cometer un error tipo I corresponde al nivel de
# significaci?n, que es lo que se solicita.

# El nivel de significaci?n est? dado por el ?rea de la regi?n de rechazo de la
# distribuci?n que deber?an seguir las medias muestrales bajo la hip?tesis nula.
# Una vez m?s, usamos la distribuci?n normal (prueba Z).

# Tomando como base el gr?fico de la distribuci?n normal que hicimos para el
# caso bilateral, marcamos la regi?n de rechazo definida por el ingeniero.
g.1.unilateral <- g.dist + geom_area(data = subset(distr, x > cota.superior),
                                     aes(y = y), colour = colores[1],
                                     fill = colores[1], alpha = 0.5)

g.1.unilateral <- g.1.unilateral + ggtitle("Pregunta 1 - hip?tesis unilateral")
print(g.1.unilateral)

# Calcular la probablidad de la regi?n de rechazo.
alfa.1.unilateral <- pnorm(cota.superior, mean = v_nulo,
                           sd = error_estandar, lower.tail = FALSE)

cat("La probabilidad de cometer un error tipo I es alfa =", alfa.1.unilateral,
    "\n\n")

cat("=======================================================================\n")



################################################################################
# Si el verdadero volumen medio de los bidones fuera de 10,05 litros, ?cu?l
# ser?a la probabilidad de que el ingeniero, que obviamente no conoce este dato,
# cometa un error de tipo II?
################################################################################

cat("\nPregunta 2\n")

# Construimos un gr?fico de la verdadera distribuci?n lo superponemos al de la
# hip?tesis nula.
media.unilateral <- 10.05

x2 <- seq(media.unilateral - 5.2 * error_estandar,
          media.unilateral + 5.2 * error_estandar, length.out = puntos)

y2 <- dnorm(x1, mean = media.unilateral, sd = error_estandar)
distr2 <- data.frame(x = x2, y = y2)

g.2.unilateral <- g.1.unilateral + stat_function(
  fun = dnorm, n = puntos, args = list(mean = media.unilateral,
                                       sd = error_estandar),
  colour = colores[3], size = 1)

g.2.unilateral <- g.2.unilateral + geom_vline(xintercept = media.unilateral,
                                              colour = colores[3],
                                              linetype = "longdash")

# El error tipo II significa no rechazar la hip?tesis nula cuando esta es
# falsa: en este caso, no rechazar la idea de que la media de la poblaci?n es
# 10 [L], siendo que en realidad es 9,95 [L]. Este tipo de error ocurre si la
# media muestral cae fuera de las regiones cr?ticas definidas por el ingeniero.

# Sombrear ?rea de la curva "verdadera" que cae fuera de la regi?n de rechazo
# de la curva correspondiente a la hip?tesis nula.
g.2.unilateral <- g.2.unilateral + geom_area(
  data = subset(distr2, x <= cota.superior), aes(y = y), colour = colores[3],
  fill = colores[3], alpha = 0.5)

g.2.unilateral <- g.2.unilateral + ggtitle("Pregunta 2 - hip?tesis unilateral")
print(g.2.unilateral)

# Calcular la probablidad de esta regi?n (beta)
beta.unilateral <- pnorm(cota.superior, mean = media.unilateral,
                         sd = error_estandar, lower.tail = TRUE)

cat("La probabilidad de cometer un error tipo II es beta =", beta.unilateral,
    "\n\n")

cat("=======================================================================\n")



################################################################################
# Como no se conoce el verdadero volumen medio, genere un gr?fico del poder
# estad?stico con las condiciones anteriores, pero suponiendo que el verdadero
# volumen medio podr?a variar de 10 a 10,25 litros.
################################################################################

cat("\nPregunta 3\n")

# Aqu? se pregunta por el poder estad?stico, la probabilidad de detectar que H0
# es falsa cuando realmente lo es, y se pide una curva de poder para diferentes
# valores de la verdadera media.

# Generamos algunos puntos en el rango dado para poder graficar.
x4 <- seq(9.75, 10.25, 0.01)

y4 <- sapply(x3, poder, error_estandar = error_estandar, limite_inf = NULL,
             limite_sup = 10.25)

distr4 <- data.frame(x = x4, y = y4)
g.3.unilateral <- ggplot(distr4, aes(x, y))
g.3.unilateral <- g.3.unilateral + geom_line(colour = colores[2])
g.3.unilateral <- g.3.unilateral + ylab("Poder estad?stico")
g.3.unilateral <- g.3.unilateral + xlab("Volumen media verdadero [L]")
g.3.unilateral <- g.3.unilateral + theme_pubr()

g.3.unilateral <- g.3.unilateral + theme(
  axis.text.x = element_text(angle = 30, size = 10))

g.3.unilateral <- g.3.unilateral + ggtitle("Pregunta 3 - hip?tesis unilateral")
print(g.3.unilateral)
cat("Ver gr?fico\n\n")
cat("=======================================================================\n")



################################################################################
# Considerando un volumen medio de 10 litros, ?cu?ntos bidones deber?an
# revisarse para conseguir un poder estad?stico de 0,9 y un nivel de
# significaci?n de 0,05?
################################################################################

cat("\nPregunta 4\n")

# Aqu? se pregunta por el tama?o de la muestra para conseguir los
# valores estipulados para los factores de la prueba: alfa = 0,05 y
# poder = 0,9.

# Calcular tama?o del efecto (d de Cohen).
efecto.unilateral <- (media.unilateral - v_nulo) / desviacion.estandar

# En caso de tratarse de una prueba z, se puede usar la funci?n pwr.norm.test()
# del paquete pwr() para calcular el tama?o de la muestra.
poder.z.unilateral <- pwr.norm.test(d = efecto.unilateral, sig.level = 0.05,
                                    power = .9, alternative = "greater")

cat("Resultado de la llamada a pwr.norm.test():\n")
print(poder.z.unilateral)

tamano.z.unilateral <- ceiling(poder.z.unilateral[["n"]])

cat("El tama?o de la muestra para una prueba z debe ser n =",
    tamano.z.unilateral, "\n\n")

# En caso de tratarse de una prueba t, se puede usar la funci?n pwr.t.test().
poder.t1.unilateral <- pwr.t.test(d = efecto.unilateral, sig.level = 0.05,
                                  power = .9, type = "one.sample",
                                  alternative = "greater")

cat("Resultado de la llamada a pwr.t.test():\n")
print(poder.t1.unilateral)

tamano.t1.unilateral <- ceiling(poder.t1.unilateral[["n"]])

cat("El tama?o de la muestra para una prueba t debe ser n =",
    tamano.t1.unilateral, "\n\n")

# Otra alternativa es usar la funci?n power.t.test(). Esta alternativa considera
# el tama?o del efecto en la escala de la variable.

# Calcular tama?o del efecto (verdadera diferencia).
diferencia.unilateral <- media.unilateral - v_nulo

poder.t2.unilateral <- power.t.test(delta = diferencia.unilateral,
                                    sd = desviacion.estandar,
                                    sig.level = 0.05, power = .9,
                                    type = "one.sample",
                                    alternative = "one.sided")

cat("Tama?o de la muestra para una prueba t, calculado con power.t.test():\n")
print(poder.t2.unilateral)

tamano.t2.unilateral <- ceiling(poder.t2.unilateral[["n"]])

cat("El tama?o de la muestra para una prueba t debe ser n =",
    tamano.t2.unilateral, "\n\n")

cat("=======================================================================\n")



################################################################################
# ?Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de
# cometer un error de tipo I a un 1% solamente?
################################################################################

cat("\nPregunta 5\n")

# Igual a la pregunta anterior, pero con alfa = 0,01.

# Prueba Z.
poder.z.unilateral <- pwr.norm.test(d = efecto.unilateral, sig.level = 0.01,
                                    power = .9, alternative = "greater")

cat("Resultado de la llamada a pwr.norm.test():\n")
print(poder.z.unilateral)

tamano.z.unilateral <- ceiling(poder.z.unilateral[["n"]])

cat("El tama?o de la muestra para una prueba z debe ser n =",
    tamano.z.unilateral, "\n\n")

# Prueba t con pwr.t.test().
poder.t1.unilateral <- pwr.t.test(d = efecto.unilateral, sig.level = 0.01,
                                  power = .9, type = "one.sample",
                                  alternative = "greater")

cat("Resultado de la llamada a pwr.t.test():\n")
print(poder.t1.unilateral)

tamano.t1.unilateral <- ceiling(poder.t1.unilateral[["n"]])

cat("El tama?o de la muestra para una prueba t debe ser n =",
    tamano.t1.unilateral, "\n\n")

# Prueba t con power.t.test().
poder.t2.unilateral <- power.t.test(delta = diferencia.unilateral,
                                    sd = desviacion.estandar,
                                    sig.level = 0.01, power = .9,
                                    type = "one.sample",
                                    alternative = "one.sided")

cat("Tama?o de la muestra para una prueba t, calculado con power.t.test():\n")
print(poder.t2.unilateral)

tamano.t2.unilateral <- ceiling(poder.t2.unilateral[["n"]])

cat("El tama?o de la muestra para una prueba t debe ser n =",
    tamano.t2.unilateral, "\n\n")
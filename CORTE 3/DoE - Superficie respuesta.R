##########################################################################
######################### SUPERFICIE RESPUESTA ###########################
##########################################################################

library(rsm)

# Creación de diseño central compuesto
# Es usual que los puntos axiales tengan una distancia de raiz de dos bajo codificación

set.seed(123)
diseno_ccc <- ccd(basis = 2, 
                  n0 = 2,
                  randomize = FALSE,
                  alpha = "rotatable", 
                  oneblock= TRUE, 
                  inscribed = FALSE, 
                  coding = list(x1 ~ (Temp - 40)/10, x2 ~ (pH - 5.5)/1.10))

as.data.frame(diseno_ccc)

# Distribucion grafica de los puntos del diseño
plot(diseno_ccc[ , c(3:4)], pch = 16)
abline(h = c(1, -1), col = "lightgrey")
abline(v = c(1, -1) , col = "lightgrey")




# Ajuste de modelo de primer orden

library(readxl)

# Importamos los datos
datos_ccc <- read_excel("C:/Users/Familia Rodriguez/Downloads/DoE_rsm_ccc.xlsx", sheet = 1)
datos_ccc

# Codificación de datos

datos_ccc <-  coded.data(data = datos_ccc, 
                         x1 ~ (tiempo - 35)/5, x2 ~ (temperatura - 155)/5)

as.data.frame(datos_ccc)

# Grafico del diseño
plot(datos_ccc[ , c(2:3)], pch = 16, main = "Diseño factorial 2^k")

# Modelo de primer orden
# FO: parametro para armar modelo de primer orden
modelo1 <- rsm(y1 ~ FO(x1, x2), data = datos_ccc)

# Resultado del ajuste
summary(modelo1)

# Superficie 3D 
par(mfrow = c(1,2))
persp(modelo1, x2 ~ x1, 
      zlab = "Rendimiento(%)", 
      contours = list(z = "bottom", col = "colors"), # posicion y color
      at = c(summary(modelo1$canonical$xs)),
      theta = 15, # coordenadas graficas
      phi = 20)

# Grafico de contornos
contour(modelo1, ~ x1 + x2, image = TRUE) 



# Ajuste de modelo de segundo orden


# Importamos los datos
datos_ccc <- read_excel("C:/Users/Familia Rodriguez/Downloads/DoE_rsm_ccc.xlsx", sheet = 2)
datos_ccc


# Codificamos los factores
datos_ccc <- coded.data(data = datos_ccc, 
                        x1 ~ (tiempo - 85)/5, x2 ~ (temperatura - 175)/5)

as.data.frame(datos_ccc)

# Grafico del diseño
plot(datos_ccc[ , c(2:3)], pch = 16, main = "Diseño CCC")
abline(h = c(1, -1), col = "lightgrey")
abline(v = c(1, -1) , col = "lightgrey")


# Modelo polinomico de segundo grado
# SO: parametro para armar modelo de segundo orden
# TWI: interacciones entre factores
# PQ: cuadrados puros
modelo2.y1 <- rsm(y1 ~ SO(x1, x2), data = datos_ccc)

# (En caso de tener en cuenta el factor bloqueo, el codigo sería):
# modelo2 <- rsm(y1 ~ bloque + SO(x1, x2), data = datos_ccc)

# Resultado del ajuste
summary(modelo2.y1)


# Modelo sin interacciones
modelo2.y1.sinI <- rsm(y1 ~ FO(x1, x2) + PQ(x1, x2), data = datos_ccc)

# Resultado del ajuste
summary(modelo2.y1.sinI)

# Superficie 3D 
par(mfrow = c(1,2))
persp(modelo2.y1.sinI, x2 ~ x1, 
      zlab = "Rendimiento(%)", 
      contours = list(z = "bottom", col = "colors"), # posicion y color
      at = c(summary(modelo2.y1.sinI$canonical$xs)),
      theta = 50, # coordenadas graficas
      phi = 20)

# Grafico de contornos
contour(modelo2.y1.sinI, ~ x1 + x2, image = TRUE)


# Valor predicho de rendimiento (%) en el punto estacionario
predict(modelo2.y1.sinI, coded.data(data.frame(tiempo = 86.8, 
                                               temperatura = 176.28),
                                    x1 ~ (tiempo - 85)/5, 
                                    x2 ~ (temperatura - 175)/5))

# Calculamos las predicciones del modelo
pred.y1 <- predict(modelo2.y1.sinI, datos_ccc, interval = "confidence")

# Creamos un dataset con la variable respuesta real (y1) y la predicha (fit) 
# por el modelo, junto con el intervalo de confianza (lwr, upr)
library(dplyr)
predicciones <- data.frame(cbind(y1 = datos_ccc$y1, pred.y1))
predicciones <- mutate(predicciones, res = y1 - fit)
predicciones <- predicciones[ , c(1, 2, 5, 3, 4)]
predicciones

# Grafico de predicciones vs valor real
library(ggplot2)
ggplot(predicciones, aes(x = y1, y = fit)) + 
  geom_point() +
  geom_line(aes(y = lwr), col = "lightgrey")+
  geom_line(aes(y = upr), col = "lightgrey")+
  geom_errorbar(aes(ymin = lwr, ymax = upr), colour="red") +
  labs(x = "Respuesta real (%)", y = "Respuesta predicha (%)") +
  geom_abline(slope = 1, color = "blue") +
  theme_classic()


par(mfrow = c(2, 2))

# Diagnostico grafico de los datos del modelo
plot(modelo2.y1.sinI)

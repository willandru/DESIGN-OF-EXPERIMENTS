# Ajuste de modelo de primer orden


# Crear el dataframe directamente con los datos
data <- data.frame(
  Temp = c(-225, -225, -215, -215, -220, -220, -220, -220),
  Presion = c(1.1, 1.3, 1.1, 1.3, 1.2, 1.2, 1.2, 1.2),
  Pureza = c(82.8, 83.5, 84.7, 85.0, 84.1, 84.5, 83.9, 84.3)
)

data
# Codificación de datos
datos_ccc <-  coded.data(data = data, 
                         x1 ~ (Temp + 220)/5, x2 ~ (Presion - 1.2)/0.1)

as.data.frame(datos_ccc)

# Grafico del diseño
plot(datos_ccc[ , c(2:3)], pch = 16, main = "Diseño factorial 2^k")

# Modelo de primer orden
# FO: parametro para armar modelo de primer orden
modelo1 <- rsm(Pureza ~ FO(x1, x2), data = datos_ccc)

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




# Ajuste de modelo de SEGUNDO ORDEN

data2 <- data.frame(x1=c(-1,-1,1,1,-1.414,1.414,0,0,0,0,0,0,0),
                    x2=c(-1,1,-1,1,0,0,-1.414,1.414,0,0,0,0,0),
                    y=c(54,45,32,47,50,53,47,51,41,39,44,42,40))
# Modelo polinomico de segundo grado
# SO: parametro para armar modelo de segundo orden
# TWI: interacciones entre factores
# PQ: cuadrados puros
modelo2.y1 <- rsm(y ~ SO(x1, x2), data = data2)

# (En caso de tener en cuenta el factor bloqueo, el codigo sería):
# modelo2 <- rsm(y1 ~ bloque + SO(x1, x2), data = datos_ccc)

# Resultado del ajuste
summary(modelo2.y1)



# Superficie 3D 
par(mfrow = c(1,2))
persp(modelo2.y1, x2 ~ x1, 
      zlab = "Rendimiento(%)", 
      contours = list(z = "bottom", col = "colors"), # posicion y color
      at = c(summary(modelo2.y1$canonical$xs)),
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

##########################################################################
########################## TOPICOS ADICIONALES ###########################
##########################################################################


########################## TRANSFORMACION BOX COX ########################

library(MASS)
library(ggplot2)

set.seed(123) # Para reproducibilidad
tiempos <- c(rnorm(50, mean=20, sd=5), rnorm(50, mean=30, sd=10), rnorm(50, mean=40, sd=15))


# Histograma original
hist(tiempos, main="Histograma Original", xlab="Tiempo", breaks=30, col="lightblue")

# GrÃ¡fico Q-Q
qqnorm(tiempos)
qqline(tiempos, col = "red")

# Aplicar transformaciÃ³n Box-Cox
boxcox_result <- boxcox(tiempos ~ 1)
lambda <- boxcox_result$x[which.max(boxcox_result$y)] # Valor de lambda Ã³ptimo

# Transformar los datos usando el lambda Ã³ptimo
tiempos_boxcox <- (tiempos^lambda - 1) / lambda

# Mostrar resultados
print("Datos originales:")
print(head(tiempos)) # Muestra los primeros 6 datos
print("Datos transformados (Box-Cox):")
print(head(tiempos_boxcox)) # Muestra los primeros 6 datos
print(paste("Valor de lambda:", lambda))


# Histograma transformado
hist(tiempos_boxcox, main="Histograma Transformado (Box-Cox)", xlab="Tiempo Transformado", breaks=30, col="lightgreen")

# GrÃ¡fico Q-Q para los datos transformados
qqnorm(tiempos_boxcox)
qqline(tiempos_boxcox, col = "red")




########################## MODELO ANCOVA ##########################

# Ejemplo 1

# Crear datos de ejemplo
set.seed(123)
n <- 60
grupo <- factor(rep(c("A", "B", "C"), each=n/3)) # Tres grupos
tiempo_estudio <- rnorm(n, mean=5, sd=2) # Tiempo de estudio en horas
rendimiento <- 50 + as.numeric(grupo) * 10 + rnorm(n, mean=0, sd=5) + tiempo_estudio * 3 # Rendimiento

# Combinar en un dataframe
datos <- data.frame(grupo, tiempo_estudio, rendimiento)

# Ver los primeros registros
head(datos)

# GrÃ¡fico de dispersiÃ³n
ggplot(datos, aes(x=tiempo_estudio, y=rendimiento, color=grupo)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Rendimiento por Grupo y Tiempo de Estudio",
       x="Tiempo de Estudio (horas)", y="Rendimiento")

# Realizar ANCOVA
modelo_ancova <- aov(rendimiento ~ grupo + tiempo_estudio, data=datos)

# Resumen del modelo
summary(modelo_ancova)


# Comparaciones post hoc
tukey_resultados <- TukeyHSD(modelo_ancova, "grupo")
print(tukey_resultados)

# GrÃ¡fico de comparaciones
plot(tukey_resultados)






# Ejemplo 2

# Se desea estudiar el tiempo de vida de una pieza (vida) cortadora de dos tipos, 
# A y B (herramienta), en función de la velocidad del torno (velocidad) en el que 
# está integrada (en revoluciones por segundo).

# Carga de datos
velocidad <- c(610, 950, 720, 840, 980, 530, 680, 540, 980, 730, 670, 
               770, 880, 1000, 760, 590, 910, 650, 810, 500)
vida <- c(18.73, 14.52, 17.43, 14.54, 13.44, 25.39, 13.34, 22.71, 
          12.68, 19.32, 30.16, 27.09, 25.40, 26.05, 33.49, 35.62, 
          26.07, 36.78, 34.95, 43.67)
herramienta <- gl(2, 10, 20, labels=c("A", "B"))
tiempovida <- data.frame(velocidad, vida, herramienta)
# Gráfico
ggplot(tiempovida,  aes(x = velocidad,  y = vida,  color = herramienta)) + 
  geom_point() 

# Comenzamos con el modelo más sencillo

library(modelr)
library(olsrr)


# Modelo con una única recta
M0 <- lm(vida ~ velocidad, data = tiempovida)

# M1: modelo con rectas paralelas
M1 <- lm(vida ~ herramienta + velocidad, data = tiempovida)

# M2: modelo con rectas no paralelas
M2 <- lm(vida ~ herramienta + velocidad + herramienta:velocidad, data = tiempovida)

# grid de valores para construir los modelos
grid <- tiempovida %>% data_grid(herramienta, velocidad) %>%gather_predictions(M0, M1, M2)

# Gráfico
ggplot(tiempovida,aes(velocidad, vida, colour = herramienta)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~ model) +
  labs(x = "Velocidad del torno", y = "Tiempo de vida") 


ggplot(tiempovida, aes(x = herramienta, y = vida)) + 
  geom_boxplot() 

# Modelo saturado
fit.vida <- lm(vida ~ velocidad * herramienta, data = tiempovida)
summary(fit.vida)
# Selección del modelo
ols_step_backward_p(fit.vida, prem = 0.05)

# Modelo saturado
fit.vida <- lm(vida ~ velocidad + herramienta, data = tiempovida)
# Parámetros estimados
summary(fit.vida)
confint(fit.vida)


ancova <- aov(fit.vida)
summary(ancova)

par(mfrow = c(2, 2))

# Diagnostico grafico de los datos del modelo
plot(ancova)

# Tests de hipótesis
ols_test_normality(fit.vida)


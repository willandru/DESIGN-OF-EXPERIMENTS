####################################################################
######################### Ejercicio 1 ##############################
####################################################################


library(readxl)

datos <- read_excel("C:/Users/adrodriguez/Downloads/Bloques aleatorios/datos_ejercicio.xlsx", sheet="E1")

names(datos)[3] <- "TIEMPO"

# ANOVA con un bloque 

anova_con_bloque <- aov(TIEMPO ~ DIA + SOLUCION, data = datos)

summary(anova_con_bloque)


# El dia si es un factor determinante y afecta el tiempo de crecimiento
# La solución tambien afecta el tiempo de crecimiento con una confianza del 95%


LSD<-LSD.test(anova_con_bloque,"SOLUCION")
print(LSD)
plot(LSD)

# La solución 1 y 2 muestran mayor impacto que la 3, pero no entre ellas.

#Normalidad
#H0: Los datos provienen de una distribución normal
shapiro.test(anova_con_bloque$residuals)

#Homocedasticidad

# H0: Las varianzas de los grupos son iguales
bartlett.test(TIEMPO ~ SOLUCION, data = datos)


# Grafico de efectos de soluciones
library(ggplot2)

ggplot(datos, aes(x = SOLUCION, y = TIEMPO, color = DIA)) +
  geom_point(size = 3) +
  geom_line(aes(group = DIA)) +
  labs(title = "Efectos de las soluciones por dia",
       x = "Soluciones",
       y = "Tiempo") +
  theme_minimal()


# Gráfico de efectos de dias
ggplot(datos, aes(x = DIA, y = TIEMPO, color = SOLUCION)) +
  geom_point(size = 3) +
  geom_line(aes(group = SOLUCION)) +
  labs(title = "Efectos de los dias por solución",
       x = "Dia",
       y = "Tiempo") +
  theme_minimal()


####################################################################
######################### Ejercicio 2 ##############################
####################################################################

library(readxl)

datos <- read_excel("C:/Users/adrodriguez/Downloads/Bloques aleatorios/datos_ejercicio.xlsx", sheet="E2")



# ANOVA cuadrados latinos

anova_cuadrados <- aov(TIEMPO ~ ENSAMBLAJE + OPERADOR + ORDEN, data = datos)

summary(anova_cuadrados)


# Al 95% de confianza ningun factor es determinante en el tiempo de ensamblaje

#Normalidad
#H0: Los datos provienen de una distribución normal
shapiro.test(anova_cuadrados$residuals)

#Homocedasticidad

# H0: Las varianzas de los grupos son iguales
bartlett.test(TIEMPO ~ ENSAMBLAJE, data = datos)










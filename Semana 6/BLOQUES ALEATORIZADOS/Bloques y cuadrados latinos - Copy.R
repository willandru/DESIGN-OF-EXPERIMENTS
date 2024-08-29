##############################################################
##################### BLOQUES ALEATORIZADOS ##################
##############################################################


# 1. Cargar datos

library(readxl)

datos <- read_excel("BLOQUES ALEATORIZADOS/datos_bloque.xlsx", sheet="Bloques")

# 2. Verificar tipo de datos

str(datos)

datos$Metodo <- as.factor(datos$Metodo)
datos$Operario <- as.factor(datos$Operario)

str (datos)


boxplot(datos$Tiempo ~ datos$Metodo)


# 3. Modelo ANOVA

anova_con_bloque <- aov(Tiempo ~ Metodo + Operario, data = datos)

summary(anova_con_bloque)

# 4. Supuestos del modelo

residuos <- residuals(anova_con_bloque)

par(mfrow = c(2, 2))
plot(anova_con_bloque)
par(mfrow = c(1, 1)) 


# Prueba Post-HOC

#install.packages("agricolae")
library(agricolae)

#LSD
print(LSD.test(anova_con_bloque,"Metodo"))

#Tukey
print( TukeyHSD(anova_con_bloque))


# Grafico de efectos de soluciones
library(ggplot2)

ggplot(datos, aes(x = Metodo, y = Tiempo, color = Operario)) +
  geom_point(size = 3) +
  geom_line(aes(group = Operario)) +
  labs(title = "Efectos de los metodos por operario",
       x = "Metodo",
       y = "Tiempo") +
  theme_minimal()

##############################################################
##################### CUADRADOS LATINOS ######################
##############################################################



datos <- read_excel("BLOQUES ALEATORIZADOS/datos_bloque.xlsx", sheet="Cuadrados Latinos")

# Tipo de datos
datos$ORDEN <- as.factor(datos$ORDEN)
datos$OPERADOR <- as.factor(datos$OPERADOR)
datos$METODO <- as.factor(datos$METODO)
datos$TIEMPO <- as.numeric(datos$TIEMPO)

str(datos)


# Analisis de varianza

modelo <- aov(TIEMPO ~ ORDEN + OPERADOR + METODO,data = datos)
summary(modelo)


boxplot(TIEMPO ~ ORDEN,data = datos)
boxplot(TIEMPO ~ OPERADOR,data = datos)
boxplot(TIEMPO ~ METODO,data = datos)

# Supuesto de normalidad
qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)


#Supuesto de homogeneidad de varianza

plot(modelo$residuals)
abline(h=0)


bartlett.test(TIEMPO ~ METODO, data = datos)


# Boxplot de residuos

plot(datos$METODO,modelo$residuals)
abline(h=0)

# Prueba Post-Hoc: LSD

library(agricolae)
LSD<-LSD.test(modelo,"METODO")
print(LSD)
plot(LSD)




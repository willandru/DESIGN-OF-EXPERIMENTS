##################################################################################
############################## ANOVA 2 FACTORES ##################################
##################################################################################

library(ggplot2)
library(gridExtra)

#Una empresa de materiales de construcción quiere estudiar la influencia que tienen 
#el grosor y el tipo de templado sobre la resistencia máxima de unas láminas de acero. 
#Para ello miden el estrés hasta la rotura (variable cuantitativa dependiente) para dos 
#tipos de templado (lento y rápido) y tres grosores de lámina (8mm, 16mm y 24 mm).


# Definición de datos
resistencia <- c(15.29, 15.89, 16.02, 16.56, 15.46, 16.91, 16.99, 17.27, 16.85,
                 16.35, 17.23, 17.81, 17.74, 18.02, 18.37, 12.07, 12.42, 12.73,
                 13.02, 12.05, 12.92, 13.01, 12.21, 13.49, 14.01, 13.30, 12.82,
                 12.49, 13.55, 14.53)
templado <- c(rep(c("rapido", "lento"), c(15,15)))
grosor <- rep(c(8, 16, 24), each = 5, times = 2)
datos <- data.frame(templado = templado, grosor = as.factor(grosor),
                    resistencia = resistencia)
head(datos)

# Descripcion de datos

p1 <- ggplot(data = datos, aes(x = templado, y = resistencia)) + 
  geom_boxplot() + theme_bw()
p2 <- ggplot(data = datos, aes(x = grosor, y = resistencia)) +
  geom_boxplot() + theme_bw()
p3 <- ggplot(data = datos, aes(x = templado, y = resistencia, colour = grosor)) +
  geom_boxplot() + theme_bw()

#Boxplot simples

grid.arrange(p1, p2, ncol = 2)

#Boxplot compuesto

p3

# Medias por tipo de templado
with(data=datos,  expr=tapply(resistencia, templado, mean))

# Medias por tipo de grosor
with(data=datos,  expr=tapply(resistencia, grosor, mean))

# Medias detalladas
with(data=datos,  expr=tapply(resistencia, list(grosor, templado), mean))


# varianza por tipo de templado
with(data=datos,  expr=tapply(resistencia, templado, var))

# varianza por tipo de grosor
with(data=datos,  expr=tapply(resistencia, grosor, var))

# varianza detalladas
with(data=datos,  expr=tapply(resistencia, list(grosor, templado), var))


# Visualización de posibles interacciones

#interacción templado vs grosor
interaction.plot(templado, grosor, resistencia, data = datos, col= 1:3, type="b")

#con ggplot
ggplot(data = datos , aes(x = templado, y = resistencia, colour = grosor, group = grosor))+
  stat_summary(fun = mean, geom = "point")+
  stat_summary(fun = mean, geom = "line") +
  labs(y = "mean(resistencia)")+
  theme_bw()

# interacción grosor vs templado
interaction.plot(grosor, templado, resistencia, data = datos, col= 2:3, type="b")

#con ggplot
ggplot(data = datos , aes(x = grosor, y = resistencia, colour = templado, group = templado))+
  stat_summary(fun = mean, geom = "point")+
  stat_summary(fun = mean, geom = "line") +
  labs(y = "mean(resistencia)")+
  theme_bw()


# ANOVA sin interacción
anova <- aov(resistencia ~ templado + grosor, data=datos)
summary(anova)


# ANOVA con interacción
anova <- aov(resistencia ~ templado + grosor + templado*grosor, data=datos)
summary(anova)


# Supuestos anova

plot(anova)

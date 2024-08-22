
data("PlantGrowth")
datos <- as.data.frame(PlantGrowth)

head(datos)

#La variable group esta codificada como un factor? 
str(datos)

datos_tr1 <- datos[datos$group=="tr1",]
datos_tr1 %>% group_by()


#Cuales son las medias y las medianas de peso para cada tratamiento?
# Using the aggregate function to get summary statistics by group
aggregate(weight ~ group, data = datos, summary)


unique(datos$group)


library(dplyr)

# Calcular el promedio de peso por grupo
promedios <- datos %>%
  group_by(group) %>%
  summarize(mean_weight = mean(weight))

# Ver los promedios
print(promedios)


# Instalar y cargar ggplot2 si aún no lo tienes
install.packages("ggplot2")
library(ggplot2)
# Crear el barplot
ggplot(promedios, aes(x = group, y = mean_weight, fill = group)) +
  geom_bar(stat = "identity") +
  labs(title = "Promedio de peso por grupo", x = "Grupo", y = "Peso Promedio") +
  theme_minimal()


# Crear un diagrama de bigotes
boxplot(weight ~ group, data = datos,
        main = "Diagrama de Bigotes por Grupo",
        xlab = "Grupo", ylab = "Peso",
        col = "lightblue", border = "darkblue")


# EXPLORACION

ctrl_df <- datos %>% filter(group == "ctrl")
trt1_df <- datos %>% filter(group == "trt1")
trt2_df <- datos %>% filter(group == "trt2")

qqnorm(ctrl_df$weight)
qqline(ctrl_df$weight)

summary(ctrl_df)
summary(trt1_df)
summary(trt2_df)

#PRUEBA DE NORMALIDAD
#qqplot
par(mfrow = c(3, 1))  

# Genera Q-Q plot para el grupo "ctrl"
qqnorm(ctrl_df$weight, main = "Q-Q Plot: ctrl")
qqline(ctrl_df$weight, col = "red")
# Genera Q-Q plot para el grupo "trt1"
qqnorm(trt1_df$weight, main = "Q-Q Plot: trt1")
qqline(trt1_df$weight, col = "blue")
# Genera Q-Q plot para el grupo "trt2"
qqnorm(trt2_df$weight, main = "Q-Q Plot: trt2")
qqline(trt2_df$weight, col = "yellow")
# Restaurar la configuración de gráficos a su estado predeterminado
par(mfrow = c(1, 1))

# Prueba de normalidad para el grupo "ctrl"
shapiro_ctrl <- shapiro.test(ctrl_df$weight)
print(shapiro_ctrl)

# Prueba de normalidad para el grupo "trt1"
shapiro_trt1 <- shapiro.test(trt1_df$weight)
print(shapiro_trt1)

# Prueba de normalidad para el grupo "trt2"
shapiro_trt2 <- shapiro.test(trt2_df$weight)
print(shapiro_trt2)
#No hay evidencia para decir que los datos no son normales

#HOMOCEDASTICIDAD
bartlett_test <- bartlett.test(weight ~ group, data = datos)
print(bartlett_test)
#No hay evidencia para decir que las varianzas no son homogeneas



#ANOVA
# Realizar ANOVA de una vía
anova <- aov(weight ~ group, data = datos)


# Ver los resultados de la ANOVA
summary(anova)

plot(anova)



posthor_resul <- TukeyHSD(anova)
print(posthor_resul)
plot(posthor_resul)

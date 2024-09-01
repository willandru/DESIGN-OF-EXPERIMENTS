########################################################################################
########################################################################################
######################## ANOVA EFECTOS SIMPLES - EJERCICIOS   ##########################
########################################################################################
########################################################################################

# Cargar los datos
data(PlantGrowth)
datos <- as.data.frame(PlantGrowth)


# Explorar los datos
# ?Cu?ntos tratamientos diferentes hay en el conjunto de datos?

boxplot(datos$weight ~ datos$group)

unique(datos$group)

# ?Cu?les son las medias y las medianas de peso para cada tratamiento?
install.packages("dplyr")
library(dplyr)

datos_trt1 <- datos[datos$group=="trt1",]
final <- datos_trt1 %>% group_by(group) %>% summarise(prom = mean(weight))

datos_trt2 <- datos[datos$group=="trt2",]
final_2 <- datos_trt2 %>% group_by(group) %>% summarise(prom = mean(weight))

datos_trt3 <- datos[datos$group=="ctrl",]
final_3 <- datos_trt3 %>% group_by(group) %>% summarise(prom = mean(weight))

finalfinal <- rbind(final, final_2, final_3)


finalfinal2 <- datos %>% group_by(group) %>% summarise(prom = mean(weight), mediana= median(weight))

# ?La variable group est? codificada como un factor?
str(datos)

pairs(datos)

aggregate(datos$weight ~ datos$group, data= datos, summary)

# Verificar si existe diferencia significativa en el crecimiento de las plantas a partir del tratamiento usado

anova <- aov(datos$weight ~ datos$group)
summary(anova)


# Verificar supuestos del modelo


residuales <- anova$residuals
shapiro.test(residuales)

par(mfrow = c(2, 2))
plot(anova)
par(mfrow = c(1, 1))

bartlett_test <- bartlett.test(datos$weight ~ datos$group)
print(bartlett_test)



# ?Que tratamiento tuvo mayor efecto sobre el crecimiento?

posthoc_result <- TukeyHSD(anova)
print(posthoc_result)
plot(posthoc_result)

## PRIMERO

datos <- read_excel("datos_preparcial2.xlsx", sheet = "1")

datos <- as.data.frame(datos)


datos$Frecuencia<- as.factor(datos$Frecuencia)
datos$Medio_ambiente<- as.factor(datos$Medio_ambiente)

str(datos)


anova <- aov(Crecimiento ~ Frecuencia * Medio_ambiente, data=datos)
summary(anova)

resi <- residuals(anova)
plot(resi)
plot(anova)

shapiro_test <- shapiro.test(resi)
print(shapiro_test)

bartlett_test <- bartlett.test(Crecimiento ~ Frecuencia, data= datos)
print(bartlett_test)


# TRANSFORMAR Y -> LOG(Y)

datos$Crecimiento <- log(datos$Crecimiento)

anova <- aov(Crecimiento ~ Frecuencia * Medio_ambiente, data=datos)
summary(anova)

resi <- residuals(anova)
plot(resi)
plot(anova)

shapiro_test <- shapiro.test(resi)
print(shapiro_test)

bartlett_test <- bartlett.test(Crecimiento ~ Frecuencia, data= datos)
print(bartlett_test)



#SEGUNDOO

datos2 <- read_excel("datos_preparcial2.xlsx", sheet = "2")
str(datos2)

datos2<- as.data.frame(datos2)

f3 <- c("Bloque 1", "Bloque 2", "Bloque 3")
Bloques <- gl(n=3, k=1, length=3*2*2, labels=f3)
Bloques

datos2$replica <- Bloques

datos2$A<- as.factor(datos2$A)
datos2$B<- as.factor(datos2$B)
datos2$C<- as.factor(datos2$C)
datos2$replica<- as.factor(datos2$replica)

av =aov(Hora ~ A*B*C + replica, data=datos2)
summary(av)


#TERCERP


datos3$ABC <- datos3$A + datos3$B +datos3$C

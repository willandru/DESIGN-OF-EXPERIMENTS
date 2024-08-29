#EJERCICIO EN CLASE --> CUADRADOS LATINOS


datos <- read_excel("BLOQUES ALEATORIZADOS/datos_ejercicio.xlsx", sheet="E2")

#Un ingeniero industrial investiga el efecto de cuatro métodos de ensamblaje 
#(A, B, C y D) sobre el tiempo de ensamblaje de un componente de televisores a 
#color. Se seleccionan cuatro operadores para el estudio. Además, el ingeniero 
#sabe que todos los métodos de ensamblaje producen fatiga, de tal modo que el 
#tiempo requerido para el último ensamblaje puede ser mayor que para el primero,
#independientemente del método. Es decir, se desarrolla una tendencia en el
#tiempo de ensamblaje requerido. Para tomar en cuenta esta fuente de variabilidad,
#el ingeniero emplea el diseño del cuadrado latino que se presenta a continuación.

#Analizar los datos de este experimento (a = 0.05) y sacar las conclusiones apropiadas.

datos$ORDEN<- as.factor(datos$ORDEN)
datos$OPERADOR<- as.factor(datos$OPERADOR)
datos$ENSAMBLAJE<- as.factor(datos$ENSAMBLAJE)

datos

modelo <- aov(TIEMPO ~ ORDEN + OPERADOR + ENSAMBLAJE, data=datos)
summary(modelo)


boxplot(TIEMPO ~ ORDEN,data = datos)
boxplot(TIEMPO ~ OPERADOR,data = datos)
boxplot(TIEMPO ~ ENSAMBLAJE,data = datos)




# Supuesto de normalidad
qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)


#Supuesto de homogeneidad de varianza

plot(modelo$residuals)
abline(h=0)


bartlett.test(TIEMPO ~ ENSAMBLAJE, data = datos)


# Boxplot de residuos

plot(datos$ENSAMBLAJE,modelo$residuals)
abline(h=0)

# Prueba Post-Hoc: LSD

library(agricolae)
LSD<-LSD.test(modelo,"ENSAMBLAJE")
print(LSD)
plot(LSD)

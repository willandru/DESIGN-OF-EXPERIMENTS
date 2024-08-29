########################################################################################
########################################################################################
################################ ANOVA EFECTOS ALEATORIOS  #############################
########################################################################################
########################################################################################

library(readxl)
library(tidyr)


datos<- read_excel("datos_anova.xlsx")

datos <- as.data.frame(t(datos)[2:ncol(datos),])
datos

data_long <- gather(datos, Telar, factor_key=TRUE)
data_long$Telar <- as.factor(data_long$Telar)
data_long

# Boxplot con cruce de respuesta y factor    
boxplot(data_long$value ~ data_long$Telar, main="Boxplot de resistencia seg?n telar",
        ylab="Resistencia", xlab="Telar")


# ANOVA para contenido de calcio

anova<- aov(data_long$value ~ data_long$Telar, data = data_long)

summary(anova)

# Estimadores de variabilidad

# Componentes de variabilidad

MSEtratamiento <- summary(anova)[[1]]["data_long$Telar","Mean Sq"]
MSEtratamiento
MSEerror <- summary(anova)[[1]]["Residuals","Mean Sq"]
MSEerror
sigma_error <- MSEerror
sigma_tratamientos <- (MSEtratamiento - MSEerror)/4
sigma_error
sigma_tratamientos

# Intervalo de confianza

media <- sigma_tratamientos/(sigma_tratamientos+ sigma_error)
media # La varianza total de los tratamientos explica un 78% de la varianza total.
L <- (1/4)*(((MSEtratamiento/MSEerror)*(1/ qf(0.025,4-1,16-4)))-1)

U <- (1/4)*(((MSEtratamiento/MSEerror)*(1/ qf(0.975,4-1,16-4)))-1)

L/(L+1)
U/(U+1)

#Variabilidad del factor explica entre el 38% y el 98% de la variabilidad de las observaciones

#peso para la variabilidad del tratamiento 
#la variabilidad que se induce por los tratamientoos puede llegar a explicar entre el 38 -98%

#EJERCICIO 1

# Un fabricante sospecha que los lotes de materia prima suministrados por su proveedor
#difieren de manera significativa en el contenido de calcio. Hay un gran número de 
#lotes actualmente en el almacén. Se seleccionan cinco de ellos para hacer un estudio.
#Un químico hace cinco determinaciones en cada lote y obtiene los resultados en el archivo 
#de Excel adjunto (E1).

datos<- read_excel("datos_anova.xlsx", sheet='E1')
datos
str(datos)
datos<- as.data.frame(datos[1:ncol(datos),])
datos


data_long <- gather(datos, Lote, factor_key=TRUE)
data_long$Lote <- as.factor(data_long$Lote)
data_long
# Boxplot con cruce de respuesta y factor    
boxplot(data_long$value ~ data_long$Lote, main="Boxplot de contenido de calcio segun lote",
        ylab="Calcio", xlab="Lote")

  #¿Existe una variación significativa en el contenido de calcio de un lote a otro? 
  #Utilizarás un nivel de significancia de 𝛼=0.05.

  anova<- aov(data_long$value ~ data_long$Lote, data = data_long)
  summary(anova)

# Estimadores de variabilidad - Componentes de variabilidad

MSEtratamiento <- summary(anova)[[1]]["data_long$Lote","Mean Sq"]
MSEtratamiento
MSEerror <- summary(anova)[[1]]["Residuals","Mean Sq"]
MSEerror
sigma_error <- MSEerror
sigma_tratamientos <- (MSEtratamiento - MSEerror)/4
sigma_error
sigma_tratamientos

  #Estimar los componentes de la varianza.

  #Encontrar un intervalo de confianza del 95% para el promedio del contenido de calcio.

media <- sigma_tratamientos/(sigma_tratamientos+ sigma_error)
media # La varianza total de los tratamientos explica un 78% de la varianza total.
L <- (1/4)*(((MSEtratamiento/MSEerror)*(1/ qf(0.025,4-1,16-4)))-1)
U <- (1/4)*(((MSEtratamiento/MSEerror)*(1/ qf(0.975,4-1,16-4)))-1)
L/(L+1)
U/(U+1)

  #Analizar los residuales de este experimento. ¿Se satisfacen los supuestos del análisis de varianza?

residuales <- anova$residuals
qqnorm(residuales)
qqline(residuales)
shapiro.test(residuales) # Residuales son normales


#EJERCICIO 2

# En un artículo de Journal of Quality Technology (vol. 13, no. 2, pp. 111-114) 
#se describe un experimento para investigar los efectos de cuatro sustancias químicas
#blanqueadoras sobre la brillantez de la pulpa. Estas cuatro sustancias químicas se
#seleccionaron al azar de una población grande de agentes blanqueadores potenciales. 
#Los datos se encuentran en el archivo de Excel adjunto (E2).

#a) ¿Existe alguna diferencia en los tipos de sustancias químicas? Utilizarás un
#nivel de significancia de 𝛼=  0.05
datos<- read_excel("datos_anova.xlsx", sheet='E2')
datos
str(datos)
datos<- as.data.frame(t(datos)[2:ncol(datos),])
str(datos)
datos


data_long <- gather(datos, Blanqueador, factor_key=TRUE)
data_long$Blanqueador <- as.factor(data_long$Blanqueador)
data_long

anova<- aov(data_long$value ~ data_long$Blanqueador, data = data_long)
summary(anova)
#b) Estimar la variabilidad debida al tipo de sustancias químicas.

MSEtratamiento <- summary(anova)[[1]]["data_long$Blanqueador","Mean Sq"]
MSEtratamiento
MSEerror <- summary(anova)[[1]]["Residuals","Mean Sq"]
MSEerror
sigma_error <- MSEerror
sigma_error
sigma_tratamientos <- (MSEtratamiento - MSEerror)/4
sigma_tratamientos



#c) Estimar la variabilidad debida al error aleatorio.

media <- sigma_tratamientos/(sigma_tratamientos+ sigma_error)
media # La varianza total de los tratamientos explica un 78% de la varianza total.
L <- (1/4)*(((MSEtratamiento/MSEerror)*(1/ qf(0.025,4-1,16-4)))-1)
U <- (1/4)*(((MSEtratamiento/MSEerror)*(1/ qf(0.975,4-1,16-4)))-1)
L/(L+1)
U/(U+1)

#d) Analizar los residuales de este experimento y comentar la adecuación del modelo.
residuales <- anova$residuals
qqnorm(residuales)
qqline(residuales)

par(mfrow(2,2))
plot(anova)
par(mfrow(1,1))
shapiro.test(residuales)
bartlett.test(value ~ Blanqueador, data=data_long)



#Ejercicio 3 – Efectos Fijos

#Se estudia la resistencia a la tensión del cemento Portland. Puedes usar económicamente
#cuatro diferentes técnicas de mezclado. Se han colectado los datos adjuntos en el archivo Excel (E3).

datos<- read_excel("datos_anova.xlsx", sheet='E3')
datos
datos<- as.data.frame(t(datos)[2:ncol(datos),])
datos


data_long <- gather(datos, Resistencia, factor_key=TRUE)
data_long$Resistencia <- as.factor(data_long$Resistencia)
data_long

boxplot(value ~Resistencia , data=data_long)

#a) Probar la hipótesis de que las técnicas de mezclado afectan la resistencia del cemento.
#Utiliza un nivel de significancia de α=0.05.

anova<- aov(data_long$value ~ data_long$Resistencia, data = data_long)
summary(anova)


#b) Usa el método de Tukey con α=0.05 para hacer comparaciones entre pares de medias.

posthoc_result <- TukeyHSD(anova)
print(posthoc_result)


install.packages('agricolae')
library(agricolae)
LSD <- LSD.test(anova, "data_long$Resistencia")
print(LSD)
plot(LSD)
# Gr?fico de los intervalos de confianza de las diferencias entre las medias
plot(posthoc_result)

#c) Construir un gráfico de probabilidad normal de los residuales. ¿Qué conclusiones
#se pueden sacar acerca de la validez del supuesto de normalidad?

residuales <- anova$residuals
qqnorm(residuales)
qqline(residuales)
  
plot(anova$fitted.values, anova$residuals)
abline(h=0)
#d) Graficar los residuales contra la resistencia a la tensión predicha. Comentar la gráfica.

shapiro.test(residuales)
bartlett.test(value ~ Resistencia, data=data_long)


#Ejercicio 4 – Prueba de Hipótesis

#A continuación, se presenta el tiempo de combustión de dos cohetes químicos con
#formulaciones diferentes. Los ingenieros de diseño están interesados tanto en la
#media como en la varianza del tiempo de combustión. Los datos se encuentran en el
#archivo Excel adjunto (E4).


datos_1<- datos[,c(1,3)]
datos_2<- datos[,c(2,4)]
names(datos_1)<-c("Tipo 1", "Tipo 2")
names(datos_2)<-c("Tipo 1", "Tipo 2")
datos<-rbind(datos_1, datos_2)
data_long<- gather(datos)
data_long

datos<- read_excel("datos_anova.xlsx", sheet='E4')
datos
datos<- as.data.frame(datos)
datos



#a) Probar la hipótesis de que las dos varianzas son iguales. Utiliza un nivel de
#significancia de α=0.05.
#H0: The variances in each of the groups are the same
bartlett.test(datos)
?bartlett.test

var1<- var(datos$`Tipo 1`)
var2<- var(datos$`Tipo 2`)

Fstais<- var1/var2
Fstais
Fscore<- qf(0.95, 9,9)
Fscore

#comparar areas
pf(Fstais, 9, 9)*2
#comparar limiters

Fprueba < qf(1-(alpha/2), 9, 9)
Fprueba > qf((alpha/2), 9, 9)


#op3
var.test(datos$`Tipo 1`, datos$`Tipo 2`, 2)
  #RTA: Sin son igual varianza pues no hay evidencia significativa para decir que las varianzas no son iguales.
#b) Utilizando los resultados del inciso (a), probar la hipótesis de que los tiempos
#de combustión promedio son iguales. Utiliza un nivel de significancia de α=0.05.
#¿Cuál es el valor p para esta prueba?



t.test(datos$`Tipo 1`,datos$`Tipo 2`, var.equal =FALSE, alternative = "two.sided")
?t.test
  
#c) Comentar el papel del supuesto de normalidad en este problema.
#Verificar el supuesto de normalidad para ambos tipos de cohetes.



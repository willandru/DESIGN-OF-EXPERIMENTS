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
datos<- as.data.frame(t(datos))
str(datos)
datos
#b) Estimar la variabilidad debida al tipo de sustancias químicas.

#c) Estimar la variabilidad debida al error aleatorio.

#d) Analizar los residuales de este experimento y comentar la adecuación del modelo.



#Ejercicio 3 – Efectos Fijos

#Se estudia la resistencia a la tensión del cemento Portland. Puedes usar económicamente
#cuatro diferentes técnicas de mezclado. Se han colectado los datos adjuntos en el archivo Excel (E3).

#a) Probar la hipótesis de que las técnicas de mezclado afectan la resistencia del cemento.
#Utiliza un nivel de significancia de α=0.05.

#b) Usa el método de Tukey con α=0.05 para hacer comparaciones entre pares de medias.

#c) Construir un gráfico de probabilidad normal de los residuales. ¿Qué conclusiones
#se pueden sacar acerca de la validez del supuesto de normalidad?
  
#d) Graficar los residuales contra la resistencia a la tensión predicha. Comentar la gráfica.




#Ejercicio 4 – Prueba de Hipótesis

#A continuación, se presenta el tiempo de combustión de dos cohetes químicos con
#formulaciones diferentes. Los ingenieros de diseño están interesados tanto en la
#media como en la varianza del tiempo de combustión. Los datos se encuentran en el
#archivo Excel adjunto (E4).

#a) Probar la hipótesis de que las dos varianzas son iguales. Utiliza un nivel de
#significancia de α=0.05.

#b) Utilizando los resultados del inciso (a), probar la hipótesis de que los tiempos
#de combustión promedio son iguales. Utiliza un nivel de significancia de α=0.05.
#¿Cuál es el valor p para esta prueba?
  
#c) Comentar el papel del supuesto de normalidad en este problema.
#Verificar el supuesto de normalidad para ambos tipos de cohetes.



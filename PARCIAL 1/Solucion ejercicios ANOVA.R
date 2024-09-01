####################################################################
######################### Ejercicio 1 ##############################
####################################################################

library(readxl)
library(tidyr)

datos <- read_excel("C:/Users/adrodriguez/Downloads/datos_anova.xlsx", sheet="E1")

data_long <- gather(datos, factor_key=TRUE)
data_long$key <- as.factor(data_long$key)


# a) diferencia significativa

anova<- aov(data_long$value ~ data_long$key, data = data_long)

summary(anova)

# Con un p-value de 0.3% y nivel de significancia de 5% existe suficiente evidencia 
# estadistica para probar una diferencia significativa en la calidad de materia 
# prima a aprtir del lote de origen

# b) componentes de varianza

replicas <- 5

MSEtratamiento <- summary(anova)[[1]]["data_long$key","Mean Sq"] 
MSEerror <- summary(anova)[[1]]["Residuals","Mean Sq"]
sigma_error <- MSEerror
sigma_tratamientos <- (MSEtratamiento - MSEerror)/replicas
sigma_error
sigma_tratamientos

# c) intervalo de confianza

alpha <- 0.05
N <- nrow(data_long)
niveles <- 5

media <- sigma_tratamientos/(sigma_tratamientos+ sigma_error)

L <- (1/4)*(((MSEtratamiento/MSEerror)*(1/ qf(alpha/2,niveles-1,N-niveles)))-1)

U <- (1/4)*(((MSEtratamiento/MSEerror)*(1/ qf(1-(alpha/2),niveles-1,N-niveles)))-1)

L/(L+1)
U/(U+1)


# La varianza del factor explica entre el 12% y el 92% de la varianza total

# d) Analisis de residuos

par(mfrow = c(2, 2))
plot(anova)
par(mfrow = c(1, 1)) 

#Normalidad
#H0: Los datos provienen de una distribución normal
shapiro.test(anova$residuals)

#Homocedasticidad

# H0: Las varianzas de los grupos son iguales
bartlett.test(value ~ key, data = data_long)


####################################################################
######################### Ejercicio 2 ##############################
####################################################################

datos <- read_excel("C:/Users/adrodriguez/Downloads/datos_anova.xlsx", sheet="E2")


datos <- as.data.frame(t(datos)[2:ncol(datos),])
rownames(datos) <- NULL  

data_long <- gather(datos, factor_key=TRUE)
data_long$key<- as.factor(data_long$key)


# a) ¿Existe diferencia estadistica?

anova<- aov(data_long$value ~ data_long$key, data = data_long)

summary(anova)

# R/ No existe evidencia estadistica de que haya diferencia en la birllantez de la pulpa a partir del producto aplicado.


# b) Variabilidad debido al tratamiento

replicas <- 5

MSEtratamiento <- summary(anova)[[1]]["data_long$key","Mean Sq"] 
MSEerror <- summary(anova)[[1]]["Residuals","Mean Sq"]
sigma_tratamientos <- (MSEtratamiento - MSEerror)/replicas
sigma_tratamientos


# c) Variabilidad debido al error

MSEerror <- summary(anova)[[1]]["Residuals","Mean Sq"]
sigma_error <- MSEerror
sigma_error

# d) Analisis de residuos

par(mfrow = c(2, 2))
plot(anova)
par(mfrow = c(1, 1)) 

#Normalidad
#H0: Los datos provienen de una distribución normal
shapiro.test(anova$residuals)

#Homocedasticidad

# H0: Las varianzas de los grupos son iguales
bartlett.test(value ~ key, data = data_long)


####################################################################
######################### Ejercicio 3 ##############################
####################################################################


datos <- read_excel("C:/Users/adrodriguez/Downloads/datos_anova.xlsx", sheet="E3")


datos <- as.data.frame(t(datos)[2:ncol(datos),])
rownames(datos) <- NULL  

data_long <- gather(datos, factor_key=TRUE)
data_long$key<- as.factor(data_long$key)



#a) Probar diferencias estadisticas

anova<- aov(data_long$value ~ data_long$key, data = data_long)

summary(anova)

# Si existe diferencia significativa al 5% y hay evidencia estadistica de que la tecnica de mezclado afecta la resistencia del cemento.


#b) Metodo Tukey


print(TukeyHSD(anova))

# El metodo Tukey permite observar que:
# la tecnica de mezclado 1 aporta más resistencia que la tecnica 4 
# la tecnica de mezclado 2 aporta más resistencia que la tecnica 4 
# la tecnica de mezclado 3 aporta más resistencia que la tecnica 4 

# ADICIONAL: prueba LSD

# Poner especial cuidado en el componente de $group.El metodo LSD asigna letras diferentes
# a las medias que son diferentes

LSD<-LSD.test(anova,"data_long$key")
print(LSD)
plot(LSD)

# Refuerza que el tipo de mezclado 2 es más resistente, el 4 es el más pequeño.

# Tukey es más conservadora, menor probabilidad de error tipo 1, pero mayor probabilidad de no detectar diferencias que si existen
# LSD es mas potente, detecta diferencias que existen más facil, pero mayor probabilidad de error tipo 1 


#c) Grafica de probabilidad de residuales

qqnorm(anova$residuals)
qqline(anova$residuals)

#d) Graficar residuales vs fitted values

plot(anova$fitted.values, anova$residuals)
abline(h=0)


####################################################################
######################### Ejercicio 4 ##############################
####################################################################



datos <- read_excel("C:/Users/adrodriguez/Downloads/datos_anova.xlsx", sheet="E4")

datos_1 <- datos[,c(1,3)]
datos_2 <- datos[,c(2,4)]

names(datos_1)<-c("Tipo 1", "Tipo 2")
names(datos_2)<-c("Tipo 1", "Tipo 2")

datos <- rbind(datos_1, datos_2)

data_long <- gather(datos)
data_long$key<- as.factor(data_long$key)


######### a) Prueba de varianzas

# Opcion 1:
# H0: Las varianzas de los grupos son iguales
bartlett.test(value ~ key, data = data_long)

# Opcion 2: prueba de F de Fisher
# H0: varianzas iguales
# Prueba de hipotesis a mano

data_long<- as.data.frame(data_long)

S1 <- sd(data_long[data_long$key=="Tipo 1",2])
S2 <- sd(data_long[data_long$key=="Tipo 2",2])

Fprueba <- (S1^2)/(S2^2)

# Calculo de p-value
alpha <- 0.05
n <- nrow(data_long[data_long$key=="Tipo 1",])

pf(Fprueba, n-1, n-1)*2

# Calculo de limites
Fprueba < qf(1-(alpha/2), n-1, n-1) 
Fprueba > qf((alpha/2), n-1, n-1) 

# Opción 3: prueba de hipotesis en R

var.test(data_long[data_long$key=="Tipo 1",2], data_long[data_long$key=="Tipo 2",2])


# R/No se rechaza la hipotesis nula, por lo que las varianzas estadisticamente son iguales.


######### b) Prueba de medias

# Opción 1: prueba de hipotesis t manual

Sp <- sqrt(((n-1)*(S1^2) + (n-1)*(S2^2))/(n+n-2))

x1 <- mean(data_long[data_long$key=="Tipo 1",2])
x2 <- mean(data_long[data_long$key=="Tipo 2",2])
n <- nrow(data_long[data_long$key=="Tipo 1",])


t <- (x1 - x2)/(Sp*(sqrt((1/n)+(1/n))))

# Calculo de p-value
alpha <- 0.05
n <- nrow(data_long[data_long$key=="Tipo 1",])

pt(-t, n+n-2)*2


# Calculo de limites
t < qt(1-(alpha/2), n+n-2) 
t > qt((alpha/2), n+n-2) 


# Opción 2: prueba de hipotesis t R

t.test(data_long[data_long$key=="Tipo 1",2], data_long[data_long$key=="Tipo 2",2])

######### c) Supuesto de normalidad


#Normalidad
#H0: Los datos provienen de una distribución normal
shapiro.test(data_long[data_long$key=="Tipo 1",2])

#H0: Los datos provienen de una distribución normal
shapiro.test(data_long[data_long$key=="Tipo 2",2])


































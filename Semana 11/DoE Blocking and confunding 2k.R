
######################################################################################################################
######################################################################################################################
################################################ BLOQUES EN DISEÑOS 2K ###############################################
######################################################################################################################
######################################################################################################################


f1 <- c(-1, 1)
f2 <- c(-1, 1) 
f3 <- c("Bloque 1", "Bloque 2", "Bloque 3")
k1 = length(f1)          # number of 1st factors 
k2 = length(f2)          # number of 2st factors 
k3 =length(f3)

r <- c(28,25,27,36,32,32,18,19,23,31,30,29)

n=k3
A = rep(f1, each=n, len=n*k1*k2) 
A
B = rep(f2, each=n*k2, len=n*k1*k2) 
B
Bloques <- gl(n=k3, k=1, length = n*k1*k2, labels = f3)
Bloques


datos <- cbind(A,B,Bloques, r)

av = aov(r ~ A*B + Bloques)  # include interaction
summary(av)

##################################################################
#### ¿Porqué es importante la formación de bloques?

f1 <- c(-1, 1)
f2 <- c(-1, 1) 
f3 <- c("Bloque 1", "Bloque 2", "Bloque 3")
k1 = length(f1)          # number of 1st factors 
k2 = length(f2)          # number of 2st factors 
k3 =length(f3)

r <- c(28,25,27+10,36,32,32+10,18,19,23+10,31,30,29+10)

n=k3
A = rep(f1, each=n, len=n*k1*k2) 
A
B = rep(f2, each=n*k2, len=n*k1*k2) 
B
Bloques <- gl(n=k3, k=1, length = n*k1*k2, labels = f3)
Bloques

av = aov(r ~ A*B + Bloques)  # incluye interaccion y bloque
summary(av)

av = aov(r ~ A*B )  # incluye interaccion pero no bloque
summary(av)

####################################################################
########## Modelo Original Tasa de filtración - 1 sola replica
 
mydata <- expand.grid( A=c(-1,1), B=c(-1,1), C=c(-1,1), D=c(-1,1) )
mydata$y <- c(45,71,48,65,68,60,80,65,43,100,45,104,75,86,70,96)
mydata
fit <- lm( y ~ .^4 , data=mydata )
summary(fit)
anova(fit)

Coeff<- coefficients(fit)
Coeficientes <- Coeff[1:length(Coeff)]
Coeficientes
Eff <- 2*coefficients(fit) #Efectos de cada factor
Efectos <- Eff[1:length(Eff)]
Efectos

tt <- anova(fit)

SumaCuadrados <- tt[[2]][1:nrow(tt)-1] # Suma de cuadrados
SumaCuadrados
Porc_Contribuciones <- 100*SumaCuadrados/sum(SumaCuadrados) # Porcentajes de contribución
Porc_Contribuciones

df_Efectos <- data.frame(Coeficientes, Efectos, SumaCuadrados, Porc_Contribuciones)
df_Efectos

library(DoE.base)
library(FrF2)

av <- aov(y ~ A*B*C*D , data=mydata )
MEPlot(av) # Efectos principales
IAPlot(av) # Diagrama de Interacción

library(FrF2)
FrF2::DanielPlot(av) #Normal Probability Plot of effects
FrF2::DanielPlot(av, half = T)  #Half Normal Probability Plot of effects

library(BsMD)
BsMD::LenthPlot(av)

######################################MODELO SIMPLIFICADO
fit <- lm( y ~ A+C+D+A:C+A:D, data=mydata )
summary(fit)
anova(fit)

layout(matrix(c(1:4),nrow=2, ncol=2, byrow=T)) 
plot(fit)
par(mfrow=c(1,1)) # Parámetros gráficos iniciales

library(lmtest)
lmtest::bptest(fit)  # Breusch-Pagan test


shapiro.test( residuals(fit))  
# Si p valor > 0.05 no podemos rechazar que la distribución sea normal.


#########################################################################
# Confusión bloque + ABCD
####################################################################
mydata <- expand.grid( A=c(-1,1), B=c(-1,1), C=c(-1,1), D=c(-1,1) )
mydata$y <- c(45,71,48,65,68,60,80,65,43,100,45,104,75,86,70,96)

mydata$ABCD <- with(mydata, A*B*C*D)
#Reduce en 20 las interacciones confundidas con ABCD
mydata[mydata$ABCD==1,"y"] <- mydata[mydata$ABCD==1,"y"] - 20

#elimina la columna ABCD
mydata <- subset( mydata, select = -ABCD )

mydata
fit <- lm( y ~ .^4, data=mydata )
summary(fit)
anova(fit)

Coeff<- coefficients(fit)
Coeficientes <- Coeff[1:length(Coeff)]
Coeficientes
Eff <- 2*coefficients(fit) #Efectos de cada factor
Efectos <- Eff[1:length(Eff)]
Efectos

tt <- anova(fit)

SumaCuadrados <- tt[[2]][1:nrow(tt)-1] # Suma de cuadrados
SumaCuadrados
Porc_Contribuciones <- 100*SumaCuadrados/sum(SumaCuadrados) # Porcentajes de contribución
Porc_Contribuciones

df_Efectos <- data.frame(Coeficientes, Efectos, SumaCuadrados, Porc_Contribuciones)
df_Efectos

library(DoE.base)
library(FrF2)

av <- aov(y ~ A*B*C*D , data=mydata )
MEPlot(av) # Efectos principales
IAPlot(av) # Diagrama de Interacción

library(FrF2)
FrF2::DanielPlot(av) #Normal Probability Plot of effects
FrF2::DanielPlot(av, half = T)  #Half Normal Probability Plot of effects

library(BsMD)
BsMD::LenthPlot(av)

######################################MODELO SIMPLIFICADO
fit <- lm( y ~ A+C+D+A:C+A:D+A:B:C:D , data=mydata )
summary(fit)
anova(fit)

layout(matrix(c(1:4),nrow=2, ncol=2, byrow=T)) 
plot(fit)

#################################################################
###################################################################
###IMPORTANCIA DE FORMACION EN BLOQUES

mydata <- expand.grid( A=c(-1,1), B=c(-1,1), C=c(-1,1), D=c(-1,1) )
mydata$y <- c(45,71,48,65,68,60,80,65,43,100,45,104,75,86,70,96)
mydata$orden <- c(8,11,1,3,9,12,2,13,7,6,16,5,14,15,10,4)

mydata$Afectados <- (mydata$orden <= 8)

#Reduce en 20 las 8 primeras corridas
mydata[mydata$Afectados,"y"] <- mydata[mydata$Afectados,"y"] - 20

#elimina la columna Afectados
mydata <- subset( mydata, select = -Afectados )

mydata

fit <- lm( y ~ A*B*C*D, data=mydata )
summary(fit)
anova(fit)

Coeff<- coefficients(fit)
Coeficientes <- Coeff[1:length(Coeff)]
Coeficientes
Eff <- 2*coefficients(fit) #Efectos de cada factor
Efectos <- Eff[1:length(Eff)]
Efectos

tt <- anova(fit)

SumaCuadrados <- tt[[2]][1:nrow(tt)-1] # Suma de cuadrados
SumaCuadrados
Porc_Contribuciones <- 100*SumaCuadrados/sum(SumaCuadrados) # Porcentajes de contribución
Porc_Contribuciones

df_Efectos <- data.frame(Coeficientes, Efectos, SumaCuadrados, Porc_Contribuciones)
df_Efectos

library(DoE.base)
library(FrF2)

av <- aov(y ~ A*B*C*D , data=mydata )
MEPlot(av) # Efectos principales
IAPlot(av) # Diagrama de Interacción

library(FrF2)
FrF2::DanielPlot(av) #Normal Probability Plot of effects
FrF2::DanielPlot(av, half = T)  #Half Normal Probability Plot of effects

library(BsMD)
BsMD::LenthPlot(av)

# No se reconoce la importancia de la interacción A:D o la de D
# Sin usar bloques adecuadamente se puede llegar a conclusiones erroneas


############################################################### BLOQUES en 2 a la k (k=3, 2 bloques) con n=2 replicas
n <- 2
f1 <- c(-1,1)
f2 <- c(-1,1)
f3 <- c(-1,1)
replicas <- c(1,2)

k1 <- length(f1)
k2 <- length(f2)
k3 <- length(f3)
k4 <- length(replicas)

A <- rep(f1, each = n, len = n*k1*k2*k3)    # first 4 only.
B <- rep(f2, each = n*k2, len = n*k1*k2*k3) 
C <- rep(f3, each = n*k1*k2, len = n*k1*k2*k3) 
ABC <- A*B*C
ABC <- as.factor(ABC)

A <- gl(k1, n, n*k1*k2*k3, factor(f1)) 
B <- gl(k2, n*k2, n*k1*k2*k3, factor(f2))
C <- gl(k3, n*k1*k2, n*k1*k2*k3, factor(f3))
replicas <- gl(k4, 1, n*k1*k2*k3, factor(replicas))

mydata <- data.frame(A,B,C,replicas, ABC)
mydata

mydata$r <- c(50,54,44,42,  46,48, 42,43,
              49,46,48,45,47,48,56,54)
              
mydata

av = aov(r ~ A+B+C + A:B+ A:C + B:C + ABC + ABC:replicas + replicas, data=mydata)  # include interaction
summary(av)

av = aov(r ~ A*B*C, data=mydata)  # include interaction
summary(av)

###############################################################2 BLOQUES en 2 a la k (k= 3) con n=4 replicas
## 1 factor perurbador de 2 niveles
n <- 4
f1 <- c(-1,1)
f2 <- c(-1,1)
f3 <- c(-1,1)
replicas <- c(1,2,3,4)

k1 <- length(f1)
k2 <- length(f2)
k3 <- length(f3)
k4 <- length(replicas)

A <- rep(f1, each = n, len = n*k1*k2*k3)    # first 4 only.
B <- rep(f2, each = n*k2, len = n*k1*k2*k3) 
C <- rep(f3, each = n*k1*k2, len = n*k1*k2*k3) 
ABC <- A*B*C
ABC <- as.factor(ABC)

A <- gl(k1, n, n*k1*k2*k3, factor(f1)) 
B <- gl(k2, n*k2, n*k1*k2*k3, factor(f2))
C <- gl(k3, n*k1*k2, n*k1*k2*k3, factor(f3))
replicas <- gl(k4, 1, n*k1*k2*k3, factor(replicas))

mydata <- data.frame(A,B,C,replicas,ABC)
mydata
str(mydata)

mydata$r <- c(50,54,60,64,44,42, 54,52, 46,48,56,58, 42,43,52,53,
              49,46,59,56,48,45,58,55,47,48,67,68,56,54, 66,74)

# BLoques confundidos con ABC

mydata

av = aov(r ~ A+B+C + A:B+ A:C + B:C + ABC + ABC:replicas + replicas, data=mydata)  # include interaction
summary(av)

av = aov(r ~ A*B*C, data=mydata)  # include interaction
summary(av)






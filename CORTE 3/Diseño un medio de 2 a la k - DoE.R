###################################################################################################
#################################### DoE Diseño 1/2 de 2k #########################################
###################################################################################################


########################################## Ejemplo 1
mydata <- expand.grid( A=c(-1,1), B=c(-1,1), C=c(-1,1))
mydata$D <- with (mydata, A*B*C)
mydata$y <- c(45,100,45,65,75,60,80,96)
mydata <- as.data.frame(mydata)

# Primer intento, correr el diseño completo
fit <- lm( y ~ .^4  , data=mydata )
summary(fit)
av <- aov( y ~ .^4  , data=mydata )
summary(av)


# Segundo intento, correr el diseño con información presente
fit <- lm( y ~ A+B+C+D+A:B+A:C+A:D  , data=mydata )
summary(fit)
av <- aov( y ~ A+B+C+D+A:B+A:C+A:D   , data=mydata )
summary(av)


library(DoE.base)
library(FrF2)

MEPlot(fit) # Efectos principales
IAPlot(fit, select = c(1,2,3,4)) # Diagrama de Interacción

Coeff<- coefficients(fit)
Coeficientes <- Coeff[2:length(Coeff)]
Coeficientes
Eff <- 2*Coeficientes #Efectos de cada factor
Efectos <- Eff[1:length(Eff)]
Efectos

tt <- anova(fit)

SumaCuadrados <- tt[[2]][1:nrow(tt)-1] # Suma de cuadrados
SumaCuadrados
Porc_Contribuciones <- 100*SumaCuadrados/sum(SumaCuadrados) # Porcentajes de contribución
Porc_Contribuciones

df_Efectos <- data.frame(Coeficientes, Efectos, SumaCuadrados, Porc_Contribuciones)
df_Efectos

library(FrF2)
FrF2::DanielPlot(av, autolab = TRUE, alpha=0.05) #Normal Probability Plot of effects
FrF2::DanielPlot(av, half = T, autolab = TRUE)  #Half Normal Probability Plot of effects


###HALF Normal Probability PLot MANUAL

abs_coef <- abs(Coeficientes)
tmp <- qqnorm( abs_coef )
#er <- qqline(  abs_coef  )
abline(v = 0)
abline(h = 0)
text( tmp$x, tmp$y, names( abs_coef ), pos=1 )

##################################################

fit <- lm( y ~ A+C+D+A:C+A:D  , data=mydata )
summary(fit)
av <- aov( y ~ A+C+D+A:C+A:D    , data=mydata )
summary(av)

layout(matrix(c(1:4),nrow=2, ncol=2, byrow=T)) 
plot(fit)

par(mfrow=c(1,1)) # Parámetros gráficos iniciales



########################################## Ejemplo 2

library(FrF2)
## Diseño con 16 corridas, 6 factores y dos generadores: E=ABC y F=BCD
myDesign <- FrF2(nruns=16, nfactors= 6, generators=c("ABC","BCD"), randomize=F)

design.info(myDesign)

MIr <- c(36.6, 35.4, 35.3, 35.8, 34.7, 34.6, 38.6, 38.1, 38.8, 38.9,
 39.5, 39.6, 36, 36, 35.1, 35.2)

myDesign <- add.response(myDesign, MIr)

summary(myDesign)
design.info(myDesign)

## Efectos principales
MEPlot(myDesign, abbrev = 5, cex.xax = 1.6, cex.main = 2)

## Interacciones dobles con alias
IAPlot(myDesign, abbrev = 5, show.alias = TRUE, lwd = 2, cex = 2,
  cex.xax = 1.2, cex.lab = 1.5)

fit <- lm(myDesign)
summary(fit)

myData <- data.frame(myDesign)
fit <- lm(MIr ~.^6, data=myData)
summary(fit)

aliases(fit)

FrF2::DanielPlot(fit,code=TRUE,alpha=0.05)

fit <- lm(MIr ~ B + C + D + F + A:E + B:D + B:F, data=myData)
summary(fit)

fit <- aov(MIr ~ B + C + D + F + A:E + B:D + B:F, data=myData)
summary(fit)


#################################### Ejemplo pliegue completo

# Fracción principal
myData <- expand.grid( A=c(-1,1), B=c(-1,1), C=c(-1,1))

## Generadores
myData$D <- with (myData, A*B)
myData$E <- with (myData, A*C)
myData$F <- with (myData, B*C)
myData$G <- with (myData, A*B*C)
myData$r <- c(85.5, 75.1, 93.2, 145.4 , 83.7, 77.6, 95, 141.8)
str(myData)
myData


fit <- lm(r ~.^7, data=myData)
summary(fit)

fit <- aov(r ~.^7, data=myData)

MEPlot(fit)

## Interacciones dobles con alias
IAPlot(fit)

summary(fit)
aliases(fit)

FrF2::DanielPlot(fit,code=TRUE,alpha=0.05)

library(BsMD)

BsMD::LenthPlot(fit)


Coeff<- coefficients(fit)
Coeficientes <- Coeff[2:length(Coeff)]
Coeficientes
Eff <- 2*Coeficientes #Efectos de cada factor
Efectos <- Eff[1:length(Eff)]
Efectos

tt <- anova(fit)

SumaCuadrados <- tt[[2]][1:nrow(tt)-1] # Suma de cuadrados
SumaCuadrados
Porc_Contribuciones <- 100*SumaCuadrados/sum(SumaCuadrados) # Porcentajes de contribución
Porc_Contribuciones

df_Efectos_1 <- data.frame(Coeficientes, Efectos, SumaCuadrados, Porc_Contribuciones)
df_Efectos_1

# Fracción secundaria
mydata <- expand.grid( A=c(1,-1), B=c(1,-1), C=c(1,-1))
str(mydata)
mydata

## Generadores
mydata$D <- with (mydata, -A*B)
mydata$E <- with (mydata, -A*C)
mydata$F <- with (mydata, -B*C)
mydata$G <- with (mydata, A*B*C)
mydata$r <- c(91.3, 136.7, 82.4, 73.4, 94.1, 143.8, 87.3, 71.9)
str(mydata)
mydata_2 <- mydata

fit <- lm(r ~.^7, data=mydata)
summary(fit)

fit <- aov(r ~.^7, data=mydata)
summary(fit)
aliases(fit)

MEPlot(fit)

## Interacciones dobles con alias
IAPlot(fit)


FrF2::DanielPlot(fit,code=TRUE,alpha=0.05)

library(BsMD)

BsMD::LenthPlot(fit)


Coeff<- coefficients(fit)
Coeficientes <- Coeff[2:length(Coeff)]
Coeficientes
Eff <- 2*Coeficientes #Efectos de cada factor
Efectos <- Eff[1:length(Eff)]
Efectos

tt <- anova(fit)

SumaCuadrados <- tt[[2]][1:nrow(tt)-1] # Suma de cuadrados
SumaCuadrados
Porc_Contribuciones <- 100*SumaCuadrados/sum(SumaCuadrados) # Porcentajes de contribución
Porc_Contribuciones

df_Efectos_2 <- data.frame(Coeficientes, Efectos, SumaCuadrados, Porc_Contribuciones)
df_Efectos_2

###############################################################
mydata <- rbind(myData, mydata)
mydata

fit <- lm(r ~.^2, data=mydata)
summary(fit)

fit <- aov(r ~.^2, data=mydata)
summary(fit)
aliases(fit)

MEPlot(fit)

## Interacciones dobles con alias
IAPlot(fit)

FrF2::DanielPlot(fit,code=TRUE,alpha=0.05)

library(BsMD)

BsMD::LenthPlot(fit)


Coeff<- coefficients(fit)
Coeficientes <- Coeff[2:length(Coeff)]
Coeficientes
Eff <- 2*Coeficientes #Efectos de cada factor
Efectos <- Eff[1:length(Eff)]
Efectos

tt <- anova(fit)

SumaCuadrados <- tt[[2]][1:nrow(tt)-1] # Suma de cuadrados
SumaCuadrados
Porc_Contribuciones <- 100*SumaCuadrados/sum(SumaCuadrados) # Porcentajes de contribución
Porc_Contribuciones

df_Efectos <- data.frame(Coeficientes, Efectos, SumaCuadrados, Porc_Contribuciones)
df_Efectos





######################################################################################################################
######################################################################################################################
################################################ DISE?OS 2K - 1 REPLICA ##############################################
######################################################################################################################
######################################################################################################################



########################################## Ejemplo de Montgomery Libro Ejemplo 6 2
mydata <- expand.grid( A=c(-1,1), B=c(-1,1), C=c(-1,1), D=c(-1,1) )
mydata$y <- c(45,71,48,65,68,60,80,65,43,100,45,104,75,86,70,96)
mydata

fit <- lm( y ~ .^4  , data=mydata )
summary(fit)

av <- aov( y ~ .^4  , data=mydata )
summary(av)

library(DoE.base)

library(FrF2)

MEPlot(fit) # Efectos principales
IAPlot(fit, select = c(1,2,3,4)) # Diagrama de Interacci?n

Coeff<- coefficients(fit)
Coeficientes <- Coeff[2:length(Coeff)]
Coeficientes
Eff <- 2*coefficients(fit) #Efectos de cada factor
Efectos <- Eff[2:length(Eff)]
Efectos

tt <- anova(fit)

SumaCuadrados <- tt[[2]][1:nrow(tt)-1] # Suma de cuadrados
SumaCuadrados
Porc_Contribuciones <- 100*SumaCuadrados/sum(SumaCuadrados) # Porcentajes de contribuci?n
Porc_Contribuciones

df_Efectos <- data.frame(Coeficientes, Efectos, SumaCuadrados, Porc_Contribuciones)
df_Efectos

#confint(fit) #intervalos ajustados de confianza del 95%

library(FrF2)

par(mfrow=c(1,1)) # Par?metros gr?ficos iniciales
FrF2::DanielPlot(fit) #Normal Probability Plot of effects
FrF2::DanielPlot(fit, half = T)  #Half Normal Probability Plot of effects

###Normal Probability PLot MANUAL
tmp <- qqnorm( coef(fit) )
#er <- qqline( coef(fit) )
abline(v = 0)
abline(h = 0)
text( tmp$x, tmp$y, names(coef(fit)), pos=3 )

###HALF Normal Probability PLot MANUAL

abs_coef <- abs(coef(fit))
tmp <- qqnorm( abs_coef )
#er <- qqline(  abs_coef  )
abline(v = 0)
abline(h = 0)
text( tmp$x, tmp$y, names( abs_coef ), pos=3 )

library(BsMD)

par(mfrow=c(1,1)) # Par?metros gr?ficos iniciales

av <- aov( y ~ .^4 , data=mydata )
summary(av)
BsMD::LenthPlot(av) # Lenth Plot

##############################################################################

fit2 <- lm( y ~ A*C*D, data=mydata ) #modelo simplificado a partir de normal pp plot
summary(fit2)
anova(fit2)

fit3 <- lm( y ~ A+C+D+A:C+A:D, data=mydata ) #modelo simplificado a partir de normal pp plot
summary(fit3)
anova(fit3)

layout(matrix(c(1:4),nrow=2, ncol=2, byrow=T)) 
plot(fit3)

par(mfrow=c(1,1)) # Par?metros gr?ficos iniciales
###Normal Probability PLot MANUAL
qqnorm( residuals(fit3), main = "QQ PLOT of residuals" )
qqline( residuals(fit3), col = "red" )
abline(v = 0)
abline(h = 0)


library(DoE.base)
library(FrF2)

MEPlot(fit3) # Efectos principales
IAPlot(fit3) # Diagrama de Interacci?n

library(qpcR)
PRESS(fit3) #Calcula prediction R2

confint(fit3) #Calcula intervalos de confianza de los parametros del modelo reducido, que es
              # mejor a todas luces

pp <- predict(fit3, interval="predict")
layout(matrix(c(1:4),nrow=2, ncol=2, byrow=T)) 
plot(fit3)

par(mfrow=c(1,1)) # Par?metros gr?ficos iniciales

library(lmtest)
# H0: varianza de los residuos es constante 
lmtest::bptest(fit3)  # Breusch-Pagan test


shapiro.test( residuals(fit3))  
# Si p valor > 0.05 no podemos rechazar que la distribuci?n sea normal.

par(mfrow=c(1,1)) # Par?metros gr?ficos iniciales

# Grafico de cubo

boxplot(y ~ A, data= fit3$model, main = "Box plot diagram", col = "green", xlab = "A",
        ylab = "Filtration Rate")

library(FrF2)

cubePlot(fit3,"A","D","C")

###################################
library(rsm)

df <- fit3$model

df <- with(df , aggregate(y, list(A=A, C=C, D=D), mean))

CR1 <- coded.data(df, x1 ~ A, x2 ~ C, x3~ D)
fit3.rsm = rsm (x ~  FO(x1, x2,x3), data = CR1)
summary(fit3.rsm)
CR1.rsmi <- update(fit3.rsm, . ~ . + TWI(x1, x2, x3))

layout(matrix(c(1,2),nrow=1, ncol=2, byrow=T)) 
contour (CR1.rsmi, ~ (x1+x2), col="black", at = list(x3=1.0),xlabs = c("A: Temperature", "C: Concentration"), main="Filtration Rate")
persp (CR1.rsmi, ~ (x1+x2), contours="col", col=rainbow(40), at = list(x3=1.0),
  xlabs = c("A: Temperature", "C: Concentration"), zlab = "Filtration Rate")

###################################

layout(matrix(c(1,2),nrow=1, ncol=2, byrow=T)) 
contour (CR1.rsmi, ~ (x2+x3), col="black", xlabs = c("C: Concentration", "D: Stirring Rate"), main="Filtration Rate")
persp (CR1.rsmi, ~ (x2+x3), contours="col", col=rainbow(40),
  xlabs = c("C: Concentration", "D: Stirring Rate"), zlab = "Filtration Rate")

###################################

layout(matrix(c(1,2),nrow=1, ncol=2, byrow=T)) 
contour (CR1.rsmi, ~ (x1+x3), col="black", xlabs = c("A: Temperature", "D: Stirring Rate"), main="Filtration Rate")
persp (CR1.rsmi, ~ (x1+x3), contours="col", col=rainbow(40),
  xlabs = c("A: Temperature", "D: Stirring Rate"), zlab = "Filtration Rate")


########################################## Ejemplo de Montgomery Libro Ejemplo 6 3 
mydata <- expand.grid( A=c(-1,1), B=c(-1,1), C=c(-1,1), D=c(-1,1) )
mydata$y <- c(1.68,1.98,4.98,5.7,3.24,3.44,9.97,9.07,2.07,2.44,7.77,9.43,4.09,4.53,11.75,16.3)
fit <- lm( y ~ .^4 , data=mydata )
summary(fit)

par(mfrow=c(1,1)) # Par?metros gr?ficos iniciales

library(FrF2)
FrF2::DanielPlot(fit, alpha = 0.1 ) #Normal Probability Plot of effects
FrF2::DanielPlot(fit, half = T, alpha = 0.1)  #Half Normal Probability Plot of effects

library(DoE.base)
library(FrF2)

MEPlot(fit) # Efectos principales
IAPlot(fit, select = c(1,2,3,4)) # Diagrama de Interacci?n

Coeff<- coefficients(fit)
Coeficientes <- Coeff[2:length(Coeff)]
Coeficientes
Eff <- 2*coefficients(fit) #Efectos de cada factor
Efectos <- Eff[2:length(Eff)]
Efectos

tt <- anova(fit)

SumaCuadrados <- tt[[2]][1:nrow(tt)-1] # Suma de cuadrados
SumaCuadrados
Porc_Contribuciones <- 100*SumaCuadrados/sum(SumaCuadrados) # Porcentajes de contribuci?n
Porc_Contribuciones

df_Efectos <- data.frame(Coeficientes, Efectos, SumaCuadrados, Porc_Contribuciones)
df_Efectos

library(BsMD)

par(mfrow=c(1,1)) # Par?metros gr?ficos iniciales

fit3 <- lm( y ~ B+C+D+B:D+B:C, data=mydata ) #modelo simplificado a partir de normal pp plot 
summary(fit3)
anova(fit3)

layout(matrix(c(1:4),nrow=2, ncol=2, byrow=T)) 
plot(fit3)

par(mfrow=c(1,1)) # Par?metros gr?ficos iniciales
###Normal Probability PLot MANUAL
qqnorm( residuals(fit3), main = "QQ PLOT of residuals" )
qqline( residuals(fit3), col = "red" )
abline(v = 0)
abline(h = 0)

library(lmtest)
lmtest::bptest(fit3)  # Breusch-Pagan test

shapiro.test( residuals(fit3))  

# Si p valor > 0.05 no podemos rechazar que la distribuci?n sea normal.

# Tranformaci?n
mydata$logy <- log(mydata$y)

fit4 <- lm( logy ~ (A*B*C*D)^4 , data=mydata )
summary(fit4)
anova(fit4)

library(BsMD)
BsMD::DanielPlot(fit4) #Normal Probability Plot of effects
BsMD::DanielPlot(fit4, half = T)  #Half Normal Probability Plot of effects

library(FrF2)
FrF2::DanielPlot(fit4) #Normal Probability Plot of effects
FrF2::DanielPlot(fit4, half = T)  #Half Normal Probability Plot of effects

fit5 <- lm( logy ~ B + C + D, data=mydata )
summary(fit5)
anova(fit5)

layout(matrix(c(1:4),nrow=2, ncol=2, byrow=T)) 
plot(fit5)

library(lmtest)
lmtest::bptest(fit5)  # Breusch-Pagan test
par(mfrow=c(1,1)) # Par?metros gr?ficos iniciales

shapiro.test( residuals(fit5))  
# Si p valor > 0.05 no podemos rechazar que la distribuci?n sea normal.





##################################################################################
################################## Diseños 2k ####################################
##################################################################################


par(mfrow=c(1,1)) # Par?metros gr?ficos iniciales

f1 <- c(-1, 1)
f2 <- c(-1, 1) 
k1 = length(f1)          # number of 1st factors 
k2 = length(f2)          # number of 2st factors 

r <- c(28,25,27,36,32,32,18,19,23,31,30,29)

n=3
A = rep(f1, each=n, len=n*k1*k2) 
A
B = rep(f2, each=n*k2, len=n*k1*k2) 
B

datos <- cbind(r,A,B)


av = aov(r ~ A*B)  # incluye interaccion
summary(av)

#interaction plot fijando para cada nivel de B y variando los niveles de A

interaction.plot(x.factor     = A,
                 trace.factor = B, 
                 response     = r, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green", "blue"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15, 13),                 ### Symbols for levels of trace var.
                 fixed=TRUE,                            ### Order by factor order in data
                 leg.bty = "o")

#interaction plot fijando para cada nivel de A y variando los niveles de B

interaction.plot(x.factor     = B,
                 trace.factor = A, 
                 response     = r, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green", "blue"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15, 13),                 ### Symbols for levels of trace var.
                 fixed=TRUE,                            ### Order by factor order in data
                 leg.bty = "o")

library(DoE.base)
library(FrF2)

MEPlot(av) # Efectos principales de la librer?a FrF2
IAPlot(av) # Diagrama de Interacci?n de la librer?a FrF2


par(mfrow=c(1,1)) # Par?metros gr?ficos iniciales

boxplot(r ~ A, main = "Box plot diagram r vs A", col = "green", xlab = "A",
        ylab = "Variable respuesta")
boxplot(r ~ B, main = "Box plot diagram r vs B", col = "green", xlab = "B",
        ylab = "Variable respuesta")

layout(matrix(c(1:4),nrow=2, ncol=2, byrow=T)) 
plot(av)

par(mfrow=c(1,1)) # Par?metros gr?ficos iniciales


########### SUPERFICIE DE RESPUESTA
library (rsm)
df <- datos
df <- as.data.frame(df)
df$A[df$A==-1] <- 15
df$A[df$A==1] <- 25
df$B[df$B==1] <- 2
df$B[df$B==-1] <- 1
df

df <- with(df , aggregate(r, list(A=A, B=B), mean))

CR1 <- coded.data(df, x1 ~ (A-20)*2/10, x2 ~ (B-1.5)*2/1)
fit2.rsm = rsm (x ~  FO(x1, x2), data = CR1)
fit2.rsm
summary(fit2.rsm)
CR1.rsmi <- update(fit2.rsm, . ~ . + TWI(x1, x2))
CR1.rsmi 

layout(matrix(c(1,2),nrow=1, ncol=2, byrow=T)) 
contour (CR1.rsmi, ~ (x1+x2), col="black", xlabs = c("Reactant Concentration", "Catalyst Amount"))
persp (CR1.rsmi, ~ (x1+x2), contours="col", col=rainbow(40),
  xlabs = c("Reactant Concentration", "Catalyst Amount"), zlab = "Conversion Yield")

par(mfrow=c(1,1)) # Par?metros gr?ficos iniciales

##################### Ejemplo 2 ^ 3 FACTORIAL

f1 <- c(-1, 1)
f2 <- c(-1, 1) 
f3 <- c(-1, 1)
k1 = length(f1)          # number of 1st factors 
k2 = length(f2)          # number of 2st factors 
k3 = length(f3)          # number of 2st factors 

r <- c(550, 604, 669, 650, 633, 601, 642, 635, 1037, 1052, 749, 868, 1075, 1063, 729, 860)

n=2
A = rep(f1, each=n, len=n*k1*k2*k3) 
A
B = rep(f2, each=n*k1, len=n*k1*k2*k3) 
B
C = rep(f3, each=n*k1*k2, len=n*k1*k2*k3) 
C

av = aov(r ~ A*B*C)  # include interaction
summary(av)

fit2 <- lm(r ~A*B*C)
summary(fit2) # show results
anova(fit2)

library(DoE.base)
library(FrF2)

MEPlot(av) # Efectos principales
IAPlot(av, select = c(1,2,3)) # Diagrama de Interacci?n

Coeff<- coefficients(fit2)
Coeficientes <- Coeff[2:length(Coeff)]
Coeficientes
Eff <- 2*coefficients(fit2) #Efectos de cada factor
Efectos <- Eff[2:length(Eff)]
Efectos

tt <- anova(fit2)

SumaCuadrados <- tt[[2]][1:nrow(tt)-1] # Suma de cuadrados
SumaCuadrados
Porc_Contribuciones <- 100*SumaCuadrados/sum(SumaCuadrados) # Porcentajes de contribuci?n
Porc_Contribuciones

df_Efectos <- data.frame(Coeficientes, Efectos, SumaCuadrados, Porc_Contribuciones)
df_Efectos

confint(fit2) #intervalos ajustados de confianza del 95%


library(FrF2)

cubePlot(fit2, "A","B","C")


################################ EJERCICIO #################################### 

f1 <- c(-1,1)
f2 <- c(-1,1)
f3 <- c(-1,1)
n <- 3
k1 <- length(f1)
k2 <- length(f2)
k3 <- length(f3)
r <- c(22,31,25,32,43,29,35,34,50,55,47,46,44,45,38,40,37,36,60,50,54,39,41,47)


A <- rep(f1,each=n,len= n*k1*k2*k3)
B <- rep(f2,each=n*k2,len= n*k1*k2*k3)
C <- rep(f3,each=n*k1*k2,len= n*k1*k2*k3)



mydata <- data.frame(A,B,C,r)

av = aov(r ~ A*B*C, data = mydata)  # incluye interaccion
summary(av)

library(DoE.base)
library(FrF2)

MEPlot(av) # Efectos principales
IAPlot(av) # Diagrama de Interacci?n

fit2 <- lm(r ~A*B*C)
summary(fit2) # show results
anova(fit2)

Coeff<- coefficients(fit2)
Coeficientes <- Coeff[2:length(Coeff)]
Coeficientes
Eff <- 2*coefficients(fit2) #Efectos de cada factor
Efectos <- Eff[2:length(Eff)]
Efectos

tt <- anova(fit2)

SumaCuadrados <- tt[[2]][1:nrow(tt)-1] # Suma de cuadrados
SumaCuadrados
Porc_Contribuciones <- 100*SumaCuadrados/sum(SumaCuadrados) # Porcentajes de contribuci?n
Porc_Contribuciones

df_Efectos <- data.frame(Coeficientes, Efectos, SumaCuadrados, Porc_Contribuciones)
df_Efectos

fit4 <- lm(r ~B+C+A:C)
summary(fit4) # show results
anova(fit4)
confint(fit4) #intervalos ajustados de confianza del 95%


########### SUPERFICIE DE RESPUESTA
library (rsm)
df <- fit4$model

df <- with(df , aggregate(r, list(A=A, B=B, C=C), mean))

CR1 <- coded.data(df, x1 ~ A, x2 ~ B, x3 ~ C)
fit2.rsm = rsm (x ~  FO(x1, x2, x3), data = CR1)
summary(fit2.rsm)
CR1.rsmi <- update(fit2.rsm, . ~ . + TWI(x1, x2, x3))

layout(matrix(c(1,2),nrow=1, ncol=2, byrow=T)) 
contour (CR1.rsmi, ~ (x1+x2), col="black", xlabs = c("A", "B"))
persp (CR1.rsmi, ~ (x1+x2), contours="col", col=rainbow(40),
  xlabs = c("A", "B"), zlab = "Response Yield")

layout(matrix(c(1,2),nrow=1, ncol=2, byrow=T)) 
contour (CR1.rsmi, ~ (x1+x3), col="black", xlabs = c("A", "C"))
persp (CR1.rsmi, ~ (x1+x3), contours="col", col=rainbow(40),
  xlabs = c("A", "C"), zlab = "Response Yield")

layout(matrix(c(1,2),nrow=1, ncol=2, byrow=T)) 
contour (CR1.rsmi, ~ (x2+x3), col="black", xlabs = c("B", "C"))
persp (CR1.rsmi, ~ (x2+x3), contours="col", col=rainbow(40),
  xlabs = c("B", "C"), zlab = "Response Yield")




datos_diametro <- read.csv("datos_diametro.csv")
datos_diametro


av = aov(vr_diametro ~ A_brillo *B_tiempo , data = datos_diametro)  # incluye interaccion
summary(av)


A <- datos_diametro$A_brillo
B <- datos_diametro$B_tiempo
r <- datos_diametro$vr_diametro

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



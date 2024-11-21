datos <- data.frame(
  A = c(-1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1),
  B = c(-1, -1, 1, 1, -1, -1, 1, 1, -1, -1, 1, 1, -1, -1, 1, 1),
  C = c(-1, -1, -1, -1, 1, 1, 1, 1, -1, -1, -1, -1, 1, 1, 1, 1),
  D = c(-1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1),
  E = c(1, -1, -1, 1, -1, 1, 1, -1, -1, 1, 1, -1, 1, -1, -1, 1),
  Y = c(-0.63, 2.51, -2.68, 1.66, 2.06, 1.22, -2.09, 1.93, 6.79, 5.47, 3.45, 5.68, 5.22, 4.38, 4.30, 4.05)
)
datos


# Primer intento, correr el dise?o completo
fit <- lm( Y ~ .^5  , data=datos )
summary(fit)
av <- aov( Y ~ .^5  , data=datos )
summary(av)

# Segundo intento, correr el dise?o completo
fit <- lm( Y ~ A+B+C+D+E+A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+C:E+D:E  , data=datos )
summary(fit)
av <- aov( Y ~ A+B+C+D+E+A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+C:E+D:E , data=datos )
summary(av)


library(DoE.base)
library(FrF2)

MEPlot(fit) # Efectos principales
IAPlot(fit, select = c(1,2,3,4,5)) 

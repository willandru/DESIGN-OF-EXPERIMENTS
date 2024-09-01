########################################################################################
########################################################################################
################################ ANOVA EFECTOS ALEATORIOS  #############################
########################################################################################
########################################################################################

library(readxl)
library(tidyr)


datos<- read_excel("C:/Users/adrodriguez/Downloads/datos_anova.xlsx",sheet="E0")

datos <- as.data.frame(t(datos)[2:ncol(datos),])

data_long <- gather(datos, Telar, factor_key=TRUE)
data_long$Telar <- as.factor(data_long$Telar)


# Boxplot con cruce de respuesta y factor    
boxplot(data_long$value ~ data_long$Telar, main="Boxplot de resistencia según telar",
        ylab="Resistencia", xlab="Telar")


# ANOVA para contenido de calcio

anova<- aov(data_long$value ~ data_long$Telar, data = data_long)

summary(anova)

# Estimadores de variabilidad

# Componentes de variabilidad

MSEtratamiento <- summary(anova)[[1]]["data_long$Telar","Mean Sq"] 
MSEerror <- summary(anova)[[1]]["Residuals","Mean Sq"]
sigma_error <- MSEerror
sigma_tratamientos <- (MSEtratamiento - MSEerror)/4
sigma_error
sigma_tratamientos

# Intervalo de confianza

media <- sigma_tratamientos/(sigma_tratamientos+ sigma_error)

L <- (1/4)*(((MSEtratamiento/MSEerror)*(1/ qf(0.025,4-1,16-4)))-1)

U <- (1/4)*(((MSEtratamiento/MSEerror)*(1/ qf(0.975,4-1,16-4)))-1)

L/(L+1)
U/(U+1)

#Variabilidad del factor explica entre el 38% y el 98% de la variabilidad de las observaciones


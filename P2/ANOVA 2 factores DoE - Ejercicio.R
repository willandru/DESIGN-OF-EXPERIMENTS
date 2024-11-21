##################################################################################
########################## ANOVA 2 FACTORES EJERCICIO ############################
##################################################################################

# Supóngase un estudio clínico que analiza la eficacia de un medicamento teniendo 
# en cuenta dos factores, el sexo (masculino y femenino) y la juventud (joven, adulto). 
# Se quiere analizar si el efecto es diferente entre alguno de los niveles de cada 
# variable por si sola o en combinación.

subject <- as.factor(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
                       18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30))
sex <- c("female", "male", "male", "female", "male", "male", "male", "female",
         "female", "male", "male", "male", "male", "female", "female", "female",
         "male", "female", "female", "male", "male", "female", "male", "male",
         "male", "male", "male", "male", "female", "male" )
age <- c("adult", "adult", "adult", "adult", "adult", "adult", "young", "young",
         "adult", "young", "young", "adult", "young", "young", "young", "adult",
         "young", "adult", "young", "young", "young", "young", "adult", "young",
         "young", "young", "young", "young", "young", "adult")
result <- c(7.1, 11.0, 5.8, 8.8, 8.6, 8.0, 3.0, 5.2, 3.4, 4.0, 5.3, 11.3, 4.6, 6.4,
            13.5, 4.7, 5.1, 7.3, 9.5, 5.4, 3.7, 6.2, 10.0, 1.7, 2.9, 3.2, 4.7, 4.9,
            9.8, 9.4)

datos <- data.frame(subject, sex, age, result)
head(datos, 4)



# 1) Realice una exploración de los datos

# 2) Compruebe si existen efectos significativos

# 3) Compruebe los supuestos de ANOVA


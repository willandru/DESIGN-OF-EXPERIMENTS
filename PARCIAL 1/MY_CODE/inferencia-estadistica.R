#PRUEBAS DE HIPOTESIS


#Varianza conocida : Z
#Una Muestra
Z_0 <- (x_barra - mu_0)/(desviacion / sqrt(n))
#Dos Muestras
Z_0 <- (x1_barra - x2_barra)/(sqrt((varianza1/n1)+(varianza2/n2)))

#Varianza desconocida: t-student

#Una Muestra
t_0 <- (x_barra - mu_0)/(desviacion / sqrt(n))

#Dos Muestras
#Varianzas muestrales iguales
S_p <- ((n1-1)*varianza1)+(n2-1)*varianza2/(n1+n2-2)
t_0 <- (x1_barra - x2_barra)/(sqrt(S_p)*sqrt((1/n1)+(1/n2)))
df <- n1 + n2 -2


#Varianzas muestrales diferentes
t_0 <- (x1_barra - x2_barra)/(sqrt((varianza1/n1)+(varianza2/n2)))
df <- ((varianza1/n1)+(varianza2-n2))^2/(((varianza1/n1)^2/(n1-1))+((varianza2/n2)^2/(n2-1)))
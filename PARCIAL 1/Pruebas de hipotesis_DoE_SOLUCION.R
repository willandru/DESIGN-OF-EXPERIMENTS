###################################################################################
###################################################################################
###################################################################################
################################ PRUEBA DE HIPOTESIS ##############################
###################################################################################
###################################################################################
###################################################################################

##########################
# Ej 1. Control de calidad
##########################

# Hipotesis nula
#H0: miu = 375 
# Hipotesis alternativa
#H1: miu != 375

miu <- 375
media <- 350
n <- 64
sigma <- 100
alpha <- 0.05

Z <- (media - miu)/(sigma/sqrt(n))

#Opción 1: comparar valores criticos
abs(Z) > qnorm(alpha/2)

#Opción 2: comparar areas bajo la curva
alpha > 2*pnorm(Z)

# SI se rechaza Hipotesis Nula

##########################
# Ej 2. Servicio Postal
##########################

# Hipotesis nula
#H0: miu = 2.5 
# Hipotesis alternativa
#H1: miu < 2.5 

miu <- 2.5
media <- 2.2
n <- 17
S <- 0.9
alpha <- 0.01

t <- (media - miu)/(S/sqrt(n))

#Opción 1: comparar valores criticos
t < qt(alpha, n-1)

#Opción 2: comparar areas bajo la curva
alpha > pt(t, n-1)

# NO se rechaza Hipotesis Nula


##########################
# Ej 3. Resistencia fibra
##########################


# Hipotesis nula
#H0: miu = 150 
# Hipotesis alternativa
#H1: miu > 150 


muestra <- c(145, 153, 150, 147)

miu <- 150
media <- mean(muestra)
n <- length(muestra)
sigma <- 3
alpha <- 0.05

Z <- (media - miu)/(sigma/sqrt(n))

#Opción 1: comparar valores criticos
Z > qnorm(1-alpha)
#Opción 2: comparar areas bajo la curva
alpha > 1-pnorm(Z)

# NO se rechaza Hipotesis Nula

# Intervalo de confianza:

min <- media - (qnorm(alpha/2)*(sigma/sqrt(n)))

max <- media + (qnorm(alpha/2)*(sigma/sqrt(n)))

print(paste("El intervalo de confianza es [",round(min(min,max),2),", ",round(max(min,max),2),"]")) 

##########################
# Ej 4. Vida en anaquel
##########################


# Hipotesis nula
#H0: miu = 120
# Hipotesis alternativa
#H1: miu > 120


muestra <- c(108,138,124,163,124,159,106,134,115,139)

miu <- 120
media <- mean(muestra)
n <- length(muestra)
S <- sd(muestra) 
alpha <- 0.01

t <- (media - miu)/(S/sqrt(n))


#Opción 1: comparar valores criticos
t > qt(1-alpha, n-1)
#Opción 2: comparar areas bajo la curva
alpha > 1-pt(t, n-1)

# NO se rechaza Hipotesis Nula

# Intervalo de confianza:

min <- media - (qt(alpha/2,n-1)*(S/sqrt(n)))

max <- media + (qt(alpha/2,n-1)*(S/sqrt(n)))

print(paste("El intervalo de confianza es [",round(min(min,max),2),", ",round(max(min,max),2),"]")) 


# Paquete de R:

t.test(muestra,mu=120,alternative="greater", conf.level = 0.99)


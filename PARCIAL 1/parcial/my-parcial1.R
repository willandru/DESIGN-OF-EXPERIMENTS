MSe<- 4.5/7
MSe
38-4.5
MSbloque <- 8*0.643
MSbloque
CuadradoMedioError <- 4.5/6
CuadradoMedioError


12374/116


a1<- 353.33
a2<- 441.66
x1 <- (1873.33-106.67)/a1
x2 <- (1873.33-106.67)/a2
x1
x2


X<- (1873.33-106.67)/5
X
  
# Crear un vector en R con los valores proporcionados
valores <- c(98, 97, 94, 101, 96, 96, 99, 100, 93, 94, 95, 96)

# Mostrar el vector
print(valores)




# Intervalo de confianza:

media <- mean(valores)
alpha<-0.05
S <- sd(valores) 
n<- 10
n <- length(valores)

min <- media - (qt(alpha/2,n-1)*(S/sqrt(n)))

max <- media + (qt(alpha/2,n-1)*(S/sqrt(n)))

print(paste("El intervalo de confianza es [",round(min(min,max),2),", ",round(max(min,max),2),"]")) 

mu0 <- mean(valores)

# Realizar la prueba t
resultado <- t.test(valores, mu = mu0)

# Mostrar el resultado
print(resultado)

ss<- 38-18+4.5
ss
ss/2
4.5/7

12.25/0.6428

#UN FACTOR 

# Ejercicio 1: Un gerente debe determinar si la vida media de unos focos es de 375.
#De su lote el sabe que la desviacione standar poblacional es 100, y la media muestral es 350, 
#el n es 64. El quiere saberlo con un intervalod e confianza del 95%

# H0: miu =375
# H1: miu ~= 375

alpha <-0.05
xprom<- 350
sigma <- 100
n <- 64
miu<-375

Z <- (xprom-miu)/(sigma/sqrt(n))
Z
#opcion 1: comparacion del estadistico con el limite""
#Debo saber en que situacione stoy: Prueba de 2 colas miu = miu0
#Z_alfa/2 
qnorm(alpha/2)  #p=probabiliad (area bajo curva)
qnorm(1-alpha/2)
q<-qnorm(alpha/2)
q1<-qnorm(alpha/2, lower.tail=FALSE)
q2<-qnorm(alpha/2)
# Cae en la region de rechazo de la cola izquierda ya que -2 < -1.95. Se rechaza H0.

(Z < q2 )|| (Z >q1)
#q norm: es de area a limite. pnorm :de limite a area
#ocpion 2: comparacion de areas.  pnorm

p<-pnorm(Z)
p*2 < alpha

#EJERCICIO 2:  uNA EMPRESA GARANTIZA QUE PUEDE REDUCIR EL TIMEPO RPOMEDIO PARA
#RECIBIR UN PAQUETE A MENOS DE 2.5 DÍAS.
#despues de utilizar la nueva compañia en 17 ocasiones, el tiempo de entrega
#promedio fie de 2.2 dias y lad esviacione standar muestral es de 0.9, n=17, 
#miu_=2.5 y alfa=0.01

#H0 >= 2.5
#H1 <2.5

alpha <-0.01
xprom<- 2.2
S <- 0.9
n <- 17
miu<-2.5

#estadistico de prueba : t-student
t<-(xprom-miu)/(S/sqrt(n))
#grados de librertad: n-1
qt(alpha, n-1)
#Como t no cayo en la región de rechazo, no rechazo la hipotesis nula.
#ocpion 2
alpha >pt(t,n-1)


#EJERCICIO 3: 
#Se requiere que la resistencia a la ruptura de una fibra sea de por lo menos 
#150 psi. La experiencia pasada indica que la desviación estándar de la resistencia
# a la ruptura es 3 psi. Se prueba una muestra aleatoria de cuatro ejemplares de
#prueba, y los resultados son y1 = 145, y2 = 153, y3 = 150 y y4 = 147.
#a) Enunciar las hipótesis que el lector considere que deberían probarse en este experimento.
#b) Probar estas hipótesis utilizando a = 0.05. ¿A qué conclusiones se llega?
#c) Construir un intervalo de confianza de 95% para la resistencia a la ruptura promedio

#H0: miu <= 150
#H1: miu > 150

sigma<-3
miu<-150
Y<-c(145,153,150,147)
xbarra<-mean(Y)
alpha<-0.05
n<-4

Z <- (xbarra-miu)/(sigma/sqrt(n))
q<-qnorm(alpha, lower.tail = FALSE)
Z>q

alpha > pnorm(Z, lower.tail = FALSE)
#No rechazo la H0, no hay evidencia suficiente a un nivel del 95% para decir que
#el promedio es de almenos 150

#INTERVALO

upper<- xbarra + qnorm(alpha/2)*sigma/sqrt(n)
lower<- xbarra - qnorm(alpha/2)*sigma/sqrt(n)
print(paste("El intervalo de confianza es [", round(upper), ",", round(lower),"]"))

#CUARTO EJERCICIO
#La vida de anaquel de una bebida carbonatada es motivo de interés. 
#Se seleccionan 10 botellas al azar y
#se prueban, obteniéndose los siguientes resultados:
#  108 138 124 163 124 159 106 134 115 139
#a) Quiere demostrarse que la vida media de anaquel excede los 120 días. 
#Establecer las hipótesis
#apropiadas para investigar esta afirmación.
#b) Probar estas hipótesis utilizando a = 0.01. ¿A qué conclusiones se llega?
#c) Encontrar el valor P para la prueba del inciso b.
#d) Construir un intervalo de confianza de 99% para la vida media de anaquel

n<-10
x<-c(108,138,124,163,124,159,106,134,115,139)
xbarra<-mean(x)
s<-sd(x)
miu<-120
alpha<-0.01

#H0: miu<=120
#H1:miu>120

#estadistico de prueba : t-student
t<-(xbarra-miu)/(s/sqrt(n))
t
#grados de librertad: n-1
q<-qt(alpha, n-1, lower.tail = FALSE)
t>q
#ocpion 2
alpha >pt(t,n-1)


##

t.test(x, alternative = "greater", mu=120, conf.level = 0.99)

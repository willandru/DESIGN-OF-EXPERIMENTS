#####################################################################################
#####################################################################################
#####################################################################################
################################ INTRODUCCION A R ###################################
#####################################################################################
#####################################################################################
#####################################################################################


# Operaciones matematicas basicas entre variables: 
#+ operador binario para sumar.
#- operador binario para restar.
#* operador binario para multiplicar.
#/ operador binario para dividir.
#^ operador binario para potencia.
#%/% operador binario para obtener el cociente en una divisi?n (n?mero entero).
#%%  operador binario para obtener el residuo en una divisi?n.
5+10
10-2
3*4
67/4
89

60%/%5
60 %% 5






# Operaciones logicas basicas entre variables: 
#< para saber si un n?mero es menor que otro.
#> para saber si un n?mero es mayor que otro.
#== para saber si un n?mero es igual que otro.
#<= para saber si un n?mero es menor o igual que otro.
#>= para saber si un n?mero es mayor o igual que otro.
#!x  # Negaci?n de x
#x & y  # Conjunci?n entre x e y
#x && y # Conjunci?n vectorial
#x | y  # Disyunci?n entre x e y
#x || y # Disyunci?n vectorial
#xor(x, y)

5 <= 60
6 != 9

(6 >60) | (5>8)

# Asignación de variable
x <- 70
z <- 5.6
y <- 'hola'
y2 <- "hola"
y == y2

k <- TRUE

# Variables y vectores


v1 <- c(6,8,12,1)
v2 <- c("hola", 34, TRUE, "ADIOS")
v3 <- c("tx1", "tx2", "tx3")
# Operaciones basicas con vectores
#min: para obtener el m?nimo de un vector.
#max: para obtener el m?ximo de un vector.
#length: para determinar la longitud de un vector.
#range: para obtener el rango de valores de un vector, entrega el m?nimo y m?ximo.
#sum: entrega la suma de todos los elementos del vector.
#prod: multiplica todos los elementos del vector.
#which.min: nos entrega la posici?n en donde est? el valor m?nimo del vector.
#which.max: nos da la posici?n del valor m?ximo del vector.
#rev: invierte un vector.
min(v1)
max(v1)
sum(v1)
prod(v1)
mean(v1)
sd(v1)
var(v1)
length(v1)
# Para el seguinete vector calcule la suma entre: la multiplicaci?n de todos los elementos, la longitud del vector y la posici?n
# en que esta el valor mas peque?o dentro del vector

m <-c(5,9,8,3,2,1)

prod(m)
length(m)

# Manejo de vectores
m[1]
m[2]<- 3
m[1:3]
m[7] <- 8
m[9] <- 3
m

m[c(1,3)]
m[c(3,1)]

# Funciones

areaRec <- function(largo, ancho){
  area <- largo*ancho
  return(area)
}

z <- areaRec(8,7)


# Cree una funci?n que reciba por parametro el peso y la altura de una persona y retorne el IMC. 
# El IMC se define como el peso divido por el cuadrado de la altura

calcuBMI<-function(peso, altura){
  BMI<- peso/ (altura^2)
  return(BMI)
}

myBMI <- calcuBMI(70, 1.70)
myBMI

# Condicionales

nota<- 4
if(nota<3){
  print("Reprobado")
}else if(nota >=3 && nota<4){
  print("Aceptable")
}else{
  print("Exelente")
}


# Cree un condicional que imprima "Mayor de edad" o "Menor de edad" segun el valor de la variable e:
e<-6


# Ciclo

for(i in 1:10){
  print(i)
}


# Cree un ciclo que imprima en consola la tabla del 5 (del 1 al 10). Use la funci?n print()

for(i in 1:10){
  print(5*i)
}


# Librerias



######################### Cargar datos desde un Excel ###############################
library(readxl)
library(dplyr)
#read_excel: sirve para cargar un archivo excel en un dataframe

datos1 <- read_excel("C:\\Users\\willi\\GITHUB\\DESIGN-OF-EXPERIMENTS\\DoE\\Clase 2 - Introduction to R\\Datos_DoE.xlsx")
datos2 <- read_excel("C:\\Users\\willi\\GITHUB\\DESIGN-OF-EXPERIMENTS\\DoE\\Clase 2 - Introduction to R\\Datos_II_DoE.xlsx")
datos <- read_excel("Datos_DoE.xlsx")

datos2 <- as.data.frame(datos)
View(datos2)
nrow(datos2)
ncol
head(datos2)
tail(datos2)
str(datos2)
############################ Tipo de dato: dataframe ################################
#data.frame: convierte una estructura en un data.frame
#head: muestra las primeras filas del dataframe
#str: muestra el tipo de datos que tienen las columnas del dataframe
#dim: dimensiones del dataframe
#nrow: numero de filas
#ncol: numero de columnas
#length: numero de columnas


####################### Manejo y calculos en dataframes #############################
# $ : para llamar columnas
# []: para llamar secciones de la tabla

x1 <- datos2$FECHA
datos2[1:3,2:5]
datos2[c(1,5,3),1:3]

# De los datos extraiga la columna NUMERO_DESEMBOLSOS y calcule cuantos valores son mayores a 100

num_des <- length(datos2$NUMERO_DESEMBOLSOS[datos2$NUMERO_DESEMBOLSOS> 100] )
num_des

# Extraiga las columnas correspondientes a las filas 5, 6, 7, 8, 9 y 10

x1 <- c(1 ,2 ,3 ,4, NA ,5 ,6 , 7)
x2 <- c(1,2,3,4,5,5,6,7)

x3<- x1[x1>4]
x4<- x2[x2>4]
sum(x3)
length(x3)
sum(x4)
length(x4)

sum(datos2$NUMERO_DESEMBOLSOS >100, na.rm =TRUE)

###################### Filtros por fila condicionales ###############################

# cree un dataframe con los datos del BANCO 9

banco9 <- datos2[datos2$NOMBRE_ENTIDAD == "BANCO 9", ]
View(banco9)

# cree un dataframe con los datos cuyo tipo de deudor sea HOGAREs


# cree un dataframe con los datos cuyo tasa promedio este entre 10 y 20

tasa_10_20 <- banco9[(banco9$TASA_PROMEDIO >=10) & (banco9$TASA_PROMEDIO <= 20),  ]
View(tasa_10_20)
# reemplazar valores NA en TASA_PROMEDIO por ceros
sum(is.na(banco9$TASA_PROMEDIO))

datos2$TASA_PROMEDIO

############################ Agrupaciones ###########################################
# library(dplyr)
# Group_by(): permite agrupar variables y hacer calculos
# summarise(): permite realizar esos calculos

library(dplyr)

tasa_x_banco <- datos2 %>% group_by(NOMBRE_ENTIDAD) %>% summarise(PROM= mean(TASA_PROMEDIO), MONTO=sum(MONTO_DESEMBOLSO_SEMANA), MAXI= max(NUMERO_DESEMBOLSOS))
View(tasa_x_banco)


tasa_banco_Deudor <- datos2 %>% group_by(NOMBRE_ENTIDAD, TIPO_DEUDOR) %>% summarise(PROM=mean(TASA_PROMEDIO),
                                                                                    MONTO=sum(MONTO_DESEMBOLSO_SEMANA),
                                                                                    MAXI= max(NUMERO_DESEMBOLSOS))
# Determine la tasa mas alta de cada banco

tasa_max_banco <- datos2 %>% group_by(NOMBRE_ENTIDAD) %>% summarise(TASA_MAX=max(TASA_PROMEDIO))
View(tasa_max_banco)
                                                                      
# Determine para cada tipo de deudor: suma de monto, suma de numero desembolsos y promedio de tasa promedio

# Determine y grafique para cada fecha el promedio de la tasa promedio

# Determine y grafique para cada fecha el promedio de la tasa promedio solo para banco 8


######################## Fusión de datafames: merge y rbind #########################
# merge: une dos dataframes
# rbind/cbind: pega dos dataframe uno tras otro


################################# GRAFICOS ###########################################333

#Histograma

#Boxplot




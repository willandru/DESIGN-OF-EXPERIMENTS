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

5+8

6-8

6*9

4/9

5^5

40 %/% 7

40 %% 7

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

5 <= 10

1==1

5!=9

(5<9) | (6>8)

(5<9) & (6>8)

# Asignación de variable

x <- 5
y <- "Hola"
z <- TRUE

# Variables y vectores

vector <- c(5,9,8,63,3,2,5)

nombres <- c("Juana", "Jaime", "Ernesto")

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

min(vector)

max(vector)

length(vector)

sum(vector)

prod(vector)

sumatoria <- sum(vector)
  
# Para el seguinete vector calcule la suma entre: la multiplicaci?n de todos los elementos, la longitud del vector y la posici?n
# en que esta el valor mas peque?o dentro del vector

m <-c(5,9,8,3,2,1)

multiplicacion <- prod(m)

longitud <- length(m)

pos_mas_pequeno <- which.min(m)  

# Manejo de vectores

m[1]

m[2:3]

m[c(3,1,6)]


x <- m[1:2]

m[1] <- 45


# Funciones

area <- function(altura, piso){
  
  area <- altura*piso 
  return(area)
  
}


# Cree una funci?n que reciba por parametro el peso y la altura de una persona y retorne el IMC. 
# El IMC se define como el peso divido por el cuadrado de la altura

IMC <- function(peso, altura){
  
  resultado <- (peso/(altura^2))
  return(resultado)
  
}

# Condicionales

m <- 5  

if(m < 41){
  
  print("Es menor a 41")
  
}

if(m < 41){
  
  print("Es menor a 41")
  
}else{
  print("Es mayor")
}


if(m < 41){
  print("Es menor a 41")
}else if((m < 41) & (m >20)){
  print("Es mayor a 20 y menor a 41")
}else{
  print("Es mayor a 41")
}


# Cree un condicional que imprima "Mayor de edad" o "Menor de edad" segun el valor de la variable e:
e<-6

if(e <= 18){
  
  print("Es menor de edad")
  
}else{
  print("Es mayor de edad")
}


# Ciclo

nombres <- c("sebastian", "juliana", "eduardo")

for(i in nombres){
  print(nombres)
}

for(i in 1:length(nombres)){
  print(nombres[i])
}

# Cree un ciclo que imprima en consola la tabla del 5 (del 1 al 10). Use la funci?n print()

for(i in 1:10){
  print(i*5)
}

i<-0
while(i<10){
  print(i)
  i <- i+1
}

# Librerias

install.packages("readxl")

library(readxl)


######################### Cargar datos desde un Excel ###############################
#library(readxl)
#read_excel: sirve para cargar un archivo excel en un dataframe

datos <- read_excel("C:\\Users\\aulasingenieria\\Downloads\\Datos_DoE.xlsx")

############################ Tipo de dato: dataframe ################################
#data.frame: convierte una estructura en un data.frame
#head: muestra las primeras filas del dataframe
#str: muestra el tipo de datos que tienen las columnas del dataframe
#dim: dimensiones del dataframe
#nrow: numero de filas
#ncol: numero de columnas
#length: numero de columnas

datos <- as.data.frame(datos)
head(datos)
str(datos)
nrow(datos)
ncol(datos)


####################### Manejo y calculos en dataframes #############################
# $ : para llamar columnas
# []: para llamar secciones de la tabla

datos$NOMBRE_ENTIDAD

datos$TASA_PROMEDIO[1:50]

datos[1,1]

datos[1,]

datos[,1]

datos[, c(5,2)]


# De los datos extraiga la columna NUMERO_DESEMBOLSOS y calcule cuantos valores son mayores a 100


sum(datos$NUMERO_DESEMBOLSOS >= 100, na.rm=TRUE)


# Extraiga las columnas correspondientes a las filas 5, 6, 7, 8, 9 y 10

datos[5:10, ]


###################### Filtros por fila condicionales ###############################

# cree un dataframe con los datos del BANCO 9

datos_9 <- datos[datos$NOMBRE_ENTIDAD=="BANCO 9",]

# cree un dataframe con los datos cuyo tipo de deudor sea HOGAREs

datos_hogares <- datos[datos$TIPO_DEUDOR=="HOGARES",]

# reemplazar valores NA en TASA_PROMEDIO por ceros

datos[is.na(datos$TASA_PROMEDIO),7] <- 0

# cree un dataframe con los datos cuyo tasa promedio este entre 10 y 20

datos_tasas <- datos[(datos$TASA_PROMEDIO>=10) & (datos$TASA_PROMEDIO<=20),]


############################ Agrupaciones ###########################################
# library(dplyr)
# Group_by(): permite agrupar variables y hacer calculos
# summarise(): permite realizar esos calculos

library(dplyr)

datos_agrupados <- datos %>% group_by(NOMBRE_ENTIDAD) %>% summarise(PROM = mean(TASA_PROMEDIO)) 


datos_agrupados <- datos %>% group_by(NOMBRE_ENTIDAD) %>% summarise(PROM = mean(TASA_PROMEDIO), MONTO = sum(MONTO_DESEMBOLSO_SEMANA, na.rm=TRUE)) 

datos_agrupados <- datos %>% group_by(NOMBRE_ENTIDAD, TIPO_DEUDOR) %>% 
  summarise(PROM = mean(TASA_PROMEDIO)) 

datos_agrupados <- datos %>% group_by(NOMBRE_ENTIDAD, TIPO_DEUDOR) %>% 
  summarise(PROM = mean(TASA_PROMEDIO), MONTO = sum(MONTO_DESEMBOLSO_SEMANA, na.rm=TRUE)) 


# Determine la tasa mas alta de cada banco

tasa_max <- datos %>% group_by(NOMBRE_ENTIDAD) %>% summarise(TASA_MAX = max(TASA_PROMEDIO, na.rm=TRUE)) 


# Determine para cada tipo de deudor: suma de monto, suma de numero desembolsos y promedio de tasa promedio
deudor <- datos %>% group_by(TIPO_DEUDOR)  %>% summarise(MONTO_TOTAL = sum(MONTO_DESEMBOLSO_SEMANA, na.rm=TRUE),
                                                         NUM_TOTAL = sum(NUMERO_DESEMBOLSOS, na.rm=TRUE),
                                                         TASA_MEDIA = mean(TASA_PROMEDIO, na.rm=TRUE))

# Determine y grafique para cada fecha el promedio de la tasa promedio



hist_tasas <- datos %>% group_by(FECHA) %>% summarise(TASA_MEDIA = mean(TASA_PROMEDIO, na.rm=TRUE)) 

plot(hist_tasas$FECHA , hist_tasas$TASA_MEDIA, type = 'l',col="blue",
     main="Historico de tasas", xlab= "", ylab="Promedio Tasa")


# Determine y grafique para cada fecha el promedio de la tasa promedio solo para banco 8

dummy <- datos[datos$NOMBRE_ENTIDAD=="BANCO 8", ]

hist_tasas <- dummy %>% group_by(FECHA) %>% summarise(TASA_MEDIA = mean(TASA_PROMEDIO, na.rm=TRUE)) 

plot(hist_tasas$FECHA , hist_tasas$TASA_MEDIA, type = 'l',col="blue",
     main="Historico de tasas", xlab= "", ylab="Promedio Tasa")


######################## Fusión de datafames: merge y rbind #########################

# rbind/cbind: pega dos dataframe uno tras otro


banco_8 <- datos[datos$NOMBRE_ENTIDAD=="BANCO 8", ]

banco_7 <- datos[datos$NOMBRE_ENTIDAD=="BANCO 7", ]


banco_7_8 <- rbind(banco_7,banco_8)

banco_7_8 <- cbind(banco_7[1:6822, ] , banco_8)

# merge: une dos dataframes

datos <- read_excel("C:\\Users\\aulasingenieria\\Downloads\\Datos_DoE.xlsx")

datos_bitcoin <- read_excel("C:\\Users\\aulasingenieria\\Downloads\\Datos_II_DoE.xlsx")



datos_final <- merge(datos, datos_bitcoin, by="NOMBRE_ENTIDAD" , all.y=TRUE )
  
################################# GRAFICOS ###########################################333

#Histograma


hist(datos$TASA_PROMEDIO, breaks= 15)

#Boxplot

boxplot(datos$TASA_PROMEDIO ~ datos$TIPO_DEUDOR)


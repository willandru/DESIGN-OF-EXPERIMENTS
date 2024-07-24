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


# Asignación de variable


# Variables y vectores


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


# Para el seguinete vector calcule la suma entre: la multiplicaci?n de todos los elementos, la longitud del vector y la posici?n
# en que esta el valor mas peque?o dentro del vector

m <-c(5,9,8,3,2,1)



# Manejo de vectores


# Funciones

# Cree una funci?n que reciba por parametro el peso y la altura de una persona y retorne el IMC. 
# El IMC se define como el peso divido por el cuadrado de la altura


# Condicionales


# Cree un condicional que imprima "Mayor de edad" o "Menor de edad" segun el valor de la variable e:
e<-6


# Ciclo


# Cree un ciclo que imprima en consola la tabla del 5 (del 1 al 10). Use la funci?n print()


# Librerias



######################### Cargar datos desde un Excel ###############################
#library(readxl)
#read_excel: sirve para cargar un archivo excel en un dataframe



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


# De los datos extraiga la columna NUMERO_DESEMBOLSOS y calcule cuantos valores son mayores a 100


# Extraiga las columnas correspondientes a las filas 5, 6, 7, 8, 9 y 10


###################### Filtros por fila condicionales ###############################

# cree un dataframe con los datos del BANCO 9


# cree un dataframe con los datos cuyo tipo de deudor sea HOGAREs


# cree un dataframe con los datos cuyo tasa promedio este entre 10 y 20


# reemplazar valores NA en TASA_PROMEDIO por ceros


############################ Agrupaciones ###########################################
# library(dplyr)
# Group_by(): permite agrupar variables y hacer calculos
# summarise(): permite realizar esos calculos

# Determine la tasa mas alta de cada banco

# Determine para cada tipo de deudor: suma de monto, suma de numero desembolsos y promedio de tasa promedio

# Determine y grafique para cada fecha el promedio de la tasa promedio

# Determine y grafique para cada fecha el promedio de la tasa promedio solo para banco 8


######################## Fusión de datafames: merge y rbind #########################
# merge: une dos dataframes
# rbind/cbind: pega dos dataframe uno tras otro


################################# GRAFICOS ###########################################333

#Histograma

#Boxplot




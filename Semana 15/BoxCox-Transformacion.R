
library(MASS)
library(ggplot2)
library(readxl)


library(tidyverse)

# Supongamos que tus datos están en un tibble llamado 'datos'
datos_wide <- datos %>%
  pivot_wider(
    names_from = Medio,      # Las categorías de 'Medio' serán los nombres de las nuevas columnas
    values_from = Valor      # Los valores de 'Valor' se asignarán a las nuevas columnas
  )

datos <- Topicos_adiconales_datos_DoE <- read_excel("Topicos adiconales_datos_DoE.xlsx")


# Supongamos que tus datos están en un tibble llamado 'datos'
datos_wide <- datos %>%
  mutate(id = row_number()) %>%
  pivot_wider(
    names_from = Medio,
    values_from = Valor
  ) %>%
  select(-id) 

datos_wide
# Histograma original
hist(tiempos, main="Histograma Original", xlab="Tiempo", breaks=30, col="lightblue")

# Gráfico Q-Q
qqnorm(tiempos)
qqline(tiempos, col = "red")

# Aplicar transformación Box-Cox
boxcox_result <- boxcox(tiempos ~ 1)
lambda <- boxcox_result$x[which.max(boxcox_result$y)] # Valor de lambda óptimo

# Transformar los datos usando el lambda óptimo
tiempos_boxcox <- (tiempos^lambda - 1) / lambda

# Mostrar resultados
print("Datos originales:")
print(head(tiempos)) # Muestra los primeros 6 datos
print("Datos transformados (Box-Cox):")
print(head(tiempos_boxcox)) # Muestra los primeros 6 datos
print(paste("Valor de lambda:", lambda))


# Histograma transformado
hist(tiempos_boxcox, main="Histograma Transformado (Box-Cox)", xlab="Tiempo Transformado", breaks=30, col="lightgreen")

# Gráfico Q-Q para los datos transformados
qqnorm(tiempos_boxcox)
qqline(tiempos_boxcox, col = "red")

# Crear el dataframe en R
data <- data.frame(
  A_brillo = c(1, 1, 0, 0, 1, 1, 0, 0),
  B_tiempo = c(1, 0, 1, 0, 1, 0, 1, 0),
  vr_diametro = c(-0.77, -0.74, 0.19, -0.01, -0.65, -0.88, 0.11, -0.03)
)

# Mostrar los datos
print(data)

# Guardar en un archivo CSV
write.csv(data, file = "datos_diametro.csv", row.names = FALSE)
dir()

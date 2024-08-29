########################################################################################
########################################################################################
################################ ANOVA EFECTOS SIMPLES   ###############################
########################################################################################
########################################################################################

# Cargar los datos
data(iris)

datos <- as.data.frame(iris)

# Exploración de datos

# Verificación de valores ausentes
sum(is.na(datos))

# Univariado
datos$Species <- as.factor(datos$Species)

boxplot(datos$Sepal.Length)
hist(datos$Sepal.Length)

boxplot(datos$Sepal.Width)
hist(datos$Sepal.Width)

boxplot(datos$Petal.Length)
hist(datos$Petal.Length)

boxplot(datos$Petal.Width)
hist(datos$Petal.Width)

# Dispersión multivariado
pairs(datos, col = datos$Species)


# Boxplot con cruce de respuesta y factor    
boxplot(datos$Sepal.Length ~ datos$Species , main="Boxplot de longitud de sepalo según especie",
        ylab="Longitud (cm)", xlab="Especie")

boxplot(datos$Sepal.Width ~ datos$Species , main="Boxplot de longitud de sepalo según especie",
        ylab="Longitud (cm)", xlab="Especie")

boxplot(datos$Petal.Length ~ datos$Species , main="Boxplot de longitud de sepalo según especie",
        ylab="Longitud (cm)", xlab="Especie")

boxplot(datos$Petal.Width ~ datos$Species , main="Boxplot de longitud de sepalo según especie",
        ylab="Longitud (cm)", xlab="Especie")

# Verificación de la normalidad de los datos mediante gráficos QQ
par(mfrow = c(2, 2))

qqnorm(iris$Sepal.Length, main = "QQ Plot - Sepal Length")
qqline(iris$Sepal.Length)

qqnorm(iris$Sepal.Width, main = "QQ Plot - Sepal Width")
qqline(iris$Sepal.Width)

qqnorm(iris$Petal.Length, main = "QQ Plot - Petal Length")
qqline(iris$Petal.Length)

qqnorm(iris$Petal.Width, main = "QQ Plot - Petal Width")
qqline(iris$Petal.Width)

par(mfrow = c(1, 1))


# ANOVA para sepalo

anova_sepalo_l <- aov(Sepal.Length ~ Species, data = datos)

summary(anova_sepalo_l)

anova_sepalo_w <- aov(Sepal.Width ~ Species, data = datos)

summary(anova_sepalo_w)


# Resultados

par(mfrow = c(2, 2))
plot(anova_sepalo_l)
par(mfrow = c(1, 1))


# ANOVA para petalo

anova_petalo_l <- aov(Petal.Length ~ Species, data = datos)

summary(anova_petalo_l)

anova_petalo_w <- aov(Petal.Width ~ Species, data = datos)

summary(anova_petalo_w)

par(mfrow = c(2, 2))
plot(anova_petalo_l)
par(mfrow = c(1, 1))


# Verificar supuestos

residuals <- residuals(anova_sepalo_l)

# Normalidad. Prueba de Shapiro-Wilk para normalidad
# Hipótesis Nula: Los datos provienen de una distribución normal.
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

# Gráfico Q-Q para visualizar la normalidad
qqnorm(residuals, main = "Q-Q Plot de los Residuos")
qqline(residuals, col = "red")

# Verificar la Homogeneidad de Varianzas
# Prueba de Bartlett para homogeneidad de varianzas
# Hipótesis Nula: Las varianzas de los grupos son iguales (homogeneidad de varianzas).
bartlett_test <- bartlett.test(Sepal.Length ~ Species, data = iris)
print(bartlett_test)

# Gráfico de los residuos vs. los valores ajustados para verificar la homogeneidad
fitted_values <- fitted(anova_result)
plot(fitted_values, residuals, main = "Residuos vs. Valores Ajustados",
     xlab = "Valores Ajustados", ylab = "Residuos")
abline(h = 0, col = "red")


# Pruebas Post-Hoc Tukey, solo si el ANOVA es significativo
if (summary(anova_sepalo_l)[[1]]["Species", "Pr(>F)"] < 0.05) {
  posthoc_result <- TukeyHSD(anova_sepalo_l)
  print(posthoc_result)
  
  # Gráfico de los intervalos de confianza de las diferencias entre las medias
  plot(posthoc_result)
} else {
  print("No se encontraron diferencias significativas entre las especies.")
}






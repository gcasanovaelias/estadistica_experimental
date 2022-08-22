# Packages ----------------------------------------------------------------

library(tidyverse)
library(broom)


# Pregunta 1 --------------------------------------------------------------

# El siguiente cuadro muestra los datos observados de temperatura de ebullición del agua a distintas altitudes. Es de interés predecir la temperatura de ebullición del agua a diferentes altitudes, por lo que el investigador desea construir un modelo estadístico a través de un análisis de regresión lineal simple.
datos1 <- {tibble(Altitud = c(5740.9, 5740.9, 4934.3, 4793.1, 4538.0, 4430.1, 4134.2, 4078.6, 4061.9, 4067.5, 3423.2, 2565.5, 1339.2, 1815.5, 972.1, 398.0, 182.5),
                 Temperatura = c(89.89, 89.84, 93.02, 92.48, 93.20, 91.53, 94.48, 92.17, 95.52, 94.39, 95.47, 94.87, 98.05, 98.12, 98.26, 97.53, 99.91))}

# Gráficos de dispersión
pairs(datos1)

ggplot(data = datos1, aes(x = Temperatura, y = Altitud)) +
  geom_point() + 
  stat_smooth(method = "lm", 
              formula = y ~ x,
              se = F)

# Modelo
modelo1 <- lm(formula = Altitud ~ Temperatura,
              data = datos1)

tidy(modelo1)
glance(modelo1)

modelo1_augment <- modelo1 %>% augment()

# Gráfico

ggplot(data = modelo1_augment, aes(x = Temperatura, y = Altitud)) +
  geom_point() +
  geom_line(aes(y = .fitted))

# Supuestos

par(mfrow = c(1, 1))
plot(modelo1)

# Normalidad de los residuos

qqnorm(modelo1_augment$.resid)
qqline(modelo1_augment$.resid)

qplot(sample = Altitud, data = datos1) + stat_qq_line()

ggplot(data = datos1, aes(sample = Altitud)) +
  # stat_qq()
  geom_qq() +
  # stat_qq_line()
  geom_qq_line()

shapiro.test(modelo1_augment$.resid) ## HO: distribución normal

# Homocedasticidad de las varianzas

plot(x = modelo1_augment$.fitted,
     y = modelo1_augment$.resid)

ggplot(data = modelo1_augment, aes(x = .fitted,
                                   y = .resid)) +
  geom_point() + 
  geom_hline(yintercept = 0,
             lty = 2,
             color = "red")


# Intervalos

# Intervalos de Confianza

confint(modelo1, level = 0.95) # confint()

predict(modelo1, interval = "confidence", level = 0.95)

# Intervalos de Predicción

predict(modelo1, interval = 'prediction')


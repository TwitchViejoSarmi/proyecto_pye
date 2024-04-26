library(readxl)
library(ggplot2)
Base_Datos <- read.csv("Base de Datos/Datos.csv")

library(readr)
Datos <- read_csv("Base de Datos/Datos.csv")

# Variables punto 1.
#print(Base_Datos$PESO..Gramos.)
#print(Base_Datos$AREA.RESIDENCIA)

ggplot(Datos, aes(x = `AREA NACIMIENTO`, y = `PESO (Gramos)`)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(title = "Comparación de peso al nacer por área de residencia",
       x = "Área de Residencia",
       y = "Peso al Nacer (g)")


# Variables punto 2.
#print(Base_Datos$SITIO.NACIMIENTO)
#print(Base_Datos$AREA.RESIDENCIA)

#Variables punto 3.
#print(Base_Datos$PESO..Gramos.)
#print(Base_Datos$TALLA..Centímetros.)
library(readxl)
library(dplyr)
Base_Datos <- read.csv("Base de Datos/Datos.csv")
Base_Datos$PESO..Gramos. <- gsub(",", ".", Base_Datos$PESO..Gramos.)
Base_Datos$PESO..Gramos. <- as.numeric(Base_Datos$PESO..Gramos.)
Base_Datos$TALLA..Centímetros. <- gsub(",", ".", Base_Datos$TALLA..Centímetros.)
Base_Datos$TALLA..Centímetros. <- as.numeric(Base_Datos$TALLA..Centímetros.)
Base_Datos <- subset(Base_Datos, PESO..Gramos. < 575) # Se hace limpieza de datos.

# Variables punto 1.
PESO <- Base_Datos$PESO..Gramos.

# Gráfico 1.
boxplot(PESO ~ Base_Datos$AREA.RESIDENCIA,
        xlab = "AREA DE RESIDENCIA",
        ylab = "PESO (KILOGRAMOS)",
        names = c("CABECERA MUNICIPAL", "CENTRO POBLADO", "RURAL DISPERSO"))

# Gráfico 2.
PESO_C <- subset(Base_Datos, AREA.RESIDENCIA == "CABECERA MUNICIPAL")$PESO..Gramos.

His_C <- hist(PESO_C, plot = FALSE)

PESO_CP <- subset(Base_Datos, AREA.RESIDENCIA == "CENTRO POBLADO (INSPECCIÓN, CORREGIMIENTO O CASERÍO)")$PESO..Gramos.

His_CP <- hist(PESO_CP, plot = FALSE)

PESO_R <- subset(Base_Datos, AREA.RESIDENCIA == "RURAL DISPERSO")$PESO..Gramos.

His_R <- hist(PESO_R, plot = FALSE)

plot(c(min(PESO), His_C$mids, max(PESO)), c(0, His_C$counts, 0), type = "b", col = "red", xlab = "PESO (g)", ylab = "FRECUENCIA")

lines(c(min(PESO), His_CP$mids, max(PESO)), c(0, His_CP$counts, 0), type = "b", col = "blue")

lines(c(min(PESO), His_R$mids, max(PESO)), c(0, His_R$counts, 0), type = "b", col = "green")

legend("topleft", legend = c("CABECERA MUNICIPAL", "CENTRO POBLADO", "RURAL DISPERSO"), 
       fill = c("red", "blue", "green"))

# Gráfico 3.
plot(c(0, His_C$mids),
     c(0, cumsum(His_C$counts)/sum(His_C$counts)),
     type = "b",
     col = "red",
     xlab = "PESO (KILOGRAMOS)",
     ylab = "FRECUENCIA RELATIVA ACUMULADA")

lines(c(0, His_CP$mids),
      c(0, cumsum(His_CP$counts)/sum(His_CP$counts)),
      type = "b",
      col = "blue")

lines(c(0, His_R$mids),
      c(0, cumsum(His_R$counts)/sum(His_R$counts)),
      type = "b",
      col = "green")
legend("topleft", legend = c("CABECERA MUNICIPAL", "CENTRO POBLADO", "RURAL DISPERSO"), 
       fill = c("red", "blue", "green"))

tabla <- Base_Datos %>%
  group_by(AREA.RESIDENCIA) %>%
  summarise(count = n(),
  media = round(mean(PESO..Gramos., na.rm = TRUE), digits = 2),
  mediana = median(PESO..Gramos., na.rm = TRUE),
  sd = round(sd(PESO..Gramos., na.rm = TRUE), digits = 2),
  q1 = quantile(PESO..Gramos., probs = 0.25, na.rm = TRUE),
  q3 = quantile(PESO..Gramos., probs = 0.75, na.rm = TRUE),
  RIC = IQR(PESO..Gramos., na.rm = TRUE)
  )
print(tabla)

# Variables punto 2.
#print(Base_Datos$SITIO.NACIMIENTO)
#print(Base_Datos$AREA.RESIDENCIA)

tabla <- table(Base_Datos$TIPO.PARTO, Base_Datos$AREA.RESIDENCIA)

print(tabla)

barplot(tabla, #prop.table(tabla),
        beside = TRUE,
        col = c("blue","red", 'green'),
        xlab = "Area de residencia",
        ylab = "Cantidad de partos")
legend("topright", legend = c("Cesarea", "Espontáneo", "Instrumentado"), 
       fill = c("blue","red", 'green'))
        #ylim= c(0,30))


#Variables punto 3.
#print(Base_Datos$PESO..Gramos.)
#print(Base_Datos$TALLA..Centímetros.)

TALLA <- Base_Datos$TALLA..Centímetros.
plot(PESO, TALLA,
     xlab = 'PESO (KILOGRAMOS)',
     ylab = 'TALLA (CENTIMETROS)')

print(cor(PESO, TALLA))
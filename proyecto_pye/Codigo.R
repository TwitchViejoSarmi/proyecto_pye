library(readxl)
Base_Datos <- read.csv("Base de Datos/Datos.csv")
Base_Datos <- subset(Base_Datos, PESO..Gramos. < 575) # Se hace limpieza de datos.


# Variables punto 1.
PESO <- gsub(",", ".", Base_Datos$PESO..Gramos.)
PESO <- as.numeric(PESO)

# Gráfico 1.
boxplot(PESO ~ Base_Datos$AREA.RESIDENCIA,
        main = "PESO EN KILOGRAMOS DISTRIBUIDO POR AREA DE RESIDENCIA",
        xlab = "AREA DE RESIDENCIA",
        ylab = "PESO (KILOGRAMOS)")

# Gráfico 2.
PESO_C <- subset(Base_Datos, AREA.RESIDENCIA == "CABECERA MUNICIPAL")$PESO..Gramos.
PESO_C <- gsub(",", ".", PESO_C)
PESO_C <- as.numeric(PESO_C)

His_C <- hist(PESO_C, plot = FALSE)

print(His_C)

PESO_CP <- subset(Base_Datos, AREA.RESIDENCIA == "CENTRO POBLADO (INSPECCIÓN, CORREGIMIENTO O CASERÍO)")$PESO..Gramos.
PESO_CP <- gsub(",", ".", PESO_CP)
PESO_CP <- as.numeric(PESO_CP)

His_CP <- hist(PESO_CP, plot = FALSE)

PESO_R <- subset(Base_Datos, AREA.RESIDENCIA == "RURAL DISPERSO")$PESO..Gramos.
PESO_R <- gsub(",", ".", PESO_R)
PESO_R <- as.numeric(PESO_R)

His_R <- hist(PESO_R, plot = FALSE)

plot(c(min(PESO), His_C$mids, max(PESO)), c(0, His_C$counts, 0), type = "b", col = "red", main = "PESO EN GRAMOS POR AREA DE RESIDENCIA", xlab = "PESO (g)", ylab = "FRECUENCIA")

lines(c(min(PESO), His_CP$mids, max(PESO)), c(0, His_CP$counts, 0), type = "b", col = "blue")

lines(c(min(PESO), His_R$mids, max(PESO)), c(0, His_R$counts, 0), type = "b", col = "green")

legend("topleft", legend = c("CABECERA MUNICIPAL", "CENTRO POBLADO", "RURAL DISPERSO"), 
       fill = c("red", "blue", "green"))

# Gráfico 3.
plot(c(0, His_C$mids),
     c(0, cumsum(His_C$counts)/sum(His_C$counts)),
     type = "b",
     col = "red",
     main = "PESO EN GRAMOS DISTRIBUIDO POR AREA DE RESIDENCIA",
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

# Variables punto 2.
#print(Base_Datos$SITIO.NACIMIENTO)
#print(Base_Datos$AREA.RESIDENCIA)

tabla <- table(Base_Datos$TIPO.PARTO, Base_Datos$AREA.RESIDENCIA)

tabla

barplot(tabla, #prop.table(tabla),
        beside = TRUE,
        col = c("blue","red", 'green'),
        main = "PESO EN GRAMOS DISTRIBUIDO POR AREA DE RESIDENCIA",
        xlab = "Area de residencia",
        ylab = "Cantidad de partos")
legend("topright", legend = c("Cesarea", "Espontáneo", "Instrumentado"), 
       fill = c("blue","red", 'green'))
        #ylim= c(0,30))


#Variables punto 3.
#print(Base_Datos$PESO..Gramos.)
#print(Base_Datos$TALLA..Centímetros.)

TALLA <- gsub(",", ".", Base_Datos$TALLA..Centímetros.)
TALLA <- as.numeric(TALLA)

plot(PESO, TALLA, 
     main = "RELACIÓN TALLA CONTRA EL PESO AL NACER",
     xlab = 'PESO (KILOGRAMOS)',
     ylab = 'TALLA (CENTIMETROS)')

print(cor(PESO, TALLA))
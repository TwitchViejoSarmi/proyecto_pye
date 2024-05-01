library(readxl)
#library(ggplot2)
Base_Datos <- read.csv("Base de Datos/Datos.csv")

#library(readr)
#Datos <- read_csv("Base de Datos/Datos.csv")

# Variables punto 1.
#print(Base_Datos$PESO..Gramos.)
#print(Base_Datos$AREA.RESIDENCIA)

#ggplot(Datos, aes(x = `AREA NACIMIENTO`, y = `PESO (Gramos)`)) +
  #geom_boxplot(fill = "skyblue", color = "blue") +
  #labs(title = "Comparación de peso al nacer por área de residencia",
       #x = "Área de Residencia",
       #y = "Peso al Nacer (g)")
PESO <- gsub(",", ".", Base_Datos$PESO..Gramos.)
PESO <- as.numeric(PESO)

# Gráfico 1.
boxplot(PESO ~ Base_Datos$AREA.NACIMIENTO,
        main = "PESO EN GRAMOS DISTRIBUIDO POR AREA DE NACIMIENTO",
        ylab = "AREA DE NACIMIENTO",
        xlab = "PESO (GRAMOS)")

# Gráfico 2.
PESO_C <- subset(Base_Datos, AREA.NACIMIENTO == "CABECERA MUNICIPAL")$PESO..Gramos.
PESO_C <- gsub(",", ".", PESO_C)
PESO_C <- as.numeric(PESO_C)

His_C <- hist(PESO_C, plot = FALSE)

PESO_CP <- subset(Base_Datos, AREA.NACIMIENTO == "CENTRO POBLADO (INSPECCIÓN, CORREGIMIENTO O CASERÍO)")$PESO..Gramos.
PESO_CP <- gsub(",", ".", PESO_CP)
PESO_CP <- as.numeric(PESO_CP)

His_CP <- hist(PESO_CP, plot = FALSE)

PESO_R <- subset(Base_Datos, AREA.NACIMIENTO == "RURAL DISPERSO")$PESO..Gramos.
PESO_R <- gsub(",", ".", PESO_R)
PESO_R <- as.numeric(PESO_R)

His_R <- hist(PESO_R, plot = FALSE)

plot(c(min(PESO), His_C$mids, max(PESO)), c(0, His_C$counts, 0), type = "b", col = "red", main = "PESO EN GRAMOS POR AREA DE NACIMIENTO", xlab = "PESO (g)", ylab = "FRECUENCIA")


# Hay que revisar las observaciones 575 y 1160
h<-hist(PESO_C)
which(PESO_C>500)
PESO_C.new<-PESO_C[-c(575,1160)]
hist(PESO_C.new)
boxplot(PESO_C.new)

#NO tiene sentidon con un valor.
h<-hist(PESO_R)


#lines(c(min(PESO), His_CP$mids, max(PESO)), c(0, His_CP$counts, 0), type = "b", col = "blue")

#lines(c(min(PESO), His_R$mids, max(PESO)), c(0, His_R$counts, 0), type = "b", col = "green")

#legend("topright", legend = c("CABECERA MUNICIPAL", "CENTRO POBLADO", "RURAL DISPERSO"), 
       #fill = c("red", "blue", "green"))
# Gráfico 3.


# Variables punto 2.
#print(Base_Datos$SITIO.NACIMIENTO)
#print(Base_Datos$AREA.RESIDENCIA)

#Variables punto 3.
#print(Base_Datos$PESO..Gramos.)
#print(Base_Datos$TALLA..Centímetros.)
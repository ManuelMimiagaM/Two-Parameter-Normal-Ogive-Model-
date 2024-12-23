# Instalar paquetes si no están instalados
if(!require(ltm)) install.packages("ltm")

options(scipen = 999)
# Cargar el paquete
library(ltm)
library(gridExtra)
library (readr)
# Cargar los datos
data <- read.csv("C:/Users/UNISA_96/Downloads/1P_MFyC_Calibración (2).csv")

# Separar la columna "Matrícula" y las respuestas de los ítems (Q1 a Q32)
respuestas <- data[, -1]  # Eliminamos la columna de "Matrícula"

# Ajustar el modelo 2PL
irt_model <- ltm(respuestas ~ z1, IRT.param = TRUE)

# Resumen de los resultados
summary(irt_model)

# Obtener parámetros de discriminación (a) y dificultad (b)
params <- coef(irt_model)

# Mostrar los parámetros
print(params)

# Graficar la Curva Característica del Ítem para el ítem 1, por ejemplo
plot(irt_model, items = 2)

# Función para graficar la CCI de un ítem específico
plot_cci <- function(item) {
  # Asegurarse de que el ítem esté dentro de los límites
  if (item < 1 || item > nrow(params)) {
    stop("Ítem fuera de límites. Asegúrate de que el índice esté entre 1 y ", nrow(params))
  }
  
  # Generar un rango de valores theta
  theta <- seq(-3, 3, length.out = 100)
  
  # Obtener los parámetros de discriminación (a) y dificultad (b) del ítem
  a <- params[item, "Dscrmn"]
  b <- params[item, "Dffclt"]
  
  # Calcular la probabilidad de respuesta correcta usando la función logística corregida
  probabilidad_correcta <- 1 / (1 + exp(-a * (theta - b)))
  
  # Graficar la Curva Característica del Ítem
  ggplot(data.frame(theta, probabilidad_correcta), aes(x = theta, y = probabilidad_correcta)) +
    geom_line() +
    ggtitle(paste("Curva Característica del Ítem", item)) +
    xlab("Habilidad Latente (Theta)") +
    ylab("Probabilidad de Respuesta Correcta") +
    theme_minimal()
}


# Graficar la CCI para el primer ítem (puedes cambiar el número para otros ítems)
plot_cci(11)

# Función para calcular la información del ítem
calc_info_item <- function(a, b, theta) {
  p <- calc_probabilidad(a, b, theta)
  info <- a^2 * p * (1 - p)
  return(info)
}

# Función para calcular la probabilidad de respuesta correcta
calc_probabilidad <- function(a, b, theta) {
  1 / (1 + exp(-a * (theta - b)))
}


# Función para graficar la Información del Ítem (IFI)
plot_ifi <- function(item) {
  # Asegurarse de que el ítem esté dentro de los límites
  if (item < 1 || item > nrow(params)) {
    stop("Ítem fuera de límites. Asegúrate de que el índice esté entre 1 y ", nrow(params))
  }
  
  # Generar un rango de valores theta
  theta <- seq(-3, 3, length.out = 100)
  
  # Obtener los parámetros de discriminación (a) y dificultad (b) del ítem
  a <- params[item, "Dscrmn"]
  b <- params[item, "Dffclt"]
  
  # Calcular la información del ítem
  info_item <- calc_info_item(a, b, theta)
  
  # Graficar la función de información del ítem
  p <- ggplot(data.frame(theta, info_item), aes(x = theta, y = info_item)) +
    geom_line() +
    ggtitle(paste("Función de Información del Ítem", item)) +
    xlab("Habilidad Latente (Theta)") +
    ylab("Información del Ítem") +
    theme_minimal()
  
  return(p)
}
# Graficar la función de información del ítem 1

plot_ifi(1)

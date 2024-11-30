# Despejar todo el ambiente
rm(list = ls())

# Cargar los paquetes necesarios
library(readr)
library(dplyr)
library(stringr)

# Especificar las rutas a los archivos
train_path <- "C:/Users/USUARIO/Documents/Trabajo/BIG Data/Taller #3/datos/train.csv"
test_path <- "C:/Users/USUARIO/Documents/Trabajo/BIG Data/Taller #3/datos/test.csv"

# Cargar los archivos en R
train_data <- read_csv(train_path)
test_data <- read_csv(test_path)

# Definir la lista completa de barrios
barrios_seleccionados <- c(
  "La Esperanza Nororiental", "La Sureña", "San Isidro", "San Luis Altos del Cabo",
  "Bosque Calderón", "Bosque Calderón Tejada", "Chapinero Alto", "El Castillo", 
  "El Paraíso", "Emaus", "Granada", "Ingemar", "Juan XXIII", "La Salle", 
  "Las Acacias", "Los Olivos", "María Cristina", "Mariscal Sucre", 
  "Nueva Granada", "Palomar", "Pardo Rubio", "San Martín de Porres", 
  "Villa Anita", "Villa del Cerro", "Antiguo Country", "Chicó Norte", 
  "Chicó Norte II", "Chicó Norte III", "Chicó Occidental", "El Chicó", 
  "El Retiro", "Espartillal", "La Cabrera", "Lago Gaitán", 
  "Porciúncula", "Quinta Camacho", "la cabrera", "parque virrey", "el virrey",
  "chico", "cabrera", "los cerezos", "floralia", "fontibon", "villa del prado", "fatima", "Rosales", 
  "rosales", "el nogal", "chapinero alto"
)

# Función para extraer el barrio del título o descripción
extraer_barrio <- function(texto, barrios) {
  if (str_detect(tolower(texto), "virrey")) {
    return("Antiguo Country")  # Asigna "Antiguo Country" si menciona "virrey"
  } else {
    barrio <- barrios[str_detect(tolower(texto), barrios)]
    if (length(barrio) > 0) {
      return(barrio[1])  # Retorna el primer barrio encontrado
    } else {
      return(NA)
    }
  }
}

# Aplicar la función de extracción de barrio y crear la variable cerca_virrey
train_data <- train_data %>%
  mutate(
    barrio = sapply(paste(title, description), extraer_barrio, barrios = barrios_seleccionados),
    cerca_virrey = ifelse(str_detect(tolower(paste(title, description)), "virrey"), 1, 0)
  )

test_data <- test_data %>%
  mutate(
    barrio = sapply(paste(title, description), extraer_barrio, barrios = barrios_seleccionados),
    cerca_virrey = ifelse(str_detect(tolower(paste(title, description)), "virrey"), 1, 0)
  )

# Contar el número de parqueaderos en cada observación
train_data <- train_data %>%
  mutate(num_parqueaderos = str_count(description, "(?i)parqueadero|garaje"))

test_data <- test_data %>%
  mutate(num_parqueaderos = str_count(description, "(?i)parqueadero|garaje"))

# Contar el número de baños en cada observación y establecer un mínimo de 1
train_data <- train_data %>%
  mutate(num_banos = pmax(1, str_count(description, "(?i)bano|ba\\s?ntilde\\s?o|bao|ba\\s?n\\s?o")))

test_data <- test_data %>%
  mutate(num_banos = pmax(1, str_count(description, "(?i)bano|ba\\s?ntilde\\s?o|bao|ba\\s?n\\s?o")))


# Visualizar las bases de datos transformadas
print(head(train_data))
print(head(test_data))

# Especificar la ruta de guardado de los archivos
train_output_path <- "C:/Users/USUARIO/Documents/Trabajo/BIG Data/Taller #3/datos/train_barrios.csv"
test_output_path <- "C:/Users/USUARIO/Documents/Trabajo/BIG Data/Taller #3/datos/test_barrios.csv"

# Exportar los archivos con el sufijo "_barrios"
write_csv(train_data, train_output_path)
write_csv(test_data, test_output_path)


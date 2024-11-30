rm(list = ls())

library(tidyverse)  # Para manipulación de datos
library(readr)   
library(glmnet)      # Para Elastic Net
library(caret)  


train_data <- read.csv("datos/train_PS3.csv")
test_data <- read.csv("datos/test_PS3.csv")


# Crear variables dummy en los datos de entrenamiento
train_data <- train_data %>%
  mutate(
    is_chico = ifelse(barrio == "chico", 1, 0),
    is_rosales = ifelse(barrio == "rosales", 1, 0),
    is_el_nogal = ifelse(barrio == "el nogal", 1, 0)
  )

# Crear variables dummy en los datos de prueba
test_data <- test_data %>%
  mutate(
    is_chico = ifelse(barrio == "chico", 1, 0),
    is_rosales = ifelse(barrio == "rosales", 1, 0),
    is_el_nogal = ifelse(barrio == "el nogal", 1, 0)
  )

# Separar el conjunto de datos de entrenamiento en 70% train y 30% test
set.seed(123)  # Fijar semilla para reproducibilidad
train_index <- createDataPartition(train_data$price, p = 0.7, list = FALSE)

train_split <- train_data[train_index, ]
val_split <- train_data[-train_index, ]

# Filtrar filas completas en las variables utilizadas
train_clean <- train_split %>%
  drop_na(year, surface_total, rooms, property_type, cerca_virrey, 
          num_banos, distance_to_nearest_transport_station, is_chico, price)

val_clean <- val_split %>%
  drop_na(year, surface_total, rooms, property_type, cerca_virrey, 
          num_banos, distance_to_nearest_transport_station, is_chico, price)

# Preparar la matriz de predictores con las variables propuestas
X <- model.matrix(price ~ year + surface_total + rooms + 
                    property_type + cerca_virrey + num_banos +
                    distance_to_nearest_transport_station +
                    I(distance_to_nearest_transport_station^2) + 
                    is_chico, 
                  data = train_clean)[,-1]  # Eliminar el intercepto

y <- train_clean$price  # Variable dependiente

# Preparar la matriz de predictores para los datos de validación
X_val <- model.matrix(price ~ year + surface_total + rooms + 
                        property_type + cerca_virrey + num_banos +
                        distance_to_nearest_transport_station +
                        I(distance_to_nearest_transport_station^2) + 
                        is_chico, 
                      data = val_clean)[,-1]  # Eliminar el intercepto

y_val <- val_clean$price  # Variable dependiente para validación

# Verificar dimensiones
cat("Dimensiones de X (train):", nrow(X), "filas y", ncol(X), "columnas\n")
cat("Dimensiones de y (train):", length(y), "observaciones\n")
cat("Dimensiones de X_val (val):", nrow(X_val), "filas y", ncol(X_val), "columnas\n")
cat("Dimensiones de y_val (val):", length(y_val), "observaciones\n")

# Ajustar el modelo Elastic Net con validación cruzada
set.seed(10101)  # Semilla para reproducibilidad
cv_fit <- cv.glmnet(X, y, alpha = 0.5, family = "gaussian", type.measure = "mae", nfolds = 10)

# Obtener el mejor lambda
best_lambda <- cv_fit$lambda.min
cat("Elastic Net - Mejor lambda:", best_lambda, "\n")

# Ajustar el modelo final con el mejor lambda
elastic_net_model <- glmnet(X, y, alpha = 0.5, lambda = best_lambda, family = "gaussian")

# Realizar predicciones en los datos de validación
predicciones <- predict(elastic_net_model, newx = X_val)

# Calcular el MAE en el conjunto de validación
mae_value <- mean(abs(predicciones - y_val))
cat("MAE del modelo Elastic Net:", mae_value, "\n")



# Imputar valores faltantes en test_data
test_data_imputed <- test_data %>%
  mutate(
    year = ifelse(is.na(year), median(train_data$year, na.rm = TRUE), year),
    surface_total = ifelse(is.na(surface_total), median(train_data$surface_total, na.rm = TRUE), surface_total),
    rooms = ifelse(is.na(rooms), median(train_data$rooms, na.rm = TRUE), rooms),
    property_type = ifelse(is.na(property_type), "Apartamento", property_type),  # Rellenar con el más común
    cerca_virrey = ifelse(is.na(cerca_virrey), 0, cerca_virrey),  # Considerar 0 como no cercano al parque
    num_banos = ifelse(is.na(num_banos), median(train_data$num_banos, na.rm = TRUE), num_banos),
    distance_to_nearest_transport_station = ifelse(is.na(distance_to_nearest_transport_station), 
                                                   median(train_data$distance_to_nearest_transport_station, na.rm = TRUE), 
                                                   distance_to_nearest_transport_station),
    is_chico = ifelse(is.na(is_chico), 0, is_chico)  # Si falta, considerar que no es "Chico"
  )

# Preparar la matriz de predictores para los datos de prueba imputados
X_test <- model.matrix(~ year + surface_total + rooms + 
                         property_type + cerca_virrey + num_banos +
                         distance_to_nearest_transport_station +
                         I(distance_to_nearest_transport_station^2) + 
                         is_chico, 
                       data = test_data_imputed)[,-1]  # Eliminar el intercepto

# Realizar predicciones con el modelo ajustado
predicciones_test <- predict(elastic_net_model, newx = X_test)

# Crear un dataframe con las columnas requeridas para la predicción
output <- data.frame(
  property_id = test_data_imputed$property_id,  # Identificador único
  price = predicciones_test    # Precio predicho
)

# Cambiar el nombre de la columna predicha a "price" (si no está correcta)
colnames(output)[2] <- "price"

# Guardar las predicciones en un archivo CSV con el formato adecuado
write.table(output, "ElasticNet_Predictions.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)

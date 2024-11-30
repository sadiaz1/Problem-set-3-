rm(list = ls())

library(tidyverse)
library(randomForest)
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


# Preparar los datos (división en entrenamiento y validación)
set.seed(123)  # Semilla para reproducibilidad
train_index <- createDataPartition(train_data$price, p = 0.7, list = FALSE)

train_split <- train_data[train_index, ]
val_split <- train_data[-train_index, ]

# Imputación de valores faltantes para train_split
train_split_imputed <- train_split %>%
  mutate(
    year = ifelse(is.na(year), median(year, na.rm = TRUE), year),
    surface_total = ifelse(is.na(surface_total), median(surface_total, na.rm = TRUE), surface_total),
    rooms = ifelse(is.na(rooms), median(rooms, na.rm = TRUE), rooms),
    property_type = ifelse(is.na(property_type), "Apartamento", property_type),
    cerca_virrey = ifelse(is.na(cerca_virrey), 0, cerca_virrey),
    num_banos = ifelse(is.na(num_banos), median(num_banos, na.rm = TRUE), num_banos),
    distance_to_nearest_transport_station = ifelse(is.na(distance_to_nearest_transport_station), 
                                                   median(distance_to_nearest_transport_station, na.rm = TRUE), 
                                                   distance_to_nearest_transport_station),
    is_chico = ifelse(is.na(is_chico), 0, is_chico)
  )

# Imputación de valores faltantes para val_split
val_split_imputed <- val_split %>%
  mutate(
    year = ifelse(is.na(year), median(year, na.rm = TRUE), year),
    surface_total = ifelse(is.na(surface_total), median(surface_total, na.rm = TRUE), surface_total),
    rooms = ifelse(is.na(rooms), median(rooms, na.rm = TRUE), rooms),
    property_type = ifelse(is.na(property_type), "Apartamento", property_type),
    cerca_virrey = ifelse(is.na(cerca_virrey), 0, cerca_virrey),
    num_banos = ifelse(is.na(num_banos), median(num_banos, na.rm = TRUE), num_banos),
    distance_to_nearest_transport_station = ifelse(is.na(distance_to_nearest_transport_station), 
                                                   median(distance_to_nearest_transport_station, na.rm = TRUE), 
                                                   distance_to_nearest_transport_station),
    is_chico = ifelse(is.na(is_chico), 0, is_chico)
  )

# Entrenar el modelo Random Forest
rf_model <- randomForest(
  price ~ year + surface_total + rooms + property_type + cerca_virrey + num_banos +
    distance_to_nearest_transport_station + I(distance_to_nearest_transport_station^2) + is_chico,
  data = train_split_imputed,
  ntree = 500,        # Número de árboles
  mtry = 3,           # Número de variables seleccionadas en cada división
  importance = TRUE,  # Calcular importancia de las variables
  na.action = na.omit # Manejo de valores faltantes
)

# Resumen del modelo
print(rf_model)

# Evaluar el modelo en el conjunto de validación
val_split_imputed$predictions <- predict(rf_model, newdata = val_split_imputed)

# Calcular el MAE en el conjunto de validación
mae_rf <- mean(abs(val_split_imputed$price - val_split_imputed$predictions))
cat("MAE del modelo Random Forest:", mae_rf, "\n")



rf_model <- randomForest(
  price ~ year + surface_total + rooms + property_type + cerca_virrey + num_banos +
    distance_to_nearest_transport_station + I(distance_to_nearest_transport_station^2) + 
    is_chico +
    # Interacciones propuestas
    is_chico:distance_to_nearest_park + 
    is_chico:distance_to_financial_center + 
    is_rosales:distance_to_nearest_park + 
    is_rosales:distance_to_financial_center + 
    is_el_nogal:distance_to_nearest_park + 
    is_el_nogal:distance_to_financial_center,
  data = train_split_imputed,
  ntree = 500,        # Número de árboles
  mtry = 3,           # Número de variables seleccionadas en cada división
  importance = TRUE,  # Calcular importancia de las variables
  na.action = na.omit # Manejo de valores faltantes
)

# Resumen del modelo
print(rf_model)

# Evaluar el modelo en el conjunto de validación
val_split_imputed$predictions <- predict(rf_model, newdata = val_split_imputed)

# Calcular el MAE en el conjunto de validación
mae_rf <- mean(abs(val_split_imputed$price - val_split_imputed$predictions))
cat("MAE del modelo Random Forest con interacciones:", mae_rf, "\n")




# Entrenar el modelo Random Forest
rf_model <- randomForest(
  price ~ year + surface_total + rooms + property_type + cerca_virrey + num_banos +
    distance_to_nearest_transport_station + I(distance_to_nearest_transport_station^2) + 
    is_chico +
    # Interacciones propuestas
    is_chico:distance_to_nearest_park + 
    is_chico:distance_to_financial_center + 
    is_rosales:distance_to_nearest_park + 
    is_rosales:distance_to_financial_center + 
    is_el_nogal:distance_to_nearest_park + 
    is_el_nogal:distance_to_financial_center,
  data = train_split_imputed,
  ntree = 500,        # Número de árboles
  mtry = 3,           # Número de variables seleccionadas en cada división
  importance = TRUE,  # Calcular importancia de las variables
  na.action = na.omit # Manejo de valores faltantes
)

# Resumen del modelo
print(rf_model)
cat("Número de árboles usados (ntree):", rf_model$ntree, "\n")
cat("Número de variables por división (mtry):", rf_model$mtry, "\n")

# Imputar valores faltantes en el conjunto de prueba definitivo
test_data_imputed <- test_data %>%
  mutate(
    year = ifelse(is.na(year), median(train_data$year, na.rm = TRUE), year),
    surface_total = ifelse(is.na(surface_total), median(train_data$surface_total, na.rm = TRUE), surface_total),
    rooms = ifelse(is.na(rooms), median(train_data$rooms, na.rm = TRUE), rooms),
    property_type = ifelse(is.na(property_type), "Apartamento", property_type),
    cerca_virrey = ifelse(is.na(cerca_virrey), 0, cerca_virrey),
    num_banos = ifelse(is.na(num_banos), median(train_data$num_banos, na.rm = TRUE), num_banos),
    distance_to_nearest_transport_station = ifelse(is.na(distance_to_nearest_transport_station), 
                                                   median(train_data$distance_to_nearest_transport_station, na.rm = TRUE), 
                                                   distance_to_nearest_transport_station),
    distance_to_nearest_park = ifelse(is.na(distance_to_nearest_park), 
                                      median(train_data$distance_to_nearest_park, na.rm = TRUE), 
                                      distance_to_nearest_park),
    distance_to_financial_center = ifelse(is.na(distance_to_financial_center), 
                                          median(train_data$distance_to_financial_center, na.rm = TRUE), 
                                          distance_to_financial_center),
    is_chico = ifelse(is.na(is_chico), 0, is_chico),
    is_rosales = ifelse(is.na(is_rosales), 0, is_rosales),
    is_el_nogal = ifelse(is.na(is_el_nogal), 0, is_el_nogal)
  )

# Realizar predicciones con el modelo Random Forest ajustado
test_data_imputed$price <- predict(rf_model, newdata = test_data_imputed)

# Crear un dataframe con las columnas requeridas
final_output <- data.frame(
  property_id = test_data_imputed$property_id,  # Identificador único
  price = test_data_imputed$price              # Precio predicho
)

# Guardar las predicciones en un archivo CSV en el formato requerido
write.table(final_output, "Final_RandomForest_Predictions.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)







# Entrenar el modelo Random Forest con las nuevas interacciones
rf_model_updated <- randomForest(
  price ~ year + surface_total + rooms + property_type + cerca_virrey + num_banos +
    distance_to_nearest_transport_station + I(distance_to_nearest_transport_station^2) + 
    is_chico +
    # Interacciones originales
    is_chico:distance_to_nearest_park + 
    is_chico:distance_to_financial_center + 
    is_rosales:distance_to_nearest_park + 
    is_rosales:distance_to_financial_center + 
    is_el_nogal:distance_to_nearest_park + 
    is_el_nogal:distance_to_financial_center +
    # Nuevas interacciones con num_parqueaderos
    num_parqueaderos:distance_to_nearest_transport_station +
    num_parqueaderos:distance_to_nearest_park +
    num_parqueaderos:distance_to_financial_center,
  data = train_split_imputed,
  ntree = 500,        # Número de árboles
  mtry = 3,           # Número de variables seleccionadas en cada división
  importance = TRUE,  # Calcular importancia de las variables
  na.action = na.omit # Manejo de valores faltantes
)

# Resumen del modelo
print(rf_model_updated)
cat("Número de árboles usados (ntree):", rf_model_updated$ntree, "\n")
cat("Número de variables por división (mtry):", rf_model_updated$mtry, "\n")

# Imputar valores faltantes en el conjunto de prueba definitivo
test_data_imputed <- test_data %>%
  mutate(
    year = ifelse(is.na(year), median(train_data$year, na.rm = TRUE), year),
    surface_total = ifelse(is.na(surface_total), median(train_data$surface_total, na.rm = TRUE), surface_total),
    rooms = ifelse(is.na(rooms), median(train_data$rooms, na.rm = TRUE), rooms),
    property_type = ifelse(is.na(property_type), "Apartamento", property_type),
    cerca_virrey = ifelse(is.na(cerca_virrey), 0, cerca_virrey),
    num_banos = ifelse(is.na(num_banos), median(train_data$num_banos, na.rm = TRUE), num_banos),
    num_parqueaderos = ifelse(is.na(num_parqueaderos), median(train_data$num_parqueaderos, na.rm = TRUE), num_parqueaderos),
    distance_to_nearest_transport_station = ifelse(is.na(distance_to_nearest_transport_station), 
                                                   median(train_data$distance_to_nearest_transport_station, na.rm = TRUE), 
                                                   distance_to_nearest_transport_station),
    distance_to_nearest_park = ifelse(is.na(distance_to_nearest_park), 
                                      median(train_data$distance_to_nearest_park, na.rm = TRUE), 
                                      distance_to_nearest_park),
    distance_to_financial_center = ifelse(is.na(distance_to_financial_center), 
                                          median(train_data$distance_to_financial_center, na.rm = TRUE), 
                                          distance_to_financial_center),
    is_chico = ifelse(is.na(is_chico), 0, is_chico),
    is_rosales = ifelse(is.na(is_rosales), 0, is_rosales),
    is_el_nogal = ifelse(is.na(is_el_nogal), 0, is_el_nogal)
  )

# Realizar predicciones con el modelo actualizado en los datos de prueba
test_data_imputed$price <- predict(rf_model_updated, newdata = test_data_imputed)

# Crear un dataframe con las columnas requeridas
final_output_updated <- data.frame(
  property_id = test_data_imputed$property_id,  # Identificador único
  price = test_data_imputed$price              # Precio predicho
)

# Guardar las predicciones en un archivo CSV en el formato requerido
write.table(final_output_updated, "Final_RandomForest_Predictions_Updated.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)

cat("Predicciones finales con nuevas interacciones guardadas en 'Final_RandomForest_Predictions_Updated.csv'")


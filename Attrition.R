# Librerías
set.seed(1222)

library(tidyverse)
library(class)


# Cargamos los datos
datos <- read.csv("watson_healthcare_modified.csv", encoding = "UTF-8")

anyNA(datos) # Buscamos NA's

#### Analisis descriptivo ####

# Estructura de los datos
glimpse(datos)

# cambiamos a tipo factor las variables categoricas
datos2 <- datos %>% mutate(across(names(datos)[sapply(datos, class) == "character"],as.factor),
                           Education = as.factor(Education), Shift = as.factor(Shift))

#verificamos
summary(datos2)

numericos <- names(datos)[sapply(datos, class) == "integer"]

ggplot(datos2, aes(x=numericos[2], fill=Attrition)) + geom_histogram()


#### Modelo knn ####

muestra <- sample(1:nrow(datos2), size = nrow(datos2)*.8)

train <- datos[muestra,c(2,numericos)]
  
test <- datos[-muestra,c(2,numericos)]

k <- sqrt(nrow(datos2))

k1 <- floor(k)
k2 <- ceiling(k)

knn1 <- knn(train, test, cl=datos[muestra,3], k=k1)
knn2 <- knn(train, test, cl=datos[muestra,3], k=k2)


#### Modelo logistic ####

## pliss react

## Prueba 22


# CASO PRACTICO MODULO 6 R

# 1.- Cargar Tidyverse

library(tidyverse)

# 2.- Cargar el archivo CSV

df <- read.csv("Titanicv2.csv")

# 3.- Revisar si hay NA en el DF

colSums(is.na(df))

# 4.- Calcular la edad promedio por grupo de sobrevivientes

df %>% 
  group_by(Survived) %>% 
  summarise(edad.promedio = mean(Age, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(edad.promedio))


df %>% 
  ggplot(aes(x=Age, fill = Survived)) +
  geom_histogram(alpha=0.5)


#No se presenta distinción entre sobreviventes y no sobrevivientes por rango de edad.


df %>% 
  ggplot(aes(x=Survived, y=Age))+
  geom_boxplot()

# La base de datos presenta información relativa a que el total de sobrevivientes
# son del genero femenino y el total de hombres murió, si traemos a colación la historia
# del filme, se dio preferencia a mujeres y niños para abordar las lanchas salvavidas
# sin importar la clase social en ese momento.

df %>% 
  ggplot(aes(x=Sex, fill = Survived))+
  geom_bar()

# En el tiempo del naufragio del Titanic, el barco requeria mucho personal obrero para
# mantener encendidas las calderas del mismo y dar servicio a los demas huespedes, por tal motivo 
# es la clase baja la que presenta mayor numero de pasajeros a bordo  y de no sobrevivientes en comparación con las otras clases.

df %>% 
  ggplot(aes(x=Pclass, fill = Survived))+
  geom_bar()


df %>% 
  group_by(Pclass) %>%
  summarise(
    total=n()
  ) %>% 
  ungroup()

df %>% 
  group_by(Pclass) %>%
  summarise(
    total=n(),
    no.survived = sum(Survived == "No")
  ) %>% 
  ungroup()


df %>% 
  group_by(Pclass) %>%
  summarise(
    total=n(),
    no.survived = sum(Survived == "No"),
    percent = 100*no.survived/total
  ) %>% 
  ungroup()

# El costo del ticket está relacionado con la tasa de supervivencia en este naufragio, 
# los tickets mas altos sobrevivieron al naufragio

df %>% 
  ggplot(aes(x=Fare,
             y=Age,
             color=Survived,
             size=Fare))+
  geom_point(alpha=0.5)

# Haciendo distincion por sexo y clase se muestra que el total del genero masculino
# tanto en las clases baja, media y alta no sobrevieron.

df %>% 
  group_by(Pclass, Sex) %>%
  summarise(
    total=n(),
    no.survived = sum(Survived == "No"),
    percent = 100*no.survived/total
  ) %>% 
  ungroup()

df %>% 
  ggplot(aes(x=Sex, fill = Survived))+
  geom_bar()

df %>% 
  ggplot(aes(x=Pclass, y=Survived, color=Sex))+
  geom_point(position="jitter")

# Si revisamos el puerto de embarque, es Southampton donde abordaron mas pasajeros en total,
# pero se distingue lo siguiente: Cherbourg es el puerto que registra el mayor número
# de pasajeros de clase alta y Queenstown solo tuvo 1 pasajero en la clase alta del genero masculino.

df %>% 
  ggplot(aes(x=Embarked)) +
  geom_bar()

df %>% 
  group_by(Embarked,Pclass, Sex) %>%
  summarise(
    total=n(),
    no.survived = sum(Survived == "No"),
    percent = 100*no.survived/total
  ) %>% 
  ungroup()


df %>% 
  group_by(Embarked) %>%
  summarise(
    total=n()
  ) %>% 
  ungroup()

df %>% 
  group_by(Sex) %>%
  summarise(
    total=n()
  ) %>% 
  ungroup()

df %>% 
  group_by(Pclass, Embarked) %>%
  summarise(
    total=n()
  ) %>% 
  ungroup()

df %>% 
  group_by(Embarked,Sex) %>%
  summarise(
    total=n()
  ) %>% 
  ungroup()

df %>% 
  group_by(Pclass) %>%
  summarise(
    total=n()
  ) %>% 
  ungroup()

df %>% 
  ggplot(aes(x=Pclass, y=Embarked, color=Sex))+
  geom_point(position="jitter")

library(rvest)
library(dplyr)
library(purrr)
library(stringr)
#cargo base
base_completa_final1 <- read.csv("base_completa_final1", stringsAsFactors = FALSE)


###########ANALISIS############
#GRAFICO n1
an_ed <- base_completa_final1%>%
  group_by(editorial) %>%
  summarise(precio_medio=mean(precio_pag)) %>%
  arrange(desc(precio_medio)) %>%
  ungroup()

library(ggplot2)

ggplot(head(an_ed,10), aes(x = reorder(editorial, precio_medio), y = precio_medio)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(
    title = "Top 10 editoriales con mayor precio por página",
    x = "Editorial",
    y = "Precio por página (promedio)"
  ) +
  theme_minimal()


#GRAFICO 2
an_aut <- base_completa_final1 %>%
  group_by(autor) %>%
  summarise(libros = n())%>%
  arrange(desc(libros)) %>%
  filter(!is.na(autor))%>%
  ungroup()

ggplot(head(an_aut,10), aes(x = reorder(autor, libros), y = libros)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(
    title = "Top 10 Autores con mas libros publicados",
    x = "Autor",
    y = "Libros"
  ) +
  theme_minimal()

install.packages("lubridate")
library(lubridate)

#GRAFICO 3
library(tidyr)
base_completa_final1<- base_completa_final1 %>%
  mutate(
    anio = str_extract(publicacion, "\\d{4}") %>% as.numeric()
  )

an_an <- base_completa_final1 %>%
  group_by(anio) %>%
  summarise (precio_anio= mean(precio_pag, na.rm=TRUE))%>%
  filter(!is.na(precio_anio))%>%
  ungroup()

an_an<- an_an %>%
  complete(anio = 2015:2025, fill=  list(precio_anio=NA))

ggplot(an_an %>% arrange(anio),aes(x = anio, y = precio_anio)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "red", size = 2) +
  scale_x_continuous(breaks = 2015:2025,limits = c(2015, 2025))+
  labs(
    title = "Evolución del precio promedio de libros por año",
    x = "Año de publicación",
    y = "Precio promedio"
  ) +
  theme_minimal()

#GRAFICO 4
an_aut2 <- base_completa_final1 %>%
  group_by(autor) %>%
  summarise(precio_prom=mean(precio_pag))%>%
  arrange(desc(precio_prom)) %>%
  filter(!is.na(autor))%>%
  ungroup()

ggplot(head(an_aut2,10), aes(x = reorder(autor, precio_prom), y = precio_prom)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(
    title = "Top 10 Autores con libros mas caros",
    x = "Autor",
    y = "Precio"
  ) +
  theme_minimal()

# GRAFICO 5: COMPARACION DE PRECIOS: CUSPIDE Y YENNY
# Función para limpiar precios
limpiar_precio <- function(precio) {
  precio %>%
    str_replace_all("[^0-9,]", "") %>%     # elimina todo menos numero y coma
    str_replace(",", ".") %>%           # cambia coma por punto
    as.numeric()
}

# Limpiar base Yenny
yenny <- base_completa_final1 %>%
  mutate(
    titulo = str_to_lower(titulo),
    precio_yenny = limpiar_precio(precio)
  ) %>%
  select(titulo, precio_yenny)

# Limpiar base Cúspide
cuspide <- base_libros %>%
  mutate(
    titulo = str_to_lower(titulo),
    precio_cuspide = limpiar_precio(precio)
  ) %>%
  select(titulo, precio_cuspide)

# Unir por título 
comparacion <- inner_join(yenny, cuspide, by = "titulo") %>%
  distinct() %>%  # elimina duplicados exactos
  filter(!is.na(precio_yenny), !is.na(precio_cuspide))

# Agregar columna de diferencia de precio
comparacion <- comparacion %>%
  mutate(
    diferencia_absoluta = precio_yenny - precio_cuspide,
    diferencia_relativa = (precio_yenny - precio_cuspide) / precio_cuspide
  )
library(ggplot2)

ggplot(comparacion, aes(x = precio_cuspide, y = precio_yenny)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Comparación de precios: Cúspide vs. Yenny",
    x = "Precio en Cúspide",
    y = "Precio en Yenny"
  ) +
  theme_minimal()

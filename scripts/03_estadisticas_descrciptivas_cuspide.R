#ANALISIS DE DATOS
an_ed <- base_final%>%
  group_by(editorial) %>%
  summarise(precio_medio=mean(precio_pag)) %>%
  arrange(desc(precio_medio)) %>%
  ungroup()
install.packages("ggplot2")
library(ggplot2)

ggplot(head(an_ed,10), aes(x = reorder(editorial, precio_medio), y = precio_medio)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 editoriales con mayor precio por página",
    x = "Editorial",
    y = "Precio por página (promedio)"
  ) +
  theme_minimal()

an_aut <- base_final %>%
  group_by(autor) %>%
  summarise(libros = n())%>%
  arrange(desc(libros)) %>%
  filter(!is.na(autor))%>%
  ungroup()

ggplot(head(an_aut,10), aes(x = reorder(autor, libros), y = libros)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Autores con mas libros publicados",
    x = "Autor",
    y = "Libros"
  ) +
  theme_minimal()
install.packages("lubridate")
library(lubridate)


an_an <- base_final %>%
  group_by(anio) %>%
  mutate(anio = year(mdy(anio)))%>%
  summarise (precio_anio= mean(precio_pag))%>%
  filter(!is.na(precio_anio))%>%
  arrange(desc(anio))%>%
  ungroup()

ggplot(head(an_an,10),aes(x = as.integer(anio), y = precio_anio)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Evolución del precio promedio de libros por año",
    x = "Año de publicación",
    y = "Precio promedio"
  ) +
  theme_minimal()

an_aut2 <- base_final %>%
  group_by(autor) %>%
  summarise(precio_prom=mean(precio_pag))%>%
  arrange(desc(precio_prom)) %>%
  filter(!is.na(autor))%>%
  ungroup()

ggplot(head(an_aut2,10), aes(x = reorder(autor, precio_prom), y = precio_prom)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Autores con libros mas caros",
    x = "Autor",
    y = "Precio"
  ) +
  theme_minimal()
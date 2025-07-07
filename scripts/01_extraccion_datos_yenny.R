library(rvest)
library(dplyr)
library(purrr)
library(stringr)
###TEMATIKA
# Función para scrapear una sola página
scrapear_pagina <- function(url) {
  pagina <- read_html(url)
  
  productos <- pagina %>%
    html_nodes(".item-product")
  
  map_df(productos, function(producto) {
    titulo <- producto %>%
      html_node(".item-name") %>%
      html_text(trim = TRUE)
    
    enlace <- producto %>%
      html_node(".item-description a") %>%
      html_attr("href")
    
    autor <- producto %>%
      html_node(".text-accent") %>%
      html_text(trim = TRUE)
    
    precio <- producto %>%
      html_node(".item-price") %>%
      html_text(trim = TRUE)
    
    data.frame(
      titulo = titulo,
      autor = ifelse(is.na(autor), NA, autor),
      precio = precio,
      enlace = enlace,
      stringsAsFactors = FALSE
    )
  })
}

# Crear URLs de todas las páginas (ajustar el número total si cambia)
paginas <- paste0("https://www.yenny-elateneo.com/libros/negocios-y-cs-economicas/?page=", 1:100, "/")

base_libros_1 <- map_dfr(paginas, ~{
  cat("Scrapeando página:", .x, "\n")
  scrapear_pagina(.x)
})


base_libros_1 <- base_libros_1 %>%
  distinct()
# Ver los primeros resultados
head(base_libros_1)

#Guardo 
write.csv(base_libros_1, "base_libros_1.csv", row.names = FALSE)
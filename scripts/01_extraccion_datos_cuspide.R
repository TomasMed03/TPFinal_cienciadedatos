library(rvest)
library(dplyr)
library(purrr)
library(stringr)

# Función para scrapear una sola página
scrapear_pagina <- function(url) {
  pagina <- read_html(url)
  
  productos <- pagina %>%
    html_nodes(".product-small")
  
  map_df(productos, function(producto) {
    titulo <- producto %>%
      html_node(".product-title a") %>%
      html_text(trim = TRUE)
    
    enlace <- producto %>%
      html_node(".product-title a") %>%
      html_attr("href")
    
    autor <- producto %>%
      html_node(".author-product-loop a") %>%
      html_text(trim = TRUE)
    
    precio <- producto %>%
      html_node(".price") %>%
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
paginas <- paste0("https://cuspide.com/categoria-producto/econom%C3%ADa-finanzas-empresa-y-gesti%C3%B3n/page/", 1:85, "/")

base_libros <- map_dfr(paginas, ~{
  cat("Scrapeando página:", .x, "\n")
  scrapear_pagina(.x)
})


base_libros <- base_libros %>%
  distinct()
# Ver los primeros resultados
head(base_libros)

extraer_info_libro <- function(enlace) {
  cat("Extrayendo info de:", enlace, "\n")
  
  pagina <- tryCatch(read_html(enlace), error = function(e) return(NULL))
  if (is.null(pagina)) return(data.frame(editorial = NA, encuadernacion = NA))
  
  tabla_atributos <- pagina %>%
    html_nodes(".woocommerce-product-attributes-item")
  
  
  buscar_valor <- function(nombre) {
    nodo <- tabla_atributos %>%
      keep(~ html_text(html_node(.x, ".woocommerce-product-attributes-item__label")) %>%
             str_to_lower() %>%
             str_detect(str_to_lower(nombre))) %>%
      map_chr(~ html_text(html_node(.x, ".woocommerce-product-attributes-item__value")), .default = NA)
    nodo[1]
  }
  
  editorial <- buscar_valor("Editorial")
  paginas <- buscar_valor("Número de páginas")
  anio <- buscar_valor("Fecha de primera publicación")
  data.frame(
    editorial = editorial,
    paginas = paginas,
    anio= anio,
    stringsAsFactors = FALSE
  )
}


# Aplicamos la función a cada enlace
info_adicional <- map_dfr(base_libros$enlace, extraer_info_libro)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)

#cargo base
base_completa_final1 <- read.csv("base_completa_final1", stringsAsFactors = FALSE)


####Información adicional
extraer_info_libro <- function(enlace) {
  cat("Extrayendo info de:", enlace, "\n")
  
  pagina <- tryCatch(read_html(enlace), error = function(e) return(NULL))
  if (is.null(pagina)) return(data.frame(editorial = NA, encuadernacion = NA))
  
  # Adaptado a <div>
  detalles <- pagina %>% html_nodes(".std div") %>% html_text(trim = TRUE)
  
  editorial <- detalles[grepl("Editorial:", detalles)]
  encuadernacion <- detalles[grepl("Encuadernación:", detalles)]
  paginas <- detalles[grepl("Páginas:", detalles)]
  publicacion <- detalles[grepl("Fecha Publicación:", detalles)]
  
  editorial <- if (length(editorial) > 0) str_remove(editorial, "Editorial:\\s*") else NA
  encuadernacion <- if (length(encuadernacion) > 0) str_remove(encuadernacion, "Encuadernación:\\s*") else NA
  paginas <- if (length(paginas) > 0) str_remove(paginas, "Páginas:\\s*") else NA
  publicacion <- if (length(publicacion) > 0) str_remove(publicacion, "Fecha Publicación:\\s*") else NA
  
  
  data.frame(editorial = editorial, encuadernacion = encuadernacion, paginas = paginas, publicacion = publicacion, stringsAsFactors = FALSE)
}

# Aplicamos la función a cada enlace
info_adicional1 <- map_dfr(base_libros_1$enlace, safely(extraer_info_libro))

# Unimos las columnas nuevas con la base original
base_completa_1 <- bind_cols(base_libros_1, info_adicional1$result)

#guardo csv
write.csv(base_completa_final1, "base_completa_final1.csv", row.names = FALSE)
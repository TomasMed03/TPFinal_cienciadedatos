#LIMPIAR LA BASE

base_libros <- base_libros %>%
  mutate(id_libro = row_number())
info_adicional <- info_adicional %>%
  mutate(id_libro = row_number())
base_final <- base_libros %>%
  left_join(info_adicional, by = "id_libro") %>%
  mutate(
    precio = str_remove_all(precio, "[^\\d,]"),
    precio = str_replace(precio, ",", "."),
    precio_pag= as.numeric(precio)/as.numeric(paginas)
  )

write.csv(base_final, "base_final.csv")
write.csv(base_libros, "base_libros.csv")
write.csv(info_adicional, "info_adicional.csv")
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
#cargo base
base_completa_1 <- read.csv("base_completa_1", stringsAsFactors = FALSE)

#limpio base
base_completa_1 <- base_completa_1 %>%
  mutate(
    paginas = paginas %>%
      str_extract("\\d+") %>%           # extrae solo números
      as.numeric(),
    precio = precio %>%
      str_replace_all("[^0-9,]", "") %>%
      str_replace(",", ".") %>%
      as.numeric()
  )

base_completa_final1<- base_completa_1 %>%
  mutate(
    precio_pag= precio / paginas
  )

#Guardo en csv
write.csv(base_completa_final1, "base_completa_final1.csv", row.names = FALSE)
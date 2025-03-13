###############################################################################
### Web Scraping - Supermercado más barato (PSO) ##############################
###############################################################################

# cargar librerias
library(rvest) # para leer html
library(data.table) # crea dataframes con menos memoria
library(dplyr) # Usa pipes (%>%) para programar en capas
library(robotstxt) # para saber si es posible o no el web scraping

# leer url
url <- "https://supermercadomasbarato.com/categoria-producto/abarrotes/harinas-y-mezclas-listas/"

# ver si es permitido hacer web_scraping (Nos arrojará TRUE)
paths_allowed(path = url)

# leer html
webpage <- read_html(url)

# extraer SKU
SKU <- webpage %>%
  html_nodes(".woocommerce-loop-product__title") %>% # inspeccionamos en el
  # navegador el elemento que queremos extraer
  html_text(trim = T) # quitamos espacios al inicio y al final

# Precio
precio <- webpage %>%
  html_nodes(".price") %>%
  html_text(trim = TRUE) %>%
  gsub("[\\$,]", "", .) %>% # usamos regex para conservar el valor numérico
  as.numeric() # convertimos el texto a numero

# stock
stock <- webpage %>%
  html_nodes(".input-text.qty.text") %>%
  html_attr("max") %>% # del nodo seleccionado, extraemos el atributo
  as.numeric()

# disponibilidad
disponibilidad <- webpage %>%
  html_nodes(".button.product_type_simple") %>%
  html_text(trim = T)

# Links
links <- webpage %>%
  html_nodes(".woocommerce-LoopProduct-link") %>%
  html_attr("href")

# unir elementos en un data.table
harinas <- data.table(
  SKU = SKU,
  Precio = precio,
  Disponibilidad = disponibilidad,
  Links = links
)

# dado que la variable stock está incompleta, la ajustamos a partir de la variable
# Disponibilidad
# crear la columna stock en nuestro data.table
harinas <- harinas[order(Disponibilidad)]
harinas[, stock := fcase(
  Disponibilidad == "Add to cart", stock[1:.N],  # Ajusta la longitud de stock
  Disponibilidad == "Read more", 0
)]

# ver primeros 6 resultados
head(harinas)

# exportar nuestro data.table en .csv
fwrite(harinas, 
       "/Users/philipramirezp/Desktop/Github/Web_scraping/Rvest/Dummies/output/supermercadomasbarato.csv",  # Ruta y nombre del archivo de salida
       sep = ";",      # Usa punto y coma (;) como separador de columnas
       quote = TRUE,   # Poner en comillas todas las columnas de texto
       dec = ".",      # Usar punto (.) como separador decimal
       row.names = FALSE,  # No incluir números de fila
       bom = TRUE,     # Agregar BOM para asegurar correcta lectura en Excel
       encoding = "UTF-8"  # Guardar con codificación UTF-8 para soportar caracteres especiales
)
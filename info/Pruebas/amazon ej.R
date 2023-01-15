#import to load the library to use and to clean/or visualize the data
library("rvest") 
library("tidyverse") 
library("dplyr")

#### ------------------Web Scraping---------######
# reading the web page and specifying the url
url <- "https://www.amazon.com.mx/b/ref=s9_acsd_hfnv_hd_bw_bAcAgpX_ct_x_ct00_w?_encoding=UTF8&node=9725407011&pf_rd_m=AVDBXBAVVSXLQ&pf_rd_s=merchandised-search-4&pf_rd_r=Q47SQG6GBGGECT1SXNZY&pf_rd_t=101&pf_rd_p=8cc61471-874f-5f7f-96d7-e2b634466523&pf_rd_i=9725377011"
pagina_web <- read_html(url)

css_producto <- "span.a-size-base-plus.a-color-base.a-text-normal"
  
producto_html <- html_nodes(pagina_web,css_producto)
producto_texto <- html_text(producto_html)
#mostramos los datos, al final presionar enter
producto_texto

#clase css del producto
css_precio <- "span.a-price-whole"
#obtenemos el contenido de la clase en código html
precio_html <- html_nodes(pagina_web,css_precio)
#limpiamos el código para obtener el texto
precio_texto <- html_text(precio_html)
precio_texto

#Eliminamos el signo de peso
#precio_limpio <- gsub("\$","",precio_texto)
#Eliminamos la coma
precio_limpio <- gsub(",","",precio_texto)
#Transformamos a numérico 
precio_numerico <- as.numeric(precio_limpio)
precio_numerico

#Unimos los datos
productos <- data.frame(Producto = producto_texto, Precio = precio_numerico)
#Para mostrar la gráfica por precio
barplot(precio_numerico)


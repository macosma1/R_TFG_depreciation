#limpiar la consola
rm(list=ls())

#invocar librerias
#import to load the library to use and to clean/or visualize the data
library(rvest) 
library(tidyverse) 
library(dplyr)
library(robotstxt) 
library(selectr)
library(xml2) 
library(stringr)
library(forcats) 
library(magrittr)
library(tidyr) 
library(ggplot2)
library(lubridate) 
library(tibble)
library(purrr) 


# reading the web page and specifying the url
url <- "https://www.amazon.es/s?k=cafeteras+de+goteo&__mk_es_ES=%C3%85M%C3%85%C5%BD%C3%95%C3%91&crid=Z5WVEPQREBPD&sprefix=cafeteras+de+goteo%2Caps%2C110&ref=nb_sb_noss_2"

#ask if is allowed to download the information --> binary answer if the answer is flase you are fuck
paths_allowed(paths = c(url))

#obtain the code HTML of the web site
pagina_web <- read_html(url)

#assign the class
#css_producto <- "a.a-link-normal.s-underline-text.s-underline-link-text.s-link-style.a-text-normal"
css_producto <- "a.a-link-normal.s-underline-text.s-underline-link-text.s-link-style.a-text-normal"

#Obtainthe the HTML code of the web site that contain the name of the product
producto_html <- html_nodes(pagina_web,css_producto)
producto_texto <- html_text(producto_html)

#mostramos los datos, al final presionar enter
producto_texto
legth(producto_texto)
tail(producto_texto)

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

#consultar tipo de dato
data.class(precio_limpio) ##character

#Transformamos a numérico 
precio_numerico <- as.numeric(precio_limpio)
precio_numerico

#Unimos los datos
productos <- data.frame(Producto = producto_texto, Precio = precio_numerico)
#Para mostrar la gráfica por precio
barplot(precio_numerico, main = "Precios de cafeteras - AMAZON")



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
link <- "https://www.europa-camiones.com/cabezas-tractoras-usadas/1-31/anuncios-cabezas-tractoras.html?"
#hacemos una variacion en el link, para que sea lo del intercambio --> paso de una pag a otra

#obtain the code HTML of the web site
page <- read_html(link)

#assign the class, paste concatena
name = page %>% html_nodes("#vehicles .margin-top-0") %>% html_text()
movie_links = page %>% html_nodes("#vehicles .margin-top-0") %>% 
  html_attr("href") %>% paste("https://www.europa-camiones.com", ., sep="")
movie_link
year = page %>% html_nodes(".margin-bottom-5:nth-child(1) strong") %>% html_text()
price = page %>% html_nodes(".font-18") %>% html_text()
km = page %>% html_nodes(".margin-bottom-5+ .margin-bottom-5 strong") %>% html_text()


## lee los link que generamos anteriormente y saca los datos
movie_links = "https://www.europa-camiones.com/cabeza-tractora-mercedes-estandar/actros-1845/4x2-euro-6-valencia/ts-vi7792141/usada.html"

get_marca = function(movie_links){
  movie_links = "https://www.europa-camiones.com/cabeza-tractora-mercedes-estandar/actros-1845/4x2-euro-6-valencia/ts-vi7792141/usada.html"
  pages = read_html(movie_links)
  marca = pages %>% html_nodes("tr:nth-child(9) a") %>% html_text()
  #marca
  return(marca)
  #%>% paste(collapse = ",") # junta diferentes elemento de varias string en solo uno
  #
}
marca

#con los links, ira a cada uno, ejecutara la funcion de marca y el resultado lo pondra en un vector, ponemos el USERNAMES = FALSE para que en el vector no salga el link
marcas = sapply(movie_links, FUN = get_marca, USE.NAMES = FALSE) # para que no salga el url
marcas   
movies = data.frame(name, year, price, km, marca, stringAsFactors=FALSE)



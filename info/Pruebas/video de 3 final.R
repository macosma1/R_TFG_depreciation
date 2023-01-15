#limpiar la consola
rm(list=ls())

#invocar librerias
#import to load the library to use and to clean/or visualize the data
{
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
}
#----------------------------functions---------------------------#

get_pais = function(movie_link){
  #link = "https://www.europa-camiones.com/cabeza-tractora-mercedes-estandar/actros-1845/4x2-euro-6-valencia/ts-vi7792141/usada.html"
  movie_page = read_html(movie_link)
  pais = movie_page %>% html_nodes("tr:nth-child(9) a") %>%
    html_text() 
  return(pais)
  #%>% paste(collapse = ",") # junta diferentes elemento de varias string en solo uno
  #
}


datos = data.frame()

#for loop
for(page_result in seq(from =1, to = 23, by = 21)){
  link = paste0("https://www.europa-camiones.com/cabezas-tractoras-usadas/1-31/anuncios-cabezas-tractoras.html?p=2", 
               page_result) #page by default has space between strings that you are trying to concatenate
  page = read_html(link)
  
  #assign the class, paste concatena
  name = page %>% html_nodes("#vehicles .margin-top-0") %>% html_text()
  movie_link = page %>% html_nodes("#vehicles .margin-top-0") %>% 
    html_attr("href") %>% paste("https://www.europa-camiones.com", ., sep="") #pone lo del link que obtiene despues de www-europa...
  year = page %>% html_nodes(".margin-bottom-5:nth-child(1) strong") %>% html_text()
  #price = link %>% html_nodes(".font-18") %>% html_text()
  #km = link %>% html_nodes(".margin-bottom-5+ .margin-bottom-5 strong") %>% html_text()
  movie_link
  
  #con los links, ira a cada uno, ejecutara la funcion de marca y el resultado lo pondra en un vector, ponemos el USERNAMES = FALSE para que en el vector no salga el link
  paises = sapply(movie_link, FUN = get_pais, USE.NAMES = FALSE) # para que no salga el url

  datos = rbind(datos, data.frame(name, year, paises, stringAsFactors=FALSE)) #rbind para no crearlo siempre, solo ir adjuntando 

  print(paste("Page: ", page_result)) #tracking problems
  
  }





pais



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
url <- "https://www.imdb.com/search/title?groups=top_250&sort=user_rating"

#ask if is allowed to download the information --> binary answer if the answer is flase you are fuck
paths_allowed(paths = c(url))

#obtain the code HTML of the web site
imdb <- read_html(url)
imdb

#assign the class
imdb %>%
  html_nodes(".lister-item-content h3 a") %>%
  html_text() -> movie_title
movie_title

#seria lo mismo que poner:
#movie_title = omdb %>% html_nodes(".lister-item-content h3 a") %>% html_text()

##ahora quiero el año, como viene en texto quiero reformatear as.Date --> lo pone como "(1999)"  y lo quiero 1999
imdb %>%
  html_nodes(".lister-item-content h3 .lister-item-year") %>%   ## nota, no pone el text -muted unbold select...
  html_text() %>%
  str_sub(start = 2, end = 5) %>%
  as.Date(format = "%Y") %>%
  year() -> movie_year
movie_year


##ahora quiero la duracion, reformateo a numero
imdb %>%
  html_nodes(".lister-item-content p .runtime") %>%  #no se pone lo de selectorgadget 
  html_text() #%>%
  str_split(" ") %>% # para que no traiga los minutos
  map_chr(1) %>%
  as.numeric() -> movie_runtime
movie_runtime


##ahora quiero genero
imdb %>%
  html_nodes(".lister-item-content p .genre") %>%  #no se pone lo de selectorgadget 
  html_text() %>%
  str_trim() -> movie_genre
movie_genre

##ahora quiero rating
imdb %>%
  html_nodes(".ratings-bar .ratings-imdb-rating") %>%  
  html_attr("data-value") %>%
  as.numeric() -> movie_rating
movie_rating


##ahora quiero cuanta gente voto
imdb %>%
  html_nodes(xpath = '//meta[@itemprop="ratingCount"]') %>%  
  html_attr("content") %>%
  as.numeric() -> movie_votes
movie_votes


#para el revenue
imdb %>%
  html_nodes(xpath = '//span[@name="nv"]') %>% 
  html_text() %>%
  str_extract(pattern = "^\\$.*") %>%
  na.omit() %>%
  as.character() %>%
  append(values = NA, after = 30) %>%
  append(values = NA, after = 46) %>%
  str_sub(start = 2, end = nchar(.) -1) %>%
  as.numeric() -> movie_revenue
movie_revenue


#junto todo
top_50 <- tibble(title = movie_title, 
                 release = movie_year,
                 runtime = movie_runtime,
                 genre = movie_genre, 
                 #rating = movie_rating,
                 votes = movie_votes)

#revenue ($millions) = movie_revenue)
top_50
getwd()
#exporta a excel

movies = dta.frame(movie_title, movie_year, movie_rating, stringAsFactor = FALSE)
write.cvs2 (top_50, "top50.csv")

gc



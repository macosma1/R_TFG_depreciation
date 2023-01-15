# view source : https://es.wikipedia.org/wiki/Pandemia_de_COVID-19_en_M%C3%A9xico

#import to load the library to use and to clean/or visualize the data
library(rvest) 
library(tidyverse) 
library(dplyr)

#### ------------------Web Scraping---------######

# reading the web page and specifying the url
url <- 'https://www.billboard.com/charts/hot-100/'
hot100page <- read_html(url)
hot100page
str(hot100page)


body_nodes <- hot100page %>%         
  html_nodes("body") %>%
  html_children()
body_nodes %>%
  html_children()

rank <- hot100page %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//span[contains(@class, 'chart-element__rank__number')]") %>%
  rvest::html_text() 

artist <- hot100page %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//span[contains(@class, 'chart-element__information__artist')]") %>%
  rvest::html_text() 
                                                
title <- hot100page %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//span[contains(@class, 'chart-element__information__song')]") %>%
  rvest::html_text()                                                 
                                                
chart_df <- data.frame(rank, artist, title)
knitr::kable(chart_df %>% head(10))
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
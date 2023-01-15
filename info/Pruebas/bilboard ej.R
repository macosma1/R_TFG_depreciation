# view source : https://es.wikipedia.org/wiki/Pandemia_de_COVID-19_en_M%C3%A9xico

#import to load the library to use and to clean/or visualize the data
library(rvest) 
library(tidyverse) 
library(dplyr)

#### ------------------Web Scraping---------######

get_chart <- function (date = Sys.Date (), positions = c(1:10), type = "hot-100") {
  
  #get url from input and read html
input <- paste0 ("https: //www.billboard. com/charts/", type, "/",date)
  chart_page <- xml2::read_html(input)
  
  #Scrape data
  rank < chart_page %>%
    rvest::html_nodes('body') %>%
    xml2::ml_find_all("//apan [contains(@class,'chart-elemen__rank__number')]") %>%
    rvest::html_text()

  artist < chart_page %>%
    rvest::html_nodes('body') %>%
    xml2::ml_find_all("//apan [contains(@class,'chart-elemen__information__artist')]") %>%
    rvest::html_text()
  
  title < chart_page %>%
    rvest::html_nodes('body') %>%
    xml2::ml_find_all("//apan [contains(@class,'chart-elemen__information__song')]") %>%
    rvest::html_text()

  
# create dataframe, remove nas and return result
chart_df<-data.frame (rank, artist, title)
chart_df<-chart_df %>%
  dplyr::filter (!is.na(rank), rank %ins% positions)
  
chart_df
}

test <- get_chart(date = "1975-01-20", positions = 1:10, type = "hot-100")
test


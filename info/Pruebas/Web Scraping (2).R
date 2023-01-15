
#------------------------------------------------------------------
#import to load the library to use and to clean the console
{
  rm(list=ls()) # clean
  library(rvest) 
  library(magrittr)
  library(tidyverse) 
  library(dplyr)
  library(ggplot2)
  library(tidyr) 
  library(purrr) 
  library(tibble)
  library(stringr)
  library(forcats) 
  
  library(robotstxt) #evaluar la accesibilidad de la pag
  #library(selectr)
  library(xml2)  # analizar documentos XML
  #library(lubridate) #para fechas y tiempos
  library(rmarkdown)
  library(patchwork) #para combinar graficos
}

#----------------------------functions---------------------------#
{
  get_marca = function(gen_link){
    gen_page = read_html(gen_link)
    brand = gen_page %>% html_nodes(".value span") %>% html_text() 
    return(brand)
  }
  
  
  get_pot = function(gen_link){
    gen_page = read_html(gen_link)
    power = gen_page %>% html_nodes(".mb-4:nth-child(1) .print-hidden:nth-child(1) .text-right") %>% html_text() %>%paste("Pot", ., sep="/") #lo pongo para que si no hay nada y no marca NA no sea menor el vector
    power = power[1]
    #%>% str_split("CV") %>% map_chr(1) #no lo borro, pero no lo pongo ahora por probar por si hay errores cuando no ponen la potencia
    return(power)
    #si en potencia no sale CV, es KM #puedo poner una de si no hay CV = NA,
    #
  }
  
  
  get_price = function(gen_link){
    gen_page = read_html(gen_link)
    price = gen_page %>% html_nodes(".color-primary.align-middle") %>% html_text()
    price= as.numeric(gsub('\\D+','', price))
    price = price[1]
    return(price)
    #si el precio no sale numerico, saldra NA
  }

}


#----------------------------Code---------------------------#
datos = data.frame()
dato = data.frame()

#ask if is allowed to download the information --> binary answer if the answer is false you are fuck
link <- "https://www.europa-camiones.com/cabezas-tractoras-usadas/1-31/anuncios-cabezas-tractoras.html?"
paths_allowed(paths = c(link))
page = read_html(link)

gen_link <- "https://www.europa-camiones.com/cabeza-tractora-daf-estandar/xf-530/euro-6-madrid/ts-vi7848039/usada.html"
gen_link <- "https://www.europa-camiones.com/cabeza-tractora-renault-estandar/t-series-480-t4x2-e6/4x2-euro-6-sistema-hidraulico-madrid/ts-vi7758533/usada.html"
gen_link <- "https://www.europa-camiones.com/cabeza-tractora-mercedes-estandar/actros-1851/4x2-euro-6-leiria/ts-vi3513035/usada.html"
#for loop
for(page_result in seq(from =1, to = 1, by = 1)){
  link = paste0("https://www.europa-camiones.com/cabezas-tractoras-usadas/1-31/anuncios-cabezas-tractoras.html?p=", 
                page_result) #page by default has space between strings that you are trying to concatenate
  page = read_html(link)
  
  #assign the class, 
  name = page %>% html_nodes(".card-title") %>% html_text() %>% str_trim() 
  country = page %>% html_nodes(".meta-location") %>% html_text() %>% str_split("-") %>% map_chr(1)
  country <-str_trim(country,side="both")
  
  city = page %>% html_nodes(".meta-location") %>% html_text()  %>% str_split("-") %>% map_chr(2)
  city <-str_trim(city,side="both")
  #date_pattern = "([\\a-zA-Z\\.-]{1,24})" #"^([a-zA-Z]+)" #24 es un numero ramdon, es para que incluya Rio de Loba- Viseu
  #city <- str_match(city, date_pattern)
  #city <- city[,2]
  #city <-str_trim(city,side="both")
  #view(city)
  
  #year - LISTO------------------------------------------------------------------------------------------------------------------------------
  {year = page %>% html_nodes(".meta") %>% html_text() #%>% paste("year", ., "NA", sep="/")
  #year <- str_spl#it_fixed(year, "/", 3)
  #year = year[,2]
  date_pattern = "^([0-9]{4})"
  year<- str_match(year, date_pattern)
  year <- year %>% na.omit() #eliminate each row that has at least one NA (outliers)
  year = year[,2]
  year = as.numeric(gsub('\\D+','', year))}
  #view(year)
  #------------------------------------------------------------------------------------------------------------------------------  
  
  #km - LISTO------------------------------------------------------------------------------------------------------------------------------
  {
  km = gen_page %>% html_nodes("small.hidden-xs , .label-secondary+ .label-secondary strong") %>% html_text() %>% paste(collapse = "/") # %>% str_split("- /")
  km <- str_split_fixed(km, "- /", 21)
  km <- str_split_fixed(km, "/", 2)
  km = km[,1]
  km =as.numeric(gsub('\\D+','', km))
  }
  #view(km)
  #-------------------------------------------------------------------
  
  #price= page %>% html_nodes(".color-primary.align-middle") %>% html_text() 
  #price= as.numeric(gsub('\\D+','', price))
  #view(price)
  
  
  #obtain the links of each vehicle to search more information
  #------------------------------------------------------------------------------------------------------
  gen_link <- page %>% html_nodes(".page-break-inside") %>% html_elements("[href]") %>% html_attr("href") %>% paste("https://www.europa-camiones.com", ., sep="") # put the link you get after www-europa...
  #gen_link
  #------------------------------------------------------------------------------------------------------ 
  
  
  #with the links, it will go to each one, execute the brand function and the result will put it in a vector, we put the USERNAMES = FALSE so that the link does not appear in the vector
  brand = sapply(gen_link, FUN = get_marca, USE.NAMES = FALSE)# to avoid the url -->FALSE
  price = sapply(gen_link, FUN = get_price, USE.NAMES = FALSE)# to avoid the url -->FALSE
  
  #power - LISTO------------------------------------------------------------------------------------------------------------------------------
  {power = sapply(gen_link, FUN = get_pot, USE.NAMES = FALSE)
  date_pattern = "Pot/([0-9]{3})[- .]CV"
  power <- str_match(power , date_pattern)
  power = power[,1]
  power =as.numeric(gsub('\\D+','', power))
  }
  #view(power)
  #------------------------------------------------------------------------------------------------------------------------------
  
  
  datos = rbind(datos, data.frame(name, country, city, power, brand, price, km, year,  stringAsFactors=FALSE)) #rbind para no crearlo siempre, solo ir adjuntando
  
  
  
  
  print(paste("Page: ", page_result)) #tracking problems
  tStart<-Sys.time()
  Sys.sleep(5)
  tFin<- Sys.time() - tStart

}
  view(datos)  
  sapply(datos,class)
  
  #exporting data to a CVS file
  
  #write.cvs2(datos, file= "web_scraping_datos.cvs")
#------------------------------------------------------------------------------------- 



#--------------------------------------------------------------------------------------------------------#
#if I want to obtain the data inside every link
{
#  get_year = function(page){
#    year = page %>% html_nodes(".print-sm-12:nth-child(1) tr:nth-child(7) .value") %>% html_text() %>% paste("year", .,"NA", sep="/")
#    year= year[1]
#    return(year)
#  }

#  get_price = function(gen_link){
#    #gen_link <- "https://www.europa-camiones.com/cabeza-tractora-iveco-estandar/stralis-as-440-s51-tp/4x2-euro-6-guipuzcoa/ts-vi7469821/usada.html"
#    gen_page = read_html(gen_link)
#    price = gen_page %>% html_nodes(".ht") %>% html_text() 
#    price= as.numeric(gsub('\\D+','', price))
#    price
#    return(price)
#  }

#  get_km = function(gen_link){
#    gen_page = read_html(gen_link)
#    km = gen_page %>% html_nodes(".table-features:nth-child(1) .print-hidden:nth-child(2) .value") %>% html_text() %>% paste("km", .,"NA", sep="/")
#     #km = gen_page %>% html_nodes(".print-hidden:nth-child(3) .value , .print-hidden:nth-child(2) .value") %>% html_text() %>% paste(collapse = "/")
#    km= km[1]
#    km= as.numeric(gsub('\\D+','', km))
#    km
#    return(km)
#este presenta el problema de si salen mas cuadros se jode
#  }


  #year = sapply(page, FUN = get_year, USE.NAMES = FALSE)
  #price = sapply(gen_link, FUN = get_price, USE.NAMES = FALSE)
  #km = sapply(gen_link, FUN = get_km, USE.NAMES = FALSE)

}




#--------------------------------------NOTES-----------------------------------------------------#
#html_text we retrive all the information
#html_nodes go to the source code of the web page and look for the node that we want, and remove it from us
#str_trim() put the text in the correct form, i.e. remove //n...
#paste concatenate
#we use str_split to cut and use the first part with map_chr(1) instead of using gsub

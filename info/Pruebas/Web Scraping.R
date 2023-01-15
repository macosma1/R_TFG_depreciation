
#------------------------------------------------------------------
#import to load the library to use and to clean the console
{
  rm(list=ls()) # clean
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
  library(rmarkdown)
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
    power = gen_page %>% html_nodes(".print-sm-12:nth-child(2) .table-features:nth-child(1) .print-hidden:nth-child(1) .value") %>% html_text() %>%paste("Pot", ., sep="/") #lo pongo para que si no hay nada y no marca NA no sea menor el vector
    power = power[1]
    #%>% str_split("CV") %>% map_chr(1) #no lo borro, pero no lo pongo ahora por probar por si hay errores cuando no ponen la potencia
    return(power)
    #si en potencia no sale CV, es KM #puedo poner una de si no hay CV = NA,
    #
  }
  
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
  
  get_km = function(gen_link){
    gen_page = read_html(gen_link)
    km = gen_page %>% html_nodes(".table-features:nth-child(1) .print-hidden:nth-child(2) .value") %>% html_text() %>% paste("km", .,"NA", sep="/")
    km= km[1]
    km= as.numeric(gsub('\\D+','', km))
    km
    return(km)
    #este presenta el problema de si salen mas cuadros se jode
  }
}


#----------------------------Code---------------------------#
datos = data.frame()

#ask if is allowed to download the information --> binary answer if the answer is false you are fuck
link <- "https://www.europa-camiones.com/cabezas-tractoras-usadas/1-31/anuncios-cabezas-tractoras.html?"
paths_allowed(paths = c(link))
page = read_html(link)

#for loop
for(page_result in seq(from =1, to = 2, by = 1)){
  link = paste0("https://www.europa-camiones.com/cabezas-tractoras-usadas/1-31/anuncios-cabezas-tractoras.html?p=",   #ver que hago para poner=p, 
                page_result) #page by default has space between strings that you are trying to concatenate
  page = read_html(link)
  
  #assign the class, 
  name = page %>% html_nodes("#vehicles .margin-top-0") %>% html_text() %>% str_trim() 
  country = page %>% html_nodes(".width-pct-100 .hidden-xs") %>% html_text() %>% str_split("-") %>% map_chr(1) 
  city = page %>% html_nodes(".hidden-xs+ small") %>% html_text() %>% str_trim()

  year = page %>% html_nodes(".margin-bottom-5:nth-child(1) strong") %>% html_text() %>% paste("year", ., "NA", sep="/")
  #year =as.numeric(gsub('\\D+','', year)) 
  #si el valor tiene Km lo elimino
  
  price= page %>% html_nodes(".font-18:nth-child(1)") %>% html_text() 
  price= as.numeric(gsub('\\D+','', price))
  price
  
  #obtain the links of each vehicle to search more information
  #------------------------------------------------------------------------------------------------------
  gen_link <- page %>% html_nodes(".page-break-inside") %>% html_elements("[href]") %>% html_attr("href") %>% paste("https://www.europa-camiones.com", ., sep="") # put the link you get after www-europa...
  gen_link
  #------------------------------------------------------------------------------------------------------ 


  #with the links, it will go to each one, execute the brand function and the result will put it in a vector, we put the USERNAMES = FALSE so that the link does not appear in the vector
  brand = sapply(gen_link, FUN = get_marca, USE.NAMES = FALSE)# to avoid the url -->FALSE
  power = sapply(gen_link, FUN = get_pot, USE.NAMES = FALSE) 
  
  #year = sapply(page, FUN = get_year, USE.NAMES = FALSE)
  #price = sapply(gen_link, FUN = get_price, USE.NAMES = FALSE)
  km = sapply(gen_link, FUN = get_km, USE.NAMES = FALSE)
  
  
  datos = rbind(datos, data.frame(name, country, city, power, brand, year, price, km, stringAsFactors=FALSE)) #rbind para no crearlo siempre, solo ir adjuntando 
  #datos = data.frame(name, country, city, power, brand, year, price, km, stringAsFactors=FALSE) #rbind para no crearlo siempre, solo ir adjuntando 
  
  #print(paste("Page: ", page_result)) #tracking problems
  tStart<-Sys.time()
  Sys.sleep(0.3)
  tFin<- Sys.time() - tStart
}
#
view(datos) 
view(price)
#------------------------------------------------------------------------------------- 
#esto lo usaria si las funciones si todos los valores estuvieran
#year = page %>% html_nodes(".margin-bottom-5:nth-child(1) strong") %>% html_text()
#year

price= page %>% html_nodes(".font-18:nth-child(1)") %>% html_text()
price= as.numeric(gsub('\\D+','', price))
price

#km = page %>% html_nodes(".margin-bottom-5:nth-child(2) strong") %>% html_text() %>% str_trim()   
#km = as.numeric(gsub('\\D+','', km)) # pongo en precio que vaya de 1-549000 pq si no los -- los pone mal
#km


#get_km = function(gen_link){
#  #gen_link <- "https://www.europa-camiones.com/cabeza-tractora-iveco-estandar/stralis-as-440-s51-tp/4x2-euro-6-guipuzcoa/ts-vi7469821/usada.html"
#  gen_page = read_html(gen_link)
#  #km = gen_page %>% html_nodes(".table-features:nth-child(1) .print-hidden:nth-child(2) .value") %>% html_text() %>% paste("km", .,"NA", sep="/")
#  km = gen_page %>% html_nodes(".print-hidden:nth-child(3) .value , .print-hidden:nth-child(2) .value") %>% html_text() %>% paste(collapse = "/")
#  km= km[1]
#  #km= as.numeric(gsub('\\D+','', km))
#  #km
#  return(km)
#  #si el valor no es mas de 100 pongo NA
#}

#--------------------------------------NOTES-----------------------------------------------------#
 #html_text we retrive all the information
 #html_nodes go to the source code of the web page and look for the node that we want, and remove it from us
 #str_trim() put the text in the correct form, i.e. remove //n...
 #paste concatenate
 #we use str_split to cut and use the first part with map_chr(1) intead of using gsub

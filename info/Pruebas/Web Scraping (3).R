
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
  library(data.table)
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
    brand = gen_page %>% html_nodes("#ts-table-features .text-right span , .under u , .pl-0 .mb-4:nth-child(1) tr:nth-child(2) .text-right , .pl-0 .mb-4:nth-child(1) tr:nth-child(1) .text-right") %>% html_text() 
    date_pattern = "([a-zA-Z]+)"
    brand <- str_match(brand , date_pattern)
    brand = brand[4]
    brand <-str_trim(brand,side="both")
    return(brand)
    }
  #tr:nth-child(8) u , .pl-0 tr:nth-child(9) u
  #.title span
  #.pl-0 tr:nth-child(8) .text-right , tr:nth-child(9) u , tr:nth-child(10) u
  
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


  get_km = function(gen_link){
    gen_page = read_html(gen_link)
    km = gen_page %>% html_nodes(".mb-4:nth-child(1) .print-hidden:nth-child(4) .text-right , .mb-4:nth-child(1) .print-hidden:nth-child(3) .text-right , .mb-4:nth-child(1) .print-hidden:nth-child(2) .text-right , .mb-4:nth-child(1) .print-hidden:nth-child(1) .text-right") %>% html_text() %>% paste(" ", .)
    date_pattern = "(([0-9]{1,3})[- .])+km"
    km <- str_match(km , date_pattern) 
    km <- km %>% na.omit() #eliminate each row that has at least one NA (outliers)
    km = km[1]    
    km =as.numeric(gsub('\\D+','', km))
    return(km)
    #si no tiene km = NA,
  }

  get_year = function(gen_link){
    
    gen_page = read_html(gen_link)
    year = gen_page %>% html_nodes("#ts-table-features .text-right") %>% html_text() %>% paste("", ., collapse="")
    #year <- str_spl#it_fixed(year, "/", 3)
    #year = year[,2]
    date_pattern = "/([0-9]{4})"
    year<- str_match(year, date_pattern)
      if (length(year)>22){
        year <- year %>% na.omit() #eliminate each row that has at least one NA (outliers)
        year = year[,2]
  
      } else { 
        year <- year 
      }
    #year1 = year[[1:21]]
    #year3 <- unlist(year)
    #year2 <- data.frame(t(sapply(year,c)))
    #year4 <- year2 %>% paste("", .)
    #year4 <- as.vector(year2[1,])
    
    #year <- unlist(year, use.names = FALSE)
    #year = as.numeric(gsub('\\D+','', year))
    return(year)
  }
    
}


#----------------------------Code---------------------------#
datos = data.frame()
dato = data.frame()

#ask if is allowed to download the information --> binary answer if the answer is false you are fuck
link <- "https://www.europa-camiones.com/cabezas-tractoras-usadas/1-31/anuncios-cabezas-tractoras.html?"
paths_allowed(paths = c(link))
page = read_html(link)

#gen_link <- "https://www.europa-camiones.com/cabeza-tractora-daf-estandar/xf-530/euro-6-madrid/ts-vi7848039/usada.html"
#gen_link <- "https://www.europa-camiones.com/cabeza-tractora-renault-estandar/t-series-480-t4x2-e6/4x2-euro-6-sistema-hidraulico-madrid/ts-vi7758533/usada.html"
#gen_link <- "https://www.europa-camiones.com/cabeza-tractora-mercedes-estandar/actros-1851/4x2-euro-6-leiria/ts-vi3513035/usada.html" #km de 2
#gen_link <- "https://www.europa-camiones.com/cabeza-tractora-scania-estandar/r-164r480/4x2-euro-2-a-coruna/ts-vi7812721/usada.html" #km de 3
#gen_link <- "https://www.europa-camiones.com/cabeza-tractora-daf-estandar/4x2-seine-maritime/ts-vi1344894/usada.html" #brand en 8
#gen_link <- "https://www.europa-camiones.com/cabeza-tractora-scania-estandar/g-410/4x2-euro-6-tarragona/ts-vi7773685/accidentada.html" #brand en 9
#gen_link <- "https://www.europa-camiones.com/cabeza-tractora-iveco-estandar/stralis-as-440/4x2-euro-6-valencia/ts-vi7774179/usada.html" #esta de 4 la marca
#gen_link <- "https://www.europa-camiones.com/cabeza-tractora-daf-autoescuela/cf/euro-4-madrid/ts-vi7809694/usada.html" #mostoles esta de 3ro el DAF
#gen_link <- "https://www.europa-camiones.com/cabeza-tractora-scania-estandar/overijssel/ts-vi7254679/usada.html" #sin km
#gen_link <- "https://www.europa-camiones.com/cabeza-tractora-volvo/mazowieckie/ts-vi7454778/usada.html"
#gen_page = read_html(gen_link)

#for loop
for(page_result in seq(from =1, to = 1, by = 1)){
  link = paste0("https://www.europa-camiones.com/cabezas-tractoras-usadas/1-31/anuncios-cabezas-tractoras.html?p=", 
                page_result) #page by default has space between strings that you are trying to concatenate
  page = read_html(link)
  
  #assign the class, 
  name = page %>% html_nodes(".card-title") %>% html_text() %>% str_trim() 
  country = page %>% html_nodes(".meta-location") %>% html_text() %>% str_split("-") %>% map_chr(1) %>%str_trim(side="both")
  #country <-str_trim(country,side="both")
  #view(country)
  {
  city = page %>% html_nodes(".meta-location") %>% html_text()  %>% str_split("-") %>% map_chr(2)
  city <-str_trim(city,side="both")
  date_pattern = "([\\a-zA-ZÀ-ÿ\u00f1\u00d1\\.-]{1,24})" #"([\\a-zA-Z\\.-]{1,24})" #24 es un numero ramdon, es para que incluya Rio de Loba- Viseu
  city <- str_match(city, date_pattern)
  city <- city[,2]
  city <-str_trim(city,side="both")
  }
  
  #obtain the links of each vehicle to search more information
  #------------------------------------------------------------------------------------------------------
  gen_link <- page %>% html_nodes(".card-body") %>% html_elements("[href]") %>% html_attr("href") #%>% paste("https://www.europa-camiones.com", ., sep="") # put the link you get after www-europa...
  date_pattern = "(/([([a-zA-Z0-9\\.-]+)])+)+/([a-zA-Z]+).html"
  gen_link <- str_match(gen_link , date_pattern)
  gen_link <- gen_link %>% na.omit()
  gen_link = gen_link[,1]
  gen_link <- gen_link %>% paste("https://www.europa-camiones.com", ., sep="")
  #gen_link
  #------------------------------------------------------------------------------------------------------ 
  
  
  #with the links, it will go to each one, execute the brand function and the result will put it in a vector, we put the USERNAMES = FALSE so that the link does not appear in the vector
  brand = sapply(gen_link, FUN = get_marca, USE.NAMES = FALSE)# to avoid the url -->FALSE
  price = sapply(gen_link, FUN = get_price, USE.NAMES = FALSE)# to avoid the url -->FALSE
  km = sapply(gen_link, FUN = get_km, USE.NAMES = FALSE)# to avoid the url -->FALSE 
  #year <- c(seq(1:21))
  year = sapply(gen_link, FUN = get_year, USE.NAMES = FALSE)# to avoid the url -->FALSE
  year = year[2,]
  
  #power - LISTO------------------------------------------------------------------------------------------------------------------------------
  {
  power = sapply(gen_link, FUN = get_pot, USE.NAMES = FALSE)
  date_pattern = "Pot/([0-9]{3})[- .]CV"
  power <- str_match(power , date_pattern)
  power = power[,1]
  power =as.numeric(gsub('\\D+','', power))
  }
  #view(power)
  #------------------------------------------------------------------------------------------------------------------------------
  
  
  datos = rbind(datos, data.frame(name,  brand, country, city, power, price, km, year,  stringAsFactors=FALSE)) #rbind para no crearlo siempre, solo ir adjuntando
  
  
  
  print(paste("Page: ", page_result)) #tracking problems
  tStart<-Sys.time()
  Sys.sleep(5)
  tFin<- Sys.time() - tStart

}
  view(datos)  
  sapply(datos,class)
  
  #exporting data to a CVS file
  
  write.cvs(datos, file= 'WebScraping.cvs')
  #punto y coma como separador de los datos. Lo último es lo que hace la función write.csv2, mientras que write.csv usa una coma para separar los datos
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

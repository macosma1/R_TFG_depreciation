# view source : https://es.wikipedia.org/wiki/Pandemia_de_COVID-19_en_M%C3%A9xico

#import to load the library to use and to clean/or visualize the data
library(rvest) 
library(tidyverse) 
library(dplyr)

#### ------------------Web Scraping---------######

# reading the web page
covid_mex <- read_html("https://es.wikipedia.org/wiki/Pandemia_de_COVID-19_en_M%C3%A9xico")

#we retrieve all the information
covid_mex %>%         # esto lo que hace es poner como html_text(covid_mex)
  html_text ()

# nodes: find one --> ve en el codigo de fuente de la pag web y busca el nodo que queramos, y nos lo saca
covid_mex %>%
  html_nodes(".mw-headline")

# different tables  ottra forma de hacerlo es html_nodes(covid_mex, ".mw-headline")
covid_mex %>%
  html_nodes(".mw-headline") %>%
  html_text()

#paragraphs --> saca el texto de parrafos
covid_mex %>%
  html_nodes("p") %>%
  html_text()


##find table
table1<- covid_mex %>%
         html_table()

class(table1)

#which number??
tabla_mx<-table1[4]   #remove[[]]
View(tabla_mx)

## saving source
fuente <- tabla-mx[34.1]

#removing not desrired values and columns
table_mx <- table_mx[-c(1.34).-6]  #--> elimina la fila 34 que era la fuente y la columna 6 que eran datos nulos

class(table_mx)
str(table_mx)

#reforming variable

#sub quitara la coma para ponerlo en forma de caracter numerico los miles
tabla_mx$`Cumulative cases` = sub("," , "", tabla_mx$`Cumulative cases`)
tabla_mx$`Active cases` = sub("," , "", tabla_mx$`Active cases`)
tabla_mx$Recoveries = sub("," , "", tabla_mx$Recoveries
tabla_mx$Deaths = sub("," , "", tabla_mx$Deaths
                                                    
str(tabla_mx)                                                    

ggplot(data= tabla_mx, aes(x=State, Y=Death)) + geom_col() + coord_flip()
ggplot(data= tabla_mx, aes(x=State, Y=`Cumulative cases``)) + geom_col() + coord_flip() + labs(y "Cumulative cases")


















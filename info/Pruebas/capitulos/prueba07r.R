
#----------------------------------------------------------------------------------------------------------------------------------#
##Bar graph
#1
  plot1<-ggplot(df_brand, aes(x = brand, y = n, fill = brand)) + geom_col() + geom_text(aes(label = n), vjust = 1.5, colour = "black") #bar graphs numbers of cabezas tractoras for every brand
  plot2<-ggplot(df_brand, aes(x = brand, y = mean_years, colour = brand)) + geom_point() #bar graphs edad promedio of cabezas tractoras for every brand
  plot1+plot2

#2
  plot3<-ggplot(df_country, aes(x = country, y = n, fill = country)) + geom_col() + geom_text(aes(label = n), vjust = 1.5, colour = "black") #bar graphs numbers of cabezas tractoras for every country
  plot4<-ggplot(df_country, aes(x = country, y = mean_years, colour = country)) + geom_point() #bar graphs edad promedio of cabezas tractoras for every country
  plot3+plot4
  
#
data_no_outlier_Mad %>% 
  ggplot() +
    aes(x = brand, fill= country) +
    geom_bar( color="black") +
    facet_grid(country ~ .) +
    geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, colour = "black")

#--> no lo pondre, en este se grafican segun el rango de edad de los vehiculos ofertados
#  ggplot(data_no_outlier_Mad, aes(x = years.category)) + geom_bar(position = "dodge", fill = "lightblue", colour = "black")

#----------------------------------------------------------------
##scatter plots y grafico de dispersion
#1
  ggplot(data_no_outlier_Mad, aes(x = years, y = price, colour = years.category)) +
    geom_point()
  
#2    
  ggplot(data_no_outlier_Mad, aes(x = years, y = km, colour = years.category)) +
    geom_point() +
    scale_shape_manual(values = c(1,2)) +
    scale_colour_brewer(palette = "Set1")
  
#--> no lo pondre
#  ggplot(data_no_outlier_Mad, aes(x = years, y = km, colour = country)) + geom_point()

#--> no se si ponerlo, pq la tendencia no tiene mucho sentido, ya que los mas viejos no suelen tener tantos km recorridos
#3  
#  ggplot(data_no_outlier_Mad, aes(x = years, y = km, colour = years.category))+geom_point()+stat_smooth(geom = "smooth", method = "lm", se = FALSE)

#----------------------------------------------------------------
###histograma
#1
  ggplot(data_no_outlier_Mad, aes(x = power)) +
    geom_histogram(fill = "#8EE5EE", colour = "black") +
    geom_density() +
    facet_grid(years.category ~ ., scales = "free")

#2
    ggplot(data_no_outlier_Mad ,aes(x = power, fill= country)) +
     geom_histogram( color="black") +
     facet_grid(country ~ .)
    
#--> no lo pondre,
  #ggplot(data_no_outlier_Mad, aes(x = years)) +
  #  geom_histogram() +
  #  facet_grid(brand ~ ., scales = "free")
  
#--> no lo pondre,
  #binsize <- diff(range(data_no_outlier_Mad$price))/6
  #ggplot(data_no_outlier_Mad, aes(x = price)) +
  #geom_histogram(binwidth = binsize,  fill = "lightblue", colour = "black") +
  #scale_fill_brewer(palette = "Set1")
  
#--> no lo pondre
  #ggplot(data_no_outlier_Mad, aes(x = years)) +
  #geom_histogram(binwidth = binsize1,  fill = "lightblue", colour = "black") +
  #geom_density() +
  #facet_grid(brand ~ ., scales = "free")
  
    
    
    
    
#----------------------------------------------------------------
#duda, se ven bien, pero no se si ponerlas:
data_no_outlier_Mad %>% 
  ggplot() +
  aes(x = price, y = years, color = country) +
  geom_point() +
  facet_grid(country ~ .)

data_no_outlier_Mad %>% 
  ggplot() +
  aes(x = years, fill= country) +
  geom_bar( color="black") +
  facet_grid(country ~ .)

data_no_outlier_Mad %>% 
  ggplot() +
  aes(x = years, fill= brand) +
  geom_bar( color="black") +
  facet_grid(brand ~ .)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# #los dejo por si se me ocurre otra idea, o sirven de inspircion,

# ##line graph /area graphs
# 
#   ggplot(datos, aes(x = year, y = price, fill = years.category)) +
#     geom_area(colour = "black", size = .2, alpha = .4) +
#     scale_fill_brewer(palette = "Blues")
# 
#   ggplot(datos, aes(x = year, y = price, fill = years.category)) +
#     geom_area(position = "fill", colour = "black", size = .2, alpha = .4) +
#     scale_fill_brewer(palette = "Blues") +
#     scale_y_continuous(labels = scales::percent)
#   
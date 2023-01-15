#calculo las variables, elimina aquellas que no cumplan condiciones ropuestas, como el no tener mas de dos valores por marca, por pais, 
df_country <- data_no_outlier_Mad %>% group_by(country) %>% summarise( n=n(), mean_year = ceiling(mean(year)), mean_years= ceiling(mean(years)), mean_price = ceiling(mean(km)), median_price=median(km), variance_price = var(km), sd_price = sd(km)) %>% filter(n>2) %>% arrange(n)#agrupa por paises y dice cuanto hay por cada uno, ademas saca la media de km y la suma total
df_brand <- data_no_outlier_Mad %>% group_by(brand) %>% summarise( n=n(), mean_year = mean(year), mean_years= mean(years), mean_price = ceiling(mean(km)), median_price=median(km), variance_price = var(km), sd_price = sd(km)) %>% filter(n>2) %>% arrange(n)

#agrupa en un vector aquellos paises que tengan mas de dos cabezas tractoras
countrylist_p <- df_country[,1]
countrylist_p <- data.frame(t(countrylist_p))
countrylist_p <- as.vector(countrylist_p)

#agrupa en un vector aquellas marcas que tengan mas de dos cabezas tractoras
brandlist_p <- df_brand[,1]
brandlist_p <- data.frame(t(brandlist_p))
brandlist_p <- as.vector(brandlist_p)

#en data_no_outlier_Mad solo mantendremos los valores que cumplan las dos condiciones anteriormente planteadas... y veo las diferencia de como cambian las dimensiones
dim(data_no_outlier_Mad)
data_no_outlier_Mad <- filter(data_no_outlier_Mad, country %in% countrylist_p, brand %in% brandlist_p)
dim(data_no_outlier_Mad)


resume<- rbind(describe(data_no_outlier_Mad$power), describe(data_no_outlier_Mad$price), describe(data_no_outlier_Mad$km), describe(data_no_outlier_Mad$years)) #similar as summary but it show the result as a dataframe
rownames(resume) <- c("Power", "Price", "Km", "Year" )

df_brand <- data_no_outlier_Mad %>% group_by(brand) %>% summarise( n=n(), mean_year = mean(year), mean_years= mean(years), mean_price = ceiling(mean(km)), median_price=median(km), variance_price = var(km), sd_price = sd(km)) %>% arrange(n)

df_country <- data_no_outlier_Mad %>% group_by(country) %>% summarise( n=n(), mean_year = ceiling(mean(year)), mean_years= ceiling(mean(years)), mean_price = ceiling(mean(km)), median_price=median(km), variance_price = var(km), sd_price = sd(km)) %>% arrange(n)#agrupa por paises y dice cuanto hay por cada uno, ademas saca la media de km y la suma total


df_year <- aggregate(data_no_outlier_Mad$price,by=list(data_no_outlier_Mad$years),mean, na.rm=TRUE)
colnames(df_year) <- c("years","mean_price")
df_year[2] <- df_year$mean_price %>% ceiling()


powerlist <- split(data_no_outlier_Mad, data_no_outlier_Mad$power) #separa en lista de dataframe segun las potencias
countrylist <- split(data_no_outlier_Mad, data_no_outlier_Mad$country) #separa en lista de dataframe segun las paises
yearslist <- split(data_no_outlier_Mad, data_no_outlier_Mad$years) #separa en lista de dataframe segun las años


#----------------------------------------------------------------------------------------------------------------------------------#

##Bar graph
ggplot(df_brand, aes(x = brand, y = n)) + geom_col() #bar graphs numbers of cabezas tractoras for every brand
ggplot(df_brand, aes(x = brand, y = mean_year)) + geom_point() #bar graphs edad promedio of cabezas tractoras for every brand

ggplot(df_country, aes(x = country, y = n)) + geom_col(position = "dodge", fill = "lightblue", colour = "black")
ggplot(df_country, aes(x = country, y = mean_years)) + geom_col(position = "dodge", fill = "lightblue", colour = "black")
ggplot(df_country, aes(x = country, y = mean_year)) + geom_point()

ggplot(data_no_outlier_Mad, aes(x = years.category)) +
  geom_bar(position = "dodge", fill = "lightblue", colour = "black")

#----------------------------------------------------------------
##scatter plots y grafico de dispersion
ggplot(data_no_outlier_Mad, aes(x = years, y = km)) +
  geom_point(shape=21, size = 1.5)

ggplot(data_no_outlier_Mad, aes(x = years, y = price)) +
  geom_point()

ggplot(data_no_outlier_Mad, aes(x = year, y = km, colour = years.category)) +
  geom_point() +
  scale_shape_manual(values = c(1,2)) +
  scale_colour_brewer(palette = "Set1")

ggplot(data_no_outlier_Mad, aes(x = years, y = km, colour = years.category)) +
  geom_point()

ggplot(data_no_outlier_Mad, aes(x = years, y = km, colour = country)) +
  geom_point()

ggplot(data_no_outlier_Mad, aes(x = years, y = price, colour = years.category)) +
  geom_point()


hw_sp <- ggplot(data_no_outlier_Mad, aes(x = years, y = km))
hw_sp +
  geom_point(colour = "grey60") +
  stat_smooth(geom = "smooth", method = "lm", se = FALSE)





#----------------------------------------------------------------
###histograma

ggplot(data_no_outlier_Mad, aes(x = years)) +
  geom_histogram()

ggplot(data_no_outlier_Mad, aes(x = power)) +
  geom_histogram()


#este no sale como quiero, pq quiero hacer antes un filtro para aquellos que no obtuvieron mas de dos datos, o hacer para aquellos que no obtuvieron una union como otros..
#ggplot(data_no_outlier_Mad, aes(x = years)) +
#  geom_histogram() +
#  facet_grid(brand ~ ., scales = "free")


# Divide the x range into 10 bins -->
binsize <- diff(range(data_no_outlier_Mad$price))/10

ggplot(data_no_outlier_Mad, aes(x = price)) +
  geom_histogram(binwidth = binsize, colour = "black") +
  scale_fill_brewer(palette = "Pastel1")

# Use years.category as the faceting variable -_> punto 6.5
ggplot(data_no_outlier_Mad, aes(x = years)) +
  geom_histogram(fill = "white", colour = "black") +
  geom_density() +
  facet_grid(years.category ~ ., scales = "free")

ggplot(data_no_outlier_Mad, aes(x = years)) +
  geom_histogram(fill = "white", colour = "black") +
  geom_density() +
  facet_grid(brand ~ ., scales = "free")

ggplot(data_no_outlier_Mad, aes(x = power)) +
  geom_histogram(fill = "white", colour = "black") +
  geom_density() +
  facet_grid(years.category ~ ., scales = "free")

ggplot(data_no_outlier_Mad, aes(x = years)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(country ~ ., scales = "free")
#----------------------------------------------------------------
ggplot(data_no_outlier_Mad, aes(x = years)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(country ~ ., scales = "free")





#pruebas


# Use years.category as the faceting variable -_> punto 6.5 --> no me funciona
ggplot(data_no_outlier_Mad, aes(x = years, y = km)) +
  geom_point(fill = "white", colour = "black") +
  geom_density() +
  facet_grid(country ~ ., scales = "free")

#https://bookdown.org/dparedesi/data-science-con-r/gapminder.html
#https://r-graphics.org/recipe-distribution-basic-boxplot


data_no_outlier_Mad %>% 
  ggplot() +
  aes(x = price, y = years, color = country) +
  geom_point() +
  facet_grid(country ~ .)

data_no_outlier_Mad %>% 
  ggplot() +
  aes(y = years, fill= country) +
  geom_histogram( color="black") +
  facet_grid(country ~ .)


data_no_outlier_Mad %>% 
  ggplot() +
  aes(x = power, fill= country) +
  geom_histogram( color="black") +
  facet_grid(country ~ .)


data_no_outlier_Mad %>% 
  ggplot() +
  aes(x = years.category, fill= country) +
  geom_bar( color="black") +
  facet_grid(country ~ .)

#a partir de aqui los mejores
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
data_no_outlier_Mad %>% 
  ggplot() +
  aes(x = brand, fill= country) +
  geom_bar( color="black") +
  facet_grid(country ~ .)

data_no_outlier_Mad %>% 
  ggplot() +
  aes(x = brand, y = years, fill= country) +
  geom_col() +
  facet_grid(country ~ .)

data_no_outlier_Mad %>% 
  ggplot() +
  aes(x = brand, y = years, fill= country) +
  geom_col() +
  facet_grid(country ~ .)

#los sig 4 son iguales (dos y dos, uno para country y otro para brand), pero el segundo se ve mejor
#----------------------------------------------------------------  
data_no_outlier_Mad %>% 
  ggplot() +
  aes(x = years, fill= country) +
  geom_histogram( color="black") +
  facet_grid(country ~ .)

data_no_outlier_Mad %>% 
  ggplot() +
  aes(x = years, fill= country) +
  geom_bar( color="black") +
  facet_grid(country ~ .)


data_no_outlier_Mad %>% 
  ggplot() +
  aes(x = years, fill= brand) +
  geom_histogram( color="black") +
  facet_grid(brand ~ .)

data_no_outlier_Mad %>% 
  ggplot() +
  aes(x = years, fill= brand) +
  geom_bar( color="black") +
  facet_grid(brand ~ .)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------




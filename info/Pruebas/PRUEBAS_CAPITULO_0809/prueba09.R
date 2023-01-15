#--------------------------------------------------------------------------------------------------------------------------------------------
#pruebas capitulo09 Modelizacion

# X = years --> independiente, cuantitativo, discreto
# Y = Price --> dependienge, cuantitativo, continuo

# resume08<- rbind(describe(data_no_outlier_Mad$years), describe(data_no_outlier_Mad$price), describe(data_no_outlier_Mad$km)) #similar as summary but it show the result as a dataframe
# rownames(resume08) <- c("Year", "Price", "Km")
# 
# 
# 
# 
# 

attach(data_no_outlier_Mad)
scatterplot3d(x=years, y=km, z=price, pch=16, cex.lab=1,
              highlight.3d=TRUE, type="h", xlab='AGE',
              ylab='Distancia (km)', zlab='Price (€)')


attach(data_no_outlier_Mad)
scatterplot3d(x=price, y=years, z=km, pch=16, cex.lab=1,
              highlight.3d=TRUE, type="h", xlab='AGE',
              ylab='Distancia (km)', zlab='Price (€)')

attach(data_no_outlier_Mad)
scatterplot3d(x=years, y=power, z=price, pch=16, cex.lab=1,
              highlight.3d=TRUE, type="h", xlab='AGE',
              ylab='Distancia (km)', zlab='Price (€)')

attach(data_no_outlier_Mad)
scatterplot3d(x=years, y=years.category, z=price, pch=16, cex.lab=1,
              highlight.3d=TRUE, type="h", xlab='AGE',
              ylab='Distancia (km)', zlab='Price (€)')

attach(data_no_outlier_Mad)
scatterplot3d(x=years.category, y=price, z=power, pch=16, cex.lab=1,
              highlight.3d=TRUE, type="h", xlab='AGE category',
              ylab='Price (€)', zlab='power (CV)')




# 
# #lineal
reg_years_price_lineal_3d = lm(price ~ years + km, data = data_no_outlier_Mad) #nos da el a y el b
summary(reg_years_price_lineal_3d)
# 
ggplot(data_no_outlier_Mad, aes(x=years, y=price, z=km)) + 
   geom_point() +
   geom_smooth(method='lm', formula=y~x+z, se=FALSE, col='dodgerblue1') +
   theme_light()

# --------------------------------------------------------------------------------------------------------------------------------------------
#exp
reg_years_price_exp_3d = lm(log(price) ~ years + km, data = data_no_outlier_Mad) #nos da el a y el b
summary(reg_years_price_exp_3d)

#--------------------------------------------------------------------------------------------------------------------------------------------
#power
reg_years_price_power_3d = lm(log(price) ~ log(years) + log(km)), data = data_no_outlier_Mad) #nos da el a y el b
summary(reg_years_price_power_3d)
























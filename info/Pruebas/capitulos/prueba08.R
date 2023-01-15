#write.csv(data_no_outlier_Mad, file= 'DATA1.csv')
#pruebas capitulo08 Modelizacion con una unica variable por el metodo de minimos cuadrados
#--------------------------------------------------------------------------------------------------------------------------------------------
resume08<- rbind(describe(data_no_outlier_Mad$years), describe(data_no_outlier_Mad$price), describe(data_no_outlier_Mad$km)) #similar as summary but it show the result as a dataframe
rownames(resume08) <- c("Year", "Price", "Km")

#Formulas: 
 #lineal
   # y = a + b * x 
   # b = Rxy * Sy / Sx = Covxy / (Sx)^2
   # a = mean(y) - (mean(x) * b)
 #exponencial
   # ln(y) = ln(a) + b * x  --> y = a * exp(b * x)
   # b = Cov(x*ln(y)) / (Sx)^2
   # ln(a) = mean(ln(y)) - (mean(x) * b)
   # despejo a
 #power
 #veo validez con R^2


# X = years --> independiente, cuantitativo, discreto
# Y = Price --> dependienge, cuantitativo, continuo

#lineal
  #calculo manual, este da resultados con mas decimales
    # b_lineal = cov(data_no_outlier_Mad$years, data_no_outlier_Mad$price) / (resume07 [4,4])^2
    # a_lineal = resume07 [2,3] - (resume07 [4,3] * b_lineal)
    # r_2_lineal = (cov(data_no_outlier_Mad$years, data_no_outlier_Mad$price) / ((resume07 [4,4]) * (resume07 [2,4])))^2
    # r_2_lineal = (cor(data_no_outlier_Mad$years, data_no_outlier_Mad$price))^2 #da igual, pero es otra forma
    # coef_depre_lineal = b_lineal / a_lineal
    # data_no_outlier_Mad <- data_no_outlier_Mad %>% mutate(y_linel = a_lineal + b_lineal * data_no_outlier_Mad$years) #creamos la Y 
  #calculo con funcionn lm
    reg_years_price_lineal = lm(price ~ years, data = data_no_outlier_Mad) #nos da el a y el b
    data_no_outlier_Mad <- data_no_outlier_Mad %>% mutate(y_lineal = fitted(reg_years_price_lineal)) #creamos la Y , el fitted no da la Y calculada del modelo
    summary(reg_years_price_lineal)
    confint(reg_years_price_lineal) #saco intervalo de confianza #resultado de arriba nos dice que, con una confianza del 95%, la pendiente se encuentra entre  -2455.887 y -2310.062.
    #residuals(reg_years_price_lineal) #saco los valores residuales
    lineal_years_price_coeff_beta <- reg_years_price_lineal$coefficients
    lineal_years_price_coeff_depre <- lineal_years_price_coeff_beta[2]/lineal_years_price_coeff_beta[1]
    lineal_years_price_coeff_R <- summary(reg_years_price_lineal)$r.square
    #lineal_years_price_coeff_R <- summary(reg_years_price_lineal)$adj.r.square #para el R ajustado

# --------------------------------------------------------------------------------------------------------------------------------------------

# #exp
  #manual
    # b_exp = cov(data_no_outlier_Mad$years, log(data_no_outlier_Mad$price)) / (resume07 [4,4])^2
    # a_exp = mean(log(data_no_outlier_Mad$price)) - (resume07 [4,3] * b_exp)
    # a_exp = exp(a_exp)
    # r_2_exp = (cov(data_no_outlier_Mad$years, log(data_no_outlier_Mad$price)) / (sd(log(data_no_outlier_Mad$price)) * (resume07 [4,4])))^2
    # r_2_exp = (cor(data_no_outlier_Mad$years, log(data_no_outlier_Mad$price)))^2 #no se pq me da diferente a lo que da en el excel
    # coef_depre_exp = exp(b_exp)
    
  #con formula
    reg_years_price_exp = lm(log(price) ~ years, data = data_no_outlier_Mad) #nos da el a y el b
    data_no_outlier_Mad <- data_no_outlier_Mad %>% mutate(y_exp = exp(fitted(reg_years_price_exp))) #creamos la Y , el fitted no da la Y calculada del modelo
    summary(reg_years_price_exp)
    confint(reg_years_price_exp) #saco intervalo de confianza.
    #residuals(reg_years_price_exp) #saco los valores residuales
    exp_years_price_coeff_beta <- reg_years_price_exp$coefficients
    exp_years_price_coeff_depre <- exp(exp_years_price_coeff_beta[2])
    exp_years_price_coeff_R <- summary(reg_years_price_exp)$r.square
    #exp_years_price_coeff_R <- summary(reg_years_price_exp)$adj.r.square #para el R ajustado
#--------------------------------------------------------------------------------------------------------------------------------------------

#power
  #manual
    # b_power = cov(log(data_no_outlier_Mad$years), log(data_no_outlier_Mad$price)) / (sd(log(data_no_outlier_Mad$years)))^2
    # a_power = mean(log(data_no_outlier_Mad$price)) - (mean(log(data_no_outlier_Mad$years)) * b_power)
    # a_power = exp(a_power)
    # r_2_power = (cov(log(data_no_outlier_Mad$years), log(data_no_outlier_Mad$price)) / (sd(log(data_no_outlier_Mad$years) * sd(log(data_no_outlier_Mad$price)))))^2
    # r_2_power = (cor(log(data_no_outlier_Mad$years), log(data_no_outlier_Mad$price)))^2 #no se pq me da diferente a lo que da en el excel
    # coef_depre_power = b_power
    
  #con formula
    reg_years_price_power = lm(log(price) ~ log(years), data = data_no_outlier_Mad) #nos da el a y el b
    data_no_outlier_Mad <- data_no_outlier_Mad %>% mutate(y_power = exp(fitted(reg_years_price_power))) #creamos la Y , el fitted no da la Y calculada del modelo
    summary(reg_years_price_power)
    confint(reg_years_price_power) #saco intervalo de confianza.
    #residuals(reg_years_price_power) #saco los valores residuales
    power_years_price_coeff_beta <- reg_years_price_power$coefficients
    power_years_price_coeff_depre <- power_years_price_coeff_beta[2]
    power_years_price_coeff_R <- summary(reg_years_price_power)$r.square
    #power_years_price_coeff_R <- summary(reg_years_price_power)$adj.r.square #para el R ajustado



#extra 
  # #la sig, no se si la pondre, ees hacerlo, pero en vez de  año, uso km
  # # X = Km --> independiente, cuantitativo, continuo
  # # Y = Price --> dependienge, cuantitativo, continuo
  # b1 = cov(data_no_outlier_Mad$years, data_no_outlier_Mad$km) / (resume07 [3,4])^2
  # a1 = resume07 [2,3] - (resume07 [3,3] * b1)
  # y1 = a1 + b1 * data_no_outlier_Mad$km
  # data_no_outlier_Mad <- data_no_outlier_Mad %>% mutate(y1 = a1 + b1 * data_no_outlier_Mad$km) #creamos la Y 



library(stargazer)
stargazer(reg_years_price_lineal, reg_years_price_exp, reg_years_price_power, type="text", df=FALSE) #coef de a le faltaria hacer el exp(a)

#graficar
#--------------------------------------------------------------------------------------------------------------------------------------------
    
#tendencia que realmente sigue
ggplot( data = data_no_outlier_Mad,
        mapping = aes( x= years,
                       y = price))+
  geom_point()+
  geom_smooth()


#-------------------------------------------------------------------------------------------------------------------------------
    
    
  #2da forma
    #varias tendencias graficadas, este da diferente al correcto, que es el de abajo deeste
    ggplot( data = data_no_outlier_Mad,
            mapping = aes( x= years,
                           y = price))+
      geom_point()+
      geom_smooth(method='lm', formula=y~x, se=FALSE, col='red', lty=5)+
      geom_smooth(method='lm', formula=y~log10(x), se=FALSE, col='dodgerblue1', lty=2)+
      geom_smooth(method='lm', formula=y~dexp(x), se=FALSE, col='purple', lty=3)+
      ggtitle("TITULO")

  #como comente arriba, este de abajo da diferente al de arriba, (uno lo calculo yo antes)
    ggplot(data_no_outlier_Mad, aes(years)) + # basic graphical object 
      geom_point(aes(y = price)) +
      geom_line(aes(y=y_lineal), colour="red",  linetype=2, size=1) + # first layer 
      geom_line(aes(y=y_exp), colour="green",  linetype=3, size=1) + # second layer
      geom_line(aes(y=y_power), colour="blue",  linetype=4, size=1)+ # second layer
      labs(title = "TITULO")+
      theme(plot.title = element_text(hjust = 0.5))

    
      # legend(x = "topright",         # Posición
      #  legend = c("J0", "J2"), # Textos de la leyenda
      #  lty = c(1, 2),          # Tipo de líneas
      #  col = c(2, 3),          # Colores de las líneas
      #  lwd = 2)                # Ancho de las líneas   
    
#agregar leyenda como la sig, no logre poner en ggplot, por lo que abajo esta en plot normal
plot2<- 
  {plot(data_no_outlier_Mad$years, data_no_outlier_Mad$price, main = "TITULO")
    legend("topright", legend = c("lineal", "power", "semilog"), lty= c(2, 3, 4), lwd = 3, col = c("red", "blue", "green")) 
    lines(data_no_outlier_Mad$years, data_no_outlier_Mad$y_lineal , col = "red")
    lines(data_no_outlier_Mad$years, data_no_outlier_Mad$y_exp , col = "blue")
    lines(data_no_outlier_Mad$years, data_no_outlier_Mad$y_power , col = "green")}

  #--------------------------------------------------------------------------------------------------------------------------------------------  
  

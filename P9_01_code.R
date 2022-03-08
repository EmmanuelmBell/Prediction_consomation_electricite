#Librairie
library("readxl")
library(stringr)
library(ggplot2)
library(tidyverse)
library(car)
library(forecast)
library(latex2exp)
library(caschrono)
library(tseries)

################################################################################
########################## FICHIER #############################################
################################################################################

#Fichier électricité
elec_cons <-read.csv("eCO2mix_RTE_energie_M.csv",dec=".",sep=",")

#Fichier température
temperature <-read.csv("calcul_DJU_13_12_2021.csv",dec=".",sep=",")



temperature_value <- temperature 

rownames(temperature_value) <- temperature_value$X
temperature_value$X <- NULL
temperature_value$Total <- NULL


temperature_mod <- data.frame(Mois = character(), Temperature = double())

temperature$X[10]


#créer un index avec  l'année et le mois 
#donc changer les noms de chaque mois en chiffres est concaténé avec l'année
for (i in 2:10) {
  for (y in 1:12) {
    # je récuppère le mois 
    year <- temperature$X[i]
    # je crée la date 
    year = as.character(year)
    month = y
    month = as.character(month)
    if (y < 10 ) {
      date = str_c(year, "-0", month)
    }else {
      date = str_c(year, "-", month)
    }
   
    new_row <- list(date,temperature_value[i,y])
    temperature_mod <- rbind(temperature_mod,new_row)
    
  }
}
#Je renomme les colonnes 
colnames(temperature_mod)[1] <- "Mois"
colnames(temperature_mod)[2] <- "DJU"

#je crée un index 
temperature_mod$index = seq(from= 1, to=108, by=1)

#je réorganise mon dataframe pour avoir les données les plus ancienne en haut 
temperature_mod <- temperature_mod[order(-temperature_mod$index),]

#Création d'une serie temporelle pour la température 
temp_ts <- ts(temperature_mod[,2],start=2012,freq=12)

#représentation graphique de la série temporelle de la température 
plot(temp_ts,xlab="Années",ylab="DJU")
title("Série temporelle du DJU")

#Données ne concernant que l'île de France
ile_france <- subset(elec_cons, elec_cons$Territoire == "Ile-de-France")

ile_france_cons <- data.frame(Mois=ile_france$Mois , Consommation_totale=ile_france$Consommation.totale)

#représenation de la série temporelle de la consommation d'électricité en île de france
ile_france_cons_ts <- ts(ile_france_cons[,2],start=2013,freq=12)

#représentation graphique de la consommation ile de France
plot(ile_france_cons_ts,xlab="Années",ylab="Consommation totale")
title("Série temporelle de la consommation en ile de france")

#Merge des données
main_df = merge(ile_france_cons, temperature_mod, by ="Mois")

main_df$index <- NULL

################################################################################
######## Correction de la consommation mensuel par rapport au DJU ##############
################################################################################

#Représentation graphique de la consomation total par dju :

ggplot(main_df, aes(x=main_df$DJU, y=main_df$Consommation_totale))+
  geom_point()+
  xlab("DJU")+
  ylab("Consommation_totale")+
ggtitle("Consommation totale selon DJU")



#Données pour la régression
main_df = main_df[13:96,0:3]

#Représentation graphique de la consomation total par dju :

ggplot(main_df, aes(x=main_df$DJU, y=main_df$Consommation_totale))+
  geom_point()+
  xlab("DJU")+
  ylab("Consommation_totale")+
  ggtitle("Consommation totale selon DJU")

#Régression linéaire : 
reg=lm(main_df$Consommation_totale~main_df$DJU)
summary(reg)

#Nous avons une p-value pour le dju qui est <2e-16 ce qui est en dessous des 5% 
#des niveaux de test 

# Evaluation visuel de la linéarité  
scatterplot(Consommation_totale~DJU, data=main_df)

## On constate que la droite de régression est comprise dans l’intervalle de confiance
##de la courbe lowess, l’hypothèse de linéarité est donc acceptable.

#Evaluation des hypothèses de validité des résultats
#Le test d’évaluation de la significativité du lien linéaire entre les deux variables est valide, si les résidus :
  #sont indépendants
  #sont distribués selon une loi Normale de moyenne 0
  #sont distribués de façon homogènes, c’est à dire, avec une variance
  #constante

#Evaluation de l'hypothèse d'indépendance des résidus
acf(residuals(reg), main="reg")


# auto-corrélation significative pour les lag 1, 6 et 12

#test de durbin Watson
durbinWatsonTest (reg)

#auto corrélation significative p-value inférieur au 5% de niveau de test 
#elle est égale à 0.002


#Evaluation de l'hypothèse de normalité des résidus
plot(reg,2)
## les point sont bien aligné sur la droite 

#test de normalité des résidu avec un shapiro
shapiro_test <- shapiro.test(reg$residuals)
## On obtient une p-value de 0.97 ce qui est au dessus des 5% de niveau de test 
## donc l'hypothèse de normalité n'est pas rejeté


#Evaluation de l'hypothèse d'homogénéité des résidus
plot(reg, 3)

## Il semblerait que certains résidus semble être deux par deux par ou trois par trois
## invalidant l'indépendance des résidus 


#évaluer cette hypothèse en employant le test de Breush-Pagan
#l’hypothèse d’homogénéité est rejetée si la p-value est inférieure à 0.05

ncvTest(reg)

## Nous avons une p-valeur qui est égale ici à 0.84 ce qui est largement au dessus 
## des 5 % de niveau de test il existe donc bien une homogénéité des residus  


# correction de la consommation 
coef_temp <- reg$coefficients[2]

main_df$Conso_cor <- main_df$Consommation_totale - coef_temp*main_df$DJU

#creation de série temporelle 
ts_conso_total<- ts(main_df$Consommation_totale,start=2014,freq=12)
ts_conso_corr <- ts(main_df$Conso_cor,start=2014,freq=12)







par(xpd=TRUE, mar=c(3,3,3,3))
ts.plot(ts_conso_total,ts_conso_corr,xlab="t",ylab="consomation",col=c(1,2),lwd=c(2,2))
legend("topright", inset = 0,legend=c("Consommation totale","Consommation corrigée"),col=c(1,2),lwd=c(1,2))
title("Consommation totale et Consommation corrigée")



################################################################################
#Effectuez une désaisonnalisation de la consommation grâce aux moyennes mobiles#
################################################################################


#Désaisonnaliser grace à la fonction decompose
decomp.x=decompose(ts_conso_corr,type="additive")
plot(decomp.x)
print(decomp.x$seasonal)
main_df$desaiso_corr = main_df$Conso_cor - decomp.x$seasonal

desaiso_corr_ts <- ts(main_df$desaiso_corr,start=2014,freq=12)
ts.plot(ts_conso_corr, desaiso_corr_ts,xlab="t",ylab="consommation",col=c(1,2),lwd=c(2,2))
legend("topright", inset = 0,legend=c("Consommation totale","consommation corrigé"),col=c(1,2),lwd=c(1,2))
title("Série temporelle consommation corrigé")


################################################################################
##### Méthode holt winters #####################################################
################################################################################

hw=ets(ts_conso_corr,model="MMM")
hw.pred=predict(hw,12)
plot(hw.pred)

summary(hw)

### Prevision pour année 2020:

x_tronc_hw_2020=window(ts_conso_corr,end=c(2019,12))

x_a_prevoir_hw_2020=window(ts_conso_corr,start=c(2020,1))

hw=ets(x_tronc_hw_2020,model="MMM")
hw.pred=predict(hw,12)

pred_hwtronc=forecast(hw,h=12,level=95)
pred_tronc=pred_hwtronc$mean
pred_l_tronc=ts(pred_hwtronc$lower,start=c(2020,1),frequency=12)
pred_u_tronc=ts(pred_hwtronc$upper,start=c(2020,1),frequency=12)
ts.plot(x_a_prevoir_hw_2020,pred_tronc,pred_l_tronc,pred_u_tronc,xlab="t",ylab="Airpass",col=c(1,2,3,3),lty=c(1,1,2,2),lwd=c(3,3,2,2))
legend("topleft",legend=c("X","X_prev"),col=c(1,2,3,3),lty=c(1,1),lwd=c(3,3))
legend("topright",legend=c("int95%_inf","int95%_sup"),col=c(3,3),lty=c(2,2),lwd=c(2,2))
title("Prédiction holt-winters pour 2020")

rmse_hw_2020 <- sqrt(mean((x_a_prevoir_hw_2020-pred_tronc)^2))

mape_hw_2020 <- mean(abs(1-pred_tronc/x_a_prevoir_hw_2020))*100

summary(hw)

### Prévision 2019 
x_tronc_hw=window(ts_conso_corr,end=c(2018,12))

x_a_prevoir_hw=window(ts_conso_corr,start=c(2019,1), end=c(2019,12))

hw=ets(x_tronc_hw,model="MMM")
hw.pred=predict(hw,12)

summary(hw)
pred_hwtronc=forecast(hw,h=12,level=95)
pred_tronc=pred_hwtronc$mean
pred_l_tronc=ts(pred_hwtronc$lower,start=c(2019,1),frequency=12)
pred_u_tronc=ts(pred_hwtronc$upper,start=c(2019,1),frequency=12)
ts.plot(x_a_prevoir_hw,pred_tronc,pred_l_tronc,pred_u_tronc,xlab="t",ylab="Airpass",col=c(1,2,3,3),lty=c(1,1,2,2),lwd=c(3,3,2,2))
legend("topleft",legend=c("X","X_prev"),col=c(1,2,3,3),lty=c(1,1),lwd=c(3,3))
legend("topright",legend=c("int95%_inf","int95%_sup"),col=c(3,3),lty=c(2,2),lwd=c(2,2))
title("Prédiction holt-winters pour 2019")


rmse_hw <- sqrt(mean((x_a_prevoir_hw-pred_tronc)^2))
  
mape_hw <- mean(abs(1-pred_tronc/x_a_prevoir_hw))*100



#################################################################################
###################### Modèle Sarima ############################################
#################################################################################



#Stationarisation de la série 
plot(acf(ts_conso_corr,lag.max=36,plot=FALSE),ylim=c(-1,1))

#La sortie ACF présente une décroissance lente vers 0, ce qui traduit un problème de non-stationnarité.
#On effectue donc une différenciation  


#On effectue donc une différenciation  (I−B12)
y_dif_1_12=diff(ts_conso_corr,lag=12,differences=1)
plot(acf(y_dif_1_12,lag.max=36,plot=FALSE),ylim=c(-1,1))


#test fuller
adf <-adf.test(y_dif_1_12)
print(adf)

#p-value qui est égale à 0.059 qui est légèrement au dessus des 5% de niveau de test 


#auto-corrélogramme partiel
plot(pacf(y_dif_1_12,lag.max=36,plot=FALSE),ylim=c(-1,1))

#On estime en premier lieu un modèle SARIMA(1,1,1)(1,1,1)12
#au vu des autocorrélogrammes empiriques simples et partiels

model1=Arima(ts_conso_corr,order=c(1,1,1),list(order=c(1,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model1)

# test de significativité
t_stat(model1)
# Les p-value de ar1 et sar1 ne sont pas en dessous des 5% de niveau de test 
# on en déduit qu'il ne sont pas significativement différent de zéro
# on teste un autre modèle

# On retire le paramètre le moins  significatif qui est sar1


#Modèle 2:
model2=Arima(ts_conso_corr,order=c(1,1,1),list(order=c(0,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model2)

t_stat(model2)

#on retire le paramètre le moins significatif 

#Modèle 3:
model3=Arima(ts_conso_corr,order=c(0,1,1),list(order=c(0,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model3)

t_stat(model3)

#Modèle 4:
model4=Arima(ts_conso_corr,order=c(0,1,1),list(order=c(0,1,0),period=12),include.mean=FALSE,method="CSS-ML")
summary(model4)

t_stat(model4)


# test de blancheur des résidus 
Box.test.2(model4$residuals,nlag=c(6,12,18,24,30,36),type="Ljung-Box",decim=5)

## On cosntate que les p-valeurs ne sont pas inférieur au niveau de test de 5% on peux donc 
##  ne pas rejetter l'hypothèse H0  qui est que le résidu suit un bruit blanc

#test de la normalité des résidus
shapiro.test(model4$residuals)

#ici nous avons une p-value qui est au dessus des 5% des niveaux de test on accepte donc l'hypothèse gaussienne


#### Prevision ######

pred_model4=forecast(model4,h=12,level=95)
pred=pred_model4$mean
pred_l=ts(pred_model4$lower,start=c(2021,1),frequency=12)
pred_u=ts(pred_model4$upper,start=c(2021,1),frequency=12)
ts.plot(ts_conso_corr,pred,pred_l,pred_u,xlab="t",ylab="Consomation Electricité",col=c(1,2,3,3),lty=c(1,1,2,2),lwd=c(1,3,2,2))


# zoom
ts.plot(window(ts_conso_corr,start=c(2020,1)),pred,pred_l,pred_u,xlab="t",ylab="Consomation Electricité",col=c(1,2,3,3),lty=c(1,1,2,2),lwd=c(1,3,2,2))


#Analyse a posteriori

#On tronque la série de l’année 2020, qu’on cherche ensuite à prévoir à partir de l’historique 2014-2019.

x_tronc=window(ts_conso_corr,end=c(2019,12))

x_a_prevoir=window(ts_conso_corr,start=c(2020,1),end=c(2020,12))


#On vérifie que le modèle 4 sur la série tronquée est toujours validé.
model4tronc=Arima(x_tronc,order=c(0,1,1),list(order=c(0,1,0),period=12),include.mean=FALSE,method="CSS-ML")
#model3tronc = auto.arima(x_tronc)
summary(model4tronc)


t_stat(model4tronc)

Box.test.2(model4tronc$residuals,nlag=c(12,24,36),type="Ljung-Box",decim=5)

shapiro.test(model4tronc$residuals)



# prevision sur 2020
pred_model4tronc=forecast(model4tronc,h=12,level=95)
pred_tronc=pred_model4tronc$mean
pred_l_tronc=ts(pred_model4tronc$lower,start=c(2020,1),frequency=12)
pred_u_tronc=ts(pred_model4tronc$upper,start=c(2020,1),frequency=12)
ts.plot(x_a_prevoir,pred_tronc,pred_l_tronc,pred_u_tronc,xlab="t",ylab="Airpass",col=c(1,2,3,3),lty=c(1,1,2,2),lwd=c(3,3,2,2))
legend("topleft",legend=c("X","X_prev"),col=c(1,2,3,3),lty=c(1,1),lwd=c(3,3))
legend("topright",legend=c("int95%_inf","int95%_sup"),col=c(3,3),lty=c(2,2),lwd=c(2,2))
title("Prédiction sur 2020")
# On constate que la majorité se trouve dans l'intervalle de prévision à 95%

rmse=sqrt(mean((x_a_prevoir-pred_tronc)^2))
rmse

mape=mean(abs(1-pred_tronc/x_a_prevoir))*100
mape

#mape = 5.94

##Prédiction sur 2019

x_tronc=window(ts_conso_corr,end=c(2018,12))

x_a_prevoir=window(ts_conso_corr,start=c(2019,1),end=c(2019,12))


#On vérifie que le modèle 4 sur la série tronquée est toujours validé.
model4tronc=Arima(x_tronc,order=c(0,1,1),list(order=c(0,1,0),period=12),include.mean=FALSE,method="CSS-ML")
#model3tronc = auto.arima(x_tronc)
summary(model4tronc)


t_stat(model4tronc)

Box.test.2(model4tronc$residuals,nlag=c(12,24,36),type="Ljung-Box",decim=5)

shapiro.test(model4tronc$residuals)

# prevision sur 2019
pred_model4tronc=forecast(model4tronc,h=12,level=95)
pred_tronc=pred_model4tronc$mean
pred_l_tronc=ts(pred_model4tronc$lower,start=c(2019,1),frequency=12)
pred_u_tronc=ts(pred_model4tronc$upper,start=c(2019,1),frequency=12)
ts.plot(x_a_prevoir,pred_tronc,pred_l_tronc,pred_u_tronc,xlab="t",ylab="Airpass",col=c(1,2,3,3),lty=c(1,1,2,2),lwd=c(3,3,2,2))
legend("topleft",legend=c("X","X_prev"),col=c(1,2,3,3),lty=c(1,1),lwd=c(3,3))
legend("topright",legend=c("int95%_inf","int95%_sup"),col=c(3,3),lty=c(2,2),lwd=c(2,2))
title("prédiction sur 2019")

rmse=sqrt(mean((x_a_prevoir-pred_tronc)^2))
rmse

mape=mean(abs(1-pred_tronc/x_a_prevoir))*100
mape




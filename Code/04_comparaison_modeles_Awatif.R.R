Conso_idf<-read.csv("Conso_idf.csv")
summary(Conso_idf)

###########creation de la serie temporelle###########
CONS<-ts(Conso_idf$conso_mensuelle,
       start = c(2016,1),
       frequency=12)
###### Representation graphique
plot(CONS, main ="Evolution de la consommation mensuelle 
     d'electricté en Île de France entre 2016 et 2024",
     cex.main=0.9,
     ylab="Consommation",
     xlab="Temps")

###############TENDANCE ET SAISONNALITE ######################
fit<-stl(CONS,s.window = "periodic")
fit$time.series
dat=decompose(CONS) 
####Representation graphique de la decomposition
plot(dat) 

###########La tendance######"""
###creation de la variable tendance
time.pts<-c(1:length(CONS))

### regression linéaire la serie temporelle sur la tendance pourvoir si ils sont liés 
mod<-lm(CONS~time.pts)
summary(mod)

###test de la tendance avec correction de l'heteroscedasticité et l'autocorrelation des residus 
library(lmtest)
library(sandwich)
coeftest(mod,vcov=NeweyWest(mod, lag=12))

##variables predites
donnee <-fitted(mod)

##representation graphiques des variables prédites 
plot(donnee)

temp.fit.lwl<-ts(donnee ,
                 start = c(2016,1),
                 frequency=12)
plot(temp.fit.lwl)
plot(CONS,ylab="",col="gray",sub="Conso",main="")
lines(temp.fit.lwl,lwd=2,col="blue")
acf(CONS)


#############Test de la stationnarité de la série  ##############

library(tseries)
library(forecast)

# 4. Tests stationnarité - en niveau
adf_  <- adf.test(CONS)      # ADF
kpss_ <- kpss.test(CONS)     # KPSS

adf_
kpss_

############IDENTIFICATION / ACF ET PACF############
acf(CONS,lag.max = 36)
pacf(CONS,lag.max=36)

############ESTIMATION###########
mod_sarma <- arima(CONS,
                   order = c(1,0,0),
                   seasonal = list(order = c(1,0,1), period = 12),
                   method = "ML")

mod_sarma

mod_sarma$coef
mod_sarma$sigma2
AIC(mod_sarma)

##########validation##########
res <- residuals(mod_sarma)

ts.plot(res,
        main = "Résidus du modèle SARMA(1,0)(1,1)[12]",
        ylab = "Résidus")
abline(h = 0, col = "red")

acf(res, lag.max = 36,
    main = "ACF des résidus")
########test 
Box.test(res,
         lag = 36,
         type = "Ljung-Box",
         fitdf = 3)


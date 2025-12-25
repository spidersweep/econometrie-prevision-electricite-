##R du projet Consommation électrique###

###ETAPE 1: le fichier R
library(readr)   #pour lire le fichier
library(dplyr)  #pour manipuler les données
library(zoo)     #pour l'interpolation du NA
library(ggplot2)   #pour faire le graphiques

df <- read_csv("conso_idf_mensuel_2016_2024_final_v2.csv")

head(df)    #aperçu du tableau juste 6 première ligne

###ETAPE 2: Vérifier les valeurs NA

colSums(is.na(df))    ##nbr de NA par colonne
                      ##on voit qu'il y a 1 valeur manquant dans la temp_moyen

###ETAPE 3: correction du NA par interpolation linéaire: car la température varie régulièrement d'un mois à l'autre
df$temp_moy <- na.approx(df$temp_moy)       ##interpolation linéaire pour remplacer les NA

sum(is.na(df$temp_moy))  # doit afficher 0 maintenant

###ETAPE 4: visualisation des données: graphique
#graphique de la série: interpréation sur word
ggplot(df, aes(x = as.Date(date_mensuelle), y = conso_mensuelle)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "Consommation mensuelle d'électricité en Île-de-France",
       x = "Date", y = "Consommation (MWh)")


##graphique de l'évolution de la température: interprétation sur word
ggplot(df, aes(x = as.Date(date_mensuelle), y = temp_moy)) +
  geom_line(color = "firebrick", size = 1) +
  labs(title = "Évolution de la température moyenne mensuelle en Île-de-France (2016–2024)",
       x = "Année", y = "Température moyenne (°C)") +
  theme_minimal()

###ETAPE 5: transformation log et normalisation des variables exogènes
df$log_conso <- log(df$conso_mensuelle)
df$temp_moy_z <- scale(df$temp_moy)
df$trend_chauffage_z <- scale(df$trend_chauffage)
df$trend_clim_z <- scale(df$trend_clim)           ##Cela crée de nouvelles colonnes centrées et réduites (moyenne = 0, écart-type = 1).

summary(df)





####ETAPE 6: CALCUL DES CORRELATIONS ENTRE CONSO ET FACTEURS EXO
# Corrélation entre consommation et température
cor(df$conso_mensuelle, df$temp_moy, use = "complete.obs")

# Corrélation entre consommation et Google Trends "chauffage"
cor(df$conso_mensuelle, df$trend_chauffage, use = "complete.obs")

# Corrélation entre consommation et Google Trends "climatisation"
cor(df$conso_mensuelle, df$trend_clim, use = "complete.obs")

# Corrélation entre consommation et Covid
cor(df$conso_mensuelle, df$covid, use = "complete.obs")

###TOUS EN MEME TEMPS: matrice de correlation
cor(df[, c("conso_mensuelle", "temp_moy", "trend_chauffage", "trend_clim", "covid")],
    use = "complete.obs")

##graphique correlation:
library(corrplot)

corr_matrix <- cor(df[, c("conso_mensuelle", "temp_moy", "trend_chauffage", "trend_clim", "covid")],
                   use = "complete.obs")

corrplot(corr_matrix, method = "circle", type = "upper",
         tl.col = "black", tl.cex = 0.8)

###ETAPE 7: RENDRE LE GRAPHIQUE en objet de série temporelle dans R
conso_ts <- ts(df$conso_mensuelle, start = c(2016, 1), frequency = 12) #création de la série temporelle:la série commence en janvier 2016/ données mensuelles (12 observations par an)
plot(conso_ts, 
     main = "Série temporelle : consommation mensuelle d'électricité (Île-de-France)",
     ylab = "Consommation (MWh)", 
     xlab = "Année",
     lwd = 2)          
##un autre type de graphique, mais il montre la même chose — c’est juste un objet R spécial (ts) qu’on peut ensuite analyser ou modéliser.

###ETAPE 8: décomposition additive:
conso_decomp <- decompose(conso_ts, type = "additive")
plot(conso_decomp)

##ETAPE 9: GRAPHIQUE ANALYSE EXPLORATOIRE


library(forecast)
library(tseries)

##création du série temporelle
conso_ts <- ts(df$conso_mensuelle, start=c(2016, 1), frequency=12)


# Graphique de la série avec ACF et PACF combinés
# Ce graphique montre si la série est stationnaire ou non
ggtsdisplay(conso_ts, main="Analyse de la Consommation d'Électricité (IDF)")


#ETAPE 10: Le Test de Dickey-Fuller Augmenté (ADF)

library(tseries)
library(forecast)

# Création de la série (fréquence 12 pour du mensuel)
conso_ts <- ts(df$conso_mensuelle, frequency=12)
# Exécution du test de Dickey-Fuller Augmenté
# Lancement du test ADF
# On utilise k=12 pour tenir compte de la saisonnalité mensuelle
test_adf <- adf.test(conso_ts, alternative = "stationary", k = 12)

# Affichage du résultat
print(test_adf)



# On relance le test sur la nouvelle série
test_final <- adf.test(conso_ts)
print(test_final)

# Visualisation pour confirmer la stationnarité: Visualisation "Box-Jenkins" (ACF/PACF)
ggtsdisplay(conso_stat, main="Série après Différenciation (1, 12)")

##TEST KPSS
library(tseries)
conso_ts <- ts(df$conso_mensuelle, start = c(2016, 1), frequency = 12)
kpss.test(conso_ts)



### ETAPE 11: LES MODELES ECONOMETRIQUE####

df <- read_csv("conso_idf_mensuel_2016_2024_final_v2.csv")


##MODELE SARMA:
library(forecast)

# Créer la série temporelle
conso_ts <- ts(df$conso_mensuelle, start = c(2016, 1), frequency = 12)

# Vérification de la stationnarité
library(tseries)
adf.test(conso_ts)     # Test de Dickey-Fuller
kpss.test(conso_ts)    # Test KPSS

# Identification visuelle des ordres (ACF/PACF)
acf(conso_ts)
pacf(conso_ts)

# Estimation du modèle SARMA(1,0)(1,1)[12]
model_sarma <- Arima(conso_ts, order = c(1, 0, 0), seasonal = list(order = c(1, 1, 0), period = 12))

# Résumé du modèle
summary(model_sarma)

# Diagnostic des résidus
checkresiduals(model_sarma)

conso_ts <- ts(df$conso_mensuelle, start=c(2016, 1), frequency=12)
# Préparation de la matrice des variables exogènes (X)
# On inclut Température, Google Trends et la variable Dummy Covid
exog_vars <- as.matrix(df[, c("temp_moy", "trend_canicule", "trend_clim", "covid")])



#### Comparaison des AIC####

library(forecast)
library(tseries)

# Créer la série temporelle mensuelle
conso_ts <- ts(df$conso_mensuelle, start = c(2016, 1), frequency = 12)

# Créer la matrice des variables exogènes
Xreg <- as.matrix(df[, c("temp_moy", "trend_chauffage", "trend_clim", "covid")])

# Modèle SARMA(p,q)(P,Q)[12]
model_sarma <- Arima(conso_ts,
                     order = c(1, 0, 0),                  # AR(1)
                     seasonal = list(order = c(1, 0, 1),  # composantes saisonnières (SAR=1, SMA=1)
                                     period = 12))

summary(model_sarma)



##SARMAX
library(forecast)

# Création de la série temporelle
conso_ts <- ts(df$conso_mensuelle, start = c(2016,1), frequency = 12)

# Variables exogènes : température, chauffage, clim, Covid
Xreg <- as.matrix(df[, c("temp_moy", "trend_chauffage", "trend_clim", "covid")])

# Estimation du modèle SARMAX
model_sarmax <- Arima(conso_ts,
                      order = c(1, 0, 0),                  # AR(1)
                      seasonal = list(order = c(1, 0, 1),  # composante saisonnière
                                      period = 12),
                      xreg = Xreg)                         # variables exogènes

# Résumé des résultats
summary(model_sarmax)

# Vérification des résidus
graphics.off()
checkresiduals(model_sarmax)

AIC(model_sarma)
AIC(model_sarmax)
BIC(model_sarma)
BIC(model_sarmax)

#Erreur de prévision (RMSE ou MAPE)
# Diviser en apprentissage et test
train <- window(conso_ts, end = c(2023, 12))
test <- window(conso_ts, start = c(2024, 1))

# SARMA (sans exogènes)
fit_sarma <- Arima(train, order = c(1,0,0),
                   seasonal = list(order = c(1,0,1), period = 12))
pred_sarma <- forecast(fit_sarma, h = length(test))

# SARMAX (avec exogènes)
fit_sarmax <- Arima(train, order = c(1,0,0),
                    seasonal = list(order = c(1,0,1), period = 12),
                    xreg = Xreg[1:length(train), ])
pred_sarmax <- forecast(fit_sarmax, h = length(test),
                        xreg = Xreg[(length(train)+1):nrow(Xreg), ])

# Calcul du RMSE (Root Mean Squared Error)
rmse_sarma <- sqrt(mean((test - pred_sarma$mean)^2))
rmse_sarmax <- sqrt(mean((test - pred_sarmax$mean)^2))

rmse_sarma
rmse_sarmax




##VISUALISATION COMPAREE:
plot(test, type = "l", col = "black", lwd = 2,
     main = "Comparaison des prévisions : SARMA vs SARMAX",
     ylab = "Consommation (MWh)", xlab = "Mois")

lines(pred_sarma$mean, col = "blue", lwd = 2, lty = 2)
lines(pred_sarmax$mean, col = "red", lwd = 2, lty = 2)
legend("topleft", legend = c("Réalité", "SARMA", "SARMAX"),
       col = c("black", "blue", "red"), lty = c(1, 2, 2), lwd = 2)



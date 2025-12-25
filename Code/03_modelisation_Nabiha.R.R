---
title: "ECONOMETRIE - PREVISION"
author: "Awatif - Lara - Nabiha  - Vanilla"
date: '2025-12-24'
output: pdf_document
---


Packages
```{r}
packs <- c("forecast", "zoo", "ggplot2")
to_install <- packs[!packs %in% installed.packages()[,"Package"]]
if(length(to_install) > 0) install.packages(to_install)

library(forecast)
library(zoo)
library(ggplot2)
```

Telecharger le fichier
```{r}
df <- read.csv("conso_idf_mensuel_2016_2024_final_v2.csv", sep = ",", header = TRUE)
```

```{r}
pick_col <- function(df, candidates) {
  for (nm in candidates) if (nm %in% names(df)) return(nm)
  return(NA_character_)
}
```

```{r}
stop_if_missing <- function(cols) {
  missing <- cols[is.na(cols)]
  if (length(missing) > 0) {
    cat("\nColonnes disponibles dans le CSV:\n")
    print(names(df))
    stop("\nERREUR: Certaines colonnes attendues sont introuvables.\n",
         "Vérifie les noms de colonnes dans ton CSV et adapte les candidats.\n")
  }
}
```

les noms de colonnes :

- Date
```{r}
col_date <- pick_col(df, c("date_mensuelle", "date", "Date", "month", "Month"))
```

- Consommation (endogène)
```{r}
col_conso <- pick_col(df, c("conso_mensuelle", "consommation", "Consommation", "conso", "y"))
```

- Température
```{r}
col_temp <- pick_col(df, c("temp_moy", "temperature_moyenne", "temp", "temperature", "tmean"))
```

- Google Trends (chauffage / canicule / climatisation)
```{r}
col_chauff <- pick_col(df, c("trend_chauffage", "chauffage", "gt_chauffage", "google_chauffage"))
col_canic  <- pick_col(df, c("trend_canicule", "canicule", "gt_canicule", "google_canicule"))
col_clim   <- pick_col(df, c("trend_clim", "trend_climatisation", "climatisation", "gt_clim", "google_climatisation"))
```

- Dummy covid
```{r}
col_covid  <- pick_col(df, c("covid", "dummy_covid", "covid_dummy", "confinement"))
```

```{r}
stop_if_missing(c(col_date, col_conso, col_temp, col_chauff, col_canic, col_clim, col_covid))
```

```{r}
df <- df[, c(col_date, col_conso, col_temp, col_chauff, col_canic, col_clim, col_covid)]
names(df) <- c("date_mensuelle", "conso_mensuelle", "temp_moy",
               "trend_chauffage", "trend_canicule", "trend_clim", "covid")
```

Nettoyage des donnés
```{r}
df$date_mensuelle <- as.Date(df$date_mensuelle)
df <- df[order(df$date_mensuelle), ]
```

```{r}
num_cols <- c("conso_mensuelle","temp_moy","trend_chauffage","trend_canicule","trend_clim","covid")
for (cc in num_cols) df[[cc]] <- as.numeric(df[[cc]])
```

```{r}
df$temp_moy <- zoo::na.approx(df$temp_moy, na.rm = FALSE)
if(any(is.na(df$temp_moy))) df$temp_moy[is.na(df$temp_moy)] <- mean(df$temp_moy, na.rm = TRUE)
```

```{r}
n <- nrow(df)
if (n != 108) {
  warning("Attention: on attend 108 mois (2016-01 à 2024-12). Ici n = ", n,
          ". Vérifie la période ou adapte le split.")
}
```

```{r}
conso_ts <- ts(df$conso_mensuelle, start = c(2016, 1), frequency = 12)
```

```{r}
train_n <- 84
test_n  <- 24
if (n < (train_n + test_n)) stop("Pas assez d'observations pour faire train(84) + test(24).")

train_idx <- 1:train_n
test_idx  <- (train_n + 1):(train_n + test_n)

conso_train <- window(conso_ts, end = c(2022, 12))
conso_test  <- window(conso_ts, start = c(2023, 1))
```

MODELE 1 : SARMA(1,0)(1,1)[12]
```{r}
model_sarma <- Arima(
  conso_train,
  order = c(1,0,0),
  seasonal = list(order = c(1,0,1), period = 12),
  include.mean = TRUE
)
```
```{r}
summary(model_sarma)
```

```{r}
fc_sarma <- forecast(model_sarma, h = length(conso_test), level = 95)

obs_test  <- as.numeric(conso_test)
pred_sarma <- as.numeric(fc_sarma$mean)

err_sarma  <- obs_test - pred_sarma
RMSE_sarma <- sqrt(mean(err_sarma^2))
MAE_sarma  <- mean(abs(err_sarma))
MAPE_sarma <- mean(abs(err_sarma / obs_test)) * 100
```


```{r}
cat("RMSE =", round(RMSE_sarma, 3), "\n")
```
```{r}
cat("MAE  =", round(MAE_sarma, 3), "\n")
```
```{r}

cat("MAPE =", round(MAPE_sarma, 3), "%\n")
```



MODELE 2 : SARIMAX (exogènes)
```{r}
xreg_train <- as.matrix(df[train_idx, c("temp_moy","trend_chauffage","trend_canicule","trend_clim","covid")])
xreg_test  <- as.matrix(df[test_idx,  c("temp_moy","trend_chauffage","trend_canicule","trend_clim","covid")])

model_sarimax <- Arima(
  conso_train,
  order = c(1,0,0),
  seasonal = list(order = c(1,0,1), period = 12),
  xreg = xreg_train,
  include.mean = TRUE
)
```

```{r}
summary(model_sarimax)
```

```{r}

fc_sarimax <- forecast(model_sarimax, xreg = xreg_test, h = length(conso_test), level = 95)

pred_sarimax <- as.numeric(fc_sarimax$mean)
err_sarimax  <- obs_test - pred_sarimax
RMSE_sarimax <- sqrt(mean(err_sarimax^2))
MAE_sarimax  <- mean(abs(err_sarimax))
MAPE_sarimax <- mean(abs(err_sarimax / obs_test)) * 100
```

```{r}
cat("RMSE =", round(RMSE_sarimax, 3), "\n")
```
```{r}
cat("MAE  =", round(MAE_sarimax, 3), "\n")
```
```{r}
cat("MAPE =", round(MAPE_sarimax, 3), "%\n")
```

```{r}
gain_RMSE <- 100 * (RMSE_sarma - RMSE_sarimax) / RMSE_sarma
gain_MAE  <- 100 * (MAE_sarma  - MAE_sarimax)  / MAE_sarma
gain_MAPE <- 100 * (MAPE_sarma - MAPE_sarimax) / MAPE_sarma
```
```{r}
cat("Gain RMSE (%) =", round(gain_RMSE, 2), "\n")
```
```{r}
cat("Gain MAE  (%) =", round(gain_MAE,  2), "\n")
```
```{r}
cat("Gain MAPE (%) =", round(gain_MAPE, 2), "\n")
```

Tableau de résultats
```{r}
res <- data.frame(
  Modele = c("SARMA(1,0)(1,1)[12]", "SARIMAX(1,0)(1,1)[12] + (Temp + GT + Covid)"),
  RMSE = c(RMSE_sarma, RMSE_sarimax),
  MAE  = c(MAE_sarma,  MAE_sarimax),
  MAPE = c(MAPE_sarma, MAPE_sarimax)
)
print(res)
```

Graphiques Observé vs Prédit + IC 95%
```{r}
dates_test <- df$date_mensuelle[test_idx]

plot_df <- data.frame(
  date = dates_test,
  observe = obs_test,
  pred_sarma = pred_sarma,
  lo_sarma = as.numeric(fc_sarma$lower[,"95%"]),
  hi_sarma = as.numeric(fc_sarma$upper[,"95%"]),
  pred_sarimax = pred_sarimax,
  lo_sarimax = as.numeric(fc_sarimax$lower[,"95%"]),
  hi_sarimax = as.numeric(fc_sarimax$upper[,"95%"])
)

p_sarma <- ggplot(plot_df, aes(x = date)) +
  geom_line(aes(y = observe), linewidth = 0.8) +
  geom_line(aes(y = pred_sarma), linewidth = 0.8, linetype = "dashed") +
  geom_ribbon(aes(ymin = lo_sarma, ymax = hi_sarma), alpha = 0.2) +
  labs(title = "Prévisions hors-échantillon (SARMA) - 2023 à 2024",
       x = "Date", y = "Consommation mensuelle") +
  theme_minimal()

p_sarimax <- ggplot(plot_df, aes(x = date)) +
  geom_line(aes(y = observe), linewidth = 0.8) +
  geom_line(aes(y = pred_sarimax), linewidth = 0.8, linetype = "dashed") +
  geom_ribbon(aes(ymin = lo_sarimax, ymax = hi_sarimax), alpha = 0.2) +
  labs(title = "Prévisions hors-échantillon (SARIMAX) - 2023 à 2024",
       x = "Date", y = "Consommation mensuelle") +
  theme_minimal()
```
```{r}
print(p_sarma)
```

```{r}
print(p_sarimax)
```

Export fichiers
```{r}
write.csv(res, "resultats_performance_prevision.csv", row.names = FALSE)
ggsave("forecast_SARMA_2023_2024.png", p_sarma, width = 9, height = 5)
ggsave("forecast_SARIMAX_2023_2024.png", p_sarimax, width = 9, height = 5)

cat("\nFichiers créés :\n",
    "- resultats_performance_prevision.csv\n",
    "- forecast_SARMA_2023_2024.png\n",
    "- forecast_SARIMAX_2023_2024.png\n", sep = "")
```

Sauvegarde
```{r}
out_dir <- "/home/etudiant/Téléchargements"

ggsave(file.path(out_dir, "forecast_SARMA_2023_2024.png"), p_sarma, width = 9, height = 5)
ggsave(file.path(out_dir, "forecast_SARIMAX_2023_2024.png"), p_sarimax, width = 9, height = 5)

list.files(out_dir, pattern = "forecast_.*2023_2024\\.png")

print(p_sarma)
p_sarimax <- ggplot(plot_df, aes(x = date)) +
  geom_line(aes(y = observe), linewidth = 0.8) +
  geom_line(aes(y = pred_sarimax), linewidth = 0.8, linetype = "dashed") +
  geom_ribbon(aes(ymin = lo_sarimax, ymax = hi_sarimax), alpha = 0.2) +
  labs(title = "Prévisions hors-échantillon (SARIMAX) - 2023 à 2024",
       x = "Date", y = "Consommation mensuelle") +
  theme_minimal()

print(p_sarimax)

```


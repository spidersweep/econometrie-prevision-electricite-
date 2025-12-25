# Modélisation et Prévision de la Consommation d'Électricité en Île-de-France

## 📋 Projet Académique - Master 1 BIDABI (2024-2025)

Ce projet vise à **modéliser et prévoir la consommation mensuelle d'électricité** en Île-de-France sur la période 2016–2024 à l'aide de méthodes économétriques avancées (modèles SARIMAX).

---

## 🎯 Objectif général

Identifier et quantifier les **déterminants de la consommation d'électricité régionale** (température, saisonnalité, comportements de recherche en ligne) et développer un **modèle de prévision robuste** pour anticiper les pics de consommation, utile aux gestionnaires de réseau et aux décideurs énergétiques.

---

## 📊 Données

### Sources principales

- **RTE (Réseau de Transport d'Électricité)** : Consommation électrique mensuelle (données consolidées eCO2mix)
- **Météo-France** : Température mensuelle Paris-Montsouris (2016–2024)
- **Google Trends** : Indices de recherche mensuels pour les termes « chauffage », « canicule », « climatisation » (région Île-de-France)

### Période couverte
- **2016–2024** (108 observations mensuelles)
- Données consolidées et finalisées (pas de données temps réel)

### Variables principales
| Variable | Description | Unité |
|----------|-------------|-------|
| `conso_mensuelle` | Consommation d'électricité | MWh |
| `temp_moy` | Température moyenne mensuelle | °C |
| `trend_chauffage` | Indice Google Trends "chauffage" | 0–100 |
| `trend_canicule` | Indice Google Trends "canicule" | 0–100 |
| `trend_clim` | Indice Google Trends "climatisation" | 0–100 |
| `covid` | Dummy période de confinement | 0/1 |

---



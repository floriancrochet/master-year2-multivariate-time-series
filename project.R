    #----- 1. INITIALISER ET IMPORTER LES LIBRAIRIES -----

    #----- 1.1. Nettoyer l'environnement

rm(list = ls())
cat("\014") # Effacer la console



    #----- 1.2. Charger les librairies

library(quantmod) # Téléchargement données financières
library(xts) # Gestion séries temporelles
library(readxl) # Lecture Excel
library(writexl) # Ecriture Excel
library(zoo) # Gestion dates/times
library(tidyverse) # Manipulation de données
library(urca) # Tests de racine unitaire (Stationnarité)
library(tsDyn) # Modèles non-linéaires (TVAR)
library(vars) # VarSelect pour les lags
library(fBasics) # Statistiques descriptives
library(seastests) # Tests de saisonnalité (WO, QS, etc.)
library(tsoutliers) # Détection des outliers (tso)
library(tseries)
library(FinTS)





    #----- 2. IMPORTER LES DONNÉES -----

    #----- 2.1. Importer les données depuis yahoo finance (Bitcoin & Or)

# Importer le Bitcoin (BTC-USD)

data_btc_raw <- getSymbols("BTC-USD",
  from = "2014-10-01",
  to = "2025-12-31",
  src = "yahoo",
  auto.assign = FALSE
)


# Importer l'Or (Gold Futures GC=F)

data_gold_raw <- getSymbols("GC=F",
  from = "2014-10-01",
  to = "2025-12-31",
  src = "yahoo",
  auto.assign = FALSE
)



# Transformer en mensuel (Moyenne)

data_btc <- apply.monthly(data_btc_raw, mean, na.rm = TRUE)
data_gold <- apply.monthly(data_gold_raw, mean, na.rm = TRUE)



    #----- 2.2. Importer les données depuis FRED (Dollar Index & VIX) -----

# Importer le Dollar Index (DTWEXBGS - Nominal Broad U.S. Dollar Index)

data_dollar_index_raw <- getSymbols("DTWEXBGS",
  from = "2014-10-01",
  to = "2025-12-31",
  src = "FRED",
  auto.assign = FALSE
)


# Importer le VIX (VIXCLS - CBOE Volatility Index)

data_vix_raw <- getSymbols("VIXCLS",
  from = "2014-10-01",
  to = "2025-12-31",
  src = "FRED",
  auto.assign = FALSE
)


# Transformer en mensuel (Moyenne)

data_dollar_index <- apply.monthly(data_dollar_index_raw, mean, na.rm = TRUE)
data_vix <- apply.monthly(data_vix_raw, mean, na.rm = TRUE)



    #----- 2.3. Importer les données depuis les fichiers locaux (MSCI World) -----

# Importer MSCI World (Excel)

msci <- read_excel("data/msci.xlsx")


# Filtrer la période pour correspondre aux autres séries

msci <- msci[msci$dates >= "2014M10" & msci$dates <= "2025M12", ]





    #----- 3. PRÉPARER ET FUSIONNER LES DONNÉES -----

    #----- 3.1. Créer l'objet XTS principal -----

# Fusionner les données en un seul objet

data_sans_date <- cbind(
  coredata(data_btc$`BTC-USD.Close`),
  coredata(data_gold$`GC=F.Close`),
  coredata(msci$MSCI),
  coredata(data_vix$VIXCLS),
  coredata(data_dollar_index$DTWEXBGS)
)


# Renommer les colonnes

colnames(data_sans_date) <- c("btc_close", "gold_close", "msci_index", "vix_index", "dollar_index")



    #----- 3.2. Créer l'objet Time Series (TS) principal -----

# Convertir le dataframe en objet ts

data_ts <- ts(data_sans_date, start = c(2014, 10), frequency = 12)


# Vérifier

head(data_ts)
summary(data_ts)



    #----- 3.2. Sauvegarder dans un fichier Excel -----

## Inclusion de la colonne 'Date' uniquement pour l'export Excel

data_ts_df <- data.frame(date = as.Date(time(data_ts)), as.data.frame(data_ts))

write_xlsx(x = data_ts_df, path = "data/data.xlsx")





    #----- 4. TRANSFORMER LES DONNÉES (Création de Y) -----

    #----- 4.1. Clôner le ts -----

Y <- data_ts



    #----- 4.2. Transformer les séries -----

## Transformation Log-Rendements (sauf VIX) en remplaçant directement dans Y
## Utilisation de rbind(NA, ...) pour aligner les dimensions (perte d'1 obs via diff)

vars_transf <- colnames(Y) != "vix_index"

Y[, vars_transf] <- rbind(NA, diff(log(Y[, vars_transf])) * 100)

Y <- na.omit(Y) # Nettoyage final (suppression de la première ligne de NA)





    #----- 5. REPRÉSENTER LES SÉRIES (AVANT vs APRÈS TRANSFORMATION) -----

## Configuration de la fenêtre graphique

par(mfrow = c(1, 2))


## Boucle sur les variables du modèle (Y) pour avoir une correspondance parfaite

for (var_name in colnames(Y)) {
  ## 1. Série en Niveau (récupérer dans data_ts)
  ## Déjà en ts
  ts_niveau <- data_ts[, var_name]
  
  plot(ts_niveau,
       main = paste("Avant Transformation (Niveau) :", var_name),
       col = "blue", ylab = "Valeur Brute", xlab = "Temps"
  )
  grid()
  
  ## 2. Série Transformée (récupérer dans Y)
  ## VIX Transformé = VIX Niveau (-1 la 1ere obs). Pour les autres, log-rendements.
  ## 1 observation en moins
  plot(Y[, var_name],
       main = paste("Après Transformation (Modèle) :", var_name),
       col = "red", ylab = "Valeur Transformée", xlab = "Temps"
  )
  abline(h = 0, col = "gray", lty = 2)
  grid()
}


## Rétablissement de la fenêtre graphique standard

par(mfrow = c(1, 1))




    #----- 6. ANALYSER LA SAISONNALITÉ -----

## Utilisation du test combiné Webel-Ollech (WO) pour détecter la saisonnalité.
## Recommandé pour sa robustesse. Si une série est saisonnière, une désaisonnalisation
## (ex: X13, STL) serait requise avant de passer aux rendements, bien que le passage
## aux log-rendements élimine souvent la saisonnalité simple.

# Appliquer le test de saisonnalité (Webel-Ollech)

for (var_name in colnames(data_ts)) {
  # On teste la série en niveau
  serie_test <- na.omit(data_ts[, var_name])

  # Test WO
  is_seasonal <- seastests::isSeasonal(serie_test, test = "wo", freq = 12)

  # Affichage
  status <- ifelse(is_seasonal, "OUI (Saisonnier)", "NON (Pas de saisonnalité)")
  cat(sprintf("%-15s : %s\n", var_name, status))
}
# Note : Si 'dollar_index' est NON saisonnier, pas besoin de correction CVS.
# Les autres variables sont des variables financières qui n'ont pas de saisonnalité
# par nature (sinon les investisseurs pourraient faire de l'arbitrage)





    #----- 7. DÉTECTER LES POINTS ATYPIQUES (OUTLIERS) -----

## Utilisation de la fonction tso() du package tsoutliers (Chen and Liu, 1993)
## Cela permet d'identifier les types d'outliers :
## - AO : Additive Outlier (Ponctuel)
## - LS : Level Shift (Changement de niveau)
## - TC : Temporary Change (Changement temporaire)

# Créer une liste pour stocker les résultats

outliers_list <- list()


# Créer une boucle pour détecter les outliers sur chaque variable de Y

for (var_name in colnames(data_ts)) {
  cat(paste0("\n--- Analyse Outliers : ", toupper(var_name), " ---\n"))

  ## Série en niveau (fréquence mensuelle)
  y_ts <- na.omit(data_ts[, var_name])

  ## Détection automatique
  ## types = c("AO", "LS", "TC") sont détectés par défaut
  outlier_fit <- tsoutliers::tso(y_ts, types = c("AO", "LS", "TC"))

  ## Stockage
  outliers_list[[var_name]] <- outlier_fit

  ## Affichage des outliers trouvés
  if (nrow(outlier_fit$outliers) > 0) {
    print(outlier_fit$outliers)
  } else {
    cat("Aucun outlier détecté.\n")
  }

  ## Graphique (Série originale vs Série ajustée + Outliers)
  if (nrow(outlier_fit$outliers) > 0) {
    plot(outlier_fit)
    mtext(paste("Détection Outliers :", var_name), side = 3, line = -2, outer = FALSE)
  } else {
    ts.plot(y_ts, main = paste("Détection Outliers :", var_name), ylab = "Valeur")
    mtext("(Aucun outlier détecté)", side = 3, line = -1, cex = 0.8)
  }
}


# Afficher la liste

print(outliers_list)




    #----- 8. ANALYSER LA STATIONNARITÉ -----

# Réaliser une fonction pour étudier automatiquement la stationnarité

check_stationarity <- function(vec, type_input = c("niveau", "rendement"), name) {
  type_input <- match.arg(type_input)
  y <- na.omit(vec)

  ## Configuration des modèles
  if (type_input == "niveau") {
    ## Niveau : Tendance déterministe incluse
    lags_opt <- ar(diff(y), method = "mle")$order
    adf_type <- "trend"
    pp_model <- "trend"
    ers_model <- "trend"
    za_model <- "both"
  } else {
    ## Rendement : Constante uniquement
    lags_opt <- ar(y, method = "mle")$order
    adf_type <- "drift"
    pp_model <- "constant"
    ers_model <- "const"
    za_model <- "intercept"
  }

  ## Exécution des Tests
  adf_res <- ur.df(y, type = adf_type, lags = lags_opt)
  pp_res <- ur.pp(y, type = "Z-tau", model = pp_model, lags = "short")
  ers_res <- ur.ers(y, type = "DF-GLS", model = ers_model, lag.max = lags_opt)
  za_res <- ur.za(y, model = za_model, lag = lags_opt)

  ## Valeurs Critiques (Seuil 5%)
  vc_adf <- adf_res@cval[1, 2]
  vc_pp <- pp_res@cval[1, 2]
  vc_ers <- ers_res@cval[1, 2]
  vc_za <- za_res@cval[2]

  ## Affichage
  print_res <- function(name, stat, vc) {
    status <- ifelse(stat < vc, "STATIONNAIRE", "NON-STAT")
    cat(sprintf("%-15s: Stat = %6.3f | VC(5%%) = %6.3f -> %s\n", name, stat, vc, status))
  }

  cat(paste0("\n--- RÉSULTATS : ", toupper(type_input), " (Lags: ", lags_opt, ") ---\n"))
  print_res("ADF", adf_res@teststat[1], vc_adf)
  print_res("Phillips-Perron", pp_res@teststat, vc_pp)
  print_res("ERS (DF-GLS)", ers_res@teststat, vc_ers)
  print_res("Zivot-Andrews", za_res@teststat, vc_za)
  cat("----------------------------------------------------------\n")

  plot(za_res)
  mtext(paste("Zivot-Andrews :", name, "-", type_input), side = 3, line = -2, outer = FALSE)
}


# Exécuter sur le DataFrame

for (var_name in colnames(Y)) {
  if (toupper(var_name) %in% c("DATE", "TIME")) next

  cat(paste0("\n>>> Test de Stationnarité pour : ", var_name, " <<<\n"))
  check_stationarity(data_ts[, var_name], type_input = "niveau", name = var_name)
  check_stationarity(Y[, var_name], type_input = "rendement", name = var_name)
}





    #----- 9. ANALYSER L'AUTOCORRÉLATION -----

par(mfrow = c(1, 4))

for (var_name in colnames(Y)) {
  acf(as.numeric(Y[, var_name]), lag.max = 36, main = paste("ACF -", var_name))
  pacf(as.numeric(Y[, var_name]), lag.max = 36, main = paste("PACF -", var_name))
}

par(mfrow = c(1, 1))





    #----- 10. STATISTIQUES DESCRIPTIVES -----

# Sélectionner les variables numériques d'intérêt (hors Dates)

vars_stats <- Y


# Calculer les statistiques de base avec fBasics::basicStats

stats_res <- fBasics::basicStats(vars_stats)


# Afficher les statistiques

print(stats_res)





    #----- 11. MODÉLISER AVEC UN VAR LINEAIRE (BENCHMARK) -----

    #----- 11.1. Réorganiser les variables -----

## Réorganisation des variables pour l'identification structurelle du VAR
## Identification par décomposition de Cholesky (structure récursive)
##
## Principe :
## Les variables sont ordonnées selon leur degré d'exogénéité contemporaine.
## Une variable placée plus haut peut affecter instantanément les suivantes,
## mais ne réagit pas aux chocs des variables placées après elle sur la même période.
##
## Ordre retenu :
## Dollar Index → MSCI World → VIX → Or → Bitcoin
##
## Justification économique :
## 1. Dollar Index :
##    Variable macro-financière globale et numéraire de cotation des actifs.
##    Déterminée par des fondamentaux lents (politique monétaire, croissance),
##    elle est supposée exogène à l'horizon mensuel.
##
## 2. MSCI World :
##    Indicateur des marchés actions globaux.
##    Réagit aux conditions financières et au dollar,
##    mais n'influence pas contemporanément le Dollar sur la même période.
##
## 3. VIX :
##    Mesure de l'incertitude et de l'aversion au risque.
##    Réagit aux mouvements du MSCI et du Dollar,
##    influence ensuite les décisions d'allocation vers les actifs refuges.
##
## 4. Or :
##    Actif refuge traditionnel, marché profond et mature.
##    Réagit aux chocs macro-financiers, actions et incertitude.
##
## 5. Bitcoin :
##    Actif le plus spéculatif et le plus réactif du système.
##    Réagit instantanément à l'ensemble des chocs,
##    sans influencer les variables macro-financières sur la même période.



## Base pour un VAR (n1) joint (BTC + Or) pour robustesse / comparaison

Y_org <- Y[, c(
  "dollar_index",
  "msci_index",
  "vix_index",
  "gold_close",
  "btc_close"
)]


# ## Base pour un VAR (n2) focalisé sur le Bitcoin
# 
# Y_org_btc <- Y[, c(
#   "dollar_index",
#   "msci_index",
#   "vix_index",
#   "btc_close"
# )]
# 
# 
# ## Base pour un VAR (n3) focalisé sur l'Or (benchmark refuge)
# 
# Y_org_gold <- Y[, c(
#   "dollar_index",
#   "msci_index",
#   "vix_index",
#   "gold_close"
# )]



    #----- 11.2. Évaluer le VAR joint (1) -----

    #----- 11.2.1. Sélectionner le nombre de retards (Lags) -----

## Nécessaire pour le test de linéarité ET pour le modèle TVAR

lag_info_1 <- VARselect(Y_org, lag.max = 8, type = "const")
p_lag_1 <- lag_info_1$selection["AIC(n)"] # 1 pour tous
cat("\nNombre de retards optimal (AIC) :", p_lag_1, "\n")



    #----- 11.2.3. Modéliser avec le VAR Linéaire -----

## Modèle VAR Linéaire (1 seul régime)
## Sert de point de comparaison pour les tests de linéarité
## On prend 2 lags pour être sûr d'avoir une bonne spécification 
## (AIC recommande 1, mais 2 valide +)

var_linear_1 <- vars::VAR(Y_org, p = 2, type = "const")
summary(var_linear_1)



    #----- 11.2.4. Valider le modèle VAR Linéaire -----

## Test autocorrélation (Portmanteau / Ljung-Box)
serial.test(var_linear_1, lags.pt = 12, type = "PT.asymptotic")

## Test normalité des résidus
normality.test(var_linear_1)

## Test hétéroscédasticité (ARCH)
arch.test(var_linear_1, lags.multi = 5, multivariate.only = TRUE)



#     #----- 11.3. Évaluer le VAR Bitcoin (2) -----
# 
#     #----- 11.3.1. Sélectionner le nombre de retards (Lags) -----
# 
# lag_info_2 <- VARselect(Y_org_btc, lag.max = 8, type = "const")
# p_lag_2 <- lag_info_2$selection["AIC(n)"] # 1 pour tous
# cat("\nNombre de retards optimal (AIC) :", p_lag_2, "\n")
# 
# 
# 
#     #----- 11.3.3. Modéliser avec le VAR Linéaire -----
# 
# var_linear_2 <- vars::VAR(Y_org_btc, p = p_lag_2, type = "const")
# summary(var_linear_2)
# 
# 
# 
#     #----- 11.3.4. Valider le modèle VAR Linéaire -----
# 
# ## Test autocorrélation (Portmanteau / Ljung-Box)
# serial.test(var_linear_2, lags.pt = 12, type = "PT.asymptotic")
# 
# ## Test normalité des résidus
# normality.test(var_linear_2)
# 
# ## Test hétéroscédasticité (ARCH)
# arch.test(var_linear_2, lags.multi = 5, multivariate.only = TRUE)
# 
# 
# 
#     #----- 11.3. Évaluer le VAR Bitcoin (3) -----
# 
#     #----- 11.3.1. Sélectionner le nombre de retards (Lags) -----
# 
# lag_info_3 <- VARselect(Y_org_gold, lag.max = 8, type = "const")
# p_lag_3 <- lag_info_3$selection["AIC(n)"] # 1 pour tous
# cat("\nNombre de retards optimal (AIC) :", p_lag_3, "\n")
# 
# 
# 
#     #----- 11.3.3. Modéliser avec le VAR Linéaire -----
# 
# var_linear_3 <- vars::VAR(Y_org_gold, p = p_lag_3, type = "const")
# summary(var_linear_3)
# 
# 
# 
#     #----- 11.3.4. Valider le modèle VAR Linéaire -----
# 
# ## Test autocorrélation (Portmanteau / Ljung-Box)
# serial.test(var_linear_3, lags.pt = 12, type = "PT.asymptotic")
# 
# ## Test normalité des résidus
# normality.test(var_linear_3)
# 
# ## Test hétéroscédasticité (ARCH)
# arch.test(var_linear_3, lags.multi = 5, multivariate.only = TRUE)





    #----- 12. MODÉLISER AVEC UN TVAR (APPROCHE COMPARATIVE) -----

## Estimation de deux versions du modèle TVAR pour tester la sensibilité au seuil.

    #----- 12.1. Identifier la variable de seuil (VIX) -----

idx_vix <- which(colnames(Y_org) == "vix_index")



    #----- 12.2. Modéliser avec le Modèle A : Seuil Économique Fixé (VIX = 20) -----

## Estimation Modèle A : Seuil Fixe (VIX = 20)
## Dans tsDyn::TVAR(), l'argument pour fixer le seuil est 'gamma' (et non 'thValue')
## On prend 2 lags

tvar_fixed <- TVAR(Y_org,
  lag = 2, nthresh = 1, model = "TAR",
  mTh = idx_vix, thDelay = 1, gamma = 20, plot = FALSE
)


    #----- 12.3. Modéliser avec le Modèle B : Seuil Estimé (Endogène) -----

## Modèle B : Seuil Estimé (Endogène)

tvar_estimated <- TVAR(Y_org,
  lag = 2, nthresh = 1, model = "TAR",
  mTh = idx_vix, thDelay = 1, trim = 0.15, plot = FALSE
)

# Passer le lag à 2 est meilleur



    #----- 12.4. Récapituler les modèles

## Résumé Modèle A (Fixe)

print(tvar_fixed)


## Résumé Modèle B (Estimé)

print(tvar_estimated)



    #----- 12.5. Valider les modèles TVAR -----

# Créer une fonction de diagnostics des résidus

diagnostics_TVAR <- function(tvar_model, Y_org, lb_lag = 12, arch_lag = 12, model_name = "") {
  cat("\n====================================\n")
  cat("Diagnostics des résidus :", model_name, "\n")
  cat("====================================\n\n")

  # Résidus
  resid_all <- residuals(tvar_model)

  # Noms des variables
  vars_names <- colnames(Y_org)

  # --- 1. Autocorrélation (Ljung-Box) ---
  cat("----- Autocorrélation (Ljung-Box) -----\n")
  for (i in 1:ncol(resid_all)) {
    cat("\nVariable :", vars_names[i], "\n")
    print(Box.test(resid_all[, i], lag = lb_lag, type = "Ljung-Box"))
  }

  # --- 2. Normalité (Jarque-Bera) ---
  cat("\n----- Normalité (Jarque-Bera) -----\n")
  for (i in 1:ncol(resid_all)) {
    cat("\nVariable :", vars_names[i], "\n")
    print(jarque.bera.test(resid_all[, i]))
  }

  # --- 3. Hétéroscédasticité (ARCH LM) ---
  cat("\n----- Hétéroscédasticité (ARCH LM) -----\n")
  for (i in 1:ncol(resid_all)) {
    cat("\nVariable :", vars_names[i], "\n")
    print(ArchTest(resid_all[, i], lags = arch_lag))
  }

  cat("\n====================================\n")
  cat("Fin diagnostics :", model_name, "\n")
  cat("====================================\n\n")
}


# Faire le diagnostic

# Diagnostics TVAR à seuil fixe
diagnostics_TVAR(
  tvar_model = tvar_fixed,
  Y_org = Y_org,
  lb_lag = 12,
  arch_lag = 12,
  model_name = "TVAR – Seuil fixe (VIX = 20)"
)

# Diagnostics TVAR à seuil estimé
diagnostics_TVAR(
  tvar_model = tvar_estimated,
  Y_org = Y_org,
  lb_lag = 12,
  arch_lag = 12,
  model_name = "TVAR – Seuil estimé"
)





    #----- 13. TESTER LA LINÉARITÉ ET SÉLECTIONNER LE MODÈLE -----

# Résultats du test de linéarité (Log-Likelihood Ratio)

# Extraction des Log-Vraisemblances
ll_linear <- logLik(var_linear_1)
# On compare le VAR linéaire au TVAR estimé (qui est le 'vrai' modèle alternatif maximisé)
ll_tvar_est <- logLik(tvar_estimated)

# Calcul du Ratio de Vraisemblance (LR)
lr_stat <- 2 * (as.numeric(ll_tvar_est) - as.numeric(ll_linear))
# Note: La distribution asymptotique n'est pas un Chi2 standard sous H0 à cause du paramètre de seuil non identifié.
# Cependant, c'est un indicateur utile.

# Comparaison AIC/HQ/BIC des 3 modèles
aic_linear <- AIC(var_linear_1)
bic_linear <- BIC(var_linear_1)
# On récupère le nombre d'observations effectives (T)
obs_linear <- nrow(residuals(var_linear_1))
hq_linear <- -2 * as.numeric(ll_linear) + 2 * log(log(obs_linear)) * attr(ll_linear, "df")

aic_fixed <- AIC(tvar_fixed)
bic_fixed <- BIC(tvar_fixed)
hq_fixed <- -2 * as.numeric(logLik(tvar_fixed)) + 2 * log(log(tvar_fixed$nobs)) * attr(logLik(tvar_fixed), "df")

aic_est <- AIC(tvar_estimated)
bic_est <- BIC(tvar_estimated)
hq_est <- -2 * as.numeric(logLik(tvar_estimated)) + 2 * log(log(tvar_estimated$nobs)) * attr(logLik(tvar_estimated), "df")

cat(sprintf("Log-Likelihood VAR linéaire      : %.4f\n", ll_linear))
cat(sprintf("Log-Likelihood TVAR (Estimé)     : %.4f\n", ll_tvar_est))
cat(sprintf("Statistique LR (2*(LL_TVAR - LL_VAR)) : %.4f\n", lr_stat))

# Comparaison des critères d'information
cat(sprintf("%-25s | %-10s | %-10s | %-10s | %-15s\n", "Modèle", "AIC", "BIC", "HQ", "Seuil"))
cat(sprintf("%-25s | %-10.4f | %-10.4f | %-10.4f | %-15s\n", "VAR Linéaire", aic_linear, bic_linear, hq_linear, "NA"))
cat(sprintf("%-25s | %-10.4f | %-10.4f | %-10.4f | %-15.4f\n", "TVAR A (Fixe VIX=20)", aic_fixed, bic_fixed, hq_fixed, 20))
cat(sprintf("%-25s | %-10.4f | %-10.4f | %-10.4f | %-15.4f\n", "TVAR B (Estimé)", aic_est, bic_est, hq_est, tvar_estimated$model.specific$thresh))

# Sélection automatique du meilleur modèle selon AIC (entre les deux TVAR)
# (Utilisé pour la suite du script, sans affichage de conclusion)
tvar_final <- NULL
model_name <- ""

if (aic_est < aic_fixed) {
  tvar_final <- tvar_estimated
  model_name <- "TVAR B (Seuil Estimé)"
} else {
  tvar_final <- tvar_fixed
  model_name <- "TVAR A (Seuil Fixe)"
}

# Le script continue avec 'tvar_final' pour les IRF






    #----- 14. ANALYSER L'IMPULSION-RÉPONSE (IRF) -----

    #-----  14.1. Visualiser la Chronologie des Régimes

# Récupérer la série des régimes

regime_series <- tsDyn::regime(tvar_final)


# Reconstruire l'axe temporel pour la série des régimesn

## Alignement temporel : Le modèle TVAR perd 2 observations initiales.
## (cette perte est due aux lags utilisés dans le modèle TVAR).
## => Reconstruction de l'axe temporel en ne gardant que les dates finales correspondantes.

all_dates <- as.Date(time(data_ts))
model_dates <- tail(all_dates, length(regime_series))


# Visualiser la chronologie des régimes (Step Chart)

plot(model_dates, regime_series,
  type = "s", # Trace en escalier pour marquer les changements instantanés
  col = "darkblue",
  lwd = 2,
  main = paste0("Chronologie des Régimes : ", model_name),
  ylab = "Régime (1 = Calme, 2 = Stress)",
  xlab = "Temps",
  xaxt = "n" # Suppression axe X par défaut pour personnalisation
)

## Ajout axe temporel annuel précis
axis.Date(1, at = seq(min(model_dates), max(model_dates), by = "year"), format = "%Y")

## Ajout des grilles
grid()

## Ajout des rug plots pour marquer les périodes de Stress (Régime 2)
stress_dates <- model_dates[regime_series == 2]
if (length(stress_dates) > 0) {
  rug(stress_dates, col = "red", ticksize = 0.03, side = 1)
}



    #----- 14.2. Estimer les IRF par Régime (Bootstrap) -----

# Mettre en place une fonction

plot_tvar_irf <- function(model, shock_var, n_boot = 500, conf_level = 0.90) {
  
  ## 1. Estimation des IRF pour les deux régimes
  ## On choque 'shock_var' et on regarde BTC et GOLD
  irf_L <- tsDyn::irf(model, impulse = shock_var, response = c("btc_close", "gold_close"),
                      n.ahead = 12, ortho = TRUE, boot = TRUE, 
                      runs = n_boot, conf = conf_level, regime = "L")
  
  irf_H <- tsDyn::irf(model, impulse = shock_var, response = c("btc_close", "gold_close"),
                      n.ahead = 12, ortho = TRUE, boot = TRUE, 
                      runs = n_boot, conf = conf_level, regime = "H")
  
  ## 2. Configuration de la zone graphique (2x2)
  ## Organisation : Gauche = Calme, Droite = Stress
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  h <- 0:12
  
  ## 3. Configuration du plot 
  ## Mise en fonction pour éviter la répétition
  legende_text <- paste0("IC: ", conf_level * 100, "% (N=", n_boot, ")")
  
  draw_irf <- function(irf_obj, resp_var, title, color) {
    mean_val <- irf_obj$irf[[shock_var]][, resp_var]
    low_val  <- irf_obj$Lower[[shock_var]][, resp_var]
    upp_val  <- irf_obj$Upper[[shock_var]][, resp_var]
    
    plot(h, mean_val, type = "l", col = color, lwd = 2, 
         ylim = range(c(low_val, upp_val)),
         main = title, xlab = "Mois", ylab = "Réponse (%)")
    lines(h, low_val, col = color, lty = 3)
    lines(h, upp_val, col = color, lty = 3)
    abline(h = 0, col = "black", lwd = 1)
    
    legend("topright", legend = legende_text, bty = "n", cex = 0.8, text.col = "gray30")
    grid()
  }
  
  ## 4. Génération des 4 graphiques
  ## Ligne 1 : Bitcoin
  draw_irf(irf_L, "btc_close", paste("CALME : Réponse du BITCOIN au choc", shock_var), "blue")
  draw_irf(irf_H, "btc_close", paste("STRESS : Réponse du BITCOIN au choc", shock_var), "red")
  
  ## Ligne 2 : Or
  draw_irf(irf_L, "gold_close", paste("CALME : Réponse de l'OR au choc", shock_var), "blue")
  draw_irf(irf_H, "gold_close", paste("STRESS : Réponse de l'OR au choc", shock_var), "red")
  
  ## Remise à zéro des paramètres graphiques
  par(mfrow = c(1, 1))
  
  ## Retourne les objets IRF pour les utiliser en FEVD
  return(list(low = irf_L, high = irf_H))
}


# Utiliser la fonction

## À 90%

res_dollar_90 <- plot_tvar_irf(tvar_final, "dollar_index", n_boot = 500, conf_level = 0.90)

res_msci_90 <- plot_tvar_irf(tvar_final, "msci_index", n_boot = 500, conf_level = 0.90)

res_vix_90 <- plot_tvar_irf(tvar_final, "vix_index", n_boot = 500, conf_level = 0.90)

res_gold_90 <- plot_tvar_irf(tvar_final, "gold_close", n_boot = 500, conf_level = 0.90)

res_btc_90 <- plot_tvar_irf(tvar_final, "btc_close", n_boot = 500, conf_level = 0.90)


## À 95%

res_dollar_95 <- plot_tvar_irf(tvar_final, "dollar_index", n_boot = 500, conf_level = 0.95)

res_msci_95 <- plot_tvar_irf(tvar_final, "msci_index", n_boot = 500, conf_level = 0.95)

res_vix_95 <- plot_tvar_irf(tvar_final, "vix_index", n_boot = 500, conf_level = 0.95)

res_gold_95 <- plot_tvar_irf(tvar_final, "gold_close", n_boot = 500, conf_level = 0.95)

res_btc_95 <- plot_tvar_irf(tvar_final, "btc_close", n_boot = 500, conf_level = 0.95)





    #----- 15. DÉCOMPOSITION DE LA VARIANCE DES ERREURS DE PRÉVISION (FEVD) -----

    #----- 15.1. Fonction de calcul de la FEVD -----

## Cette fonction calcule la part de variance expliquée par chaque choc

calcul_fevd_from_irf <- function(irf_obj) {
  ## Extraction des matrices de réponses (irf_obj contient tous les chocs)
  irf_data <- irf_obj$irf
  
  ## 1. Calcul du carré des réponses (contributions unitaires)
  irf_sq <- lapply(irf_data, function(x) x^2)
  
  ## 2. Somme cumulée des carrés sur l'horizon h
  irf_cum <- lapply(irf_sq, apply, 2, cumsum)
  
  ## 3. Calcul de la variance totale du système à chaque horizon
  tot_var <- Reduce("+", irf_cum)
  
  ## 4. Normalisation pour obtenir les parts en % (somme = 100%)
  fevd_list <- lapply(irf_cum, function(x) x / tot_var)
  
  return(fevd_list)
}


    #----- 15.2. Estimation des IRF Complètes (Sans Bootstrap) -----

## On calcule les réponses de TOUTES les variables à TOUS les chocs simultanément.
## L'absence de bootstrap (boot=FALSE) permet un calcul instantané et exhaustif.

irf_full_L <- tsDyn::irf(tvar_final, n.ahead = 12, ortho = TRUE, boot = FALSE, regime = "L")
irf_full_H <- tsDyn::irf(tvar_final, n.ahead = 12, ortho = TRUE, boot = FALSE, regime = "H")


## Application de la fonction FEVD sur les deux régimes

fevd_calme  <- calcul_fevd_from_irf(irf_full_L)
fevd_stress <- calcul_fevd_from_irf(irf_full_H)



    #----- 15.3. Analyser la variance du Bitcoin -----

## Objectif : Identifier "Qui explique la volatilité du Bitcoin ?" (Autonomie vs Contagion)

target_name <- "btc_close"

h_analyse <- 12 # Horizon d'analyse (ex: 12 mois)



    #----- 15.3.1. Extraire les Tables Complètes (Horizon 1 à 12) -----

## Cette fonction crée une table : Lignes = Horizon, Colonnes = Source du Choc

get_full_fevd_table <- function(fevd_res, target) {
  ## Pour chaque source de choc, on extrait la colonne correspondant à la cible
  mat_fevd <- sapply(fevd_res, function(choc_mat) choc_mat[, target])

  ## Renomme les lignes pour correspondre à l'horizon (0 à n)
  rownames(mat_fevd) <- 0:(nrow(mat_fevd) - 1)

  return(as.data.frame(mat_fevd))
}


## Calcul des tables complètes (en %)

table_calme <- get_full_fevd_table(fevd_calme, target_name) * 100
table_stress <- get_full_fevd_table(fevd_stress, target_name) * 100



    #----- 15.3.2. Afficher les résultats complets -----

## Décomposition FEVD : BITCOIN [Régime CALME]

print(round(table_calme, 2))


## Décomposition FEVD : BITCOIN [Régime STRESS]

print(round(table_stress, 2))



    #----- 15.3.3. Préparer pour la Visualisation -----

## On extrait juste la dernière ligne (horizon 12) pour le graphique comparatif

vec_calme_h <- as.numeric(table_calme[as.character(h_analyse), ])
vec_stress_h <- as.numeric(table_stress[as.character(h_analyse), ])


## Tableau comparatif pour le graphique

comparaison <- rbind(Calme = vec_calme_h, Stress = vec_stress_h)
colnames(comparaison) <- colnames(table_calme) # Remettre les noms des variables


## Résumé Comparatif à h = 12 mois

print(round(comparaison, 2))



    #----- 15.4. Analyse de la Variance de l'Or -----

target_name_gold <- "gold_close"


    #----- 15.4.1. Extraire les Tables Complètes (Horizon 1 à 12) -----

table_calme_gold <- get_full_fevd_table(fevd_calme, target_name_gold) * 100
table_stress_gold <- get_full_fevd_table(fevd_stress, target_name_gold) * 100



    #----- 15.4.2. Afficher les résultats complets -----

## Décomposition FEVD : OR [Régime CALME]

print(round(table_calme_gold, 2))

## Décomposition FEVD : OR [Régime STRESS]

print(round(table_stress_gold, 2))



    #----- 15.3.3. Préparer pour la Visualisation -----

## Extraction

vec_calme_gold_h <- as.numeric(table_calme_gold[as.character(h_analyse), ])
vec_stress_gold_h <- as.numeric(table_stress_gold[as.character(h_analyse), ])


## Tableau comparatif

comparaison_gold <- rbind(Calme = vec_calme_gold_h, Stress = vec_stress_gold_h)
colnames(comparaison_gold) <- colnames(table_calme_gold)


## Résumé Comparatif OR à h = 12 mois

print(round(comparaison_gold, 2))



    #----- 15.3.4. Analyser la trajectoire FEVD (MSCI vs VIX) -----

## Définition d'une palette de couleurs sémantique et professionnelle (Flat UI)

colors_fevd <- c(
  "dollar_index" = "#27ae60",  # Vert émeraude (Plus stable visuellement)
  "msci_index"   = "#2c3e50", # Bleu nuit (Institutionnel / Marchés)
  "vix_index"    = "#c0392b", # Rouge brique (Risque, moins agressif que le rouge pur)
  "gold_close"   = "#d4af37", # Or métallique (Plus réaliste que le jaune vif)
  "btc_close"    = "#e67e22" # Orange Bitcoin (On garde celui-là, c'est l'identité visuelle du BTC)
)


## Définition de l'horizon

horizons <- as.numeric(rownames(table_calme))


    #----- 15.3.4.1 Analyser pour le BitCoin -----

## Régime Calme

plot(horizons, table_calme$dollar_index, type = "l", lwd = 2, col = colors_fevd["dollar_index"],
     ylim = c(0, 100), xlab = "Horizon (Mois)", ylab = "Variance expliquée (%)",
     main = "Décomposition de la Variance du Bitcoin (Régime Calme)")

lines(horizons, table_calme$msci_index, lwd = 2, col = colors_fevd["msci_index"], lty = 4)
lines(horizons, table_calme$vix_index, lwd = 2, col = colors_fevd["vix_index"], lty = 2)
lines(horizons, table_calme$btc_close, lwd = 2, col = "gray40", lty = 3) # Gris pour le choc propre
abline(v = 12, col = "gray80", lty = 3)

legend("right", legend = c("DOLLAR", "MSCI", "VIX", "Propre BTC"),
       col = c(colors_fevd["dollar_index"], colors_fevd["msci_index"], colors_fevd["vix_index"], "gray40"), 
       lwd = 2, lty = c(1, 4, 2, 3), bty = "n")


## Régime Stress

plot(horizons, table_stress$dollar_index, type = "l", lwd = 2, col = colors_fevd["dollar_index"],
     ylim = c(0, 100), xlab = "Horizon (Mois)", ylab = "Variance expliquée (%)",
     main = "Décomposition de la Variance du Bitcoin (Régime Stress)")

lines(horizons, table_stress$msci_index, lwd = 2, col = colors_fevd["msci_index"], lty = 4)
lines(horizons, table_stress$vix_index, lwd = 2, col = colors_fevd["vix_index"], lty = 2)
lines(horizons, table_stress$btc_close, lwd = 2, col = "gray40", lty = 3)
abline(v = 12, col = "gray80", lty = 3)

legend("right", legend = c("DOLLAR", "MSCI", "VIX", "Propre BTC"),
       col = c(colors_fevd["dollar_index"], colors_fevd["msci_index"], colors_fevd["vix_index"], "gray40"), 
       lwd = 2, lty = c(1, 4, 2, 3), bty = "n")



    #----- 15.3.4.2 Analyser pour l'Or -----

## Régime Calme

plot(horizons, table_calme_gold$dollar_index, type = "l", lwd = 2, col = colors_fevd["dollar_index"],
     ylim = c(0, 100), xlab = "Horizon (Mois)", ylab = "Variance expliquée (%)",
     main = "Décomposition de la Variance de l'Or (Régime Calme)")

lines(horizons, table_calme_gold$msci_index, lwd = 2, col = colors_fevd["msci_index"], lty = 4)
lines(horizons, table_calme_gold$vix_index, lwd = 2, col = colors_fevd["vix_index"], lty = 2)
lines(horizons, table_calme_gold$gold_close, lwd = 2, col = "gray40", lty = 3) # Gris pour le choc propre
abline(v = 12, col = "gray80", lty = 3)

legend("right", legend = c("DOLLAR", "MSCI", "VIX", "Propre GOLD"),
       col = c(colors_fevd["dollar_index"], colors_fevd["msci_index"], colors_fevd["vix_index"], "gray40"), 
       lwd = 2, lty = c(1, 4, 2, 3), bty = "n")


## Régime Stress

plot(horizons, table_stress_gold$dollar_index, type = "l", lwd = 2, col = colors_fevd["dollar_index"],
     ylim = c(0, 100), xlab = "Horizon (Mois)", ylab = "Variance expliquée (%)",
     main = "Décomposition de la Variance de l'Or (Régime Stress)")

lines(horizons, table_stress_gold$msci_index, lwd = 2, col = colors_fevd["msci_index"], lty = 4)
lines(horizons, table_stress_gold$vix_index, lwd = 2, col = colors_fevd["vix_index"], lty = 2)
lines(horizons, table_stress_gold$gold_close, lwd = 2, col = "gray40", lty = 3)
abline(v = 12, col = "gray80", lty = 3)

legend("right", legend = c("DOLLAR", "MSCI", "VIX", "Propre GOLD"),
       col = c(colors_fevd["dollar_index"], colors_fevd["msci_index"], colors_fevd["vix_index"], "gray40"), 
       lwd = 2, lty = c(1, 4, 2, 3), bty = "n")


    #----- 15.5. Visualiser Graphiquement (Bitcoin & Or) -----

## Configuration de la zone graphique pour les deux graphiques côte à côte

par(mfrow = c(1, 1), mar = c(5, 5, 4, 10), xpd = TRUE)


    #----- 15.5.1. Afficher les graphiques pour le BITCOIN -----

plot_data_btc <- t(comparaison)

barplot(
  plot_data_btc,
  beside = FALSE,
  col = colors_fevd[rownames(plot_data_btc)],
  main = paste("FEVD BITCOIN (h =", h_analyse, ")"),
  ylab = "Variance expliquée (%)",
  xlab = "Régime de marché",
  border = "white",
  las = 1,
  space = 0.1,
  xlim = c(0, 3)
)

legend(
  "right",
  inset = c(-0.25, 0),
  legend = c("Dollar", "MSCI World", "VIX", "Or", "Bitcoin"), 
  fill = colors_fevd, 
  title = "Source du Choc", bty = "n", cex = 1
)



    #----- 15.5.2. Afficher les graphiques pour l'OR -----

plot_data_gold <- t(comparaison_gold)

barplot(
  plot_data_gold,
  beside = FALSE,
  col = colors_fevd[rownames(plot_data_gold)],
  main = paste("FEVD OR (h =", h_analyse, ")"),
  ylab = "Variance expliquée (%)",
  xlab = "Régime de marché",
  border = "white",
  las = 1,
  space = 0.1,
  xlim = c(0, 3)
)

legend(
  "right",
  inset = c(-0.25, 0),
  legend = c("Dollar", "MSCI World", "VIX", "Or", "Bitcoin"),
  fill = colors_fevd, 
  title = "Source du Choc", bty = "n", cex = 1
)


## Restauration des paramètres par défaut

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, xpd = FALSE)



#### Fin du script

### Chargement des bibliothèques nécessaires
library(data.table)
library(lubridate)
library(dplyr)
library(kableExtra)
library(ggplot2)
# install.packages("data.table")
# install.packages("lubridate")
# install.packages("dplyr")
# install.packages("kableExtra")
# install.packages("ggplot2")

rm(list=ls())
debut <- proc.time()

##########################################################################################################################
# Importation
##########################################################################################################################

wd <- getwd()
setwd(wd)

wd_data <- paste(wd,"Données",sep = "/")
# wd_data <- "C:/Users/maxva/Personnel/Documents/Séminaire FQ"

Brute <- fread(file.path(wd_data, "Brute.csv"))
SP500 <- fread(file.path(wd_data, "S&P500.csv"))
T_bills <- fread(file.path(wd_data, "T30.csv"))
FF_Size_BtM <- fread(file.path(wd_data, "Size-BtM.csv"))
FF_Size_Mom <- fread(file.path(wd_data, "Size-Mom.csv"))
FF_Size_BtM_Prof <- fread(file.path(wd_data, "Size-BtM-Profitability.csv"))
FF_Size_BtM_Invest <- fread(file.path(wd_data, "Size-BtM-Investments.csv"))


##########################################################################################################################
# Nettoyer et filtrer les données de l'importation
# 
# Brute = Importation des données mensuelles des actions américaines de 1925 à aujourd'hui de CRSP
# Quaterly = Seulement les données à chaque trimestre
# Mensuel = Données mensuelles de Brute avec toutes les données non essentielles effacées et filtrées
# Top1000 = Univers des 1000 plus grandes capitalisations boursières à chaque trimestre
##########################################################################################################################

### Suppression de la colonne PERMNO
Brute <- subset(Brute, select = -PERMNO)

### Suppression des lignes manquant trop de données
Brute <- Brute[!(is.na(Brute$PRC) | is.na(Brute$RET) | Brute$COMNAM == "" | is.na(Brute$SHROUT)), ]

### Suppression des lignes qui n'ont pas de valeurs numériques comme RET (rendement)
Brute[, RET := as.numeric(RET)]
Brute <- Brute[!is.na(Brute$RET), ]

### Suppression des doublons
Brute <- unique(Brute, by = c("date", "COMNAM"))

### Formatage des colonnes
Brute[, date := as.Date(date, format = '%d/%m/%Y')]
Brute[, COMNAM := as.character(COMNAM)]
Brute[, DIVAMT := as.numeric(DIVAMT)]
Brute$DIVAMT[is.na(Brute$DIVAMT)] <- 0
Brute[, PRC := as.numeric(PRC)]
Brute$PRC <- abs(Brute$PRC)
Brute[, SHROUT := as.numeric(SHROUT)]

### Ajustement si BID et ASK sont NA
spread <- 0.0025
Brute$BID[is.na(Brute$BID)] <- Brute$PRC[is.na(Brute$BID)] * (1-(spread/2))
Brute$ASK[is.na(Brute$ASK)] <- Brute$PRC[is.na(Brute$ASK)] * (1+(spread/2))

### Calcul de MarketCap (capitalisation boursière)
Brute[, MarketCap := round(abs(PRC * SHROUT), 2)]

### Création des variables Quaterly et Mensuel
Quarterly <- Brute[, .(date, COMNAM, MarketCap, PRC, BID, ASK, RET)]
Mensuel <- Brute[, .(date, COMNAM, RET, DIVAMT, PRC, SHROUT, MarketCap, BID)]

# Suppression des lignes qui ont moins de 3 ans de données mensuelles
firstdata <- aggregate(date ~ COMNAM, Mensuel, FUN = function(x) min(x))
Quarterly <- merge(Quarterly, firstdata, by = "COMNAM")

names(Quarterly)[which(names(Quarterly) == "date.x")] <- "date"
names(Quarterly)[which(names(Quarterly) == "date.y")] <- "FirstData"

Quarterly$FirstData <- Quarterly$FirstData + years(3) - days(10)
Quarterly <- Quarterly[Quarterly$date >= Quarterly$FirstData]
Quarterly <- subset(Quarterly, select = -FirstData)

### Filtrage pour les données trimestrielles
Quarterly <- Quarterly[month(date) %in% c(3, 6, 9, 12)]

### Suppression des lignes avec NA MarketCap
Quarterly <- Quarterly[complete.cases(MarketCap)]

### Trouver 1000 plus grandes capitalisations boursières pour chaque trimestre
Quarterly[, rank := frank(-MarketCap, ties.method = "first"), by = date]
Top1000 <- Quarterly[rank <= 1000]
Top1000 <- subset(Top1000, select = -rank)

### Filtrer Mensuel avec seulement les titres qui sont dans Top1000
# Permet de réduire la taille de nos données
CieTop1000 <- unique(Top1000$COMNAM)
Mensuel <- subset(Mensuel, COMNAM %in% CieTop1000)
setorder(Mensuel,date)
setorder(Mensuel, COMNAM)



##########################################################################################################################
# Trouver le nombre de données disponibles pour 1 an, 2 ans et 3 ans pour chaque titre
# Code nécessaire pour éviter des problèmes dans la fonction get_data
#
# split_Mensuel = Données mensuelles de Mensuel regroupées par noms de compagnie
##########################################################################################################################

### Préparation des variables en argument de la fonction Nombre_Donnees.cpp
split_Mensuel <- split(Mensuel, Mensuel$COMNAM)
Nom_Top1000 <- Top1000$COMNAM
date_Top1000 <- as.Date(Top1000$date)

dateMoinUn_Top1000 <- as.Date(Top1000$date - years(1) + days(10))
dateMoinDeux_Top1000 <- as.Date(Top1000$date - years(2) + days(10))
dateMoinTrois_Top1000 <- as.Date(Top1000$date - years(3) + days(10))

### Nombre_Donnees.cpp
Rcpp::sourceCpp("NombreDonnees.cpp")
CompteDonnees <- NombreDonnees(split_Mensuel,Nom_Top1000,date_Top1000,dateMoinUn_Top1000,dateMoinDeux_Top1000,dateMoinTrois_Top1000)

###  Inclure la sortie de NombreDonnees dans Top1000
Top1000$UnAn <- CompteDonnees$Compte_1
Top1000$DeuxAns <- CompteDonnees$Compte_2
Top1000$TroisAns <- CompteDonnees$Compte_3

### Suppression des titres manquant trop de données
# Nécessaire pour éviter des problèmes dans la fonction get_data
Top1000 <- subset(Top1000, UnAn > 10)
Top1000 <- subset(Top1000, DeuxAns > 21)
Top1000 <- subset(Top1000, TroisAns > 32)



##########################################################################################################################
# Trouver l'index de chaque titre de Top1000 dans split_Mensuel
# Facilite l'exécution de get_data
##########################################################################################################################

NomCie <- names(split_Mensuel)
findPosition <- function(Top1000, NomCie) {
  comnam <- Top1000$COMNAM
  
  positions <- rep(-1, length(comnam)) # Initialiser à -1 pour indiquer que comnam n'a pas été trouvé
  
  matches <- match(comnam, NomCie)
  
  positions[!is.na(matches)] <- matches[!is.na(matches)]
  
  return(positions)
}
index <- findPosition(Top1000,NomCie)
Top1000$Index <- index - 1



##########################################################################################################################
# Chercher les données nécessaires pour construire le portefeuille
##########################################################################################################################

Rcpp::sourceCpp("get_data.cpp")
Top1000_data <- get_data(Top1000, split_Mensuel)


### Inclure la sortie de get_data dans Top1000
Top1000 <- subset(Top1000, select = -Index)
Top1000$volatility <- Top1000_data$volatility;
Top1000$NPY <- Top1000_data$NPY;
Top1000$momentum <- Top1000_data$momentum;
Top1000$Dividendes <- Top1000_data$Dividendes;
Top1000$Returns <- Top1000_data$Returns;
Top1000 <- subset(Top1000, volatility != 0.00)



##########################################################################################################################
# Création des portefeuilles Conservateur et Spéculatif
#
# PFConserv = Seulement les données des titres qui forment le portefeuille conservateur à chaque trimestre
# PFSpecul = Seulement les données des titres qui forment le portefeuille spéculatif à chaque trimestre
##########################################################################################################################

### Commence par réduire l'univers des plus grandes capitalisations boursières par
# 500 titres moins volatiles pour conservateur
# 500 titres plus volatiles pour spéculatif
Top1000[, rankVol := frank(volatility, ties.method = "first"), by = date]

# Classification des titres en conservateur et spéculatif
# 1 = conservateur
# 0 = spéculatif
Top1000 <- Top1000 %>%
  group_by(date) %>%
  mutate(selection = ifelse(rankVol <= max(rankVol)/2, 1, 0))

### Création de la variable du portefeuille conservateur
# PF = Portefeuille
# Conserv = Conservateur
# PFConserv = Portefeuille conservateur
PFConserv <- Top1000[Top1000$selection == 1,]
PFConserv <- as.data.table(PFConserv)
PFConserv <- subset(PFConserv, select = -selection)

### Création de la variable du portefeuille spéculatif
# PFSpecul = Portefeuille spéculatif
PFSpecul <- Top1000[Top1000$selection == 0,]
PFSpecul <- subset(PFSpecul, select = -selection)
PFSpecul <- as.data.table(PFSpecul)

### Classement des titres selon NPY et momentum
PFConserv[, rankNPY := frank(NPY, ties.method = "first"), by = date]
PFSpecul[, rankNPY := frank(NPY, ties.method = "first"), by = date]

PFConserv[, rankMomentum := frank(momentum, ties.method = "first"), by = date]
PFSpecul[, rankMomentum := frank(momentum, ties.method = "first"), by = date]

### Calcul des points totaux pour chaque titre
PFConserv[, Points := PFConserv$rankNPY + PFConserv$rankMomentum]
PFSpecul[, Points := PFSpecul$rankNPY + PFSpecul$rankMomentum]

### Formation des portefeuilles avec les 100 titres
PFConserv[, rankPoints := frank(-Points, ties.method = "first"), by = date]
PFSpecul[, rankPoints := frank(Points, ties.method = "first"), by = date]

PFConserv <- PFConserv[rankPoints <= 100]
PFSpecul <- PFSpecul[rankPoints <= 100]

setorder(PFConserv,date, -Points)
setorder(PFSpecul,date, -Points)

### Filtrage des colonnes nécessaires
PFConserv <- PFConserv[, .(date, COMNAM, PRC, BID, ASK, Dividendes, Returns)]
PFSpecul <- PFSpecul[, .(date, COMNAM, PRC, BID, ASK, Dividendes, Returns)]



##########################################################################################################################
# Calculs des performances des portefeuilles conservateur et spéculatif
#
# split_PFConserv = Données du portefeuille conservateur regroupées par trimestre
# split_PFSpecul = Données du portefeuille spéculatif regroupées par trimestre
# retConserv = Performance du portefeuille conservateur (Sortie de returns.cpp)
# retSpecul = Performance du portefeuille spéculatif (Sortie de returns.cpp)
##########################################################################################################################

### Montant de l'investissement inital
Investie <- 100 

### Variables pour faciliter l'exécution de returns.cpp
vec_date_Conser <- unique(PFConserv$date)
vec_date_Spec <- unique(PFSpecul$date)
split_PFConserv <- split(PFConserv, PFConserv$date)
split_PFSpecul <- split(PFSpecul, PFSpecul$date)

### Fonction returns.cpp
Rcpp::sourceCpp("returns.cpp")
DataPFConserv <- returns(split_PFConserv,vec_date_Conser, Investie)
DataPFSpecul <- returns(split_PFSpecul,vec_date_Spec, Investie)

### Turnover trimestriel
turnover_conservateur <- DataPFConserv$Turnover
turnover_speculatif <- DataPFSpecul$Turnover
turnover_mean_conservateur <- mean(turnover_conservateur)
turnover_mean_speculatif <- mean(turnover_speculatif)

### Ajustement de la sortie
retConserv <- do.call(cbind, DataPFConserv)
retSpecul <- do.call(cbind, DataPFSpecul)

retConserv <- as.data.table(retConserv)
retSpecul <- as.data.table(retSpecul)
retConserv$Valeur <- round(as.numeric(retConserv$Valeur), 2)
retConserv$`Rendement trimestre` <- round(as.numeric(retConserv$`Rendement trimestre`)*100, 2)
retSpecul$Valeur <- round(as.numeric(retSpecul$Valeur), 2)
retSpecul$`Rendement trimestre` <- round(as.numeric(retSpecul$`Rendement trimestre`)*100, 2)
retConserv$date <- as.Date(retConserv$date)
retSpecul$date <- as.Date(retSpecul$date)



##########################################################################################################################
# Évolution du montant initialement investi dans le S&P500 "Value-Weighted"
#
# SP500 = Données mensuelles du S&P500 value-weighted
# SP500_trimestriel = Performance trimestrielle du S&P500 value-weighted
##########################################################################################################################

### Nettoyage des données de l'importation
colnames(SP500) <- c("date", "Value-Weighted", "Equal-Weighted", "Total_Market_Value", "Returns")
SP500 <- SP500[!is.na(SP500$Returns)]
SP500[, date := as.Date(date, format = '%d/%m/%Y')]
SP500 <- SP500[SP500$date > min(SP500$date) + years(3) - days(10)]

### Boucle pour itérer période par période
SP500$Valeur_Investissement <- NA
SP500$Real_returns <- NA

SP500$Valeur_Investissement[1] <- Investie
for (i in 2:nrow(SP500)) {
  SP500$Real_returns[i] <- (SP500$Total_Market_Value[i] / SP500$Total_Market_Value[i-1]) - 1
  SP500$Valeur_Investissement[i] <- SP500$Valeur_Investissement[i-1] * (1 + SP500$`Value-Weighted`[i])
}

### Seulement garder les données pour comparer avec nos portefeuilles
SP500$trimestre <- paste0(year(SP500$date), ceiling(month(SP500$date)/3))

trimestres <- SP500$date[month(SP500$date) %in% c(3, 6, 9, 12)]

### Transformation des rendements mensuels en rendements trimestriels
RendTrim <- aggregate(SP500$`Value-Weighted` ~ trimestre, data = SP500, FUN = function(x) (prod(1+x)-1)*100)
SP500_trimestriel <- RendTrim$`SP500$\`Value-Weighted\``



##########################################################################################################################
# Graphique pour l'évolution d'un montant investi dans les portefeuilles conservateur, S&P500 et spéculatif
##########################################################################################################################

### Rassemblement des valeurs trimestrielles
SP500_valeurs <- SP500$Valeur_Investissement[month(SP500$date) %in% c(3, 6, 9, 12)]
Conserv_valeurs <- retConserv$Valeur
Specul_valeurs <- retSpecul$Valeur

### Création d'un dataframe combiné avec les valeurs des portefeuilles
table_valeurs <- cbind(trimestres, Conserv_valeurs, SP500_valeurs, Specul_valeurs)
table_valeurs <- as.data.table(table_valeurs)
colnames(table_valeurs) <- c("Trimestres", "Conservateur", "S&P500", "Speculatif")
table_valeurs$Trimestres <- as.Date(table_valeurs$Trimestres)

### Graphique
ggplot(table_valeurs, aes(x = Trimestres)) +
  geom_line(aes(y = Conservateur, color = "Conservateur")) +
  geom_line(aes(y = `S&P500`, color = "S&P500")) +
  geom_line(aes(y = Speculatif, color = "Speculatif")) +
  labs(x = "", y = "", color = "Portefeuilles") +
  theme_minimal() +
  scale_y_log10(labels = scales::comma,
                breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000)) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y", limits = c(min(table_valeurs$Trimestres), max(table_valeurs$Trimestres))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        axis.line = element_line(colour = "grey", linewidth = 0.5, linetype = "solid"),
        axis.line.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor.y = element_line(color = "gray", linetype = "dashed"))


##########################################################################################################################
# Histogramme : Never a Lost Decade
##########################################################################################################################

### Rassemblement des rendements trimestriels
SP500_trimestriel <- SP500_trimestriel # Rappel
Conserv_trimestriel <- retConserv$`Rendement trimestre`
Specul_trimestriel <- retSpecul$`Rendement trimestre`

### Création de table_rendements
# Dataframe combiné avec tous les rendements des portefeuilles conservateur, spéculatif et S&P500
table_rendements <- cbind(trimestres, SP500_trimestriel, Conserv_trimestriel, Specul_trimestriel)
table_rendements <- as.data.table(table_rendements)
colnames(table_rendements) <- c("Trimestres", "S&P500", "Conservateur", "Speculatif")
table_rendements$Trimestres <- as.Date(table_rendements$Trimestres)
table_rendements$`S&P500` <- as.numeric(table_rendements$`S&P500`)
table_rendements$Conservateur <- as.numeric(table_rendements$Conservateur)
table_rendements$Speculatif <- as.numeric(table_rendements$Speculatif)


### Calcul des rendements annuels de chaque portefeuille
table_rendements$annee <- year(table_rendements$Trimestres)
rend_annuels_sp500_df <- aggregate(table_rendements$`S&P500` ~ annee, data = table_rendements, FUN = function(x) (prod(1 + x/100) - 1)*100)
rend_annuels_conservateur_df <- aggregate(table_rendements$Conservateur ~ annee, data = table_rendements, FUN = function(x) (prod(1 + x/100) - 1)*100)
rend_annuels_speculatif_df <- aggregate(table_rendements$Speculatif ~ annee, data = table_rendements, FUN = function(x) (prod(1 + x/100) - 1)*100)

annees <- unique(table_rendements$annee)
annees_1930 <- annees[-1]

### Rendements annuels du S&P500
rend_annuels_sp500 <- rend_annuels_sp500_df$`table_rendements$\`S&P500\``
rend_annuels_sp500_1930 <- rend_annuels_sp500[-1] # 1930 à aujourd'hui

### Rendements annuels du conservateur
rend_annuels_conservateur <- rend_annuels_conservateur_df$`table_rendements$Conservateur`
rend_annuels_conservateur_1930 <- rend_annuels_conservateur[-1] # 1930 à aujourd'hui

### Rendements annuels du spéculatif
rend_annuels_speculatif <- rend_annuels_speculatif_df$`table_rendements$Speculatif`
rend_annuels_speculatif_1930 <- rend_annuels_speculatif[-1] # 1930 à aujourd'hui

### Création de rend_annuels
# rend_annuels = Rendements annuels du S&P500, conservateur et spéculatif
rend_annuels <- cbind(annees_1930, rend_annuels_sp500_1930, rend_annuels_conservateur_1930, rend_annuels_speculatif_1930)
colnames(rend_annuels) <- c("Année", "S&P500", "Conservateur", "Spéculatif")
rend_annuels <- as.data.table(rend_annuels)

### Calcul des rendements annualisés par décennie de chaque portefeuille de rend_annuels
rend_annuels$decennie <- paste0(substr(rend_annuels$Année, 1, 3), "0s", substr(rend_annuels$Année, 5, nchar(rend_annuels$Année)))
rend_decennie_sp500_df <- aggregate(rend_annuels$`S&P500` ~ decennie, data = rend_annuels, FUN = function(x) (prod(1 + x/100)^(1/length(x)) - 1)*100)
rend_decennie_conservateur_df <- aggregate(rend_annuels$Conservateur ~ decennie, data = rend_annuels, FUN = function(x) (prod(1 + x/100)^(1/length(x)) - 1)*100)
rend_decennie_speculatif_df <- aggregate(rend_annuels$Spéculatif ~ decennie, data = rend_annuels, FUN = function(x) (prod(1 + x/100)^(1/length(x)) - 1)*100)

### Création de rend_decennie
# Dataframe avec les rendements des décennies de chaque portefeuille
decennies <- unique(rend_annuels$decennie)
rend_decennie_sp500 <- rend_decennie_sp500_df$`rend_annuels$\`S&P500\``
rend_decennie_conservateur <- rend_decennie_conservateur_df$`rend_annuels$Conservateur`
rend_decennie_speculatif <- rend_decennie_speculatif_df$`rend_annuels$Spéculatif`

rend_decennie_sp500 <- as.numeric(rend_decennie_sp500)
rend_decennie_conservateur <- as.numeric(rend_decennie_conservateur)
rend_decennie_speculatif <- as.numeric(rend_decennie_speculatif)

rend_decennie <- cbind(decennies, rend_decennie_conservateur, rend_decennie_sp500, rend_decennie_speculatif)
rend_decennie <- as.data.table(rend_decennie)
colnames(rend_decennie) <- c("Décennies", "Conservateur", "S&P500", "Spéculatif")

### Histogramme des rendements à chaque décennie
rend_decennie_transposed <- t(rend_decennie[, -1])
rend_decennie_transposed <- as.data.table(rend_decennie_transposed)
names <- c("Conservateur","S&P500", "Spéculatif")

barplot(sapply(rend_decennie_transposed, as.numeric), beside = TRUE, 
        col = c("black", "darkgrey", "lightgray"), 
        ylim = c(-1,25),
        main = "Rendements par décennie",
        ylab = "Rendements (%)",
        names.arg = decennies,
        legend.text = names,
        args.legend = list(x = "topright", cex = 0.7),
        cex.names = 0.7)



##########################################################################################################################
# Création des portefeuilles pour le tableau Conservative Formula versus Other Factors
#
# PF = portefeuille
# Chaque variable de cette section commençant par "PF" suivi d'un critère est une variable contenant toutes les
# informations des titres dans le portefeuille formé avec le critère qui suit "PF".
# Ex : PFSmall = portefeuille création avec le critère des plus petites capitalisations boursières
#
# ret = returns
# Chaque variable de cette section commençant par "ret" suivi d'un critère est une variable qui contient les performances
# du portefeuille créé avec le critère qui suit "ret". (La sortie de returns.cpp)
# Ex : retSmall = Performance du portefeuille des plus petites capitalisations boursières et la sortie de returns.cpp 
##########################################################################################################################

### Ajustement de variables pour la suite
setDT(Top1000)
retFormula <- retConserv$`Rendement trimestre`

### Création du portefeuille Small (Plus petite capitalisation boursière)
PFSmall <- Top1000[, .(COMNAM, date, MarketCap, PRC, BID, ASK, RET, Dividendes, Returns)]
PFSmall[, rankMarketCap := frank(MarketCap, ties.method = "first"), by = date]
PFSmall <- PFSmall[rankMarketCap <= 100]
setorder(PFSmall,date)

# Calcul des rendements du portefeuille
vec_date_Small <- unique(PFSmall$date)
split_PFSmall <- split(PFSmall, PFSmall$date)
DataPFSmall <- returns(split_PFSmall,vec_date_Small, Investie)

# Ajustement de la sortie de résultat
retSmall <- do.call(cbind, DataPFSmall)
retSmall <- as.data.table(retSmall)
retSmall$Valeur <- round(as.numeric(retSmall$Valeur), 2)
retSmall$`Rendement trimestre` <- round(as.numeric(retSmall$`Rendement trimestre`)*100, 2)
retSmall$date <- as.Date(retSmall$date)


### Création du portefeuille Momentum 12-1
PFMomentum <- Top1000[, .(COMNAM, date, PRC, BID, ASK, RET,momentum, Dividendes,Returns)]
PFMomentum[, rankMomentum := frank(-momentum, ties.method = "first"), by = date]
PFMomentum <- PFMomentum[rankMomentum <= 100]
setorder(PFMomentum,date)

# Calcul des rendements du portefeuille
vec_date_Momentum <- unique(PFMomentum$date)
split_PFMomentum <- split(PFMomentum, PFMomentum$date)
DataPFMomentum <- returns(split_PFMomentum,vec_date_Momentum, Investie)

# Ajustement de la sortie de résultat
retMomentum <- do.call(cbind, DataPFMomentum)
retMomentum <- as.data.table(retMomentum)
retMomentum$Valeur <- round(as.numeric(retMomentum$Valeur), 2)
retMomentum$`Rendement trimestre` <- round(as.numeric(retMomentum$`Rendement trimestre`)*100, 2)
retMomentum$date <- as.Date(retMomentum$date)


### Création du portefeuille Low Vol (basse volatility historique de 3 ans)
PFLowvol <- Top1000[, .(COMNAM, date, PRC, BID, ASK, RET,rankVol, Dividendes,Returns)]
PFLowvol <- PFLowvol[rankVol <= 100]
setorder(PFLowvol,date)

# Calcul des rendements du portefeuille
vec_date_LowVol <- unique(PFLowvol$date)
split_PFLowvol <- split(PFLowvol, PFLowvol$date)
DataPFLowvol <- returns(split_PFLowvol,vec_date_LowVol, Investie)

# Ajustement de la sortie de résultat
retLowvol <- do.call(cbind, DataPFLowvol)
retLowvol <- as.data.table(retLowvol)
retLowvol$Valeur <- round(as.numeric(retLowvol$Valeur), 2)
retLowvol$`Rendement trimestre` <- round(as.numeric(retLowvol$`Rendement trimestre`)*100, 2)
retLowvol$date <- as.Date(retLowvol$date)


### Création du portefeuille NPY
PFNPY <- Top1000[, .(COMNAM, date, PRC, BID, ASK, RET,NPY, Dividendes,Returns)]
PFNPY[, rankNPY := frank(-NPY, ties.method = "first"), by = date]
PFNPY <- PFNPY[rankNPY <= 100]
setorder(PFNPY,date)

# Calcul des rendements du portefeuille
vec_date_NPY <- unique(PFNPY$date)
split_PFNPY <- split(PFNPY, PFNPY$date)
DataPFNPY <- returns(split_PFNPY,vec_date_NPY, Investie)

# Ajustement de la sortie de résultat
retNPY <- do.call(cbind, DataPFNPY)
retNPY <- as.data.table(retNPY)
retNPY$Valeur <- round(as.numeric(retNPY$Valeur), 2)
retNPY$`Rendement trimestre` <- round(as.numeric(retNPY$`Rendement trimestre`)*100, 2)
retNPY$date <- as.Date(retNPY$date)



##########################################################################################################################
# Tableau : Conservative Formula versus Other Factors
#
# tous_les_rendements_trimestriels = Rendements trimestriels de tous les portefeuilles de ce tableau
# tous_les_rendements_annuels = Rendements annuels de tous les portefeuilles de ce tableau
# T_bills = Rendements annuels des T-Bills américains 30 jours, téléchargé de CRSP
# vs_other_factors = Toutes les informations à mettre dans le tableau
#
# rend_ suivi d'un portefeuille du tableau est une variable contenant seulement les rendements annuels de ce portefeuille
# Ex : rend_small = Rendements annuels du portefeuille small (petites capitalisations boursières)
##########################################################################################################################

### Création de tous_les_rendements_trimestriels
# Dataframe avec les rendements trimestriels de chaque portefeuille du tableau
annees <- unique(year(trimestres))
tous_les_rendements_trimestriels <- cbind(trimestres, retConserv$`Rendement trimestre`, SP500_trimestriel, retSmall$`Rendement trimestre`,
                                          retMomentum$`Rendement trimestre`, 
                                          retLowvol$`Rendement trimestre`, retNPY$`Rendement trimestre`)

colnames(tous_les_rendements_trimestriels) <- c("Trimestres", "Formula", "Market", "Small", "Momentum", "Low Vol", "NPY")

tous_les_rendements_trimestriels <- as.data.table(tous_les_rendements_trimestriels)
tous_les_rendements_trimestriels$Trimestres <- as.Date(tous_les_rendements_trimestriels$Trimestres)
tous_les_rendements_trimestriels$annee <- year(tous_les_rendements_trimestriels$Trimestres)

### Création de variables pour les rendements annuels de chaque portefeuille
rend_formula <- rend_annuels_conservateur
rend_market <- rend_annuels_sp500
rend_small <- aggregate(tous_les_rendements_trimestriels$Small ~ annee, data = tous_les_rendements_trimestriels, FUN = function(x) (prod(1 + x/100) - 1)*100)
rend_momentum <- aggregate(tous_les_rendements_trimestriels$Momentum ~ annee, data = tous_les_rendements_trimestriels, FUN = function(x) (prod(1 + x/100) - 1)*100)
rend_lowvol <- aggregate(tous_les_rendements_trimestriels$`Low Vol` ~ annee, data = tous_les_rendements_trimestriels, FUN = function(x) (prod(1 + x/100) - 1)*100)
rend_npy <- aggregate(tous_les_rendements_trimestriels$NPY ~ annee, data = tous_les_rendements_trimestriels, FUN = function(x) (prod(1 + x/100) - 1)*100)

rend_small <- rend_small$`tous_les_rendements_trimestriels$Small`
rend_momentum <- rend_momentum$`tous_les_rendements_trimestriels$Momentum`
rend_lowvol <- rend_lowvol$`tous_les_rendements_trimestriels$\`Low Vol\``
rend_npy <- rend_npy$`tous_les_rendements_trimestriels$NPY`

### Création de tous_les_rendements_annuels
# Dataframe avec les rendements annuels de tous les portefeuilles du tableau
tous_les_rendements_annuels <- cbind(rend_formula, rend_market, rend_small, rend_momentum, rend_lowvol, rend_npy)
tous_les_rendements_annuels <- as.data.table(tous_les_rendements_annuels)

colnames(tous_les_rendements_annuels) <- c("Formula", "Market", "Small", "Momentum", "Low Vol", "NPY")

### Rendements simples
return_simple <- colMeans(tous_les_rendements_annuels)
return_simple <- unlist(round(as.numeric(return_simple),1))

### Rendements composés
return_compounded <- apply(tous_les_rendements_annuels,MARGIN = 2, FUN = function(x) (prod(1 + x/100)^(1/length(x)) - 1)*100)
return_compounded <- unlist(round(as.numeric(return_compounded),1))

### Différence entre les rendements simples et composés
difference_simple_compounded <- return_simple - return_compounded
difference_simple_compounded <- unlist(round(as.numeric(difference_simple_compounded),1))

### Volatilité
volatility <- apply(tous_les_rendements_annuels,MARGIN = 2 , FUN = function(x) sd(x))
volatility <- unlist(round(as.numeric(volatility),1))

### Facteur de réduction du risque (de-risking factor)
volatility_formula <- volatility[1]

de_risking_factor <- (volatility_formula/volatility) * 100
de_risking_factor <- unlist(round(as.numeric(de_risking_factor),0))

### Ratio de Sharpe
T_bills <- as.data.table(T_bills)
colnames(T_bills) <- c("Date", "Rendements")
T_bills <- T_bills[year(Date) >= min(tous_les_rendements_trimestriels$annee)]

rf <- T_bills$Rendements * 100

excess <- tous_les_rendements_annuels - rf
excess <- colMeans(excess)

sharpe_ratio <- excess / volatility
sharpe_ratio <- unlist(round(as.numeric(sharpe_ratio),2))

### Rendements ajustés pour le même niveau de risque
return_same_risk <- round(return_simple * de_risking_factor / 100, 1)

### Création de vs_other_factors
# Dataframe avec toutes les colonnes à mettre dans le tableau
vs_other_factors <- cbind(return_simple, return_compounded, difference_simple_compounded,
                          volatility, de_risking_factor, sharpe_ratio, return_same_risk)
vs_other_factors <- as.data.frame(vs_other_factors)
vs_other_factors <- t(vs_other_factors)
colnames(vs_other_factors) <- c(" ", " ", " ", " ", " ", " ")
rownames(vs_other_factors) <- c("Rendement simple (%)", "Rendement composé (%)",
                                "Différence (simple - composé) (%)", "Volatilité (%)",
                                "Facteur de réduction du risque (%)", "Ratio de Sharpe (simple)",
                                "Rendement ajusté au risque (%)")

### Tableau : Conservative Formula versus Other Factors
kable(vs_other_factors, caption = "La formule conservatrice versus d'autres facteurs", digits = 3) %>%
  kable_styling(full_width = FALSE, font_size = 15) %>%
  add_header_above(c("1925-2023" = 1, "Formula" = 1, "Market" = 1, "Small" = 1, "Momentum" = 1, "Low Vol" = 1, "NPY" = 1))


##########################################################################################################################
# Graphique pour analyse de risque/rendement de tous les portefeuilles du tableau précédent
##########################################################################################################################

couleurs <- c("blue", "darkred", "green", "yellow", "pink", "cyan")

plot(volatility, return_simple, type = "p", pch = 16, xlab = "Écart-type (%)", ylab = "Rendement simple (%)",
     col = couleurs, cex = 2.5,
     main = "Analyse Risque / Rendement",
     family = "serif",
     xlim = c(10,45),
     ylim = c(0, 25))

segments(x0 = volatility[2], y0 = -1,
         x1 = volatility[2], y1 = 26,
         col = couleurs[2], lty = "dashed")

segments(x0 = 8.6, y0 = return_simple[2],
         x1 = 46.5, y1 = return_simple[2],
         col = couleurs[2], lty = "dashed")

legend("topright", 
       legend = c("Formula", "Market", "Small", "Momentum", "Low Vol", "NPY"),
       col = couleurs,
       pch = 16)

grid()

##########################################################################################################################
# Graphique Risque/Rendement Conservateur vs Spéculatif
#
# Variable dans cette section contenant "xxx_deux_ptfs" est une variable contenant la valeur avant le "_" pour deux
# portefeuilles, soit le conservateur et le spéculatif
# Ex : volatility_deux_ptfs = volatilité des portefeuilles conservateur et spéculatif
##########################################################################################################################

### Préparation des variables pour le graphique
deux_ptfs <- cbind(rend_formula, rend_annuels_speculatif)
deux_ptfs <- as.data.table(deux_ptfs)

colnames(deux_ptfs) <- c("Conservateur", "Spéculatif")

return_simple_deux_ptfs <- colMeans(deux_ptfs)
return_simple_deux_ptfs <- unlist(round(as.numeric(return_simple_deux_ptfs),1))

return_compounded_deux_ptfs <- apply(deux_ptfs, MARGIN = 2, FUN = function(x) (prod(1 + x/100)^(1/length(x)) - 1)*100)
return_compounded_deux_ptfs <- unlist(round(as.numeric(return_compounded_deux_ptfs),1))

volatility_deux_ptfs <- apply(deux_ptfs, MARGIN = 2 , FUN = function(x) sd(x))
volatility_deux_ptfs <- unlist(round(as.numeric(volatility_deux_ptfs),1))

### Graphique
couleurs <- c("deepskyblue3", "violetred1")

plot(volatility_deux_ptfs, return_compounded_deux_ptfs, type = "p", pch = 15, xlab = "Écart-type (%)", ylab = "Rendement composé (%)",
     col = couleurs, cex = 2.5,
     main = "Analyse Risque / Rendement",
     las = 1,
     frame.plot = FALSE,
     family = "serif",
     xlim = c(10,45),
     ylim = c(0, 20))

text(volatility_deux_ptfs[1]-4.2,return_compounded_deux_ptfs[1]+1.2 , "Conservateur", pos = 4, cex = 0.9, col = "black")

text(volatility_deux_ptfs[2]-3.5, return_compounded_deux_ptfs[2]+1.2, "Spéculatif", pos = 4, cex = 0.9, col = "black")



##########################################################################################################################
# La formule conservatrice versus Portefeuilles de Fama-French
# 4 graphiques
#
# Variables contenants les données de différents portefeuilles Fama-French:
# FF_Size_BtM = Données du portefeuilles Fama-French Size/Book-to-Market
# FF_Size_Mom = Données du portefeuilles Fama-French Size/Momentum
# FF_Size_BtM_Prof = Données du portefeuilles Fama-French Size/Book-to-Market/Profitability
# FF_Size_BtM_Invest = Données du portefeuilles Fama-French Size/Book-to-Market/Investement
#
# Ces données ont été téléchargées sur le site web : 
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
#
# sharpe_ratio_ = Variable contenant tous les ratios de Sharpe. Le deuxième "_" sera suivi du nombre 1 pour le premier
#                 graphique Fama-French de l'article, 2 pour le second, 3 pour le troisième et 4 pour le quatrième
##########################################################################################################################



##########################################################################################################################
# Size/BtM de 1929 à 2023
#
# High book-to-value = value
# Low book-to-value = growth
##########################################################################################################################

### Attribution de noms appropriés pour former le graphique
colnames(FF_Size_BtM) <- c("annee","SG","SN","SV","BG","BN","BV")
colnames(FF_Size_Mom) <- c("annee","SL","SN","SW","BL","BN","BW")

FFnames <- c("annee","SG1","SG2","SG3","SG4","SBM21","SBM22","SBM23","SBM24","SBM31","SBM32",
             "SBM33","SBM34", "SV1","SV2","SV3","SV4", "BG1","BG2","BG3","BG4", "BBM21","BBM22","BBM23","BBM24",
             "BBM31","BBM32","BBM33","BBM34","BV1","BV2","BV3","BV4")

colnames(FF_Size_BtM_Prof) <- FFnames
colnames(FF_Size_BtM_Invest) <- FFnames

### Calcul des ratios de Sharpe
Sharpe_Formula <- sharpe_ratio[1]

FF_Size_BtM_2 <- FF_Size_BtM[-(1:2),]
excess <- FF_Size_BtM_2[,-1] - rf
excess <- colMeans(excess)

volatilite_1 <- apply(FF_Size_BtM_2[,-1], MARGIN = 2,FUN = function (x) sd(x) )

sharpe_ratio_1 <- excess / volatilite_1
sharpe_ratio_1 <- c(Sharpe_Formula,sharpe_ratio_1)
sharpe_ratio_1 <- t(as.data.table(sharpe_ratio_1))

colnames(sharpe_ratio_1) <- c("Formula","SG","SN","SV","BG","BN","BV")

### Graphique
barplot(sharpe_ratio_1, beside = TRUE,ylim = c(0,0.6),col = NA,cex.names = 0.8,xaxt = "n",yaxt = "n",border = NA)

segments(x0 = 0.3, y0 = seq(0.1, 0.6, 0.1),
         x1 = 14.5, y1 = seq(0.1, 0.6, 0.1),
         col = "grey", lty = "dashed")
         
segments(x0 = 0.3, y0 = 0,
         x1 = 14.5, y1 = 0,
         col = "black")

barplot(sharpe_ratio_1, 
        add = TRUE,
        beside = TRUE,
        col = c("skyblue", "darkgrey", "darkgrey", "black", "darkgrey", "darkgrey", "darkgrey"),
        cex.names = 1.1,
        family = "serif",
        yaxt = "n",
        ylim = c(0,0.6))

axis(2,las = 2, family = "serif",cex.axis = 1.05, col = NA)

title(main = "Sharpe Ratios 1929-2023 versus Fama-French Size/BtM", 
      family = "serif",
      cex.main = 1.4)



##########################################################################################################################
# Size/Mom de 1929 à 2023
##########################################################################################################################

### Calcul des ratios de Sharpe
FF_Size_Mom_2 <- FF_Size_Mom[-(1:2),]

excess <- FF_Size_Mom_2[,-1] - rf
excess <- colMeans(excess)

volatilite_2 <- apply(FF_Size_Mom_2[,-1], MARGIN = 2,FUN = function (x) sd(x) )

sharpe_ratio_2 <- excess / volatilite_2
sharpe_ratio_2 <- c(Sharpe_Formula,sharpe_ratio_2)
sharpe_ratio_2 <- t(as.data.table(sharpe_ratio_2))

colnames(sharpe_ratio_2) <- c("Formula","SL","SN","SW","BL","BN","BW")

### Graphique
barplot(sharpe_ratio_2, beside = TRUE,ylim = c(0,0.6),col = NA,cex.names = 0.8,xaxt = "n",yaxt = "n",border = NA)

segments(x0 = 0.3, y0 = seq(0.1, 0.6, 0.1),
         x1 = 14.5, y1 = seq(0.1, 0.6, 0.1),
         col = "grey", lty = "dashed")

segments(x0 = 0.3, y0 = 0,
         x1 = 14.5, y1 = 0,
         col = "black")

barplot(sharpe_ratio_2, 
        add = TRUE,
        beside = TRUE,
        col = c("skyblue", "darkgrey", "darkgrey", "black", "darkgrey", "darkgrey", "darkgrey"),
        cex.names = 1.1,
        family = "serif",
        yaxt = "n",
        ylim = c(0,0.6))

axis(2,las = 2, family = "serif",cex.axis = 1.05, col = NA)

title(main = "Sharpe Ratios 1929-2023 versus Fama-French Size/Mom", 
      family = "serif",
      cex.main = 1.4)



##########################################################################################################################
# Size/BtM/Profitablility de 1963 à 2023
##########################################################################################################################

### Noms des bandes
noms_FFGraphique <- c("Formula","Low Prof","2","3","High Prof","Low Prof","2","3","High Prof","Low Prof","2","3","High Prof","Low Prof","2","3","High Prof","Low Prof","2","3","High Prof","Low Prof","2","3","High Prof","Low Prof","2","3","High Prof","Low Prof","2","3","High Prof")

### Prendre les données à partir de 1963 à 2023 du taux sans risque
rf_2 <- rf[-(1:35)]

### Calcul des ratios de Sharpe
excess <- FF_Size_BtM_Prof[,-1] - rf_2
excess <- colMeans(excess)

volatilite_3 <- apply(FF_Size_BtM_Prof[,-1], MARGIN = 2,FUN = function (x) sd(x) )

sharpe_ratio_3 <- excess / volatilite_3
sharpe_ratio_3 <- c(Sharpe_Formula,sharpe_ratio_3)
sharpe_ratio_3 <- t(as.data.table(sharpe_ratio_3))

colnames(sharpe_ratio_3) <- noms_FFGraphique

### Graphique
graphique <- barplot(sharpe_ratio_3, beside = TRUE,ylim = c(-0.2,0.7),col = NA,cex.names = 0.8,xaxt = "n",yaxt = "n",border = NA)

segments(x0 = 0, y0 = seq(0.1, 0.7, 0.1),
         x1 = 67, y1 = seq(0.1, 0.7, 0.1),
         col = "grey", lty = "dashed")

segments(x0 = 0, y0 = 0,
         x1 = 67, y1 = 0,
         col = "black")

graphique <- barplot(sharpe_ratio_3, 
                     add = TRUE,
                     las = 2,
                     beside = TRUE,
                     col = c("skyblue", rep("grey50",4), rep("grey90",4), rep("grey50",4),"black",  rep("grey90",3),
                             rep("grey50",4), rep("grey90",4), rep("grey50",4), rep("grey90",4),rep("grey50",4)),
                     cex.names = 0.9,
                     family = "serif",
                     yaxt = "n",
                     names.arg = character(length(sharpe_ratio_3)),
                     ylim = c(0,0.7))

axis(2,las = 2, family = "serif",cex.axis = 1.05, col = NA, pos = 1,
     at = seq(0, 0.7, by = 0.1))

title(main = "Sharpe Ratios 1964-2023 versus Fama-French Size/BtM/Profitability", 
      family = "serif",
      cex.main = 1.2)

### Définir les étiquettes principales de l'axe des x
mtext(side=1, text=noms_FFGraphique,
      at=c(seq(1.5,66,2)),
      line=-5.4, cex=0.85,family = "serif",las = 2)

mtext(side=1, text=c("Small Growth", "Small BM2", "Small BM3", "Small Value","Big Growth", "Big BM2", "Big BM3", "Big Value"),
      at=c(mean(graphique[2:5]), mean(graphique[6:9]), mean(graphique[10:13]), mean(graphique[14:17]), 
           mean(graphique[18:21]), mean(graphique[22:25]), mean(graphique[26:29]), mean(graphique[30:33])),
      line=-2.7, cex=0.9,font = 2,family = "serif")

### Ajouter les barres verticales sous l'axe des x
positions_ligne <- c(0.9, 2.3, 10.5, 18.5, 26.5, 34.5, 42.5, 50.5, 58.5, 66.5)

for (position in positions_ligne) {
  segments(x0 = position, y0 = 0, 
           x1 = position, y1 = -0.145, 
           lwd = 2, col = "grey60")
}



##########################################################################################################################
# Size/BtM/Investements de 1963 à 2023
##########################################################################################################################

### Noms des bandes
noms_FFGraphique <- c("Formula","Low Inv","2","3","High Inv","Low Inv","2","3","High Inv","Low Inv","2","3","High Inv","Low Inv","2","3","High Inv","Low Inv","2","3","High Inv","Low Inv","2","3","High Inv","Low Inv","2","3","High Inv","Low Inv","2","3","High Inv")

### Calcul des ratios de Sharpe
excess <- FF_Size_BtM_Invest[,-1] - rf_2
excess <- colMeans(excess)

volatilite_4 <- apply(FF_Size_BtM_Invest[,-1], MARGIN = 2,FUN = function (x) sd(x) )

sharpe_ratio_4 <- excess / volatilite_4
sharpe_ratio_4 <- c(Sharpe_Formula,sharpe_ratio_4)
sharpe_ratio_4 <- t(as.data.table(sharpe_ratio_4))

colnames(sharpe_ratio_4) <- noms_FFGraphique

### Graphique
graphique <- barplot(sharpe_ratio_4, beside = TRUE,ylim = c(-0.2,0.7),col = NA,cex.names = 0.8,xaxt = "n",yaxt = "n",border = NA)

segments(x0 = 0, y0 = seq(0.1, 0.7, 0.1),
         x1 = 67, y1 = seq(0.1, 0.7, 0.1),
         col = "grey", lty = "dashed")

segments(x0 = 0, y0 = 0,
         x1 = 67, y1 = 0,
         col = "black")

graphique <- barplot(sharpe_ratio_4, 
                     add = TRUE,
                     las = 2,
                     beside = TRUE,
                     col = c("skyblue", rep("grey50",4), rep("grey90",4), rep("grey50",4),"black",  rep("grey90",3),
                             rep("grey50",4), rep("grey90",4), rep("grey50",4), rep("grey90",4),rep("grey50",4)),
                     cex.names = 0.9,
                     family = "serif",
                     yaxt = "n",
                     names.arg = character(length(sharpe_ratio_4)),
                     ylim = c(0,0.7))

axis(2,las = 2, family = "serif",cex.axis = 1.05, col = NA, pos = 1,
     at = seq(0, 0.7, by = 0.1))

title(main = "Sharpe Ratios 1964-2023 versus Fama-French Size/BtM/Investements", 
      family = "serif",
      cex.main = 1.2)

### Définir les étiquettes principales de l'axe des x
mtext(side=1, text=noms_FFGraphique,
      at=c(seq(1.5,66,2)),
      line=-5.4, cex=0.85,family = "serif",las = 2)

mtext(side=1, text=c("Small Growth", "Small BM2", "Small BM3", "Small Value","Big Growth", "Big BM2", "Big BM3", "Big Value"),
      at=c(mean(graphique[2:5]), mean(graphique[6:9]), mean(graphique[10:13]), mean(graphique[14:17]), 
           mean(graphique[18:21]), mean(graphique[22:25]), mean(graphique[26:29]), mean(graphique[30:33])),
      line=-2.7, cex=0.9,font = 2,family = "serif")

### Ajouter les barres verticales sous l'axe des x
positions_ligne <- c(0.9, 2.3, 10.5, 18.5, 26.5, 34.5, 42.5, 50.5, 58.5, 66.5)

for (position in positions_ligne) {
  segments(x0 = position, y0 = 0, 
           x1 = position, y1 = -0.145, 
           lwd = 2, col = "grey60")
}



##########################################################################################################################
# Tableau : After Trading Net Returns (%)
#
# after_trading_net_returns = toutes les données de ce tableau pour les titres U.S.
##########################################################################################################################

### Fixer les frais de transactions comme dans l'article
frais_transaction_bas <- 0.001 # 10 bps
frais_transaction_haut <- 0.003 # 30 bps

### Calcul de chaque ligne du tableau
gross_return <- return_simple[1]

turnover_quarterly <- mean(turnover_conservateur)

trading_costs_low <- frais_transaction_bas * turnover_quarterly * 4

trading_costs_high <- frais_transaction_haut * turnover_quarterly * 4

trading_costs_prct <- trading_costs_high/gross_return * 100

net_return <- gross_return - trading_costs_high

after_trading_net_returns <- cbind(gross_return, turnover_quarterly, trading_costs_low, trading_costs_high, 
                          trading_costs_prct, net_return)
after_trading_net_returns <- as.data.frame(after_trading_net_returns)
after_trading_net_returns <- t(after_trading_net_returns)

colnames(after_trading_net_returns) <- c("U.S.")
rownames(after_trading_net_returns) <- c("Rendement brut", "Turnover trimestriel",
                                "Coûts de transaction estimés - Bas", "Coûts de transaction estimés - Haut",
                                "Coûts de transaction (Haut) en % du rend.", "Rendement net (Haut)")

### Tableau
kable(after_trading_net_returns, caption = "Rendement net après frais (%)", digits = 2) %>%
  kable_styling(full_width = FALSE, font_size = 15)



##########################################################################################################################
# Calculs supplémentaires
##########################################################################################################################

### Création de variables avec les rendements annuels des portefeuilles du tableau "Conservative Formula versus Other Factors" :
# Avant2016 = 1929 à 2016 
# Apres2016 = 2016 à 2023
# Apres2020 = 2020 à 2023
Rendements <- cbind(tous_les_rendements_annuels,rend_annuels_speculatif)
colnames(Rendements) <- c("Formula", "Market", "Small", "Momentum", "Low Vol", "NPY", "Speculatif")

Avant2016 <- apply(Rendements[0:89],MARGIN = 2, FUN = function(x) (prod(1 + x/100)^(1/length(x)) - 1)*100)
Apres2016 <- apply(Rendements[90:95],MARGIN = 2, FUN = function(x) (prod(1 + x/100)^(1/length(x)) - 1)*100)
Apres2020 <- apply(Rendements[92:95],MARGIN = 2, FUN = function(x) (prod(1 + x/100)^(1/length(x)) - 1)*100)


##########################################################################################################################

fin <- proc.time()
temps_execution <- fin - debut
print(paste("Le temps d'exécution est de",round(temps_execution[3],0), "secondes."))

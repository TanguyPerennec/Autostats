setwd("~/MEGA/ARTICLES/Packages/autostats")
library(dplyr)
library(stats)
library(stringr)
library(xlsx)
library(readxl)
#library(tidyverse)
library(autostats)


##################
# DATA CLEANING  #
##################
DF <- read_excel("~/MEGA/ARTICLES/These_psychiatrie/Datas/Copie de Copie de thèse.xlsx",
                 sheet = "données recueillies",
                 range = "B1:AA109")

DF2 <- read_excel("~/MEGA/ARTICLES/These_psychiatrie/Datas/Copie de Copie de thèse.xlsx",
                  sheet = "données recueillies en codage",
                  range = "B1:AA109")

codage <- read_excel("~/MEGA/ARTICLES/These_psychiatrie/Datas/Copie de Copie de thèse.xlsx",
                     sheet = "codage des variables")


#transformation de l'âge
DF %>%
   separate(Age, c("années", "mois"),sep = "a") -> DF
str_replace_all(DF$mois, c("mois" = "","ns" = "")) -> DF$mois
as.numeric(DF$mois) -> DF$mois
as.numeric(DF$années) -> DF$années
DF$mois <- round(DF$mois*10/120,2)
DF$mois[is.na(DF$mois)] <- 0 #pas de NA initialement mais introduits si mois =""
DF$années <- DF$années + DF$mois
DF %>%
   select(-mois) -> DF
colnames(DF)[2] <- "Age"

#modif colonnes
DF$Sexe[DF$Sexe == "masculin"] <- "homme"
DF[DF == 9] <- NA
DF[DF == "non renseigné"] <- NA
DF[DF == "?"] <- NA
DF[DF == "aucun"] <- "non"
DF[DF == "aucune"] <- "non"
DF[DF == "sacrifications"] <- "scarifications"
#DF$mode <- str_replace(DF$mode, "ts par ", "")
#DF$mode <- str_replace(DF$mode, "intoxication aux ", "ingestion de ")
DF$facteur_precipitant <- str_replace(DF$facteur_precipitant, "facteur précipitant : ", "")
DF$facteur_precipitant <- str_replace(DF$facteur_precipitant, "facteur precipitant : ", "")
DF$facteur_precipitant <- str_replace(DF$facteur_precipitant, "facteur declenchant : ", "")
DF$facteur_precipitant <- str_replace(DF$facteur_precipitant, "cause", "situation")
DF$scolarite <- str_replace_all(DF$scolarite, c("descolarisé"="descolarisation", "descolarisée"="descolarisation", "difficulté"="difficultés","difficulte"="difficultés","scolaire"="scolaires","ss"="s"))
DF$scolarite <- str_replace(DF$scolarite, "scolarisation", "scolarite")
DF$scolarite <- str_replace(DF$scolarite, ", redoublement", "")
DF$diagnostic <- str_replace(DF$diagnostic, "troubles", "trouble")
DF$TTT <- str_replace(DF$diagnostic, "anxiolytiques seuls", "anxiolytique seul")
#le changement de la casse/accents à lieu dans table1


########### CODAGE
# mode
DF2[DF2 == 9] <- NA
DF2$Mode[DF2$Mode == 0] <- "IMV"
DF2$Mode[DF2$Mode == 1] <- "Ingestion de produit toxique"
DF2$Mode[DF2$Mode == 2] <- "Tentative de pendaison"
DF2$Mode[DF2$Mode == 3] <- "Autre"
DF$mode <- as.factor(DF2$Mode)
relevel(DF$mode,"IMV")

DF2$orientation[DF2$orientation == 0] <- "RAD sans RDV"
DF2$orientation[DF2$orientation == 1] <- "RDV avec suivi psy"
DF2$orientation[DF2$orientation == 2] <- "RAD avec consultation post urgence"
DF2$orientation[DF2$orientation == 3] <- "RAD avec consultation adolescent"
DF2$orientation[DF2$orientation == 4] <- "Hospitalisation adulte"
DF2$orientation[DF2$orientation == 5] <- "Hospitalisation sur ESPACE"
DF$orientation <- as.factor(DF2$orientation)


DF2$conduite_risque[DF2$conduite_risque == 0] <- "Non"
DF2$conduite_risque[DF2$conduite_risque == 1 | DF2$conduite_risque == "1 et 4"] <- "Fugue"
DF2$conduite_risque[DF2$conduite_risque == 2] <- "Violence"
DF2$conduite_risque[DF2$conduite_risque == 3] <- "Risque sexuel"
DF2$conduite_risque[DF2$conduite_risque == 4] <- "Scarifications non suicidaires"
DF$conduite_risque <- as.factor(DF2$conduite_risque)
relevel(DF$conduite_risque,"Non")

DF2$lieu_vie[DF2$lieu_vie == 0] <- "Chez les 2 parents"
DF2$lieu_vie[DF2$lieu_vie == 1] <- "Chez un des deux parents"
DF2$lieu_vie[DF2$lieu_vie == 2] <- "Placement"
DF2$lieu_vie[DF2$lieu_vie == 3] <- "Autre"
DF$lieu_vie <- as.factor(DF2$lieu_vie)
relevel(DF$lieu_vie,"Chez les 2 parents")


DF2$suivi[DF2$suivi == 0] <- "non"
DF2$suivi[DF2$suivi == 1] <- "psychologue libéral"
DF2$suivi[DF2$suivi == 2] <- "psychiatre libéral"
DF2$suivi[DF2$suivi == 3] <- "CMP"
DF2$suivi[DF2$suivi == 4] <- "Autre"
DF$suivi <- as.factor(DF2$suivi)
relevel(DF$suivi,"non")

DF2$TTT[DF2$TTT == 0] <- "non"
DF2$TTT[DF2$TTT == 1] <- "anxiolytiques seuls"
DF2$TTT[DF2$TTT == 2] <- "antidépresseur"
DF2$TTT[DF2$TTT == 3] <- "antipsychotique"
DF2$TTT[DF2$TTT == 4] <- "thymorégulateur"
DF2$TTT[DF2$TTT == 5] <- "autre"
DF$TTT <- as.factor(DF2$TTT)
relevel(DF$TTT,"non")

DF2$diagnostic[DF2$diagnostic == 0] <- "non"
DF2$diagnostic[DF2$diagnostic == 1] <- "épisode dépressif"
DF2$diagnostic[DF2$diagnostic == 2] <- "trouble psychotique"
DF2$diagnostic[DF2$diagnostic == 3] <- "trouble bipolaire"
DF2$diagnostic[DF2$diagnostic == 4] <- "trouble du spectre autistique"
DF2$diagnostic[DF2$diagnostic == 5] <- "trouble des conduites"
DF2$diagnostic[DF2$diagnostic == 6] <- "TDAH"
DF2$diagnostic[DF2$diagnostic == 7] <- "trouble anxieux"
DF2$diagnostic[DF2$diagnostic == 8] <- "trouble autre"
DF$diagnostic <- as.factor(DF2$diagnostic)
relevel(DF$diagnostic,"non")


DF2$antecedent_familiaux[DF2$antecedent_familiaux == 0] <- "non"
DF2$antecedent_familiaux[DF2$antecedent_familiaux == 1] <- "trouble de l'humeur"
DF2$antecedent_familiaux[DF2$antecedent_familiaux == 2] <- "psychose"
DF2$antecedent_familiaux[DF2$antecedent_familiaux == 3] <- "trouble addictif"
DF2$antecedent_familiaux[DF2$antecedent_familiaux == 4] <- "autre"
DF$antecedent_familiaux <- as.factor(DF2$antecedent_familiaux)
relevel(DF$antecedent_familiaux,"non")


DF2$facteur_precipitant[DF2$facteur_precipitant == 0] <- "non"
DF2$facteur_precipitant[DF2$facteur_precipitant == 1] <- "conflit familial"
DF2$facteur_precipitant[DF2$facteur_precipitant == 2] <- "conflit amical"
DF2$facteur_precipitant[DF2$facteur_precipitant == 3] <- "rupture sentimentale"
DF2$facteur_precipitant[DF2$facteur_precipitant == 4] <- "décès d'un proche"
DF2$facteur_precipitant[DF2$facteur_precipitant == 5] <- "autre"
DF$facteur_precipitant <- as.factor(DF2$facteur_precipitant)
relevel(DF$facteur_precipitant,"non")



DF$hospitalisation_anterieure[DF$hospitalisation_anterieure == "passage aux UMP"] <- "non"
DF$hospitalisation_anterieure <-as.factor(DF$hospitalisation_anterieure)
relevel(DF$hospitalisation_anterieure,"oui")

y="Récidive"

##############

# TABLE 1
source("R/table1.R")
table1(DF,"Récidive",ynames=c("Pas de récidive","Récidive")) -> tab1


# Reglog
source("R/reglog.R")
reglog(DF,"Récidive") -> tabf

tab1
tabf

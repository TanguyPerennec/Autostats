setwd("~/MEGA/ARTICLES/Packages/autostats")
load("data/description_indiv_decode.rda")
library(dplyr)
library(stats)
library(stringr)
library(xlsx)
library(ROCR)



#Datas INCA3
##############
data("description_indiv_decode")
DF <- description_indiv_decode
y="sex_PS"



##########
library(readr)
DF <- read_delim("data/thesepsyclean.csv",
                            ";", escape_double = FALSE, trim_ws = TRUE)
DF[,-1]-> DF
y <- "Récidive"



# Parmetres
###########
explicatives = colnames(DF)[colnames(DF) != y]
alpha = 0.05
verbose = TRUE
min_multivariate=2
alpha_max=0.2
round = 3
method = "backward"
rowstimevariable = 10
confirmation = TRUE
##########




verbose=TRUE
alpha=0.05
###############
source("R/table1.R")
tbf <- table1(DF,y)
tbf

source("R/excel.R")
excel(tbf,title_sheet=NULL,"rslts_table1")

source("R/reglog.R")
tbf <- reglog(DF,y,min_multivariate=2,method = "stepwise")
tbf

source("R/propensity.R")
propensity(DF,y="sex_PS",tbf)


c(rep(NA,20),sample(c(0,1),20,replace=TRUE)) -> menopause
c(rep("homme",20),rep("femme",20)) -> sexe
cbind(sexe,menopause) -> DF



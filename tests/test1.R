setwd("~/MEGA/ARTICLES/Packages/autostats")
load("data/description_indiv_decode.rda")
library(dplyr)
library(stats)
library(stringr)
library(xlsx)
source("R/table1.R")


##############
data("description_indiv_decode")
DF <- description_indiv_decode
y="sex_PS"
explicatives=colnames(DF)[colnames(DF) != y]

verbose=TRUE
alpha=0.05
###############

tbf <- table1(DF,y="sex_PS")
tbf

source("R/excel.R")
excel(tbf,title_sheet=NULL,"rslts_table1")

source("R/reglog.R")
tbf <- reglog(DF,y="sex_PS",min_multivariate=2)
tbf

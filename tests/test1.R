setwd("~/MEGA/ARTICLES/Packages/autostats")
load("data/description_indiv_decode.rda")
source("R/table1.R")


tbf <- table1(DF,y="sex_PS")
tbf

source("R/xlsx.R")
xlsx(tbf,title_sheet=NULL,"rslts_table1")

source("R/reglog.R")
tbf <- reglog(DF,y="sex_PS")

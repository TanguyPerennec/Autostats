setwd("~/MEGA/ARTICLES/Packages/autostats")
load("data/description_indiv_decode.rda")
source("R/table1.R")


DF <- description_indiv_decode
y="sex_PS"
tbf <- table1(DF,y="sex_PS")
tbf

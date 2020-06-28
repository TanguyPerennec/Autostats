setwd("~/MEGA/ARTICLES/Packages/autostats")
load("data/description_indiv_decode.rda")
library(dplyr)
library(stats)
library(stringr)
library(xlsx)
library(ROCR)



##############
data("description_indiv_decode")
DF <- description_indiv_decode
y="sex_PS"

verbose=TRUE
alpha=0.05
###############
source("R/table1.R")
tbf <- table1(DF,y)
tbf

source("R/excel.R")
excel(tbf,title_sheet=NULL,"rslts_table1")

source("R/reglog.R")
tbf <- reglog(DF,y,min_multivariate=2)
tbf

source("R/propensity.R")
propensity(DF,y="sex_PS",tbf)


#library(cobalt)
#data(lalonde,package="cobalt")
#DF <- lalonde
#source("R/propensity.R")
#propensity(DF,y="nodegree")

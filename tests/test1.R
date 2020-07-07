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
y = "sex_PS"


#Datas psy
##########
library(readr)
DF <- read_delim("data/thesepsyclean.csv",
                            ";", escape_double = FALSE, trim_ws = TRUE)
DF[, -1] -> DF
y <- "Récidive"
source("R/reglog.R")
reglog(DF,y)

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












#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################










if(method == "forward" || method == "stepwise"){
   # the two methods are equivalent but in stepwise, variables are removed if < alpha once in the model
   # For each step, a variable is added to the model
   # With deviance test, we add the variable Xj from which p-value of deviance test that compare the 2 models is minimal
   # Stop when each variables are added or if p-value is superior to threeshold

   # Methodes
   # test de déviance : possible seulement si modèles emboîtés
   # critère de choix :
   # - AIC : Akaike Informative Criterion
   # - BIC : Bayesian Informative Criterion
   # On choisira le modèle avec le plus petit AIC/BIC


   anova_glm = 0
   fin = 1
   dev_matrix[-1] -> remainings
   vector() -> used

   while (anova_glm < 0.2 & length(remainings) > 0){

      # 1er modèle réalisé avec le modèle avec la plus faible déviance
      formule = paste0(y,'~',dev_matrix[1])
      for (v in used){formule = paste0(formule,"+",v)}
      formule = formula(formule)
      firth <- FALSE
      model1 <- logit(DF[,c(y,var_uni)])
      if(firth) logistf::logistf(formule, data=DF, pl = FALSE, firth = TRUE) -> model1

      fin = fin + 1
      anova_glm_list <- matrix(ncol = 2,nrow=length(remainings))

      # Comparaison des 2 modèles en fonction du critère
      critere <- function(model,critere_choix=deviance){
         if(logistf::is.logistf(model)) {
            deviance <- -2*(model$loglik[1])
         }else{
            deviance <- model$deviance
         }
         return(deviance)
         #AIC
         #BIC
      }

      d = 1
      for(f in remainings){

         expl <- all.vars(formule)[-1] #les anciennes variables
         nvlle_var <- c(f,expl)
         formule2 <- paste0(y,'~',nvlle_var[1])
         for (k in nvlle_var[-1]){formule2 <- paste0(formule2,"+",k)}
         formule2 = formula(formule2)

         # les 2ème modèles potentiels models sont réalisés
         firth <- FALSE
         model1 <- logit(DF[,c(y,nvlle_var)])
         if(firth) logistf::logistf(formule2, data=DF, pl = FALSE, firth = TRUE) -> model2

         critere(model1) -> deviance1
         critere(model2) -> deviance2
         diff_critere <- (deviance2-deviance1)
         formule_diff = paste0("~",all.vars(formule)[2])
         if (length(all.vars(formule)[-1]) > 1){
            for(k in 2:length(all.vars(formule)[-1])){formule_diff=paste0(formule_diff,"+",all.vars(formule)[k+1])}
         }
         formule_diff <- formula(formule_diff)
         anova_glm <- anova(model2,formula=formule_diff)$pval

         nom_model <- all.vars(formule2)[-1][1]
         for(ex in 2:length(all.vars(formule2)[-1])){nom_model <- paste0(nom_model,"+",all.vars(formule2)[-1][ex])}

         anova_glm_list[d,] <- c(nom_model,anova_glm)

         if(verbose) cat('\n\nLe modèle contenant les variables [',(all.vars(formule)),'] est comparé au modèle contenant [',(all.vars(formule2)),"]
               le modèle 1 à une déviance de ",deviance1,' contre ',deviance2,'pour le 2ème modèle
               le meilleur modèle est le modèle',ifelse(diff_critere>0,'1','2'))

         d = d+1
      }
      anova_glm_list[order(anova_glm_list[,2])][1] -> newformula
      if(verbose) cat("\n\n#####\n le meilleur modèle est",newformula)
      paste0(y,"~",newformula) -> newformula
      formula(newformula) -> newformula
      used <- all.vars(newformula)[-1]
      for (var_delete in all.vars(newformula)[-1]){
         remainings[remainings != var_delete] -> remainings
      }
   }

   model2 -> modelfinal
   used -> explicatives_multi

   if (method == "stepwise"){#variables are removed if < alpha once in the model
      summary(model2)$coefficients[-1,4] -> pval_multi
      names(pval_multi[pval_multi > alpha]) -> name_inf_to_alpha
      for (expl_to_delete in name_inf_to_alpha){
         vector_var[match(expl_to_delete,rownames(rslt)[-1])] -> expl_to_delete
         #Checking for dependencies between variables
         explicatives_multi[explicatives_multi != expl_to_delete] -> explicatives_multi
      }
   }

   glm(DF[,c(y,explicatives_multi)], family = "binomial") -> mod_multi
}


if(method == "backward"){
   #Si la méthode descendante utilise un test de déviance, nous éliminons ensuite la variable
   #Xj dont la valeur p associée à la statistique de test de déviance est la plus grande. Nous
   #nous arrêtons lorsque toutes les variables sont retirées du modèle ou lorsque la valeur p est
   #plus petite qu’une valeur seuil.
   while (length(explicatives_multi) < min_multivariate) {
      #so as to make a new model with alpha = alpha + 0.02 if there is less variable left than 'min_multivariate'
      pas <- 0
      nb_p <- 0 #number of significant p
      explicatives_multi <- explicatives
      length_expl <- vector()
      length_expl[pas + 1] <- length(explicatives_multi) # so as to recorded model length to stop if an infinite boucle append
      length_model <- 1 # so as to initiate the loop

      while (nb_p < length_model & length(explicatives_multi) > 0) {
         #while all of variables left (with each levels) in the model are not significants and while there are still some variables left
         pas <- pas + 1

         ## Model with all (new) explicatives_multi variables
         glm(DF[,c(y,explicatives_multi)], family = binomial, data = DF[,c(y,explicatives_multi)]) -> mod_multi

         pval <- summary(mod_multi)$coefficients[, 4] #all p-values
         pval <- pval[-1] #remove intercept
         i_pval <- rank(-pval) #vector with each position of pval ordered with 1 as the worst pval
         order_pval <- order(-pval)
         length_model <- length(pval) # != of length(explicatives) if levels > 2
         vect_explicative_multi <- vect_explicative_multi[-1] #removing empty first row
         level_var_multi <- rownames(summary(mod_multi)$coefficients)[-1] # == names(vect_explicative_multi) - NAs

         if (verbose) {
            o = 0
            cat("\n\nVariables left for multiple analysis :")
            for (var_left in level_var_multi) {
               o <- o + 1
               fin_message <- ifelse(i_pval[o]==1,"st (worst p-val => will be deleted).","th")
               cat("\n       ", var_left," (p = ",pval[o],")    |    rank : ",i_pval[o],fin_message)
            }
         }

         o <- 0
         for (var_left in level_var_multi) {
            o <- o+1
            if (i_pval[o]==1){
               match_var <- vector_var[match(var_left,rownames(rslt)[-1])]
               explicatives_multi <- explicatives_multi[-match(match_var,explicatives_multi)]
            }
         }

         length_expl[pas+1] <- length(explicatives_multi)
         nb_p <- length(pval[pval < alpha_multi])
         if (length(explicatives_multi) == length_expl[pas]) break
      }
      alpha_multi <- alpha_multi + 0.02
      if(alpha_multi > alpha_max){
         cat("\n maximum alpha has been reached (",alpha_multi-0,02,")")
         break
      }
      if(verbose & length(explicatives_multi) < min_multivariate){
         cat("\nThere is no variable with significative OR in the new multivariate model (with alpha = ",alpha_multi - 0.02,")")
         cat("\n                >> trying with alpha = ", alpha_multi,"\n")
      }
   }
   if(alpha_multi > alpha) message("\nSignificance threeshold as been change in the multivariate modele to ",alpha_multi- 0.02)
}



explicatives_multi <- NULL


if(method == "PS"){
   alpha_PS = 0.25
   rslt[-1,4] -> pval
   pval[pval < alpha_PS] -> pval_significant
   pval[pval >= alpha_PS] -> pval_nonsignificant
   explicatives_multi_candidates <- vector()

   for(new_var in names(pval_significant)){#Gets the coressponding variable terms
      rslt[match(new_var,rownames(rslt)),1] -> new_expl
      str_split(new_expl," ")[[1]][1] -> new_expl #corresponding variable
      explicatives_multi_candidates <- c(explicatives_multi_candidates,new_expl)
   }

   glm(DF[,c(y,explicatives_multi_candidates)], family = binomial) -> mod_multi
   explicatives_multi_candidates -> explicatives_multi
   summary(mod_multi)$coefficients[-1,1] -> coeffs

   for(new_var in names(pval_significant)){
      summary(mod_multi)$coefficients[match(new_var,rownames(summary(mod_multi)$coefficients)),4] -> pval
      if(is.na(pval)) {
         explicatives_multi_candidates[explicatives_multi_candidates != new_expl] -> explicatives_multi_candidates
         explicatives_multi[explicatives_multi != new_var] -> explicatives_multi
      }else{
         if (pval < 0.1){
            #we keep the variable
         }else{
            #we delete the variable if change in definite model < 20% of the full model
            rslt[match(new_var,rownames(rslt)),1] -> new_expl
            str_split(new_expl," ")[[1]][1] -> new_expl #corresponding variable
            explicatives_multi_candidates[explicatives_multi_candidates != new_expl] -> explicatives_multi_candidates

            # We check wether the model has change for more than 15% with the deleting
            glm(DF[,c(y,explicatives_multi)], family = binomial, data = DF[,c(y,explicatives_multi)]) -> mod_multi_new
            summary(mod_multi_new)$coefficients[-1,1] -> coeff_new #all coeffs without intercept
            match(names(coeff_new),names(coeffs)) -> match_coeff_new #coressponding place in old coeffs for each new coeff
            (coeff_new-coeffs[match_coeff_new])/coeffs[match_coeff_new] -> relative_changes

            if(TRUE %in% relative_changes > 20/100){
               #we keep the variable
               explicatives_multi_candidates = c(new_var,explicatives_multi_candidates)
            }else{explicatives_multi <- explicatives_multi[explicatives_multi != new_var]}
         }
      }
   }

   names(pval_nonsignificant) -> nonsignificants_vars
   for(new_var in nonsignificants_vars){
      rslt[match(new_var,rownames(rslt)),1] -> new_expl
      str_split(new_expl," ")[[1]][1] -> new_expl
      #any variable not selected for the original multivariate model is added back one at a time,
      #with significant covariates and confounders retained earlier. This step can be helpful in identifying variables that, by themselves, are not significantly related to the outcome but make an important contribution in the presence of other variables. Any that are significant at the 0.1 or 0.15 level are put in the model, and the model is iteratively reduced as before but only for the variables that were additionally added.
   }


   # Definitive model
   glm(DF[,c(y,explicatives_multi)], family = binomial) -> mod_multi

}


if(method == "Augmented backward"){}


if(method == "Best subset"){}


if(method == "Univariable"){}


if(method == "LASSO"){}

##################################################



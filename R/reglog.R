#' Logistic Regression Function
#'
#' @description reglog is used to make logistic regression and gives one matrix with variables, odds-ratios, confidence intervals and p-values of univariate and multivariate models.
#'
#' @usage reglog <- function(DF,y,explicatives,...)
#'
#' @param DF dataframe, matrix or tibble that contains all explicatives variables and the variable to explain.
#' @param y character : variable to explain.
#' @param explicatives character vector : variables that should explain y in the logistic regression. Takes all columns but y from the dataframe if kept empty.
#' @param method the method to be used to select variables in the multivariate model.The default method is the backward elimination. See details for more informations.
#' @param ... Other arguments that can be passed to reglog. See 'details' for more informations.
#'
#' @details Method used to select the variables in the multivariate model can be set it the "method" parameter.
#'  "backward" elimination is the default technique : the least significant effect that does not meet the
#'  level "alpha" for staying in the model is removed. The process is repeated until no other effect in the
#'  model meets the specified level for removal. \n In "forward" selection, the p-value is computed for each effect
#'  not in the model and examines the largest of these statistics. If it is significant at entry level "alpha",
#'  the corresponding effect is added to the model.Once an effect is entered in the model, it is never removed
#'  from the model. The process is repeated until none of the remaining effects meet the specified level for entry.
#'
#' @param alpha num : significance threeshold used to delete non-significant variables in the multivariate model.
#' @param verbose logical : if TRUE, explainations are displayed in the console while running the function.
#' @param min_multivariate num : the minimum number of variables that should be kept in the multivariate model. If the number of significant variables using alpha threeshold is under min_multivariate, alpha is increased by 0.02 points till alpha_max is reached.
#' @param alpha_max num : maximum threeshold used to select the minimum multivariate variables wanted.
#' @param method
#' @param round num : number of digits to display in the final table.
#'
#' @details
#' @details {There is one fairly common circumstance in which both convergence problems and the Hauck-Donner phenomenon (and trouble with \sfn{step}) can occur. This is when the fitted probabilities are extremely close to zero or one. Consider a medical diagnosis problem with thousands of cases and around fifty binary explanatory variables (which may arise from coding fewer categorical factors); one of these indicators is rarely true but always indicates that the disease is present. Then the fitted probabilities of cases with that indicator should be one, which can only be achieved by taking \hat\beta_i = \infty. The result from \sfn{glm} will be warnings and an estimated coefficient of around +/- 10 [and an insignificant t value].}
#'
#' @return reglog returns a matrix with all OR obtain from univariate model and OR obtain from the multivariate model
#' @export
#' @import dplyr
#' @import MASS
#' @import logistf
#' @import stringr
#' @import safeBinaryRegression
#' @importFrom autostats progressbar
#'
#' @references Bursac, Z., Gauss, C.H., Williams, D.K. et al. Purposeful selection of variables in logistic regression. Source Code Biol Med 3, 17 (2008). https://doi.org/10.1186/1751-0473-3-17
#' @references Heinze G, Schemper M. A solution to the problem of separation in logistic regression. Stat Med. 2002;21(16):2409-2419. doi:10.1002/sim.1047
#' @examples
reglog <- function(DF,
                  y,
                  explicatives = colnames(DF)[colnames(DF) != y],
                  alpha = 0.05,
                  verbose = TRUE,
                  min_multivariate=2,
                  alpha_max=0.2,
                  round = 3,
                  method = "backward",
                  rowstimevariable = 10,
                  confirmation = TRUE) {


   source("R/dataprep.R")
   #To ignore the warnings during usage
   #options(warn=-1)
   #options("getSymbols.warning4.0"=FALSE)



   ##################################################
   #    Arguments verification / transformation     #
   ##################################################

   if (is.data.frame(DF) || is.matrix(DF) || is.tbl(DF)){
      DF <- as.data.frame(DF)
   } else{stop("No dataframe has been provided. Make sure 'DF' is a dataframe, a tibble or a matrix")}


   if (!is.vector(explicatives))
      stop("explicatives should be a vector of characters")

   #Removes explicatives not in DF
   explicatives_out_DF <- explicatives[!(explicatives %in% colnames(DF))]
   if(length(explicatives_out_DF) > 0){
      msg_error <- explicatives_out_DF[1]
      if(length(explicatives_out_DF) > 1){
         for (expl_out_DF in explicatives_out_DF[-1]){
            msg_error <- paste0(msg_error,", ",expl_out_DF)
         }
         msg_error <- paste0(msg_error," are not part of DF columns")
      }else{msg_error <- paste0(msg_error," is not part of DF columns")}
      stop(msg_error)
   }


   if (!is.character(y) ||!(y %in% colnames(DF)))
      stop("y must be a character variable, part of DF")


   if (y %in% explicatives) {#if y is in 'explicatives' it is deleted
      message('y is part of "explicatives" and is removed from it')
      explicatives[-match(y, explicatives)]
   }


   if (!is.logical(verbose))
      stop("'verbose' must be logical")


   if (!is.numeric(min_multivariate))
      stop("min_multivariate must be numeric")


   if (!is.numeric(round) || round <= 0)
      stop("round must be numeric and positive")


   if (!(method %in% c("backward","forward","stepwise","PS")))
      stop("'method' should be one of this methods : backward, forward, stepwise, PS")

   if(!is.logical(confirmation))
      stop("'confirmation' must be logical")
   ##################################################





   ##################################################
   #               1) DATA CLEANING                 #
   ##################################################
   if (verbose) cat(
"\n---+-----------------------------+-----------------------------------------------------------------------------------
   |                             |
   |      1) DATA CLEANING       |
   |                             |
   +-----------------------------+\n
")


   DF <- data_prep_complete(DF,y,verbose=T)
   explicatives <- colnames(DF)[colnames(DF) != y]
   ##################################################







   ##################################################
   #            2) UNIVARIATE MODEL                 #
   ##################################################
   if (verbose) cat(
"\n---+-----------------------------+-----------------------------------------------------------------------------------
   |                             |
   |    2) UNIVARIATE MODEL      |
   |                             |
   +-----------------------------+\n
")


   # Constructing 'vect_explicative'

   #
   vect_explicative <- vector()
   n = 1
   for (var in explicatives) {#making a vector with the name of the variable displayed as many times as (levels - 1)
      if (is.numeric(DF[, var])) {
         vect_explicative[n] <- var
         n <- n + 1
      } else{
         DF[, var] <- factor( DF[, var])
         length(levels(DF[, var])) -> levels_var
         vect_explicative[n:(n + levels_var - 2)] <- rep(var, (levels_var - 1))
         n <- n + levels_var - 1
      }
   }
   #


   # Rslt matrice construction
   rslt <- matrix("-", nrow = (length(vect_explicative) + 1), ncol = 7)
   rownames(rslt) <- rep(" ",(length(vect_explicative) + 1))
   rslt[1, ] <- c("", "OR", "IC", "p", "OR", "IC", "p")
   #

   vector_var <-  vector()
   vector_firth <- vector()
   var_i = 0
   i = 0
   dev_matrix <- matrix(ncol = 2,nrow=length(explicatives)) #matrice de deviance


   getinfo_glm <- function(mod = mod_uni,K=k,var=var_uni){
      coef <- ifelse(firth,mod$coefficients[K + 1],summary(mod)$coefficients[K+1,1])
      OR <- signif(exp(coef), round) #exp of logit function
      pval <- ifelse(firth,mod$prob[K+1],summary(mod)$coefficients[K + 1, 4])
      IC <- suppressMessages(confint(mod))
      IC_paste <- paste0("[", round(exp(IC[K + 1, 1]), round), ";", round(exp(IC[K + 1, 2]), round), "]")
      name <- ifelse(firth,names(mod$coefficients)[K + 1],rownames(summary(mod)$coefficients)[K + 1])
      level_var <- str_split(name, ifelse(firth,"DF\\[, var_uni\\]",var), n = 2)[[1]][2]
      name_var <- ifelse(level_var == "",var,paste0(var, "  (", level_var, ")"))
      ligne <- c(name_var, OR, IC_paste, pval,level_var)
      return(ligne)
   }


   for (var_uni in explicatives) {

      progressbar(total = length(vect_explicative),i,variable = var_uni)

      firth <- FALSE

      mod_uni <- logit(DF[,c(y,var_uni)])

      #saving deviances for each model
      var_i <- var_i+1
      dev_matrix[var_i,] <- c(var_uni,ifelse(firth,-2*mod_uni$loglik[2],mod_uni$deviance))


      k = 0
      if (is.numeric(DF[, var_uni])) {
         i <- i + 1
         k <- k + 1
         ligneR <- getinfo_glm()
         vector_var[i] <- var_uni
         rslt[i + 1, ] <- c(ligneR[1:4], "-", "-", "-")
         row.names(rslt)[i + 1]<- ifelse(firth,paste0(var_uni,ligneR[5]),rownames(summary(mod_uni)$coefficients)[k + 1])
      }else{
         while (k + 1 < length(levels(DF[, var_uni]))) {
            i <- i + 1
            k <- k + 1
            ligneR <- getinfo_glm()
            vector_var[i] <- var_uni
            rslt[i + 1, ] <- c(ligneR[1:4], "-", "-", "-")
            row.names(rslt)[i + 1] <- ifelse(firth,paste0(var_uni,ligneR[5]),rownames(summary(mod_uni)$coefficients)[k + 1])
         }
      }
   }

   if (length(vector_firth) > 0){
      for (var in vector_firth){message("\ncomplete separation (Hauck-Donner phenomenon) occurred for ",var)}
      if (verbose) cat("\nThe Firth's Bias-Reduced Logistic Regression has been used to compute these variables")
   }


   if(verbose) cat("
---------------------------------------------------------------------------------------------------------------------
                   ")
   ##################################################








   ##################################################
   #               MULTIVARIATE MODEL               #
   ##################################################
   if (verbose) cat("
\n---+-----------------------------+-----------------------------------------------------------------------------------
   |                             |
   |    3) MULTIVARIATE MODEL    |
   |                             |
   +-----------------------------+\n
")

   explicatives_multi <- NULL
   alpha_multi <- alpha
   dev_matrix <- dev_matrix[order(dev_matrix[,2])] #variable avec le moins de déviance

   if (verbose) cat("\nThe method used for variable selection is the ",method," method\n\n")






   ############   ############   ############   ############   ############   ############
   #-------------------------------  LIMITE DE CLEANANCE  -------------------------------#
   row.names(rslt) <- NULL
   rslt
   ############   ############   ############   ############   ############   ############



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

      while(anova_glm < 0.2 & length(remainings) > 0){

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

               if(verbose){
                  o = 0
                  cat("\n\nVariables left for multiple analysis :")
                  for (var_left in level_var_multi) {
                     o <- o+1
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




   ##################################################
   #                 RESULT MATRIX                  #
   ##################################################
   OR <- exp(summary(mod_multi)$coefficients[, 1])
   pval <- summary(mod_multi)$coefficients[, 4]
   IC <- suppressMessages(confint(mod_multi))
   i <- 0

   for (OR_var in names(OR)[-1]) {#-1 remove intercept
      i <- i+1
      n_ligne <- match(OR_var,rownames(rslt))
      p <- round(pval[i+1], round)
      p <- ifelse(p == 0, "<0.001", p)
      IC_paste <- paste0("[", round(exp(IC[i + 1, 1]), round), ";", round(exp(IC[i + 1, 2]), round), "]")
      rslt[n_ligne, 5:7] <- c(signif(OR[i + 1], round), IC_paste, p)
   }

   for(n in 1:length(rslt[-1,4])){
      p = as.numeric(rslt[n+1,4])
      round(p, round) -> p
      ifelse(p == 0, "<0.001", p) -> rslt[n+1,4]
   }
   ##################################################

      row.names(rslt) <- NULL
      return(rslt)
}

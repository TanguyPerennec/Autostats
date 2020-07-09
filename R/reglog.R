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
            rowstimevariable = 10) {

   #To ignore the warnings during usage
   #options(warn=-1)
   #options("getSymbols.warning4.0"=FALSE)

   ##################################################
   #    Arguments verification / transformation     #
   ##################################################

   if (is.data.frame(DF) || is.matrix(DF) || is.tbl(DF)){
      DF <- as.data.frame(DF)
   } else{
      stop("No dataframe has been provided. Make sure 'DF' is a dataframe, a tibble or a matrix")
   }

   if (!is.vector(explicatives) & FALSE %in% (explicatives %in% colnames(DF)))
      stop("explicatives should be a vector of characters, part of DF")

   if (y %in% explicatives) {#if y is in 'explicatives' we must delete it
      message('y is part of "explicatives" and is removed from it')
      explicatives[-match(y, explicatives)]
   }

   #
   # y must be a character variable that correspond to a Bernoulli variable (binary code, 1 as event, 0 as no event, no NA) in DF
   #
   if (!is.character(y) ||!(y %in% colnames(DF)))
      stop("y must be a character variable, part of DF")
   DF <- DF[!is.na(DF[, y]), ] # we remove all NA in DF$y
   levels_y <- levels(as.factor(DF[, y])) #we get the levels of y that should be 0 or 1
   if (length(levels_y) != 2)
      stop("y should be a factor with 2 levels")
   if (FALSE %in% (levels_y %in% c(0,1))){
      DF[, y] <- as.character(DF[, y]) # prevents errors if DF[,y] is a factor
      stringr::str_detect(levels_y, "non") -> non_position
      stringr::str_detect(levels_y, "no ") -> no_position
      stringr::str_detect(levels_y, "not ") -> not_position
      if(non_position || no_position || not_position){#if there is a level with "no " or "non" or "not " in it, it will be the 0 level
         if(!is.na(match(TRUE,non_position))) match(TRUE,non_position) -> level_non
         if(!is.na(match(TRUE,no_position))) match(TRUE,no_position) -> level_non
         if(!is.na(match(TRUE,not_position))) match(TRUE,not_position) -> level_non
            DF[, y][DF[, y] == levels_y[level_non]] <- 0
            DF[, y][DF[, y] != 0] <- 1
         }else{#else it will be the first that will be 0
            DF[, y][DF[, y] == levels_y[1]] <- 0
            DF[, y][DF[, y] == levels_y[2]] <- 1
         }
   if(verbose)
      cat('\ny has been changed to 0/1 factor with ',levels_y[1],' = 0 and ',levels_y[2],' = 1')
   }
   DF[, y] <- as.factor(DF[, y])

   if (!is.logical(verbose))
      stop("'verbose' must be logical")

   if (!is.numeric(min_multivariate))
      stop("min_multivariate must be numeric")

   if (!is.numeric(round) || round <= 0)
      stop("round must be numeric and positive")

   if (!(method %in% c("backward","forward","stepwise","PS")))
      stop("'method' should be one of this methods : backward, forward, stepwise, PS")
   ##################################################


   ##################################################
   #               1) DATA CLEANING                 #
   ##################################################
   if (verbose) cat(
         "\n\n+-----------------------------+\n",
         "  1) DATA CLEANING :",
         "\n+-----------------------------+\n")


   # 1.1 Clean constant variables
   ######
   for (var_i in length(explicatives):1) {
      # Check for each variable 'var' in explicatives if
      # numeric variable is constant or if factor as only one level
      # and then deleate it
      var <- explicatives[var_i]

      if (is.numeric(DF[,var])) {
         if (sd(DF[,var], na.rm = TRUE) == 0) {
            #removes constant variable (sd = 0)
            if (verbose) message("\n", var, " is a constant value and is deleted")
            explicatives <- explicatives[-var_i]
         }
      }else{
         #convert each non numeric into a factor and check whether they have one level or not
         as.factor(DF[,var]) -> DF[,var]
         if (length(levels(DF[, var])) < 2) {
            #removes factor with only one level
            if (verbose) message("\n", var, " has only one level and is deleted")
            explicatives <- explicatives[-var_i]
         }else{
            # check if each level has values for each level of y
            #for (lev in table(DF[, var], DF[, y])) {
            #   if (lev == 0) explicatives <- explicatives[-var_i]
            #}
         }
      }
   }


   # 1.2 Deleting row with NA in it
   ######
   DF_glm <- subset.data.frame(DF, select = c(y, explicatives))
   DF_complete <- DF_glm[complete.cases(DF_glm),]

   if (verbose)
      cat("\nDropping all rows with NA in an explicative variable...")
      cat("\n",(nrow(DF_glm) - nrow(DF_complete))," rows deleted (",round(100*(nrow(DF_glm) - nrow(DF_complete))/(nrow(DF_glm)),0),"%)")


   if (verbose & (nrow(DF_complete) < rowstimevariable * length(DF_complete))) {
      cat("\n\nDropping all the rows with a NA in explicative variables drove to drop too many rows.")
      cat("\nColumns are deleted one by one from the column with the most NAs to the column with the less NA,
          till the new dataframe has as many rows as 10 times the number of variables remaining\n\n")
   }

   i = 0
   deleted_columns <- vector()
   nb_NA <- apply(DF_glm[, explicatives], 2, function(x) sum(is.na(x))) #nb of deleted NA by columns
   nb_NA <- nb_NA[order(-nb_NA)]

   while (nrow(DF_complete) < rowstimevariable * length(DF_complete)) {
      #if dropping all the rows with NA dropped more than "rowstimevariable" time the number of variables, we remove the line with the most NA
      i <- i + 1
      match(names(nb_NA[i]), explicatives) -> j
      deleted_columns[i] <- explicatives[j]
      progressbar(i = i,variable = explicatives[j],text = "Deleting columns... ",range = 5)
      explicatives <- explicatives[-j]
      DF_glm <- subset.data.frame(DF, select = c(y, explicatives))
      DF_complete <- DF_glm[complete.cases(DF_glm), ]
   }

   DF_glm <- DF_complete

   if (verbose) cat("\n\n\nDeleted columns are :", deleted_columns)

   for (name in explicatives) { #cleaning variables with only one level
      for (n_by_level in table(DF_glm[, name])) {
         if (n_by_level == 0) {
            if (verbose) message("\nSince NA cleaning, ",name,"has only one level and is deleted")
            explicatives <- explicatives[-match(name, explicatives)]
            break
         }
      }
   }

   DF_glm <- subset.data.frame(DF_glm, select = c(y, explicatives))

   if (verbose) cat("\n\nData cleaning is over.\n\nExplicatives variables remaining are :\n",explicatives,
         "\n\n+----------------------------------------+\n
         It remains ",length(explicatives),"variables and ",nrow(DF_glm),"observations",
         "\n\n##################################################\n")
   ######

   ##################################################


   # Constructing 'vect_explicative'
   #####
   vect_explicative <- vector()
   n = 1
   for (var in explicatives) {#making a vector with the name of the variable displayed as many times as (levels - 1)
      if (is.numeric(DF_glm[, var])) {
         vect_explicative[n] <- var
         n <- n + 1
      } else{
         length(levels(DF_glm[, var])) -> levels_var
         vect_explicative[n:(n + levels_var - 2)] <- rep(var, (levels_var - 1))
         n <- n + levels_var - 1
      }
   }
   #####



   ##################################################
   #               UNIVARIATE MODEL                 #
   ##################################################
   if (verbose) cat(
      "\n\n+-----------------------------+\n",
      "  2) UNIVARIATE MODEL :",
      "\n+-----------------------------+\n\n")

   rslt <- matrix("-", nrow = (length(vect_explicative) + 1), ncol = 7)
   rownames(rslt) <- rep(" ",(length(vect_explicative) + 1))
   rslt[1, ] <- c("", "OR", "IC", "p", "OR", "IC", "p")
   i = 0
   vector_var = vector()
   vector_firth <- vector()
   var_i = 0
   dev_matrix <- matrix(ncol = 2,nrow=length(explicatives))

   for (var_uni in explicatives) {
      progressbar(total = length(vect_explicative),i,variable = var_uni)

      DF_glm %>%
         select(y, var_uni) -> DF_uni

      complete_separation <- FALSE
      complete_separation <- tryCatch(glm(DF_uni, family = binomial, data = DF_uni,separation="find") -> mod_uni,
               error = function(e) {# if "fitted probabilities numerically 0 or 1 occurred"
                  msg <- paste0(var_uni," is causing perfect separation")
                  msg
                  firth <- TRUE
                  return(firth)
               },
               finally={}
               )
      complete_separation -> firth
      Clogg = FALSE

      if(complete_separation){
      # If there is a perfect separation : (Heinz et Al)
      #1. Omission of NV from the model : provides no information about the effect of this unusually strong and therefore important risk factor and furthermore does not allow adjusting effects of the other risk factors for the effect of NV. Therefore, this option is totally inappropriate.
      #2. Changing to a different type of model : Models whose parameters have di􏰃erent interpretations that are not risk-related (option 2) may be less appealing.
      #3. Use of an ad hoc adjustment (data manipulation) : While simple adjustments of cell frequencies can have undesirable properties (Agresti and Yang), Clogg et al. pursued a more elaborate approach: creat p = sum(yi/n) with y=(0,1) ; add pk/g artifficial responses and (1−p)k/g artifficial non-responses to each of the g groups of distinct risk factor patterns, and then to do a standard analysis on the augmented data set. k is the number of parameters to estimate
      Clogg_manipulation <- function(data=DF_uni){
         k = 2 #numberofparametre
         pi=vector()
         new_DF_uni <- as.matrix(data)
         row.names(new_DF_uni) <- NULL
         table(data) -> table_Clogg
         n_to_add = table(data)
         g_number <- length(colnames(table_Clogg))
         for (g in 1:length(colnames(table_Clogg))){
            n = sum(table_Clogg[,g])
            pi[g] = table_Clogg[2,g]/n
            n_to_add[1,g] <- (1-pi[g])*g_number/k
            n_to_add[2,g] <- (pi[g])*g_number/k
         }
         round(n_to_add,0) -> n_to_add
         for(col in 1:g){
            for (n in 1:2){
               if(n_to_add[n,col]> 0) {
                  new_ligne <- rep(colnames(n_to_add)[col],n_to_add[n,col])
                  new_matrix <- cbind(rep(as.numeric(n-1),n_to_add[n,col]),new_ligne)
                  rbind(new_DF_uni,new_matrix) -> new_DF_uni
               }
            }
         }
         new_DF_uni <- as.data.frame(new_DF_uni)
         table(new_DF_uni)
         table(data)
         return(new_DF_uni)
      }
      if (Clogg) Clogg_manipulation(DF_uni)
      #4. Exact logistic regression : permits replacement of the unsuitable maximum likelihood estimate by a median unbiased estimate [4]: let xir denote the value of the rth risk factor for individual i (16i6n; 26r6k) and let xi1=1 for all i. Then the median unbiased estimate of a parameter 􏰁r as well as corresponding inference are based on the exact null distribution of the su􏰂cient statistic Tr = 􏰊ni=1 yixir of 􏰁r, conditional on the observed values of the other su􏰂cient statistics Tr′ ; r′ ̸= r. An e􏰂cient algorithm is available to evaluate these conditional distributions [16] which should contain a su􏰂cient number of elements. This requirement may be violated with a single continuous risk factor but also with multiple dichotomous risk factors. In the endometrial cancer study we cannot apply exact logistic regression because there are two continuous risk factors in the model leading to degenerate distributions of all su􏰂cient stat 5) im
      #5. Standard analysis with BettaˆNV set to a ‘high’ value (for example, the value of BettaˆNV of that iteration at which the log-likelihood changed by less than 10−6).permits replacement of the unsuitable maximum likelihood estimate by a median unbiased estimate [4]: let xir denote the value of the rth risk factor for individual i (16i6n; 26r6k) and let xi1=1 for all i. Then the median unbiased estimate of a parameter 􏰁r as well as corresponding inference are based on the exact null distribution of the su􏰂cient statistic Tr = 􏰊ni=1 yixir of 􏰁r, conditional on the observed values of the other su􏰂cient statistics Tr′ ; r′ ̸= r. An e􏰂cient algorithm is available to evaluate these conditional distributions [16] which should contain a su􏰂cient number of elements. This requirement may be violated with a single continuous risk factor but also with multiple dichotomous risk factors. In the endometrial cancer study we cannot apply exact logistic regression because there are two continuous risk factors in the model leading to degenerate distributions of all su􏰂cient statistics.permits replacement of the unsuitable maximum likelihood estimate by a median unbiased estimate [4]: let xir denote the value of the rth risk factor for individual i (16i6n; 26r6k) and let xi1=1 for all i. Then the median unbiased estimate of a parameter 􏰁r as well as corresponding inference are based on the exact null distribution of the su􏰂cient statistic Tr = 􏰊ni=1 yixir of 􏰁r, conditional on the observed values of the other su􏰂cient statistics Tr′ ; r′ ̸= r. An e􏰂cient algorithm is available to evaluate these conditional distributions [16] which should contain a su􏰂cient number of elements. This requirement may be violated with a single continuous risk factor but also with multiple dichotomous risk factors. In the endometrial cancer study we cannot apply exact logistic regression because there are two continuous risk factors in the model leading to degenerate distributions of all su􏰂cient statistics.
      #6. Firth's method
      if(firth) logistf::logistf(DF[,y]~DF[,var_uni], data = DF_uni, pl = FALSE, firth = TRUE) -> mod_uni
      vector_firth <- c(vector_firth,var_uni)
      # 7. re-cast the model
      }


      #saving deviances for each model
      var_i <- var_i+1
      var_uni -> dev_matrix[var_i,1]
      if(firth){
         -2*mod_uni$loglik[2]-> dev_matrix[var_i,2]
      }else{
         mod_uni$deviance -> dev_matrix[var_i,2]
      }


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

      k = 0
      if (is.numeric(DF_glm[, var_uni])) {
         i <- i + 1
         k <- k + 1
         ligneR <- getinfo_glm()
         vector_var[i] <- var_uni
         ligne <- c(ligneR[1:4], "-", "-", "-")
         level_var <- ligneR[5]
         rslt[i + 1, ] <- ligne
         row.names(rslt)[i + 1]<- ifelse(firth,paste0(var_uni,level_var),rownames(summary(mod_uni)$coefficients)[k + 1])
      }else{
         while (k + 1 < length(levels(DF_glm[, var_uni]))) {
            i <- i + 1
            k <- k + 1
            ligneR <- getinfo_glm()
            vector_var[i] <- var_uni
            ligne <- c(ligneR[1:4], "-", "-", "-")
            level_var <- ligneR[5]
            rslt[i + 1, ] <- ligne
            row.names(rslt)[i + 1] <- ifelse(firth,paste0(var_uni,level_var),rownames(summary(mod_uni)$coefficients)[k + 1])
         }
      }
   }

   if (length(vector_firth) > 0){
      for (var in vector_firth){
         message("\ncomplete separation (Hauck-Donner phenomenon) occurred for ",var)
      }
      if (verbose) cat("\nThe Firth's Bias-Reduced Logistic Regression has been used to compute these variables")
   }






   ##################################################
   #               MULTIVARIATE MODEL               #
   ##################################################
   if (verbose) cat(
      "\n\n+-----------------------------+\n",
      " 3) MULTIVARIATE MODEL :",
      "\n+-----------------------------+\n\n")

   explicatives_multi <- NULL
   alpha_multi <- alpha


   # deviance par modele univarié
   dev_matrix[order(dev_matrix[,2])] -> dev_matrix_ordered #variable avec le moins de déviance


   #Getting all the p-val for each variable
   pvals_from_deviance <- function(datas=DF_glm,ynew=y,var,explicatives_multi_model=explicatives_multi){
      datas %>%
         select(ynew, all_of(explicatives_multi_model)) %>%
         glm(., family = binomial, data = .) -> model_multi
      summary(model_multi)$deviance -> deviance_total
      deviance_vars = vector()
      for (var in explicatives_multi_model){
         datas %>%
            select(ynew, all_of(explicatives_multi_model)) %>%
            select(-var) %>%
            glm(., family = binomial, data = .) -> model_multi_withoutvar
         summary(model_multi_withoutvar)$deviance -> deviance_total_withoutvar
         deviance_vars <- c(deviance_vars, deviance_total_withoutvar - deviance_total)
      }
      cbind(explicatives_multi_model,deviance_vars) -> synthese
      cbind(synthese,summary(model_multi)$coefficients[-1,4]) -> synthese
      return(pval)
   }


   if (verbose) cat("\nThe method used for variable selection is the ",method," method\n\n")

   if(method == "forward" || method == "stepwise"){# the two methods are equivalent but in stepwise, variables are removed if < alpha once in the model
   # For each step, a variable is added to the model
   # With deviance test, we add the variable Xj from which p-value of deviance test that compare the 2 models is minimal
   # Stop when each variables are added or if p-value is superior to threeshold

   # Methodes
   # test de déviance : possible seulement si modèles emboîtés
   # critère de choix :
      # - AIC : Akaike Informative Criterion
      # - BIC : Bayesian Informative Criterion
   # On choisira le modèle avec le plus petit AIC/BIC


      anova_glm = 1
      fin = 1

      wh0le(anova_glm <       dev_matrix_ordered -> remainings
      vector() -> used
0.2 & fin < length(dev_matrix_olength(remainingse == 0) r
         # 1er modèle réalisé avec le modèle avec la plus faible déviance
         formule = paste0(y,'~',dev_matrix_ordered[1])
         for (v in used){
            formule = paste0(formule,"+",v)
         }
         formule = formula(formule)
         firth <- FALSE
         firth <- tryCatch(glm(formule,data = DF_glm,family=binomial,separation="find") -> model1,
                           error = function(e) {# if "fitted probabilities numerically 0 or 1 occurred"
                              msg <- paste0(var_uni," is causing perfect separation")
                              msg
                              firth <- TRUE
                              return(firth)
                           },
                           finally={}
         )
         if(firth) logistf::logistf(formule, data=DF_glm, pl = FALSE, firth = TRUE) -> model1


         fin= fin +1
         nvlle_var <- paste0(dev_matrix_ordered[1:fin])
         expl <- all.vars(formule)[-1] #les anciennes variables
         nvlle_var <- c(nvlle_var,expl)
         nvlle_var <- paste0(nvlle_var, collapse= "+")
         formule2 <- paste(y,'~',nvlle_var)
         formule2 = formula(formule2)


         # les 2ème modèles potentiels models sont réalisés et comparés à model1
         for (var_notintegrated in remainings){
            
            "y~"
            for (ancienne_var in all.vars(formule)[-1]{
               paste0(formule2,'+',ancienne_var) -> formule2
            }
               paste0(formule2,'+',var_notintegrated) -> formule2
               formule2 = formula(formule2)
               print(formule2)
         }
         firth <- FALSE
         firth <- tryCatch(glm(formule2,data = DF_glm,family=binomial,separation="find") -> model2,
                           error = function(e) {# if "fitted probabilities numerically 0 or 1 occurred"
                              msg <- paste0(var_uni," is causing perfect separation")
                              msg
                              firth <- TRUE
                              return(firth)
                           },
                           finally={}
         )

         if(firth) logistf::logistf(formule2, data=DF_glm, pl = FALSE, firth = TRUE) -> model2

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
         critere(model1) -> deviance1
         critere(model2) -> deviance2
         diff_critere <- (deviance2-deviance1)
         anova(model2,model1)$pval -> anova_glm
         if(verbose){
            cat('\n\nLe modèle contenant les variables [',(all.vars(formule)),'] est comparé au modèle contenant [',(all.vars(formule2)),"]")
            cat('\n le modèle 1 à une déviance de ',deviance1,' contre ',deviance2,'pour le 2ème modèle')
            cat('\nle meilleur modèle est le modèle',ifelse(diff_critere>0,'1','2'))
     model_


      }ml1










      rank(rslt##########################[,,rownames(summary(mod_multi)$coefficients)) -> match_summary

            if(is.na(match_summary)){
               message(new_var,' may has a linear relation to another variable. It has been deleted in the multivariate model')
            }else{
            summary(mod_multi)$coefficients[match_summary,4] -> pval
            if(pval > alpha) explicatives_multi[explicatives_multi != new_expl] -> explicatives_multi
            }









            if (me "stepwise"){#variables are removed if < alpha once in the model
            summary(mod_multi)$coefficients[-1,4] -> pval_multi
            names(pval_multi[pval_multi > alpha]) -> name_inf_to_alpha
            for (expl_to_delete in name_inf_to_alpha){
               vector_var[match(expl_to_delete,rownames(rslt)[-1])] -> expl_to_delete
               #Checking for dependencies between variables
               explicatives_multi[explicatives_multi != expl_to_delete] -> explicatives_multi
            }
            }
         }
         DF_glm %>%
            select(y, all_of(explicatives_multi)) %>%
            glm(., family = binomial, data = .) -> mod_multi
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
               DF_glm %>%
                  select(y, all_of(explicatives_multi)) %>%
                  glm(., family = binomial, data = .) -> mod_multi

               pval <- summary(mod_multi)$coefficients[, 4] #all p-values
               pval <- pval[-1] #remove intercept
               i_pval <- rank(-pval) #vector with each position of pval ordered with 1 as the worst pval
               order_pval <- order(-pval)
               length_model <- length(pval) # != of length(explicatives) if levels > 2
               vect_explicative_multi <- rslt[,1] # vector with all explicatives_multi with each levels
               vect_explicative_multi <- vect_explicative_multi[-1] #removing empty first row
               level_var_multi <- rownames(summary(mod_multi)$coefficients) # == names(vect_explicative_multi) - NAs
               level_var_multi <- level_var_multi[-1]

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

         DF_glm %>%
            select(y, all_of(explicatives_multi_candidates)) %>%
            glm(., family = binomial, data = .) -> mod_multi
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
                  DF_glm %>%
                     select(y, all_of(explicatives_multi_candidates)) %>%
                     glm(., family = binomial, data = .) -> mod_multi_new
                  summary(mod_multi_new)$coefficients[-1,1] -> coeff_new #all coeffs without intercept
                  match(names(coeff_new),names(coeffs)) -> match_coeff_new #coressponding place in old coeffs for each new coeff
                  (coeff_new-coeffs[match_coeff_new])/coeffs[match_coeff_new] -> relative_changes

                  if(TRUE %in% relative_changes > 20/100){
                     #we keep the variable
                     explicatives_multi_candidates = c(new_var,explicatives_multi_candidates)
                  }else{
                     #we definitely delete the variable
                     explicatives_multi[explicatives_multi != new_var] -> explicatives_multi
                  }
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
         DF_glm %>%
            select(y, all_of(explicatives_multi)) %>%
            glm(., family = binomial, data = .) -> mod_multi

            }
   ##################################################


   ##################################################
   #              MATRICE DE RÉSULTATS              #
   ##################################################
   OR <- exp(summary(mod_multi)$coefficients[, 1]) #exp de la fonction logit
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

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
#' @details Method used to select the variables in the multivariate model can be set it the "method" parameter.\
#'  "backward" elimination is the default technique : the least significant effect that does not meet the \
#'  level "alpha" for staying in the model is removed. The process is repeated until no other effect in the \
#'  model meets the specified level for removal. \n In "forward" selection, the p-value is computed for each effect \
#'  not in the model and examines the largest of these statistics. If it is significant at entry level "alpha", \
#'  the corresponding effect is added to the model.Once an effect is entered in the model, it is never removed \
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
#'
#' @references Bursac, Z., Gauss, C.H., Williams, D.K. et al. Purposeful selection of variables in logistic regression. Source Code Biol Med 3, 17 (2008). https://doi.org/10.1186/1751-0473-3-17
#'
#' @examples
reglog <- function(DF,
            y,
            explicatives = colnames(DF)[colnames(DF) != y],
            alpha = 0.05,
            verbose = TRUE,
            min_multivariate=2,
            alpha_max=0.2,
            round = 3,
            method = "backward") {

   source('R/progressbar.R')

   ##################################################
   #    Arguments verification / transformation     #
   ##################################################

   if (is.data.frame(DF) || is.matrix(DF) || is.tbl(DF)){
      DF <- as.data.frame(DF)
   } else{
      stop("No dataframe has been provided. Make sure 'DF' is a dataframe, a tibble or a matrix")
   }

   if (!is.vector(explicatives))
      stop("explicatives should be a vector of characters")

   if (!is.character(y) ||!(y %in% colnames(DF)))
      stop("y must be a character variable, part of DF")

   if (y %in% explicatives)
      explicatives[-match(y, explicatives)]

   if (!is.logical(verbose))
      stop("'verbose' must be logical")

   if (!is.numeric(min_multivariate))
      stop("min_multivariate must be numeric")

   if (!is.numeric(round) || round <= 0)
      stop("round must be numeric and positive")

   if (!(method %in% c("backward","forward","stepwise")))
      stop("'method' should be one of this methods : backward,forward,stepwise")

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
            for (lev in table(DF[, var], DF[, y])) {
               if (lev == 0) explicatives <- explicatives[-var_i]
            }
         }
      }
   }
   ######

   # 1.2 Clean y and convert it to (1;0)
   ######
   DF[, y] <- as.factor(DF[, y])
   DF <- DF[!is.na(DF[, y]), ] #remove NA on y
   levels_y <- levels(DF[, y])
   if (length(levels_y) != 2) stop("y should be a factor with only 2 levels")
   DF[, y] <-  as.character(DF[, y])
   i = 0
   for (i in 1:2) { #replacing levels by 0 or 1
      DF[, y][DF[, y] == levels_y[i]] <- i
   }
   DF[, y] <- as.factor(DF[, y])
   ######

   # 1.3 Deleting row with NA in it
   ######
   DF_glm <- subset.data.frame(DF, select = c(y, explicatives))
   DF_complete <- DF_glm[complete.cases(DF_glm),]

   if (verbose) cat("\nDropping all rows with NA in an explicative variable...")
   if (verbose & (nrow(DF_complete) < 10 * length(DF_complete))) {
      cat("\n\nDropping all the rows with a NA in explicative variables drove to drop too many rows.")
      cat("\nColumns are deleted one by one from the column with the most NAs to the column with the less NA,
          till the new dataframe has as many rows as 10 times the number of variables remaining\n\n")
   }

   i = 0
   deleted_columns <- vector()
   nb_NA <- apply(DF_glm[, explicatives], 2, function(x) sum(is.na(x))) #nb of deleted NA by columns
   nb_NA <- nb_NA[order(-nb_NA)]

   while (nrow(DF_complete) < 10 * length(DF_complete)) {
      #if dropping all the rows with NA dropped more than 10 time the number of variables, we remove the line with the most NA
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
         "\n\n+----------------------------------------+\n")
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

   for (var_uni in explicatives) {
      progressbar(total = length(vect_explicative),i,variable = var_uni)

      DF_glm %>%
         select(y, var_uni) -> DF_uni

      glm(DF_uni, family = binomial, data = DF_uni) -> mod_uni

      #try to handle the "fitted probabilities numerically 0 or 1 occurred" warning message
      #####
      firth <- FALSE
      if(TRUE %in% (abs(mod_uni$coefficients[-1]) > 14)){#checking if coefficient is above 10 or under -10
         logistf::logistf(DF[,y]~DF[,var_uni], data = DF_uni, pl = TRUE, firth = TRUE) -> mod_uni
         firth <- TRUE
         vector_firth <- c(vector_firth,var_uni)
      }
      #####

      getinfo_glm <- function(mod = mod_uni){
         coef <- ifelse(firth,mod_uni$coefficients[k + 1],summary(mod_uni)$coefficients[k + 1,1])
         OR <- signif(exp(coef), round) #exp of logit function
         pval <- ifelse(firth,mod_uni$prob[k+1],summary(mod_uni)$coefficients[k + 1, 4])
         IC <- suppressMessages(confint(mod_uni))
         IC_paste <- paste0("[", round(exp(IC[k + 1, 1]), round), ";", round(exp(IC[k + 1, 2]), round), "]")
         name <- ifelse(firth,names(mod_uni$coefficients)[k + 1],rownames(summary(mod_uni)$coefficients)[k + 1])
         level_var <- str_split(name, ifelse(firth,"DF\\[, var_uni\\]",var_uni), n = 2)[[1]][2]
         name_var <- ifelse(level_var == "",var_uni,paste0(var_uni, "  (", level_var, ")"))
         name_var <- ifelse(firth,var_uni,name_var)
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
      message("\ncomplete separation (Hauck-Donner phenomenon) occurred for ",vector_firth)
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

      if (verbose) cat("\nThe method used for variable selection is the ",method," method\n\n")

      if(method == "forward"){
         rank(rslt[,4],ties.method = "first")-> ranking
         for (i in 1:(length(ranking)-1)){
            names(ranking[match(i,ranking)]) -> new_var #new_var is the var with the minimum p-value
            rslt[match(new_var,rownames(rslt)),1] -> new_expl
            str_split(new_expl," ")[[1]][1] -> new_expl #corresponding variable
            if (!(new_expl %in% explicatives_multi)){#once a variable is the multivariate model it never leaves it
               explicatives_multi <- c(explicatives_multi, new_expl)
               cat("\n",i,"...........",new_expl)
               DF_glm %>%
                  select(y, all_of(explicatives_multi)) %>%
                  glm(., family = binomial, data = .) -> mod_multi
            }
            match(new_var,rownames(summary(mod_multi)$coefficients)) -> match_summary
            if(is.na(match_summary)){
               message(new_var,' may has a linear relation to another variable. It has been deleted in the multivariate model')
            }else{
            summary(mod_multi)$coefficients[match_summary,4] -> pval
            if(pval > alpha) explicatives_multi[explicatives_multi != new_expl] -> explicatives_multi
            }
         }
      }

      if(method == "backward"){
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

      # MATRICE DE RÉSULTATS
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


      row.names(rslt) <- NULL
      return(rslt)
}

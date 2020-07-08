#' Logistic Regression Function
#'
#' @description reglog is used to make logistic regression and gives one matrix with variables, odds-ratios, confidence intervals and p-values of univariate and multivariate models.
#'
#'
#'
#' @param DF dataframe, matrix or tibble that contains all explicatives variables and the variable to explain.
#' @param y character : variable to explain.
#' @param explicatives character vector : variables that should explain y in the logistic regression. Takes all columns but y from the dataframe if kept empty.
#' @param method the method to be used to select variables in the multivariate model.The default method is the backward elimination. See details for more informations.
#' @param alpha num : significance threeshold used to delete non-significant variables in the multivariate model.
#' @param verbose logical : if TRUE, explainations are displayed in the console while running the function.
#' @param min_multivariate num : the minimum number of variables that should be kept in the multivariate model. If the number of significant variables using alpha threeshold is under min_multivariate, alpha is increased by 0.02 points till alpha_max is reached.
#' @param alpha_max num : maximum threeshold used to select the minimum multivariate variables wanted.
#' @param method
#' @param round num : number of digits to display in the final table.
#' @param rowstimevariable : minimum number of times row has to be bigger than variables
#' @param confirmation logical : ask confirmation before doing a transformation
#' @param exit character or character vector specifying the exit of the fonction : results in the console or saved in excel file
#' @param keep character or vector of character (optional) : variables that will be kept in the model no matter of the criteria. Every variable known in the litterature to have interaction with y or other 'keep' variable should be listed in 'keep'.
#'
#' @details Method used to select the variables in the multivariate model can be set it the "method" parameter.
#'  "backward" elimination is the default technique : the least significant effect that does not meet the
#'  level "alpha" for staying in the model is removed. The process is repeated until no other effect in the
#'  model meets the specified level for removal. In "forward" selection, the p-value is computed for each effect
#'  not in the model and examines the largest of these statistics. If it is significant at entry level "alpha",
#'  the corresponding effect is added to the model.Once an effect is entered in the model, it is never removed
#'  from the model. The process is repeated until none of the remaining effects meet the specified level for entry.
#'
#'
#' @return reglog returns a matrix with all OR obtain from univariate model and OR obtain from the multivariate model
#' @export
#' @import dplyr
#' @import logistf
#' @import stringr
#'
#' @references Bursac, Z., Gauss, C.H., Williams, D.K. et al. Purposeful selection of variables in logistic regression. Source Code Biol Med 3, 17 (2008). https://doi.org/10.1186/1751-0473-3-17
#' @references Heinze G, Schemper M. A solution to the problem of separation in logistic regression. Stat Med. 2002;21(16):2409-2419. doi:10.1002/sim.1047
reglog <- function(DF,
                  y,
                  explicatives = colnames(DF)[colnames(DF) != y],
                  keep = FALSE,
                  alpha = 0.05,
                  verbose = TRUE,
                  min_multivariate=2,
                  alpha_max=0.2,
                  round = 3,
                  method = "backward",
                  rowstimevariable = 10,
                  confirmation = TRUE,
                  exit = c("console","excel"))
{


   source("R/dataprep.R")
   source("R/multivariate_selection.R")
   source("R/logit.R")
   source("R/multivariate_selection.R")

   #To ignore the warnings during usage
   options(warn = -1)
   options("getSymbols.warning4.0" = FALSE)



   ##################################################
   #    Arguments verification / transformation     #
   ##################################################

   if (is.data.frame(DF) ||
       is.matrix(DF) ||
       is.tbl(DF))
   {
      DF <- as.data.frame(DF)
   } else {
      stop("No dataframe has been provided. Make sure 'DF' is a dataframe, a tibble or a matrix")
   }


   if (!is.vector(explicatives))
      stop("explicatives should be a vector of characters")

   #Removes explicatives not in DF
   explicatives_out_DF <- explicatives[!(explicatives %in% colnames(DF))]
   if (length(explicatives_out_DF) > 0)
   {
      msg_error <- explicatives_out_DF[1]
      if (length(explicatives_out_DF) > 1)
      {
         for (expl_out_DF in explicatives_out_DF[-1])
         {
            msg_error <- paste0(msg_error, ", ", expl_out_DF)
         }
         msg_error <- paste0(msg_error, " are not part of DF columns")
      } else{
         msg_error <- paste0(msg_error, " is not part of DF columns")
      }
      stop(msg_error)
   }


   if (!is.character(y) || !(y %in% colnames(DF)))
      stop("y must be a character variable, part of DF")

   if (y %in% explicatives)
   {
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

   if (!is.logical(confirmation))
      stop("'confirmation' must be logical")


   if (!is.logical(keep))
   {
      if (!is.character(keep) & !is.vector(keep))
         stop("keep should be a vector of character or characters")
      if (FALSE %in% (keep %in% explicatives))
         stop("some of keep elements are not in 'explicatives'")
   }

   ##################################################





   ##################################################
   #               1) DATA CLEANING                 #
   ##################################################
   if (verbose) cat(
"\n\n\n\n\n
\n---+-----------------------------+-------------------------------------------------------------------------------------------
   |                             |
   |      1) DATA CLEANING       |
   |                             |
   +-----------------------------+\n
")

   DF <- data_prep_complete(DF,y,verbose = TRUE,keep)
   explicatives <- colnames(DF)[colnames(DF) != y]
   ##################################################




   ##################################################
   #            2) UNIVARIATE MODELS                 #
   ##################################################
   if (verbose) cat(
"\n
\n---+-----------------------------+--------------------------------------------------------------------------------------------
   |                             |
   |    2) UNIVARIATE MODEL      |
   |                             |
   +-----------------------------+\n
")


   # Constructing 'vect_explicative'
   #
   vect_explicative <- vector()
   n = 1
   #making a vector with the name of the variable displayed as many times as (levels - 1)
   for (var in explicatives)
   {
      if (is.numeric(DF[, var]))
      {
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
   dev_matrix <- matrix(ncol = 2,nrow = length(explicatives)) #matrice de deviance


   getinfo_glm <- function(mod = mod_uni,
                           K = k,
                           var = var_uni)
      {
      coef <- mod$coefficients[K + 1]
      OR <- signif(exp(coef), round) #exp of logit function
      pval <- mod$prob[K + 1]
      IC <- suppressMessages(confint(mod))
      IC_paste <- paste0("[", round(exp(IC[K + 1, 1]), round), ";", round(exp(IC[K + 1, 2]), round), "]")
      name <- names(mod$coefficients)[K + 1]
      level_var <- stringr::str_split(name, var, n = 2)[[1]][2]
      name_var <- ifelse(level_var == "", var, paste0(var, "  (", level_var, ")"))
      ligne <- c(name_var, OR, IC_paste, pval, level_var)
      return(ligne)
      }


   for (var_uni in explicatives)
   {

      progressbar(total = length(vect_explicative),i,variable = var_uni)

      mod_uni <- logit(DF[,c(y,var_uni)])

      #saving deviances for each model
      var_i <- var_i + 1
      dev_matrix[var_i,] <- c(var_uni,-2*mod_uni$loglik[2])


      k = 0
      if (is.numeric(DF[, var_uni]))
      {
         i <- i + 1
         k <- k + 1
         ligneR <- getinfo_glm()
         vector_var[i] <- var_uni
         rslt[i + 1, ] <- c(ligneR[1:4], "-", "-", "-")
         row.names(rslt)[i + 1] <- paste0(var_uni,ligneR[5])
      } else{
         while (k + 1 < length(levels(DF[, var_uni]))) {
            i <- i + 1
            k <- k + 1
            ligneR <- getinfo_glm()
            vector_var[i] <- var_uni
            rslt[i + 1, ] <- c(ligneR[1:4], "-", "-", "-")
            row.names(rslt)[i + 1] <- paste0(var_uni,ligneR[5])
         }
      }
   }


   if (verbose) cat("
------------------------------------------------------------------------------------------------------------------------------
                   ")
   ##################################################



   ##################################################
   #               MULTIVARIATE MODEL               #
   ##################################################
   if (verbose) cat("\n
\n---+-----------------------------+--------------------------------------------------------------------------------------------
   |                             |
   |    3) MULTIVARIATE MODEL    |
   |                             |
   +-----------------------------+\n
")

   dev_matrix <- dev_matrix[order(dev_matrix[,2])] #variable avec le moins de déviance

   if (verbose)
      cat("\nThe method used for variable selection is the ",method," method\n\n")
   keep2 <- keep




   #==================================================================================================#
   #==================================================================================================#
   # CLEANANCE
   rslt
   #==================================================================================================#
   #==================================================================================================#





   explicatives_remainings <- multivariate_selection(DF,y,explicatives, principal_factor = FALSE,method = "backward",criteria = "AIC",check_interactions = FALSE,alpha = 0.05,keep = keep2)

   last_model <- logit(DF[,c(y,explicatives_remainings)])
   ##################################################








   ##################################################
   #                 RESULT MATRIX                  #
   ##################################################
   OR <- exp(last_model$coefficients)
   pval <- last_model$prob
   i <- 0


   for (OR_var in names(OR)[-1])
   {
      #-1 remove intercept
      i <- i + 1
      n_ligne <- match(OR_var, rownames(rslt))
      p <- round(pval[i + 1], round)
      p <- ifelse(p == 0, "<0.001", p)
      IC_paste <- paste0("[", round(exp(last_model$ci.lower[i + 1]), round), ";", round(exp(last_model$ci.upper[i + 1]), round), "]")
      try(rslt[n_ligne, 5:7] <- c(signif(OR[i + 1], round), IC_paste, p))
   }

   for (n in 1:length(rslt[-1, 4]))
   {
      p = as.numeric(rslt[n + 1, 4])
      round(p, round) -> p
      rslt[n + 1, 4] <- ifelse(p == 0, "<0.001", p)
   }
   ##################################################



   # EXIT
   ##################################################
   row.names(rslt) <- NULL
   if ("console" %in% exit)
   {
      cat("\n\n\n")
      print(rslt)
   }
   if ("excel" %in% exit)
   {
      excel(rslt, title_sheet = "logisitc regression", file_name = "results.xlsx")
   }
   ##################################################

   return(rslt)
}

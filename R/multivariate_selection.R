#' Multivariate selection
#'
#' @param DF dataframe
#' @param y character
#' @param explicatives vector of character : variables to be selected in the multivariate model
#' @param principal_factor (optional) : principal criteria to explain y. First variable to be selected and won't be removed from selection.
#' @param method character
#' @param criteria character
#' @param check_interactions logical
#' @param alpha num
#' @param verbose logical
#' @param keep character or vector of character (optional) : variables that will be kept in the model no matter of the criteria. Every variable known in the litterature to have interaction with y or other 'keep' variable should be listed in 'keep'.
#'
#' @return
#' @export
#'
#' @examples
multivariate_selection <-
   function(DF,
            y,
            explicatives = colnames(DF)[colnames(DF) != y],
            keep=FALSE,
            principal_factor=FALSE,
            method = "backward",
            criteria = "deviance",
            check_interactions = FALSE,
            alpha = 0.05,
            verbose=TRUE)
   {



      source("R/formulation.R")

      var_to_formula <- function(vars, y)
      {
         formule <- vars[1]
         for (t in 2:length(vars))
         {
            formule <- paste0(formule,"+", vars[t])
         }
         formule <- paste0(y, "~", formule)
         formule <- formula(formule)
         return(formule)
      }


      # MODEL 0
      if ("forward" %in% method)
      {
         formule <- formula(paste0(y,"~",1))
         model0 <- glm(formule,family = "binomial",data = DF) # NULL MODEL
      } else if ("backward" %in% method)
         model0 <- logit(DF[, c(y, colnames(DF)[colnames(DF) != y])]) # FULL MODEL


   explicatives_multi_test <- c("null model",explicatives)





   # BACKWARD MODEL
   ##############################
   if ("backward" %in% method)
   {

      #Determination of the principal factor : specified or determined by the variable for which the deviance is the smallest
      if (!is.logical(principal_factor))
      {
         if (!(principal_factor %in% explicatives))
            stop("the principal factor must be in 'explicatives'")
         if (verbose)
            cat("\nFirst variable set is <",principal_factor,">\n because it is the principal factor specifed")
         principal_factor -> keep[length(keep) + 1]
      }
      if (is.logical(principal_factor))
      {
         # Determination of univariate p-val for all elements
         models1 <- data.frame(row.names = explicatives_multi_test)
         for (j in seq(explicatives_multi_test))
         {
            if (j == 1)
            {
               model1 <- glm(DF[ ,y]~1, family = "binomial")
            } else
            {
               model1 <- glm(DF[ ,y]~DF[, explicatives_multi_test[j]], family = "binomial")
            }
            models1$deviance[j] <- model1$deviance
         }
         principal_factors <- models1[order(models1$deviance),,drop = FALSE]
         principal_factor <- rownames(principal_factors)[1]
         if (verbose)
            cat("\nFirst variable set is <",principal_factor,">\n because the model with this variable is better than all other models with 0 (null model) or 1 variable\n")

      }


      explicatives_remains <- explicatives[explicatives != principal_factor]
      formule <- formula(paste0(y,"~",principal_factor))
      model1 <- glm(formule,data = DF, family = "binomial")


      # Determination of interactions
      if (check_interactions)
      {
         interact_table <-  data.frame(row.names = explicatives_remains)
         for (i in seq(explicatives_remains))
         {
            formule1 <- paste(y, "~", principal_factor, "+", explicatives_remains[i])
            formule2 <- paste(formule1, "+", principal_factor, ":", explicatives_remains[i])
            formule1 <- formula(formule1)
            formule2 <- formula(formule2)
            model_interact <- glm(formule2,data = DF, family = "binomial")
            anova <- anova(model_interact,test = "LRT")
            pvalue <- anova$Pr[4]
            interact_table$pval[i] <- pvalue
         }
         interact_table <- interact_table[order(interact_table$pval),,drop = FALSE] #df with all p-values of likelihood ratio test between model with and without interaction
         signif_interact_table <- row.names(interact_table)[interact_table[,1] < 0.05]
         for (j in seq(signif_interact_table))
         {
            signif_interact_table[j] <- paste0(principal_factor,":",signif_interact_table[j])
         }
         interactives_remains <- signif_interact_table
      }


      # Backward selection for interaction
      if (check_interactions)
      {
         if (length(interactives_remains) > 0)
         {
            for (i in seq(interactives_remains))
            {
               formule <- formulation(DF[,c(y,explicatives_remains)])
               for (i in seq(interactives_remains))
               {
                  interactive_formule <-
                     ifelse(i == 1, interactives_remains[i], paste0("+", interactives_remains[i]))
               }
               newterms <- interactive_formule
               formule <- update(formule, ~ . + newterms)
               model0 <- glm(formule, data = DF, family = "binomial")
               pvals <- summary(model0)$coefficients[,4]

               #Elimination one by one of all interaction variable based on PVALUE
               interact_pval <- pvals[match(interactives_remains,names(pvals))]
               interact_pval <- interact_pval[order(-interact_pval)]
               if (interact_pval[1] > alpha)
               {
                  interactives_remains <- interactives_remains[interactives_remains != names(interact_pval)]
               }
            }
         }
      }


      vars_remainings <- explicatives
      formule <- formulation(DF[,c(y,explicatives)])
      last_model <- glm(formule, data = DF, family = "binomial")


      #Backward selection for all variables
      #####
      #Si la méthode descendante utilise un test de déviance, nous éliminons ensuite la variable
      #Xj dont la valeur p associée à la statistique de test de déviance est la plus grande. Nous
      #nous arrêtons lorsque toutes les variables sont retirées du modèle ou lorsque la valeur p est
      #plus petite qu’une valeur seuil.

      # Test 2 models // return p-val of the test corresponding to "critere
      ## CRITERIA
      # 1. significance criteria =to compare the log-likelihoods of 2 nested models : the variable with the
         # A. Wald test : only if there is 1 difference
         # B. Score test : only if there is 1 difference
         # C. Loglikelihood ratio test : to be prefered if multiple coeffs are tested => only valable way
      # 2. information criteria
      models_test <- function(model,
                              y,
                              var_diff,
                              critere_choix = "deviance")
      {
         if (!(critere_choix %in% c("deviance", "AIC", "BIC")))
            stop("criteria is not on the list")

         if (critere_choix == "deviance")
         {
            formule_diff_terms <- attr(model$terms, "term.labels")
            formule_diff_terms <- formule_diff_terms[formule_diff_terms != var_diff]
            formule_diff <- var_to_formula(formule_diff_terms,y)
            model2 <- glm(formule_diff, data = DF, family = "binomial")
            anova_test <- anova(model, model2, test = "LRT")
            pval <- anova_test$Pr[nrow(anova_test)]
         }
         return(pval)
      }





      while (length(vars_remainings) > 0)
      {
         model_test_df <- matrix(nrow = length(vars_remainings), ncol = 2)
         for (i in seq(vars_remainings))
         {
            pval <- models_test(model = last_model, y, var_diff = vars_remainings[i])
            model_test_df[i,] <- c(vars_remainings[i],pval)
         }
         model_test_df <- as.data.frame(model_test_df)
         colnames(model_test_df) <- c("name","pval")
         model_test_df[order(-model_test_df$pval),] -> model_test_df
         model_test_df$pval <- as.numeric(as.character(model_test_df$pval))
         model_test_df$name <- as.character(model_test_df$name)

         k <- 1
         while (model_test_df[k,1] %in% keep)
         {
            k <- k + 1
         }

         if (model_test_df[k,2] > alpha)
         {
            if (verbose)
               cat("\nThe best model exlude <",model_test_df[k,1], "> (with likelihood ratio test : p =",round(as.numeric(model_test_df[k,2]),3),"), so this variable is now excluded")
            vars_remainings <- vars_remainings[vars_remainings != model_test_df[k,1]]
         }
         else
         {
            if (verbose)
               cat("\nThe best model do not exlude any variable of the variables remaining (with risk",alpha,"):",vars_remainings)
            break
         }
      }

      return(vars_remainings)
      #####
   }

}

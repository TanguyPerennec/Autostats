multivariate_selection <-
   function(DF,
            y,
            explicatives = colnames(DF)[colnames(DF) != y],
            principal_factor=NULL,
            method = "backward",
            criteria = "Wald",
            check_interactions = TRUE,
            alpha = 0.05)
   {
      # MODEL 0
      if ("forward" %in% method)
      {
         model0 <- glm(DF[,y] ~ 1,family = "binomial") # NULL MODEL
      } else if ("backward" %in% method)
         model0 <- logit(DF[, c(y, colnames(DF)[colnames(DF) != y])]) # FULL MODEL


   explicatives_multi_test <- c("null model",explicatives)

   if ("backward" %in% method)
   {
      #Determination of the principal factor : wether specified or the variable for which the deviance is the smallest
      #####
      if (is.null(principal_factor)) {
         # Determination of univariate p-val for all elements
         models1 <- data.frame(row.names = explicatives_multi_test)
         for (j in seq(explicatives_multi_test))
         {
            if (j == 1)
               model1 <- glm(DF[ ,y]~1, family = "binomial")
            else
               model1 <- glm(DF[ ,y]~DF[, explicatives_multi_test[j]], family = "binomial")
            models1$deviance[j] <- model1$deviance
         }
         principal_factors <- models1[order(models1$deviance),,drop = FALSE]
         principal_factor <-  rownames(principal_factors)[1]
      }
      #####


      explicatives_remains <- explicatives[explicatives != principal_factor]

      model1 <- glm(DF[ ,y]~DF[, principal_factor], family = "binomial")


      # Determination of interactions
      #####
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
      #####

      # Backward selection for interaction
      #ERREUR
      #####
      source("R/formulation.R")
      if (length(interactives_remains) > 0)
      {
         for (i in seq(interactives_remains))
         {
         formule <- formulation(DF[,c(y,explicatives_remains)])
            for (i in seq(interactives_remains))
            {
               interactive_formule <- paste0("+",interactives_remains[i])
            }
            newterms <- interactive_formule
            formule <- update(formule, ~ . newterms)
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
      #####


      #Backward selection for all
      #####
      #Si la méthode descendante utilise un test de déviance, nous éliminons ensuite la variable
      #Xj dont la valeur p associée à la statistique de test de déviance est la plus grande. Nous
      #nous arrêtons lorsque toutes les variables sont retirées du modèle ou lorsque la valeur p est
      #plus petite qu’une valeur seuil.


      #####






   }



 vars_remainings <- explicatives



#####
#MODEL1
   model1 <- logit(DF_multi1)
#####


#####
#MODEL 2
   newvar <- vars_remainings[i]
   DF_multi2 <- DF[,colnames(DF_multi1,newvar)]
   model2 <- logit(DF_multi2)
#####

#####
#COMPARISON OF MODEL 1 AND MODEL 2
## CRITERIA
# 1. significance criteria =to compare the log-likelihoods of 2 nested models :
      # A. Wald test : only if there is 1 difference
      # B. Score test : only if there is 1 difference
      # C. Loglikelihood ratio test : to be prefered if multiple coeffs are tested => only valable way
# 2. information criteria


   # Test entre 2 models
   models_test <- function(model,
                           y,
                           var_diff,
                           critere_choix = "deviance")
   {
      if (!(critere_choix %in% c("deviance", "AIC", "BIC")))
         stop("criteria is no on the list")

      if (critere_choix == "deviance")
      {
         formule_diff_terms <- attr(model$terms, "term.labels")
         formule_diff_terms <- formule_diff_terms[formule_diff_terms != var_diff]
         formule_diff <- paste0(y, "~", formule_diff_terms[1])
         for (t in seq(formule_diff_terms[-1]))
         {
            formule_diff <- paste0(formule_diff, "+", formule_diff_terms[t])
         }
         formule_diff <- formula(formule_diff)
         model2 <- glm(formule_diff, data = DF, family = "binomial")
         anova_test <- anova(model, model2, test = "LRT")
         pval <- anova_test$Pr[nrow(anova_test)]
      }
      return(pval)
   }






      if (logistf::is.logistf(model))
         {
         deviance <- -2 * (model$loglik[1])
         } else{
         deviance <- model$deviance
         }
      return(deviance)
      #AIC
      #BIC
   }




}












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
   #4. Exact logistic regression : permits replacement of the unsuitable maximum likelihood estimate by a median unbiased estimate [4]: let xir denote the value of the rth risk factor for individual i (16i6n; 26r6k) and let xi1=1 for all i. Then the median unbiased estimate of a parameter 􏰁r as well as corresponding inference are based on the exact null distribution of the su􏰂cient statistic Tr = 􏰊ni=1 yixir of 􏰁r, conditional on the observed values of the other su􏰂cient statistics Tr′ ; r′ ̸= r. An e􏰂cient algorithm is available to evaluate these conditional distributions [16] which should contain a su􏰂cient number of elements. This requirement may be violated with a single continuous risk factor but also with multiple dichotomous risk factors. In the endometrial cancer study we cannot apply exact logistic regression because there are two continuous risk factors in the model leading to degenerate distributions of all su􏰂cient stat 5) im
   #5. Standard analysis with BettaˆNV set to a ‘high’ value (for example, the value of BettaˆNV of that iteration at which the log-likelihood changed by less than 10−6).permits replacement of the unsuitable maximum likelihood estimate by a median unbiased estimate [4]: let xir denote the value of the rth risk factor for individual i (16i6n; 26r6k) and let xi1=1 for all i. Then the median unbiased estimate of a parameter 􏰁r as well as corresponding inference are based on the exact null distribution of the su􏰂cient statistic Tr = 􏰊ni=1 yixir of 􏰁r, conditional on the observed values of the other su􏰂cient statistics Tr′ ; r′ ̸= r. An e􏰂cient algorithm is available to evaluate these conditional distributions [16] which should contain a su􏰂cient number of elements. This requirement may be violated with a single continuous risk factor but also with multiple dichotomous risk factors. In the endometrial cancer study we cannot apply exact logistic regression because there are two continuous risk factors in the model leading to degenerate distributions of all su􏰂cient statistics.permits replacement of the unsuitable maximum likelihood estimate by a median unbiased estimate [4]: let xir denote the value of the rth risk factor for individual i (16i6n; 26r6k) and let xi1=1 for all i. Then the median unbiased estimate of a parameter 􏰁r as well as corresponding inference are based on the exact null distribution of the su􏰂cient statistic Tr = 􏰊ni=1 yixir of 􏰁r, conditional on the observed values of the other su􏰂cient statistics Tr′ ; r′ ̸= r. An e􏰂cient algorithm is available to evaluate these conditional distributions [16] which should contain a su􏰂cient number of elements. This requirement may be violated with a single continuous risk factor but also with multiple dichotomous risk factors. In the endometrial cancer study we cannot apply exact logistic regression because there are two continuous risk factors in the model leading to degenerate distributions of all su􏰂cient statistics.
   #6. Firth's method
   if(firth) logistf::logistf(DF[,y]~DF[,var_uni], data = DF_uni, pl = FALSE, firth = TRUE) -> mod_uni
   vector_firth <- c(vector_firth,var_uni)
   # 7. re-cast the model
}

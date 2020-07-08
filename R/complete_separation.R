########### REFERENCES ##############
# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqwhat-is-complete-or-quasi-complete-separation-in-logisticprobit-regression-and-how-do-we-deal-with-them/
# https://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression


#1) make sure that y is not a dichotomous version of a variable in the model

#' Check dichotomous distribution for one variable
#'
#' @param DF : a dataframe
#'
#' @return check_dichotomous returns a boolean TRUE if y is a dichotomous variable of x
#' @export
#'
#' @examples
check_dichotomous <-
   function(var,y,DF)
   {

      table(DF[, y], DF[, var]) -> table
      table[1, ] -> table0
      table[2, ] -> table1
      table0[table0 != 0] -> value0
      table1[table1 != 0] -> value1
      as.numeric(names(value0)) -> names_value0
      as.numeric(names(value1)) -> names_value1
      check <- FALSE
      if (TRUE %in% c(is.na(names_value0), is.na(names_value1))) {
         cat("not all numerics")
      } else{
         names_value0[order(names_value0)] -> names_value0
         names_value1[order(names_value1)] -> names_value1
         if (max(names_value0) < min(names_value1)) {
            check <- TRUE
            cat(
               "\n\n",
               y,
               " is fully explicated by ",
               var,
               " :\n
             x = (",
               trunc(min(names_value0)),
               ":",
               trunc(min(names_value1)),
               "[     y = 0
             x = (",
               trunc(min(names_value1)),
               ":",
               (trunc(max(names_value1)) + 1),
               "[     y = 1"
            )
         }
         if (min(names_value0) > max(names_value1)) {
            check <- TRUE
            cat(
               "\n\n",
               y,
               " is fully explicated by ",
               var,
               " :\n
             x = (",
               trunc(min(names_value1)),
               ":",
               trunc(min(names_value0)),
               "[     y = 1
             x = (",
               trunc(min(names_value0)),
               ":",
               (trunc(max(names_value0)) + 1),
               "[     y = 0"
            )
         }
      }
      return(check)
   }

#2)If it is quasi-complete separation, the easiest strategy is the "Do nothing" strategy. This is because that the maximum likelihood for other predictor variables are still valid. The drawback is that we don’t get any reasonable estimate for the variable X that actually predicts the outcome variable effectively.  This strategy does not work well for the situation of complete separation.
#Another simple strategy is to not include X in the model. The problem is that this leads to biased estimates for the other predictor variables in the model. Thus, this is not a recommended strategy.
#Possibly we might be able to collapse some categories of X if X is a categorical variable and if it makes substantive sense to do so.
#Exact method is a good strategy when the data set is small and the model is not very large.
#Firth logistic regression is another good strategy. It uses a penalized likelihood estimation method. Firth bias-correction is considered as an ideal solution to separation issue for logistic regression
#Bayesian method can be used when we have some additional information on the parameter estimates of the predictor va



###################
#  CHOICES
###################
complete_separation <- function(var,
                                y,
                                DF,
                                continue = FALSE)
   {

   if (is.numeric(DF[,var]) || length(levels(as.factor(DF[,var]))) == 2)
   {
      dichotomous <- check_dichotomous(var,y,DF)
      dicho_msg <- ifelse(
         dichotomous,
         paste0("(",var,"seems to be a dichotomous variation of ",y,")"),
         "(not advised)"
      )
   }else
   {
      dicho_msg <- "(not advised)"
   }




   if (continue == FALSE)
   {
      cat(
         "
      What method should be applied to",
         var,
         " ?
       1. Omission in the model.",
         dicho_msg,
       "2. Changing to a different type of model.
       3. Use of an ad hoc adjustment (data manipulation).
       4. Exact logistic regression [13].
       5. Standard analysis with betaˆNV",
         var,
         "et to a ‘high’ value (for example, the value of 􏰀ˆNV ",
         var,
         " that iteration at which the log-likelihood changed by less than 10−6)."
      )#
      choice = 0
      while (!(choice %in% c(1, 2, 3, 4, 5))) {
         choice <- readline("Choice ? (1,2,3,4,5)       ")
      }
      }else
      {
         choice <- continue
      }


   if (grepl(1,choice))
   {
      DF <- DF[DF != DF[, var]]
   }

}

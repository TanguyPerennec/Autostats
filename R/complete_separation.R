########### REFERENCES ##############
# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqwhat-is-complete-or-quasi-complete-separation-in-logisticprobit-regression-and-how-do-we-deal-with-them/
# https://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression


#1) make sure that y is not a dichotomous version of a variable in the model

#' Check dichotomous distribution for one variable
#'
#' @param DF : a dataframe
#' @param var : the variable
#' @param y : the variable to explain
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
      names_value0 <- names(value0)
      names_value1 <- names(value1)
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





###############
#2) Data manipulation
clogg_transformation <- function(DF)
   {
      DF <- as.data.frame(DF)
      if (ncol(DF) > 2)
         stop("clogg transformation is only treated for univariate analysis")
      k = nrow(DF) #numberofparametre
      pi = vector()
      new_DF_uni <- as.matrix(DF)
      row.names(new_DF_uni) <- NULL
      table(DF) -> table_Clogg
      n_to_add <- table(DF)
      g_number <- length(colnames(table_Clogg))
      for (g in 1:length(colnames(table_Clogg)))
      {
         n = sum(table_Clogg[, g])
         pi[g] = table_Clogg[2, g] / n
         n_to_add[1, g] <- (1 - pi[g]) * k/g_number
         n_to_add[2, g] <- (pi[g]) * k/g_number
      }
      for (c in 1:2)
      {
         for (d in 1:2)
         {
            for (n in seq(n_to_add[c, d]))
            {
               DF[k + n, 1] <- rownames(n_to_add)[c]
               DF[k + n, 2] <- colnames(n_to_add)[d]
            }
            k <- nrow(DF)
         }
      }
   DF <- as.data.frame(DF)
   return(DF)
   }



#  CHOICES
###################
#' Choice of complete separation method
#'
#' @param var variable to be evaluate
#' @param y the variable to explain
#' @param DF the dataframe
#' @param continue (optional)
#'
#' @return
#' @export
#'
#'
#' @examples
complete_separation <- function(var,
                                y,
                                DF,
                                continue = FALSE)
   {

   if (is.numeric(DF[,var]) || length(levels(as.factor(DF[,var]))) == 2)
   {
      #dichotomous <- check_dichotomous(var,y,DF)
      dicho_msg <- "(not advised)" #décommenter lorsque check_dicotomous
      #ifelse(
       #  dichotomous,
       #  paste0("(",var,"seems to be a dichotomous variation of ",y,")"),
       #  "(not advised)"
      #)
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
       1. Omission in the variable.",
         dicho_msg,
       "
       2. Changing to a different type of model (not available)
       3. Use of an ad hoc adjustment (Clogg transformation)
       4. Exact logistic regression (not available)
       5. Standard analysis with beta set to a ‘high’ value (ex : so that an iteration change the log-likelihood by less than 10−6) (not available)
       6. Firth's Bias-Reduced Logistic Regression"

      )#
      choice = 0
      available <- c("1","6")
      msg_a <- " "
      for (h in available)
      {
         msg_a <- paste0(msg_a,"  ",h)
      }
      msg <- paste0("\nChoice (",available," ) ?     ")
      while (!(choice %in% available))
      {
         choice <- readline(msg)
      }
   }else
   {
      choice <- continue
   }

   if (grepl("1",choice))
   {
      DF <- DF[DF != DF[, var]]
      as.data.frame(DF) -> DF
      DF$method[1] <- "logit"
   }
   if (grepl("2",choice))
   {
      DF <- clogg_transformation(DF)
      DF <- as.data.frame(DF)
      DF$method[1] <- "logit"
   }
   if (grepl("6",choice))
   {
      DF$method[1] <- "firth"
   }
   if (grepl("c",choice))
   {
      continue <- strsplit(choice,"c")[[1]]
   }
   DF$continue[1] <- continue
   return(DF)
}

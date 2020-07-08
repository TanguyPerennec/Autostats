#' Function Logit
#'
#' @param DF a dataframe
#' @param y (optional) a variable to explain by logit function. If kept empty, y is the first element of DF.
#'
#' @return
#' @export
#' @import safeBinaryRegression
#'
#' @examples
logit <- function(DF,y = colnames(DF)[1], response = FALSE){

   source("R/formulation.R")
   source("R/dataprep.R")
   source("R/complete_separation.R")


   # 1) Getting the elements
   ##################################################
   formule <- formulation(DF,y)
   #####


   # 2) Verify conditions and transformation
   ##################################################
   if (!is.factor(DF[, y]))
   {
      DF <- as.data.frame(DF)
      DF[,y] <- tobinary(DF[, y])
   }
   #####


   # 3) Regression
   ##################################################
   error_glm <- TRUE
   response <- FALSE
   firth_method <- FALSE

   while (error_glm)
   {
      tryCatch(
      {
         error_glm <- FALSE
         model <- glm(formule, data = DF, family = "binomial")
      },
      error = function(er)
      {
         error_glm <- TRUE
         if (grepl("separation", er))
         {
            cat("\nComplete or quasi complete separation occured for : ")
            for (var in colnames(DF)[-1])
            {
               cat("\n- ", var)
            }
            DF <- complete_separation(var, y, DF, continue = response)
            response <- DF$continue[1]
            DF$continue <- NULL
            if (DF$method[1] == "firth")
            {
               cat("\n logistic regression using Firth's Bias-Reduced Logistic regression has been performed\n")
               firth_method = TRUE
               error_glm <- FALSE
            }
            DF$method <- NULL
         }else
         {
            stop(er)
         }
      })
   }

      #####

   logistf::logistf(formule, data = DF,firth = firth_method) -> model
   return(model)
}




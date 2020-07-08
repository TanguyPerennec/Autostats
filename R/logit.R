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
   while (error_glm)
   {
      tryCatch(
      {
         model <- glm(formule, data = DF, family = "binomial")
      },
      error = function(er)
      {
         if (grepl("separation", er))
         {
            cat("\nComplete or quasi complete separation occured for : ")
            for (var in colnames(DF)[-1])
            {
               if (grepl(var, er))
               {
                  cat("\n- ", var)
                  DF <- complete_separation(var, y, DF, continue = response)
                  response <- DF$continue[1]
                  DF$continue <- NULL

                  if (DF$method == "Firth")
                  {
                     DF$method <- NULL
                     logistf::logistf(formule, data = DF)
                     error_glm <- FALSE
                  } else{
                     DF$method <- NULL
                  }
               }
            }
         }else {
            stop(er)
         }
      })
   }


      #####


   return(model)
}




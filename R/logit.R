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
logit <- function(DF,y=colnames(DF)[1]){

   source("R/formulation.R")
   source("R/dataprep.R")
   source("R/complete_separation.R")


   # 1) Getting the elements
   ##################################################
   formule <- formulation(DF,y)
   #####


   # 2) Verify conditions and transformation
   ##################################################
   if (!is.factor(DF[,y]))
   {
      DF <- as.data.frame(DF)
      DF[,y] <- tobinary(DF[,y])
   }
   #####


   # 3) Regression
   ##################################################
   tryCatch(
      {
         model <- glm(formule, data = DF,family = "binomial")
      },
         error = function(er)
         {
            if (grepl("separation",er))
            {
               cat("\nComplete separation for : ")
               for (var in colnames(DF)[-1])
               {
                  if (grepl(var,er))
                  {
                     cat("\n- ",var)
                     DF <- complete_separation(var,y,DF)
                  }
               }
            }
         }

   )
   confint(model)
   #####


   return(model)
}




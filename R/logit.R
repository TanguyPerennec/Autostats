#' Function Logit
#'
#' @param DF a dataframe
#' @param y (optional) a variable to explain by logit function. If kept empty, y is the first element of DF.
#'
#' @return
#' @export
#'
#' @examples
logit <- function(DF,y=colnames(DF)[1]){

   source("R/formulation.R")
   source("R/dataprep.R")

   # 1) Getting the elements
   ##################################################
   formule <- formulation(DF,y)
   #####


   # 2) Verify conditions and transformation
   ##################################################
   if (!is.factor(DF[,y])){
      DF <- as.data.frame(DF)
      DF[,y] <- tobinary(DF[,y])
   }
   #####


   # 3) Regression
   ##################################################
   model <- glm(formule, data = DF,family = "binomial")
   #####


   return(model)
}




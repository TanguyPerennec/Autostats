#' Object to formule transformation
#'
#' @param object object that can be coerced into a formula object
#' @param y (optional) : the variable that will be the first term of the formula. If kept empty, y will be the first element in the object given to the function.
#'
#' @return formulation returns a formula object
#' @export formule
#'
#' @examples explicatives <- c("event","sexe","weight")
#' @examples formule <- formulation(explicatives)
formulation <- function(object,y=object[1]){




   # 1. Change explicatives if y is in it
   #############################################
   if(y %in% explicatives)
      explicatives <- explicatives[-y]
   #####




   # 2. Getting a vector with all explicatives into 'object'
   ###########################################################
   if(is.data.frame(object) || is.matrix(object) || is.tbl(object)){
      if(is.null(colnames(object))){
         object <- object[1,]
      }else{
         object <- colnames(object)
      }
   }else{
      if(is.vector(object) || is.character(object)){
         if(length(object) == 0) stop("object is null")
      }else{
         stop("the object is not cohercible into formula")
      }
   }
   #####




   # 3. Coerce object to formula
   #############################################
   formule <- paste0(y,"~",object[1])
   for(explicative in object[-1]){
      formule <- paste0(formule,"+",explicative)
   }
   formule <- formula(formule)
   ###########




   return(formule)
}



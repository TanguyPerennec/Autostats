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
formulation <- function(object,y=NULL){




   # 1. Getting y
   #############################################
   if(is.null(y)){
      if(is.data.frame(object) || is.matrix(object) || is.tbl(object)){
         y <- colnames(object)[1]

      }else{object <- object[1]}
   }
   #####




   # 2. Remove y from object
   #############################################
   if(is.data.frame(object) || is.matrix(object) || is.tbl(object)){
      object <- as.data.frame(object)
      object %>%
         select(-y) -> object
   }else{object[object != y]}
   #####




   # 2. Getting a vector with all explicatives into 'object'
   ###########################################################
   if(is.data.frame(object) || is.matrix(object) || is.tbl(object)){
      if(is.null(colnames(object))){
         object <- object[1,]
      }else{object <- colnames(object)}
   }else{
      if(is.vector(object) || is.character(object)){
         if(length(object) == 0) stop("object is null")
      }else{stop("the object is not cohercible into formula")}
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



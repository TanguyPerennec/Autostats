
#' Function Logit
#'
#' @param DF a dataframe
#' @param y a variable to explain by logit function
#'
#' @return
#' @export
#'
#' @examples
logit <- function(DF,y=colnames(DF)[1]){

}


explicatives <- colnames(DF)
if(y %in% explicatives)
   explicatives[-y]

explicatives = "caca"

formule <- paste0(y,"~",explicatives[1])
for(explicative in explicatives[-1]){
   formule <- paste0(formule,"+",explicative)
}

formule()

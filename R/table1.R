#' Table 1
#'
#' @param DF dataframe : datas
#' @param y character : columns that separte the dataframe
#' @param ynames vector of characters (optionnal) : names to be put in table one columns instead of y levels.
#' @param overall booleen (optionnal) : TRUE if an "overall" column is wanted
#' @param mutation numeric : number of categories to display for one variable. If more than "mutation" categories, the categories after this threeshold are wrapped into a "others" categorie.
#'
#' @return results are in a matrix
#' @export
#' @import stringr
#' @import stats
#' @examples
table1 <- function(DF,y,ynames=NULL,overall=TRUE,mutation=40){
   # first transformations
   DF <- as.data.frame(DF)
   DF[,y] <- as.factor(DF[[y]])
   if(is.null(ynames)) ynames <- levels(DF[,y])
   # param verification
   if(!is.data.frame(DF)) stop("datas must be a dataframe")
   if(!is.character(y) || !(y %in% colnames(DF))) stop("y must be a character variable, part of DF")
   if(!is.vector(ynames)) stop("ynames should be a vector of characters")
   if(length(ynames) != length(levels(DF[,y]))) stop("ynames should be of as many labels than y levels")
   if(!is.logical(overall)) stop("'overall' should be a booleen")
   if(!is.numeric(mutation)) stop("'mutation' should be numeric")


   # Paramètre de base
   i <- 1
   levels_y <- length(levels(DF[,y]))
   tabf <- matrix(nrow = 1,ncol=levels_y+2)
   ligne <- c("",ynames,"")
   tabf <- ligne

   for(var in DF[,2:length(DF)]){

      i <- i + 1
      varname <- colnames(DF)[i]
      ligne <- varname

      if (is.numeric(var)){
         mean_vars <- aggregate(var~DF[,y],FUN="mean",na.rm=T)
         sd_vars <- aggregate(var~DF[,y],FUN="sd",na.rm=T)
         for (j in 1:levels_y){ #pour chaque modalité
            mean_vars_level <- round(mean_vars[j,2],2)
            sd_vars_level <- round(sd_vars[j,2],2)
            ligne <- c(ligne,paste0(mean_vars_level,"±",sd_vars_level))
         }
         verif_level <- margin.table(table(var,DF[,y]),2) #verif prevents to have a table with 0 in a level
         verif <- TRUE
         for(lev in verif_level){
            if(lev == 0) verif <- FALSE
         }
         if(verif & !is.na(sd(var))){
            p <- signif(t.test(var~DF[,y])$p.value,3)
         }else{clig <- NA}

         ligne <- c(ligne,p)
         tabf <- rbind(tabf,ligne) #ajout de la ligne au tableau
      }else{
         var <- stringr::str_to_lower(var)
         var <- stringr::str_replace(var, "é", "e")
         var <- stringr::str_replace(var, "è", "e")
         var <- stringr::str_replace(var, "/", "")
         for (it in var){
            if(!is.na(it)){
            if(stringr::str_length(it) > 20){
               var[var == it] <- paste0(substr(it,1,20),"...")
            }
            }
         }
         var <- as.factor(var)
         if(length(levels(var)) >= mutation){
            nvar <- as.vector(var)
            for(other_levels_i in mutation:length(levels(var))){
               other_levels <- levels(var)[other_levels_i]
               nvar[nvar == other_levels] <- "others"
            }
            nvar -> var
            as.factor(var) -> var
         }
         if(length(levels(var)) >= 2) {
         tb <- table(DF[,y],var) # on crée un tableau croisé
         tbm <- margin.table(tb,1) # Sommes par ligne
         verif_level <- margin.table(table(var,DF[,y]),2)
         verif <- TRUE
         for(lev in verif_level){
            if(lev == 0) verif <- FALSE
         }
         if(verif){
         clig <- chisq.test(var,DF[,y])$p.value  # Calcul du Chi2
         clig <- signif(clig,3)
         }else{clig <- NA}

         if (length(levels(var)) == 2){   # Variables à 2 niveaux
            if (levels(var)[1] == "non"){var <- relevel(var,"oui")}
            ligne <- paste0(ligne, " (",levels(var)[1],") - no. (%)")

            for (j in 1:levels_y){
               no <- tb[j,1]
               pn <- signif(100*no/tbm[j],3)
               ligne <- c(ligne,paste0(no," (",pn,"%)"))
            }
            ligne <- c(ligne,clig)
            tabf <- rbind(tabf,ligne) #ajout de la ligne au tableau
         }else{
            ligne <- c(paste0(varname," - no. (%)"),rep(" ",levels_y),clig)
            tabf <- rbind(tabf,ligne)
            for (n in 1:length(levels(var))){ #Pour chaque niveau de la variable
               ligne <- paste0("        ",levels(var)[n])
               for (j in 1:levels_y){
                  no <- tb[j,n]
                  pn <- signif(100*no/tbm[j],3)
                  ligne <- c(ligne,paste0(no," (",pn,"%)"))
               }
               ligne <- c(ligne," ")
               tabf <- rbind(tabf,ligne) #ajout de la ligne au tableau
            }
         }
      }
      }
   }
   row.names(tabf) <- NULL
   return(tabf)
}

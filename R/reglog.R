#' Regression logistique
#'
#' @param DF dataframe : dataframe with y and all explicative variables.
#' @param y character : variable to explain.
#' @param explicatives character vector : variables that should explain y with logistic regression. Take all columns but y from the dataframe if kept empty.
#' @param alpha num : significance threeshold.
#'
#' @return rslt
#' @export
#' @import dplyr
#'
#' @examples
reglog <- function(DF,y,explicatives=colnames(DF)[colnames(DF) != y],alpha=0.05){

   ### first transformations and verifications ###
   if(is.data.frame(DF) || is.matrix(DF)){
      DF <- as.data.frame(DF)
      }else{stop("No dataframe has been provided. Make sure 'DF' is a dataframe or a matrix")}
   if(!is.vector(explicatives)) stop("explicatives should be a vector of characters")
   if (y %in% explicatives) explicatives[-match(y,explicatives)]
   if(!is.character(y) || !(y %in% colnames(DF))) stop("y must be a character variable, part of DF")

   for (var_i in length(explicatives):1){ #cleaning dataset
      explicatives[var_i] -> var
      if(is.numeric(DF[,var])){
         if(sd(DF[,var],na.rm = TRUE) == 0) explicatives <- explicatives[-var_i]#removes constant variable
      }else{
         as.factor(DF[,var]) -> DF[,var]
         if(length(levels(DF[,var])) < 2){#removes factor with only one level
            explicatives <- explicatives[-var_i]
         }else{
            for(lev in table(DF[,var],DF[,y])){
               if(lev == 0) explicatives <- explicatives[-var_i]
            }
         }
      }
   }


   #cleaning y
   DF[,y] <- as.factor(DF[,y])
   levels(DF[,y]) -> levels_y
   if(length(levels_y) != 2) stop("y should be a factor with only 2 levels")
   DF[,y] <-  as.character(DF[,y])
   i=0
   for (level in levels_y){#replacing levels by 0 or 1
      DF[,y][DF[,y]==level] <- i
      i <- 1+i}
   DF[,y] <- as.factor(DF[,y])
   DF <- DF[!is.na(DF[,y]),] #removing NA on y
   ###

   #### UNIVARIATE###
   # Drop all of the rows with NA
   DF_glm <- subset(DF,select = c(y,explicatives))
   DF_glm[complete.cases(DF_glm),] -> DF_glm
   apply(DF_glm[,explicatives],2,function(x)sum(is.na(x))) -> nb_NA
   nb_NA[order(-nb_NA)] -> nb_NA
   i = 0
   while(nrow(DF_glm) < 10*length(DF_glm)){#if dropping all the rows with NA dropped all the rows, we remove the line with the most NA
      i <- i + 1
      cat("\nthere is no intersection between all variables. ",nb_NA[i]," is the column with the more NAs and is deleted\n")
      match(names(nb_NA[i]),explicatives) -> j
      explicatives <- explicatives[-j]
      DF_glm <- subset(DF,select = c(y,explicatives))
      DF_glm[complete.cases(DF_glm),] -> DF_glm
   }
   cat("remainings columns are :", explicatives)

   for(name in explicatives){#cleaning variables with only one level.
      for (n_by_level in table(DF_glm[,name])){
         if(n_by_level == 0){
         explicatives <- explicatives[-match(name,explicatives)]
         break
         }else{}
      }
   }

   DF_glm %>%
      select(y,all_of(explicatives)) -> DF_glm
   mod1 <- glm(DF_glm,family = binomial,data = DF_glm)

   # MATRICE DE RÉSULTATS
   HR <- exp(summary(mod1)$coefficients[,1]) #exp de la fonction logit
   pval <- summary(mod1)$coefficients[,4]
   rslt <- matrix(nrow = (length(explicatives)+1),ncol = 3)
   rslt[1,] <- c("","OR","p")
   i <- 0
   round <- 3
   for (col in explicatives){
      i <- i + 1
      p <- round(pval[i+1],round)
      p <- ifelse(p == 0,"<0.001",p)
      rslt[i+1,] <- c(col,signif(HR[i+1],round),p)
   }
   return(rslt)
}


############
##########

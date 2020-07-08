source("R/progressbar.R")
source("R/logit.R")

#' Coerce a column into a binary column
#'
#' @param col column to change into a binary column
#' @param verbose : TRUE to display some information in the console
#'
#' @return
#' @export
#' @import stringr
tobinary <- function(col,
                     verbose = T)
{
   names(col) -> name
   levels_col <- levels(as.factor(col)) #get the levels of col (that should be 0 or 1)

   if (length(levels_col) != 2)
      stop("col should be a factor with 2 levels")

   if (FALSE %in% (levels_col %in% c(0, 1))) {
      col <- as.character(col) # prevents errors if DF[,col] is a factor
      stringr::str_detect(levels_col, "non") -> non_position
      stringr::str_detect(levels_col, "no ") -> no_position
      stringr::str_detect(levels_col, "not ") -> not_position
      if (non_position ||
          no_position ||
          not_position)
      {
         #if there is a level with "no " or "non" or "not " in it, it will be the 0 level
         if (!is.na(match(TRUE, non_position)))
            match(TRUE, non_position) -> level_non
         if (!is.na(match(TRUE, no_position)))
            match(TRUE, no_position) -> level_non
         if (!is.na(match(TRUE, not_position)))
            match(TRUE, not_position) -> level_non
         col[col == levels_col[level_non]] <- 0
         col[col != 0] <- 1
      } else
      {
         #else it will be the first that will be 0
         col[col == levels_col[1]] <- 0
         col[col == levels_col[2]] <- 1
      }
      if (verbose)
         cat(
            name,
            '\n has been changed to 0/1 factor with ',
            levels_col[1],
            ' = 0 and ',
            levels_col[2],
            ' = 1'
         )
   }
   col <- as.factor(col)
   return(col)
}



#' Looking for complete NA separation
#'
#' @param DF dataframe
#' @param verbose logical : TRUE to display some information in the console
#'
#' @return
#' @export
#'
#' @examples
NA_separation <-
   function(DF,
            verbose=T)
{
   DF <- as.data.frame(DF)
   variables = colnames(DF)
   i_p = 0
   for (variable1 in variables)
   {
      #select variables with more than n/factor NAs
         for (variable2 in variables[variables != variable1])
            {
            progressbar(i = i_p,total = length(variables) * (length(variables) - 1),variable = variable1)
            i_p = i_p + 1
            DFtest <- DF[, c(variable1, variable2)]
            nb_NA <- sum(is.na(DFtest[, 2]))
            if (length(levels(as.factor(DF[, 1]))) == 2 &
                length(levels(as.factor(DF[, 2]))) == 2) {
               if (nb_NA > (nrow(DF)/length(levels(as.factor(DF[, 2])))))
                  {
                  table(DFtest[, 1], DFtest[, 2], useNA = "always") -> table
                  addmargins(table) -> table
                  for (i in 1:2)
                  {
                     if (table[i, 3] == table[i, 4])
                     {
                        if (table[i, 3] == table[4, 3])
                        {
                           cat("complete NA separation")
                        } else{
                           cat("uncomplete NA separation")
                        }
                     }
                  }
               }
            }
         }
      }
   }




NA_rm_for_glm <-
   function(DF,
            y,
            rowlimit_factor = 10,
            min_explicatives = 10,
            floor_pval = 0.5,
            verbose = T,
            method_NA = c("lessNA", "significance"),
            keep = FALSE)
{
   DF <- as.data.frame(DF)
   explicatives1 <- colnames(DF)[colnames(DF) != y]
   DF <- DF[!is.na(DF[,y]),]
   DF_complete <- DF[complete.cases(DF),]
   explicatives <- colnames(DF)[colnames(DF) != y]

   if (nrow(DF_complete) < rowlimit_factor * length(DF_complete))
   {

      nb_NA <- apply(DF[, explicatives], 2, function(x) sum(is.na(x))) #nb NA by columns
      nb_NA <- nb_NA[!(keep %in% names(nb_NA))]
      nb_NA <- nb_NA[order(-nb_NA)]
      i = 1

      if ("significance" %in% method_NA)
      {
         for (var in names(nb_NA))
         {
            DF_uni <- subset.data.frame(DF, select = c(y, var))
            DF_uni <- DF_uni[complete.cases(DF_uni),]
            if (nrow(DF_uni) < 5 || length(levels(as.factor(DF_uni[,2]))) < 2)
            {
               explicatives <- explicatives[explicatives != var]
            }else
            {
               model <- stats::glm(formula = DF_uni[,y]~DF_uni[,var],family = "binomial")
               p_valanova <- anova(model,formula(DF_uni[,y]~1),test = "Chisq")$Pr[2]
               if (p_valanova > floor_pval)
               {#if all pvals are > 0.5
                  explicatives <- explicatives[explicatives != var]
                  if (verbose)
                     cat("\n\n\n",var," is non significant with overall population (",floor_pval," threeshold) and is deleted")
               }
            }
         }
      }

      DF <- DF[,c(y,explicatives)]
      DF_complete <- DF[complete.cases(DF),]

      if ("lessNA" %in% method_NA)
      {
         while (nrow(DF_complete) < rowlimit_factor * length(DF_complete) & (length(explicatives) > min_explicatives) & i < length(nb_NA))
         {
            nb_NA <- apply(DF[, explicatives], 2, function(x) sum(is.na(x))) #nb NA by columns
            nb_NA <- nb_NA[!(keep %in% names(nb_NA))]
            nb_NA <- nb_NA[order(-nb_NA)]
            nb_NA <- nb_NA[nb_NA > 0]
            explicatives <- explicatives[explicatives != names(nb_NA[1])]
            progressbar(i = i,variable = names(nb_NA[1]),text = "Deleting columns... ",range = 5)
            DF <- subset.data.frame(DF, select = c(y, explicatives))
            DF_complete <- DF[complete.cases(DF), ]
            i <- i + 1
         }
      }
   }

   DF_complete -> DF
   deleted_columns <-  explicatives1[!(explicatives1 %in% explicatives)]
   if (verbose) cat("\n\n\nDeleted columns are :", deleted_columns)
   return(DF_complete)

}




checkforfactor <-
   function(DF,
            vars = colnames(DF),
            confirmation = TRUE,
            verbose = TRUE)
{
   DF <- as.data.frame(DF)
   for (var_i in length(vars):1)
   {
      # Check for each variable 'var' in vars if numeric variable is constant
      # and if factor as only one level
      # and then deleate it if so
      var <- vars[var_i]

      if (is.numeric(DF[,var]))
      {
         as.factor(DF[,var]) -> DF[,var]
         if (length(levels(DF[, var])) < 2) {#removes factor with only one level
            if (verbose) message("\n", var, " is constant and is deleted")
            vars <- vars[-var_i]
         }
         rep <- "N"
         if (length(levels(DF[, var])) < 7) {
            message("\n", var, " is a numeric variable with only",length(levels(DF[, var]))," different values and could be considered as a factor")
            if (confirmation)
            {
               rep <- readline(paste0("Change",var,"into factor ? O/N"))
            }else
            {
               rep <- "O"
            }
            if (rep != 'O') as.numeric(DF[,var]) -> DF[,var]
         }else
         {
            as.numeric(DF[,var]) -> DF[,var]
         }
      }else
      {#convert each non numeric into a factor and check whether they have one level or not
         as.factor(DF[,var]) -> DF[,var]
         if (length(levels(DF[, var])) < 2)
         {
            #removes factor with only one level
            if (verbose)
               message("\n", var, " has only one level and is deleted")
            vars <- vars[-var_i]
         }
      }
   }
   return(DF)
}




format_data <-
   function(DF,
            type=NULL)
{
   as.data.frame(DF) -> DF

   if ("plain" %in% type)
   {
      for (i in 1:length(DF))
      {
         if (is.character(DF[,i]))
         {
            DF[,i] <- stringr::str_to_lower(DF[,i])
            DF[,i] <- stringr::str_replace_all(DF[,i],"[éèêë]","e")
            DF[,i] <- stringr::str_replace_all(DF[,i],"[àâ]","a")
            DF[,i] <- stringr::str_replace_all(DF[,i],"[ï]","i")
            DF[,i] <- stringr::str_replace_all(DF[,i],"[ù]","u")
            DF[,i] <- stringr::str_replace_all(DF[,i],"[ôö]","o")
         }
      }
   }

   return(DF)
}



######################################################
#########      DATA PREP COMPLETE            #########
######################################################


data_prep_complete <-
   function(DF,
            y,
            verbose = TRUE,
            keep = FALSE
   )
{
   verbose = T

   DF <- as.data.frame(DF)
   DF1 <- DF

   # Caractere preparation
   DF <- format_data(DF)

   # Data prep of y
   DF[,y] <- tobinary(DF[,y])



   # get rid of NAs
   DF <- NA_rm_for_glm(DF,y,keep = FALSE)

   # Clean constant variables
   DF <- checkforfactor(DF)

   if (verbose)
      cat("\n",(nrow(DF1) - nrow(DF))," rows deleted (",round(100*(nrow(DF1) - nrow(DF))/(nrow(DF1)),0),"%)","...........",nrow(DF),"rows remaining")

   explicatives <- colnames(DF)[colnames(DF) != y]

   if (verbose) cat("\n\nData cleaning is over.\n\nExplicatives variables remaining are :\n",explicatives,
   "It remains ",length(explicatives),"variables and ",nrow(DF),"observations
   \n\n+-----------------------------------------------------------------------------------------------------------------+\n")

   DF <- as.data.frame(DF)

   return(DF)
}





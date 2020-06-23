#' Regression logistique
#'
#' @param DF dataframe : dataframe with y and all explicative variables.
#' @param y character : variable to explain.
#' @param explicatives character vector : variables that should explain y with logistic regression. Take all columns but y from the dataframe if kept empty.
#' @param alpha num : significance threeshold.
#' @param verbose logical : TRUE for explaination in the console
#'
#' @return rslt
#' @export
#' @import dplyr
#' @import MASS
#'
#' @examples
reglog <-
   function(DF,
            y,
            explicatives = colnames(DF)[colnames(DF) != y],
            alpha = 0.05,
            verbose = TRUE,
            min_multivariate=1) {

      ### setup
      round = 3

      ### first transformations and verifications ###
      if (is.data.frame(DF) || is.matrix(DF)) {
         DF <- as.data.frame(DF)
      } else{
         stop("No dataframe has been provided. Make sure 'DF' is a dataframe or a matrix")
      }
      if (!is.vector(explicatives))
         stop("explicatives should be a vector of characters")
      if (y %in% explicatives)
         explicatives[-match(y, explicatives)]
      if (!is.character(y) ||
          !(y %in% colnames(DF)))
         stop("y must be a character variable, part of DF")
      if (!is.logical(verbose))
         stop("'verbose' must be logical")
      if (!is.numeric(min_multivariate))
         stop("min_multivariate must be numeric")

      ##########################
      #   CLEANING DATASET     #
      ##########################
      if (verbose) {
         cat(
            "\n\n+-----------------------------+\n  1) CLEANING DATAFRAME :\n+-----------------------------+\n"
         )
      }
      for (var_i in length(explicatives):1) {
         explicatives[var_i] -> var
         if (is.numeric(DF[, var])) {
            if (sd(DF[, var], na.rm = TRUE) == 0) {
               if (verbose) {
                  message("\n", var, " is a constant value and is deleted")
               }
               explicatives <- explicatives[-var_i]
            }#removes constant variable
         } else{
            as.factor(DF[, var]) -> DF[, var]
            if (length(levels(DF[, var])) < 2) {
               #removes factor with only one level
               if (verbose) {
                  message("\n", var, " has only one level and is deleted")
               }
               explicatives <- explicatives[-var_i]
            } else{
               for (lev in table(DF[, var], DF[, y])) {
                  if (lev == 0)
                     explicatives <- explicatives[-var_i]
               }
            }
         }
      }

      #cleaning y
      DF[, y] <- as.factor(DF[, y])
      levels(DF[, y]) -> levels_y
      if (length(levels_y) != 2)
         stop("y should be a factor with only 2 levels")
      DF[, y] <-  as.character(DF[, y])
      i = 0
      for (level in levels_y) {
         #replacing levels by 0 or 1
         DF[, y][DF[, y] == level] <- i
         i <- 1 + i
      }
      DF[, y] <- as.factor(DF[, y])
      DF <- DF[!is.na(DF[, y]), ] #removing NA on y
      ###

      # Drop all of the rows with NA
      if (verbose) {
         message("\nDropping all the rows with a NA in an explicative variable...")
      }
      DF_glm <- subset(DF, select = c(y, explicatives))
      DF_glm[complete.cases(DF_glm), ] -> DF_glm
      apply(DF_glm[, explicatives], 2, function(x)
         sum(is.na(x))) -> nb_NA
      nb_NA[order(-nb_NA)] -> nb_NA
      i = 0
      m = 1
      deleted_columns <- vector()
      wait_sign <- c("-", "\\", "|", "/")
      if (verbose &
          nrow(DF_glm) < 10 * length(DF_glm)) {
         cat(
            "\nDropping all the rows with a NA in explicative variables drove to drop too many rows.\nColumns are deleted one by one from the column with the most NAs to the column with the less NA till the new dataframe has as many rows as 10 times the number of variables remaining\n\n"
         )
      }
      while (nrow(DF_glm) < 10 * length(DF_glm)) {
         #if dropping all the rows with NA dropped all the rows, we remove the line with the most NA
         i <- i + 1
         i %% 40
         match(names(nb_NA[i]), explicatives) -> j
         if (i %% 10 == 0) {
            m <- m + 1
         }
         cat('\r',
             "   ",
             wait_sign[m %% 4 + 1],
             "Deleting columns... ",
             explicatives[j])
         deleted_columns[i] <- explicatives[j]
         explicatives <- explicatives[-j]
         DF_glm <- subset(DF, select = c(y, explicatives))
         DF_glm[complete.cases(DF_glm), ] -> DF_glm
      }
      if (verbose) {
         cat("\n\n\nDeleted columns are :", deleted_columns)
      }

      for (name in explicatives) {
         #cleaning variables with only one level.
         for (n_by_level in table(DF_glm[, name])) {
            if (n_by_level == 0) {
               if (verbose) {
                  message("\nSince NA cleaning, ",
                          name,
                          "has only one level and is deleted")
               }
               explicatives <- explicatives[-match(name, explicatives)]
               break
            } else{
            }
         }
      }

      DF_glm %>%
         select(y, all_of(explicatives)) -> DF_glm
      if (verbose) {
         cat(
            "\n\nData cleaning is over.\nExplicatives variables remaining are :\n",
            explicatives,
            "\n+----------------------------------------+\n"
         )
      }
      ##########################


      vect_explicative <- vector()
      n = 1
      for (var in explicatives) {
         #making a vector with the name of the variable displayed as many times as (levels - 1)
         if (is.numeric(DF_glm[, var])) {
            vect_explicative[n] <- var
            n <- n + 1
         } else{
            length(levels(DF_glm[, var])) -> levels_var
            vect_explicative[n:(n + levels_var - 2)] <-
               rep(var, (levels_var - 1))
            n <- n + levels_var - 1
         }
      }
      vect_explicative

      ##########################
      #       UNIVARIATE       #
      ##########################
      rslt <- matrix("-", nrow = (length(vect_explicative) + 1), ncol = 7)
      rslt[1, ] <- c("", "OR", "IC", "p", "OR", "IC", "p")
      i = 0
      for (var_uni in explicatives) {
         DF_glm %>%
            select(y, var_uni)  %>%
            glm(., family = binomial, data = .) -> mod_uni
         k = 0
         if (is.numeric(DF_glm[, var_uni])) {
            i <- i + 1
            k <- k + 1
            OR <- round(exp(summary(mod_uni)$coefficients[k + 1, 1]), round) #exp of logit function
            pval <- round(summary(mod_uni)$coefficients[k + 1, 4], round)
            pval <- ifelse(pval == 0, "<0.001", pval)
            IC <- confint(mod_uni)
            IC_paste <- paste0("[", round(exp(IC[k + 1, 1]), round), ";", round(exp(IC[k + 1, 2]), round), "]")
            level_var <- str_split(rownames(summary(mod_uni)$coefficients)[k + 1], var_uni, n=2)[[1]][k + 1]
            name_var <- ifelse(level_var == "",var_uni,paste0(var_uni, "  (", level_var, ")"))
            name_var
            ligne <- c(name_var, OR, IC_paste, pval, "-", "-", "-")
            rslt[i + 1, ] <- ligne
         } else{
            while (k + 1 < length(levels(DF_glm[, var_uni]))) {
               i <- i + 1
               k <- k + 1
               OR <-
                  round(exp(summary(mod_uni)$coefficients[k + 1, 1]), round) #exp of logit function
               pval <-
                  round(summary(mod_uni)$coefficients[k + 1, 4], round)
               pval <- ifelse(pval == 0, "<0.001", pval)
               IC <- confint(mod_uni)
               IC_paste <-
                  paste0("[",
                         round(exp(IC[k + 1, 1]), round),
                         ";",
                         round(exp(IC[k + 1, 2]), round),
                         "]")
               level_var <-
                  str_split(rownames(summary(mod_uni)$coefficients)[k + 1], var_uni, n =
                               2)[[1]][2]
               name_var <-
                  ifelse(level_var == "",
                         var_uni,
                         paste0(var_uni, "  (", level_var, ")"))
               ligne <- c(name_var, OR, IC_paste, pval, "-", "-", "-")
               names(ligne) <- NULL
               rslt[i + 1, ] <- ligne
            }
         }
      }




      ##########################
      #       MULTIVARIATE     #
      ##########################
      explicatives_multi = NULL
      alpha_multi <- alpha

      while (length(explicatives_multi) < min_multivariate) {
         #so as to make new model with alpha = alpha + 0.02 if there is no variable left
         pas <- 0
         nb_p <- 0 #number of significant p
         explicatives_multi <- explicatives
         length_expl <- vector()
         length_expl[pas + 1] <- length(explicatives_multi) # so as to recorded model length to stop if an infinite boucle append
         length_model <- 1 # so as to initiate the loop

         while (nb_p < length_model) {
            #while all of variables left (with each levels) in the model are not significants
            pas <- pas + 1
            ## Model with all (new) explicatives_multi variables
            DF_glm %>%
               select(y, all_of(explicatives_multi)) %>%
               glm(., family = binomial, data = .) -> mod_multi
            if(pas==1) rownames(rslt) <- rownames(summary(mod_multi)$coefficients)
            pval <- summary(mod_multi)$coefficients[, 4] #all p-values
            pval <- pval[-1] #remove intercept
            length_model <- length(pval) # != of length(explicatives) if levels > 2
            vect_explicative_multi <- rslt[,1] # vector with all explicatives_multi with each levels
            n = 1
            for (var_multi in explicatives_multi) {
               #making a vector with the name of the variable displayed as many times as (levels - 1)
               if (is.numeric(DF_glm[, var_multi])) {
                  vect_explicative_multi[n] <- var_multi
                  n <- n + 1
               } else{
                  length(levels(DF_glm[, var_multi])) -> levels_var_multi
                  vect_explicative_multi[n:(n + levels_var_multi - 2)] <- rep(var_multi, (levels_var_multi - 1))
                  n <- n + levels_var_multi - 1
               }
            }
            i_pval <- order(-pval) #vector with each position of pval ordered with 1 as the worst pval
            l <- 0
            for (k in i_pval) {
               l + 1 -> l
               if (k == 1) {
                  match(vect_explicative_multi[l],explicatives_multi) -> j
                  message(explicatives_multi[j]," is deleted, the worst p-val so far ( p=", pval[j],")")
                  explicatives_multi <- explicatives_multi[-j]
                  break
               }
            }
            length_expl[pas + 1] <- length(explicatives_multi)
            nb_p <- length(pval[pval < alpha_multi])
            if (length(explicatives_multi) == length_expl[pas]) break
         }
         alpha_multi <- alpha_multi + 0.02
         message("There is no variable with significative OR in the new multivariate model (with alpha = ",alpha_multi - 0.02,")")
         message("                >> trying with alpha = ", alpha_multi)
      }


      # MATRICE DE RÉSULTATS
      OR <- exp(summary(mod_multi)$coefficients[, 1]) #exp de la fonction logit
      pval <- summary(mod_multi)$coefficients[, 4]
      IC <- confint(mod_multi)
      i <- 0

      for (OR_var in names(OR)[-1]) {#-1 remove intercept
         i <- i+1
         n_ligne <- match(OR_var,rownames(rslt))

         p <- round(pval[i+1], round)
         p <- ifelse(p == 0, "<0.001", p)
         IC_paste <- paste0("[", round(exp(IC[i + 1, 1]), round), ";", round(exp(IC[i + 1, 2]), round), "]")
         rslt[n_ligne, 5:7] <- c(signif(OR[i + 1], round), IC_paste, p)
         }
      row.names(rslt) <- NULL
      return(rslt)
   }


############
##########

#' Table 1
#'
#' @param DF dataframe : datas
#' @param y character : columns that separte the dataframe
#' @param ynames vector of characters (optionnal) : names to be put in table one columns instead of y levels.
#' @param overall booleen (optionnal) : TRUE if an "overall" column is wanted
#' @param mutation numeric : number of categories to display for one variable. If more than "mutation" categories, the categories after this threeshold are wrapped into a "others" categorie.
#' @param legend booleen (optionnal) : TRUE if a legend is wanted under the table
#' @param title booleen (optionnal) : TRUE if a title is wanted
#'
#' @return results are in a dataframe
#' @export
#' @import stringr
#' @import stats
#' @examples
table1 <-
   function(DF,
            y,
            ynames = NULL,
            overall = TRUE,
            mutation = 40,
            legend = FALSE,
            title = FALSE) {


      ##################################################
      #    Arguments verification / transformation     #
      ##################################################

      if (is.data.frame(DF) || is.matrix(DF)) {
         DF <- as.data.frame(DF)
      } else{
         stop("No dataframe has been provided. Make sure 'DF' is a dataframe or a matrix")
      }

      if (!is.character(y) ||!(y %in% colnames(DF)))
         stop("y must be a character variable, part of DF")

      DF[, y] <- as.factor(DF[[y]])
      levels_y <- length(levels(DF[, y]))

      if (is.null(ynames))
         ynames <- levels(DF[, y])

      if (!is.data.frame(DF))
         stop("datas must be a dataframe")

      if (!is.character(y) || !(y %in% colnames(DF)))
         stop("y must be a character variable, part of DF")

      if (!is.vector(ynames))
         stop("ynames should be a vector of characters")

      if (length(ynames) != length(levels(DF[, y])))
         stop("ynames should be of as many labels than y levels")

      if (!is.logical(overall))
         stop("'overall' should be a booleen")

      if (!is.logical(legend))
         stop("'legend' should be a booleen")

      if (!is.logical(legend))
         stop("'title' should be a booleen")

      if (!is.numeric(mutation))
         stop("'mutation' should be numeric")

      ##################################################


      ##################################################
      #                First two lines                 #
      ##################################################
      tabf <- matrix(nrow = 1, ncol = levels_y + 2)
      tabf <- c("characteristics", ynames, "p-value")
      ligne2 <- rep("", levels_y + 2)
      for (k in 1:levels_y) {
         ligne2[k + 1] <- paste0("N = ", table(DF[, y])[k]) #number of observations for each levels of y
      }
      tabf <- rbind(tabf, ligne2)
      ##################################################

      y_index <- match(y, colnames(DF))
      DF_without_y <- DF[, -y_index]
      i <- 1

      ### loop for each characteristics (var) ###
      space <- 20

      for (var in DF_without_y) {
         i <- i + 1
         progressbar(total = length(DF),i)
         varname <- colnames(DF)[i]
         ligne <- varname
         sign <- NULL



         if (is.numeric(var)) {
            mean_vars <- aggregate(var ~ DF[, y], FUN = "mean", na.rm = T)
            sd_vars <- aggregate(var ~ DF[, y], FUN = "sd", na.rm = T)
            for (j in 1:levels_y) {
               #pour chaque modalité
               mean_vars_level <- round(mean_vars[j, 2], 2)
               sd_vars_level <- round(sd_vars[j, 2], 2)
               ligne <-
                  c(ligne,
                    paste0(mean_vars_level, "±", sd_vars_level))
            }
            verif <- TRUE
            for (lev in margin.table(table(var, DF[, y]), 2)) {
               if (lev == 0)
                  verif <- FALSE
            }#verif prevents to have a table with 0 in a level
            if (verif &
                sd(var, na.rm = T) != 0) {
               p <- signif(t.test(var ~ DF[, y])$p.value, 3)
            } else{
               p <- "-"
            }
            ligne <- c(ligne, p)
            tabf <- rbind(tabf, ligne)
         }


         else{
            #if non numeric
            var <- stringr::str_to_lower(var)
            var <- stringr::str_replace(var, "é", "e")
            var <- stringr::str_replace(var, "è", "e")
            var <- stringr::str_replace(var, "/", "")
            for (it in var) {
               if (!is.na(it)) {
                  if (stringr::str_length(it) > 40)
                     var[var == it] <- paste0(substr(it, 1, 40), "...")
               }
            }
            var <- as.factor(var)
            if (length(levels(var)) >= mutation) {
               nvar <- as.vector(var)
               for (other_levels_i in mutation:length(levels(var))) {
                  other_levels <- levels(var)[other_levels_i]
                  nvar[nvar == other_levels] <- "others"
               }
               nvar -> var
               as.factor(var) -> var
            }
            if (length(levels(var)) >= 2) {
               tb <- table(DF[, y], var)
               tbm <- margin.table(tb, 1)
               verif_level <- margin.table(table(var, DF[, y]), 2)
               verif <- TRUE
               for (lev in verif_level) {
                  if (lev == 0)
                     verif <- FALSE
               }
               if (verif) {
                  condition_chi2 <- 0
                  condition_chi2_B <- TRUE
                  for (m in chisq.test(var, DF[, y], simulate.p.value = TRUE)$expected) {
                     if (m < 5) {
                        #counting EXPECTED modalities under 5. if one > 3 and only 2 column we can apply yate's correction otherwise we apply Fisher
                        condition_chi2_B <- FALSE
                        if (m < 3) {
                           condition_chi2_B <- FALSE
                           break
                        } else{
                           condition_chi2 <- condition_chi2 + 1
                        }
                     }
                  }

                  if (condition_chi2 > 1) {
                     condition_chi2_B <- FALSE
                  }
                  if (condition_chi2 == 1 &
                      length(levels(var)) > 2) {
                     condition_chi2_B <- FALSE
                  }
                  if (condition_chi2_B) {
                     clig <-
                        chisq.test(var, DF[, y])$p.value                    # Chi2 test
                     clig <- signif(clig, 3)
                  } else{
                     clig <- fisher.test(var, DF[, y], simulate.p.value = TRUE)$p.value
                     clig <- signif(clig, 3)
                     sign <- "‡"
                  }
               }
            } else{
               clig <- NA
            }


            ## Variable with 2 levels
            if (length(levels(var)) == 2) {
               if (levels(var)[1] == "non" ||
                   levels(var)[1] == "no") {
                  var <-
                     ifelse(levels(var)[1] == "no",
                            relevel(var, "yes"),
                            relevel(var, "oui"))
               }
               ligne <-
                  paste0(ligne, " (", levels(var)[1], ") - no. (%)")

               for (j in 1:levels_y) {
                  no <- tb[j, 1]
                  pn <- signif(100 * no / tbm[j], 3)
                  ligne <- c(ligne, paste0(no, " (", pn, "%)"))
               }
               ligne <- c(ligne, paste0(clig, sign))
               tabf <- rbind(tabf, ligne) #ajout de la ligne au tableau
            } else{
               ligne <-
                  c(paste0(varname, " - no. (%)"),
                    rep(" ", levels_y),
                    paste0(clig, sign))
               tabf <- rbind(tabf, ligne)
               for (n in 1:length(levels(var))) {
                  #Pour chaque niveau de la variable
                  ligne <- paste0("        ", levels(var)[n])
                  for (j in 1:levels_y) {
                     no <- tb[j, n]
                     pn <- signif(100 * no / tbm[j], 3)
                     ligne <- c(ligne, paste0(no, " (", pn, "%)"))
                  }
                  ligne <- c(ligne, " ")
                  tabf <-
                     rbind(tabf, ligne) #ajout de la ligne au tableau
               }
            }
         }
      }
      row.names(tabf) <- NULL
      ###

      ### dataframe transformation ###
      as.data.frame(tabf) -> rslt
      colnames(rslt) <- tabf[1, ]
      rslt <- rslt[-1, ]
      ###

      if (legend) {
         text_legend <-
            "p-values are obtained with a Chi2 or Fisher exact test (‡) for categorical variables and by student test for continuous variables"
         rslt$legend <- text_legend
      } else{
         text_legend <- NULL
      }

      if (title) {
         title_text <-
            "Table 1. Patients baseline characteristics by study group"
         rslt$title <- text_legend
      } else{
         title_text <- NULL
      }


      return(rslt)

   }

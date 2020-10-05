#' Table 1
#'
#' @param DF dataframe : datas to be analysed
#' @param y character : column that can separate the dataframe
#' @param ynames vector of characters (optionnal) : names to be put in table one columns instead of y levels.
#' @param make.names (optionnal) : TRUE so that the colnames provided are formated
#' @param round (optionnal) : the number of digits to be display in the results
#' @param overall booleen (optionnal) : TRUE if an "overall" column is wanted
#' @param overall_name (optionnal) : name of the 'overall' column
#' @param mutation numeric : number of modalities to display for one variable. If there is more than "mutation" categories, the modalities after this threeshold are wrapped into an "others" categorie.
#' @param legend booleen (optionnal) : TRUE if a legend is wanted under the table
#' @param title booleen (optionnal) : TRUE if a title is wanted
#' @param tests booleen (optionnal) : TRUE if you want tests to be executed
#' @param norm_test booleen (optionnal) : TRUE if you want tests on normal distribution to be performed has condition to student test
#' @param Khi2 booleen (optionnal) : if TRUE, khi2 is prefered to fisher
#' @param exit character : 'html', 'console' or 'excel'
#'
#' @return dataframe, excel file or html table depending on the exit parameter
#' @export
#' @import stringr
#' @import stats
#' @import flextable
table1 <- function(DF,
                   y,
                   ynames = NULL,
                   make.names = TRUE,
                   overall = TRUE,
                   overall_name = "Overall",
                   tests = TRUE,
                   norm_test = TRUE,
                   Khi2 = TRUE,
                   mutation = 40,
                   legend = TRUE,
                   title = TRUE,
                   round = 2,
                   exit = 'html')
{


   ##################################################
   #    Arguments verification / transformation     #
   ##################################################

   ## Dataframe
   if (is.data.frame(DF) || is.matrix(DF)) {
      DF <- as.data.frame(DF,row.names = NULL)
      if ( (make.names(colnames(DF)) != colnames(DF))) {
         colnames_prep(colnames(DF)) -> colnames(DF)
         colnames_prep(y) -> y
      }
   } else {
      stop("No dataframe has been provided. Make sure 'DF' is a dataframe, a tibble or a matrix")
   }

   if (!is.character(y) || !(y %in% colnames(DF)))
      stop("y must be a character variable, part of DF")

   DF[, y] <- as.factor(DF[[y]])
   levels_y <- length(levels(DF[, y]))
   DF_without_y <- DF[, -match(y, colnames(DF))]

   if (is.null(ynames))
      ynames <- levels(DF[, y])

   if (!is.vector(ynames))
      stop("ynames should be a vector of characters")

   if (length(ynames) != length(levels(DF[, y])))
      stop("ynames should be of as many labels than y levels")

   if (!is.logical(overall))
      stop("'overall' should be a booleen")

   if (!is.logical(legend))
      stop("'legend' should be a booleen")

   if (!is.logical(title))
      stop("'title' should be a booleen")

   if (!is.numeric(mutation))
      stop("'mutation' should be numeric")

   format_data(DF) -> DF #plain and no plural factors
   ##################################################



   ##################################################
   #                     HEADER                     #
   ##################################################
   tabf <- matrix(nrow = 1, ncol = levels_y + 2)
   first_column <- 1 #first column in the table with an ynames
   if (tests) {
      if (overall) {
         tabf <- c("characteristics", "overall", ynames, "p-value")
         ligne2 <- rep("", levels_y + 3)
         first_column <- first_column + 1
      }else {
         tabf <- c("characteristics", ynames, "p-value")
         ligne2 <- rep("", levels_y + 2)
      }
   } else {
      if (overall) {
         tabf <- c("characteristics", "overall", ynames)
         ligne2 <- rep("","", levels_y + 2)
         first_column <- first_column + 1
      }else {
         tabf <- c("characteristics", ynames)
         ligne2 <- rep("", levels_y + 1)
      }
   }



   for (k in 1:levels_y) {
      ligne2[k + first_column] <- paste0("N = ", table(DF[, y])[k]) #number of observations for each levels of y
   }
   numbers_observation <- ligne2
   if (overall) {
      overall_observations <- sum(table(DF[, y]))
      ligne2[2] <- paste0("N = ", overall_observations)
   }
   tabf <- rbind(tabf, ligne2)
   ##################################################


   i <- 0
   num_variables <- vector()
   colnames_definitive <- colnames_prep(colnames(DF)[colnames(DF) !=y],type = "presentation")

   for (column in colnames(DF_without_y)) {
      # useful for flextable presentation
      i + 1 -> i
      if (is.numeric(DF_without_y[,column])) {
         num_variables <- c(num_variables,i)
      }
   }

   i <- 0

   ########################################################
   ###        Loop for each characteristics (var)       ###
   ########################################################
   for (var in DF_without_y) {
      i <- i + 1
      varname <- colnames(DF_without_y)[i]
      progressbar(total = length(DF_without_y),i,variable = varname)
      ligne1 <- colnames_definitive[i]


      sign <- NULL # note if a special test is done (fischer, Wilcoxon...)

      if (is.numeric(var)) {
         ###  if numeric  ###########################################
         if (exit == "html") {
            ligne2 <- "\t \t Mean (SD)"
            ligne3 <- "\t \t Median [min - max]"
         } else {
            ligne2 <- "      Mean (SD)"
            ligne3 <- "      Median [min - max]"
         }
         # Descriptive calculs
         mean_vars <- aggregate(var ~ DF[, y], FUN = "mean")
         mean_overall <- round(mean(var,na.rm = TRUE), round)
         median_vars <- aggregate(var ~ DF[, y], FUN = "median")
         median_overall <- round(median(var,na.rm = TRUE),round)
         sd_vars <- aggregate(var ~ DF[, y], FUN = "sd")
         sd_overall <- round(sd(var,na.rm = TRUE),round)
         min_vars <- aggregate(var ~ DF[, y], FUN = "min")
         min_overall <- round(min(var, na.rm = TRUE),round)
         max_vars <- aggregate(var ~ DF[, y], FUN = "max")
         max_overall <- round(max(var, na.rm = TRUE),round)
         length_vars <- aggregate(var ~ DF[, y], FUN = "length")

         if (overall) {
            ligne1 <- c(ligne1," ")
            ligne2 <- c(ligne2,paste0(mean_overall, " (", sd_overall,") "))
            ligne3 <- c(ligne3,paste0(median_overall," [",min_overall," - ",max_overall,"] "))
         }

         for (j in 1:levels_y) {
            # round and save all results
            mean_vars_level <- round(mean_vars[j, 2], round)
            sd_vars_level <- round(sd_vars[j, 2], round)
            median_vars_level <- round(median_vars[j, 2], round)
            min_vars_level <- round(min_vars[j, 2], round)
            max_vars_level <- round(max_vars[j, 2], round)
            ligne1 <- c(ligne1," ")
            ligne2 <- c(ligne2,paste0(mean_vars_level, " (", sd_vars_level,") "))
            ligne3 <- c(ligne3,paste0(median_vars_level," [",min_vars_level," - ",max_vars_level,"] "))
         }



         if (tests) {
            # 1. Verification des conditions d'application
            verif <- TRUE
            ncst <- TRUE
            p <- "-"
            if (nrow(mean_vars) == levels_y) {
               for (var_mod in 1:levels_y) {
                  if (length_vars[var_mod,2] < 8) {
                     ncst <- FALSE
                  }
               }
               if (ncst & !(NA %in% sd_vars[,2])) {
                  for (var_mod in 1:levels_y) {
                     if (length_vars[var_mod,2] < 31 & length_vars[var_mod,2] > 7 & sd_vars[var_mod,2] != 0) {
                        p_shapiro <- shapiro.test(var[DF[,y] == levels(DF[, y])[var_mod]])$p.value
                        if (p_shapiro < 0.05)
                           verif <- FALSE
                     }
                  }
                  variance_equal <- ifelse(var.test(var ~ DF[, y])$p.value > 0.05,TRUE,FALSE)
               }
               if (ncst & sd(var, na.rm = TRUE) != 0) {
                  # Conditions d'application = loi normale ou n > 30 respectées si verif == TRUE
                  if (verif) {
                     if (variance_equal) {
                        p <- signif(t.test(var ~ DF[, y],var.equal = TRUE)$p.value, 3)
                     } else{
                        p <- signif(t.test(var ~ DF[, y],var.equal = FALSE)$p.value, 3)
                        p <- paste0(p," (a)")
                     }
                  } else {
                     p <- signif(wilcox.test(var ~ DF[, y])$p.value,3)
                     p <- paste0(p," (c)")
                  }
               } else {
                  p <- "-"
               }
            }
            ligne1 <- c(ligne1, p)
         }else {
            ligne1 <- c(ligne1, " ")
         }
         ligne2 <- c(ligne2, " ")
         ligne3 <- c(ligne3, " ")
         tabf <- rbind(tabf,ligne1)
         tabf <- rbind(tabf,ligne2)
         tabf <- rbind(tabf,ligne3)
      }


      #
      ###########################################
      ###########################################
      #



      else{
         ###  if non numeric  ###########################################

         for (it in var) { #if a modality's lenght is > 40, we shrink it to 40 characters to fit in the table
            if (!is.na(it)) {
               if (stringr::str_length(it) > 40)
                  var[var == it] <- paste0(substr(it, 1, 40), "...")
            }
         }

         var <- as.factor(var)
         if (length(levels(var)) >= mutation) { #if there is more than 'mutation' modalities, the last modalities are grouped in 'others' modality
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
               if (condition_chi2 == 1 & length(levels(var)) > 2) {
                  condition_chi2_B <- FALSE
               }
               if (condition_chi2_B) {
                  clig <- chisq.test(var, DF[, y])$p.value                    # Chi2 test
                  clig <- signif(clig, 3)
               } else{
                  clig <- fisher.test(var, DF[, y], simulate.p.value = TRUE)$p.value
                  clig <- signif(clig, 3)
                  sign <- " (b)"
               }
            }
         } else{
            clig <- NA
         }


         ## Variable with 2 levels #############################
         if (length(levels(var)) == 2) {

            if (levels(var)[1] == "non" || levels(var)[1] == "NON" || levels(var)[1] == 0 || levels(var)[1] == "0") {
               if (levels(var)[2] == "oui")
                  var <- relevel(var, "oui")
               if (levels(var)[2] == "OUI")
                  var <- relevel(var, "OUI")
               if (levels(var)[2] == "1") {
                  var <- as.character(var)
                  var[var == "0"] <- "non"
                  var[var == "1"] <- "oui"
                  var <- as.factor(var)
                  var <- relevel(var, "oui")
               }
               if (levels(var)[2] == 1) {
                  var <- as.character(var)
                  var[var == 0] <- "non"
                  var[var == 1] <- "oui"
                  var <- as.factor(var)
                  var <- relevel(var, "oui")
               }
            }
            if (levels(var)[1] == "no" & levels(var)[2] == "yes") {
               var <- relevel(var, "yes")
            }

            ligne <- paste0(ligne1, " (", levels(var)[1], ") - no. (%)")

            if (overall) {
               overall_count <- addmargins(tb)[levels_y + 1,1]
               percent_overall <- signif(100*overall_count / length(var),3)
               ligne <- c(ligne, paste0(overall_count,"  (",percent_overall,"%)"))
            }

            for (j in 1:levels_y) {
               no <- tb[j, 1]
               pn <- signif(100 * no / tbm[j], 3)
               ligne <- c(ligne, paste0(no, " (", pn, "%)"))
            }
            ligne <- c(ligne, paste0(clig, sign))
            tabf <- rbind(tabf, ligne) #ajout de la ligne au tableau


         } else {
            ## Variable with more than 2 levels #############################

            # First line with only the test result
            if (overall) {
               ligne <- c(paste0(colnames_definitive[i], " - no. (%)"), rep(" ", levels_y + 1), paste0(clig, sign))
            } else {
               ligne <- c(paste0(colnames_definitive[i], " - no. (%)"), rep(" ", levels_y), paste0(clig, sign))
            }
            tabf <- rbind(tabf, ligne)

            # other lines
            for (n in 1:length(levels(var))) {

               # For each modality
               if (exit != "html") {
                  ligne <- paste0("        ", levels(var)[n])
               }else {
                  ligne <- paste0(" \t \t  ", levels(var)[n])
               }

               if (overall) {
                  overall_count <- addmargins(tb)[levels_y,n]
                  percent_overall <- signif(100*overall_count / length(var),3)
                  ligne <- c(ligne,paste0(overall_count,"  (",percent_overall,"%)"))
               }

               for (j in 1:levels_y) {
                  no <- tb[j, n]
                  pn <- signif(100 * no / tbm[j], 3)
                  ligne <- c(ligne, paste0(no, " (", pn, "%)"))
               }
               ligne <- c(ligne, " ")
               tabf <- rbind(tabf, ligne) #ajout de la ligne au tableau
            }
         }
      }
   }
   row.names(tabf) <- NULL
   ########################################################

   ### dataframe transformation ###
   as.data.frame(tabf) -> rslt
   colnames(rslt) <- tabf[1, ]
   rslt <- rslt[-1, ]
   ###

   if ("html" %in% exit) {
      rslt <- as.data.frame(rslt[-1,])
      colsy <- vector()
      for (n in seq(ynames)) {
         start_nb_observation <- ifelse(overall,2,1)
         colsy[n] <- paste(ynames[n],numbers_observation[n + start_nb_observation],sep = "\n")
      }

      if (overall) {
         coloverall <- paste(overall_name,overall_observations,sep = "\nN = ")
         definite_names <- c("characteristics",coloverall,colsy)

      } else {
         definite_names <- c("characteristics",colsy)
      }
      if (tests) {
         definite_names <- c(definite_names,"p value")
      }
      colnames(rslt) <- definite_names

      rslt <- flextable(rslt, col_keys = colnames(rslt))

      rslt <- add_footer(rslt, characteristics = "p-values have been obtained from a two-sided student test for continuous variables and from a khi-2 test for categorical variables, unless specified otherwise : \n
                         - a : Student test with Welch approximation
                         - b : Fisher's exact test
                         - c : Wilcoxon test" )
      nb_colums <- ifelse(overall,levels_y + 1,levels_y)
      nb_colums <- ifelse(tests,nb_colums + 1,nb_colums)
      rslt <- merge_at(rslt, j = 1:nb_colums, part = "footer")
      rslt <- valign(rslt, valign = "bottom", part = "footer")
      if (title) {
         rslt <- add_header_lines(rslt,paste0("Table 1. Patients baseline characteristics by study group ( ", colnames_prep(y,type = "presentation"),' )'))
      }
      rslt %>% fontsize(i = 1, part = "header", size = 24) %>%
         bold(i = 1, part = "header", bold = TRUE) -> rslt
      rslt %>% fontsize(i = 2, part = "header", size = 20) %>%
         bold(i = 2, part = "header", bold = TRUE) -> rslt

      rslt <- theme_booktabs(rslt)
      rslt <- autofit(rslt)
      rslt %>% align(j = 2:nb_colums,align = 'center') -> rslt
      rslt %>% align(j = 2:nb_colums,align = 'center',part = "header") -> rslt
   }

   return(rslt)

}

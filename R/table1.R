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
#' @param paired
#' @param strata
#' @param method
#' @param adjusted
#' @param significance
#' @param name_sheet
#'
#' @return dataframe, excel file or html table depending on the exit parameter
#' @export
#' @import stringr
#' @import stats
#' @import flextable
#' @import RVAideMemoire
table1 <- function(DF,
                   y,
                   paired=FALSE,
                   strata=NULL,
                   ynames = NULL,
                   make.names = TRUE,
                   overall = TRUE,
                   overall_name = "Overall",
                   tests = TRUE,
                   method = "classic",
                   adjusted = NULL,
                   norm_test = TRUE,
                   Khi2 = TRUE,
                   mutation = 40,
                   legend = TRUE,
                   title = TRUE,
                   round = 2,
                   significance = F,
                   exit = 'html',
                   name_sheet = 'results',
                   num_function = "auto",
                   available_data=TRUE
                   )
{


   version_pkg = '0.0.7'

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
   } else stop("No dataframe has been provided. Make sure 'DF' is a dataframe, a tibble or a matrix")

   ## y
   if (!is.character(y) || !(y %in% colnames(DF)))
      stop("y must be a character variable, part of DF")
   DF[, y] <- as.factor(DF[, y])

   levels_y <- length(levels(DF[, y]))
   DF_without_y <- DF[, -match(y, colnames(DF))]

   if (is.null(ynames)) {
      ynames <- levels(DF[, y])
   } else if (!is.vector(ynames) | length(ynames) != levels_y) {
      stop(paste0("ynames should be a vector of characters of length",levels_y))
   }

   if (!is.logical(overall))
      stop("'overall' should be a booleen")

   if (!is.logical(legend))
      stop("'legend' should be a booleen")

   if (!is.logical(title))
      stop("'title' should be a booleen")

   if (!is.numeric(mutation))
      stop("'mutation' should be numeric")

   if (!is.logical(paired))
      stop("'paired' should be a logical")

   if (is.logical(make.names)) {
      if (make.names) {
         colnames_definitive <- colnames_prep(colnames(DF)[colnames(DF) != y],type = "presentation")
      } else {
         colnames_definitive <- colnames(DF)[colnames(DF) != y]
      }
   } else {
      warning('"make.name" should be logical ; "make.name" is considered as False')
      colnames_definitive <- colnames(DF)
   }

   DF_without_y_and_all <- DF_without_y
   if (paired & !is.null(strata)) {
      for (colstrata in strata) {
         DF_without_y_and_all <-  DF_without_y_and_all[,-match(colstrata,colnames(DF_without_y_and_all))]
      }
   }


   autostats::format_data(DF) -> DF #plain and no plural factors
   ##################################################



   ##################################################
   #                     HEADER                     #
   ##################################################
   tabf <- matrix(nrow = 1, ncol = levels_y + 2)
   first_column <- 1 #first column in the table with an ynames
   tabf <- c("Characteristics")
   if (available_data)
      tabf <- c(tabf,"Available data")
   if (overall)
      tabf <- c(tabf,"Overall")
   tabf <- c(tabf,ynames)
   if (tests)
      tabf <- c(tabf,'p-value')

   first_column <- match(ynames[1],tabf)
   ligne2 <- rep("", length(tabf))


   for (k in 1:levels_y) {
      ligne2[k + first_column -1] <- paste0("N = ", table(DF[, y])[k]) #number of observations for each levels of y
   }
   numbers_observation <- ligne2
   if (overall) {
      overall_observations <- sum(table(DF[, y]))
      ligne2[first_column -1] <- paste0("N = ", overall_observations)
   }
   tabf <- rbind(tabf, ligne2)
   ##################################################

   # useful loop for flextable presentation
   i <- 0
   num_variables <- vector()
   for (column in colnames(DF_without_y)) {
      i + 1 -> i
      if (is.numeric(DF_without_y[,column])) {
         num_variables <- c(num_variables,i)
      }
   }



   ########################################################
   ###        Loop for each characteristics (var)       ###
   ########################################################

   i <- 0
   one_ligne <- FALSE
   for (var in DF_without_y_and_all) {
      i <- i + 1
      varname <- colnames(DF_without_y_and_all)[i]
      progressbar(total = length(DF_without_y_and_all),i,variable = varname)
      ligne1 <- colnames_definitive[i]

      sign <- NULL # store a note if a special test is done (fischer, Wilcoxon...)


      if (available_data) {
         n_available <- sum(!is.na(var))
      }



      #--------------------------------------------------------------------------------------------------------------------#
      #--  if numeric  ----------------------------------------------------------------------------------------------------#
      #--------------------------------------------------------------------------------------------------------------------#

      if (is.numeric(var)) {

         ligne1 <- c(ligne1,n_available)

            # Legend
            if (length(num_function) < 3) {
               ligne2 <- NULL
               ligne3 <- NULL
               one_ligne <- TRUE
            } else {
                  tabulation <- ifelse(exit == "html","\t \t ","      ")
                  tabulation <- ifelse(length(num_function) > 2,"",tabulation)
                  median_bis <- ifelse("quartile" %in% num_function,"[Q1-Q3]","[min - max]")
                  ligne2 <- ifelse(exit == "html","\t \t Mean (SD)","      Mean (SD)")
                  ligne3 <- ifelse(exit == "html",paste0("\t \t Median ",median_bis),paste0("      Median ",median_bis))
            }





         #####  DESCRIPTIVE CALCULS #####
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
         quartiles_vars <- aggregate(var ~ DF[, y], FUN = "quantile")
         quartiles_overall <- quantile(var, na.rm = TRUE)
         length_vars <- aggregate(var ~ DF[, y], FUN = "length")
         shapiro_vars <- aggregate(var ~ DF[, y], FUN = "shapiro.test")
         shapiro_overall <- shapiro.test(var)$p.value
         normal <- TRUE
         if ((shapiro_overall < 0.05) & overall)
            normal <- FALSE
         if (TRUE %in% (shapiro_vars[,2][,2] < 0.05))
            normal <- FALSE


         # Add overall results if overall
         if (overall) {
            if (length(num_function) < 3 & (num_function != "auto")) {
               ligne1 <- c(ligne1,ifelse("mean" %in% num_function,paste0(mean_overall, " ± ",sd_overall), paste0(median_overall," [",ifelse("quartile" %in% num_function,round(quartiles_overall[2],round),min_overall)," - ",ifelse("quartile" %in% num_function,round(quartiles_overall[4],round),max_overall),"] ")))
            } else if (num_function == "auto") {
               ligne1 <- c(ligne1,ifelse(normal,paste0(mean_overall, " ± ",sd_overall), paste0(median_overall," [",round(quartiles_overall[2],round)," - ",round(quartiles_overall[4],round),"] ")))
            } else {
               ligne1 <- c(ligne1," ")
               ligne2 <- c(ligne2,paste0(mean_overall, " ± ",sd_overall))
               ligne3 <- c(ligne3,paste0(median_overall," [",ifelse("quartile" %in% num_function,round(quartiles_overall[2],round),min_overall)," - ",ifelse("quartile" %in% num_function,round(quartiles_overall[4],round),max_overall),"] "))
            }
         }

         # round and save all results
         for (j in 1:levels_y) {
            mean_vars_level <- round(mean_vars[j, 2], round)
            sd_vars_level <- round(sd_vars[j, 2], round)
            median_vars_level <- round(median_vars[j, 2], round)
            min_vars_level <- round(min_vars[j, 2], round)
            max_vars_level <- round(max_vars[j, 2], round)
            quartiles_vars_level <- quartiles_vars[j, 2]
            min_vars_level <- ifelse("quartile" %in% num_function,round(quartiles_vars_level[2],round),min_vars_level)
            max_vars_level <- ifelse("quartile" %in% num_function,round(quartiles_vars_level[4],round),max_vars_level)

            if (length(num_function) < 3 & (num_function != "auto")) {
               ligne1 <- c(ligne1,ifelse("mean" %in% num_function,paste0(mean_vars_level, " (", sd_vars_level,") "), paste0(median_vars_level," [",min_vars_level," - ",max_vars_level,"] ")))
            } else if (num_function == "auto") {
               ligne1 <- c(ligne1,ifelse(normal,paste0(mean_vars_level, " ± ",sd_vars_level), paste0(median_vars_level," [",round(quartiles_vars_level[2],round)," - ",round(quartiles_vars_level[4],round),"] ")))
            } else{
               ligne1 <- c(ligne1," ")
               ligne2 <- c(ligne2,paste0(mean_vars_level, " (", sd_vars_level,") "))
               ligne3 <- c(ligne3,paste0(median_vars_level," [",min_vars_level," - ",max_vars_level,"] "))
            }
         }
         #####

         ##### STATISTICAL TESTS #####

         ### if not paired ###
         if (tests & !paired) {
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
                  # if normale or n > 30 : verif == TRUE
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
         }else

         ### if paired ###
         if (tests & paired) {
            Strate <- vector()
            var_strat <- vector()
            y_strat <- vector()
            for (strate_i in strata) {
               Strate <-  c(Strate,DF[,strate_i])
               var_strat <- c(var_strat,var)
               y_strat <- c(y_strat,DF[,y])
            }
            p <- survival::clogit(as.factor(y_strat-1)~as.numeric(var_strat)+strata(Strate))$p.value
         }
         ##
         else{
            ligne1 <- c(ligne1, " ")
         }
         ligne2 <- c(ligne2, " ")
         ligne3 <- c(ligne3, " ")
         tabf <- rbind(tabf,ligne1)
         tabf <- rbind(tabf,ligne2)
         tabf <- rbind(tabf,ligne3)
      }






      #--------------------------------------------------------------------------------------------------------------------#
      #--  if non numeric  ------------------------------------------------------------------------------------------------#
      #--------------------------------------------------------------------------------------------------------------------#
      else{

         #if a modality's lenght is > 40 characters, we shrink it to 40 characters to fit in the table
         for (it in var) {
            if (!is.na(it)) {
               if (stringr::str_length(it) > 40)
                  var[var == it] <- paste0(substr(it, 1, 40), "...")
            }
         }

         var <- as.factor(var)
         #if there is more than 'mutation' modalities, the last modalities are grouped in 'others' modality
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

            # Relevel to have the 'yes' level first
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
            }

            tb <- table(DF[, y], var,useNA = "no")
            tbm <- margin.table(tb, 1)

            # Application conditions verification
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

               if (condition_chi2 > 1 | (condition_chi2 == 1 & length(levels(var)) > 2) ) {
                  condition_chi2_B <- FALSE
               }

               ## if paired ##
               if (paired & tests) {
                     Strate = vector()
                     var_strat <- vector()
                     y_strat <- vector()
                     for (strate_i in strata) {
                        Strate <-  c(Strate,DF[,strate_i])
                        var_strat <- c(var_strat,as.character(var))
                        y_strat <- c(y_strat,DF[,y])
                     }

                     clig <- survival::clogit(as.factor(y_strat-1)~as.factor(var_strat)+strata(Strate))$p.value
                     clig <- signif(clig, 3)
                     clig <- ifelse(significance, clig,ifelse(clig < 0.0001,"< 0.0001",clig))

               ## if not paired ##
                }else {
                  # if verif ok chi2, else Fisher
                  if (condition_chi2_B) {
                     clig <- chisq.test(var, DF[, y])$p.value
                     clig <- signif(clig, 3)
                  } else{
                     clig <- fisher.test(var, DF[, y], simulate.p.value = TRUE)$p.value
                     clig <- signif(clig, 3)
                     sign <- " (b)"
                  }
               }
            }
         } else{
            clig <- NA
         }


         ## Variable with 2 levels #############################
         if (length(levels(var)) == 2) {


            ligne <- c(paste0(ligne1, " (", levels(var)[1], ") - no. (%)"),n_available)

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
            ligne <- paste0(colnames_definitive[i], " - no. (%)")
            if (available_data)
               ligne <- c(ligne,n_available)
            ligne <- c(ligne, rep(" ", ifelse(overall,levels_y + 1,levels_y)), paste0(clig, sign))

            tabf <- rbind(tabf, ligne)

            # other lines
            for (n in 1:length(levels(var))) {

               # For each modality
               if (exit != "html") {
                  ligne <- paste0("        ", levels(var)[n])
               }else {
                  ligne <- paste0(" \t \t  ", levels(var)[n])
               }
               if (available_data)
                  ligne <- c(ligne," ")

               tb <- table(DF[, y], var,useNA = "no")
               tbm <- margin.table(tb, 1)



               if (overall) {
                  overall_count <- addmargins(tb)[levels_y+1,n]
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

   if ("excel" %in% exit)
      xlsx::write.xlsx(
         rslt,
         paste0(name_sheet,".xlsx"),
         sheetName = "name_sheet",
         append = FALSE,
         row.names = FALSE
      )

   if ("html" %in% exit) {
      colnames_rslt <- colnames(rslt)
      nb_colums <- length(rslt[1,])
      rslt <- as.data.frame(rslt[-1,])
      colsy <- vector()
      for (n in seq(ynames)) {
         start_nb_observation <- ifelse(overall,2,1)
         colsy[n] <- paste(ynames[n],numbers_observation[n + start_nb_observation],sep = "\n")
      }

      rslt <- flextable(rslt, col_keys = colnames_rslt)
      text_footer <- ""
      text_footer <- "Data shown are number (%), and mean ± SD or median (25th – 75th percentiles) if not normally distributed \n"
      text_footer <- paste0(text_footer,ifelse(test = paired,"p-values have been obtained from paired t test for continuous variables and mac nemar test for categorical variables","p-values have been obtained from a two-sided student test for continuous variables and from a khi-2 test for categorical variables, unless specified otherwise : \n
                         - a : Student test with Welch approximation
                         - b : Fisher's exact test
                         - c : Wilcoxon test"))
      rslt <- add_footer(rslt, characteristics = text_footer)
      nb_colums <- ifelse(overall,levels_y + 1,levels_y)
      nb_colums <- ifelse(tests,nb_colums + 1,nb_colums)
      #rslt <- merge_at(rslt, j = 1:(nb_colums), part = "footer")
      rslt <- valign(rslt, valign = "bottom", part = "footer")
      if (title) {
         rslt <- add_header_lines(rslt,paste0("Table X. Patients baseline characteristics by study group ( ", colnames_prep(y,type = "presentation"),' )'))
      }
      rslt %>%
         fontsize(i = 1, part = "header", size = 24) %>%
         bold(i = 1, part = "header", bold = TRUE) -> rslt
      rslt %>%
         fontsize(i = 2, part = "header", size = 20) %>%
         bold(i = 2, part = "header", bold = TRUE) -> rslt

      rslt <- theme_booktabs(rslt)
      rslt <- autofit(rslt)
      rslt %>% align(j = 2:nb_colums,align = 'center') -> rslt
      rslt %>% align(j = 2:nb_colums,align = 'center',part = "header") -> rslt
   }



   print(paste0('version : ',version_pkg))
   return(rslt)

}

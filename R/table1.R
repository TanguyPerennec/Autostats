#' Table 1
#'
#' @param DF dataframe : datas
#' @param y character : columns that separte the dataframe
#' @param ynames vector of characters (optionnal) : names to be put in table one columns instead of y levels.
#' @param overall booleen (optionnal) : TRUE if an "overall" column is wanted
#' @param mutation numeric : number of categories to display for one variable. If more than "mutation" categories, the categories after this threeshold are wrapped into a "others" categorie.
#' @param legend booleen (optionnal) : TRUE if a legend is wanted under the table
#' @param title booleen (optionnal) : TRUE if a title is wanted
#' @param tests booleen (optionnal) : TRUE if you want tests to be executed
#' @param norm_test booleen (optionnal) : TRUE if you want tests on normal distribution to be performed has condition to student test
#' @param Khi2 booleen (optionnal) : if TRUE, khi2 is prefered to fisher
#' @param exit character : among 'html', 'console' or 'excel'
#'
#' @return dataframe, excel file or html table
#' @export
#' @import stringr
#' @import stats
#' @import flextable
table1 <- function(DF,
            y,
            ynames = NULL,
            overall = TRUE,
            tests = TRUE,
            norm_test = TRUE,
            Khi2 = TRUE,
            mutation = 40,
            legend = TRUE,
            title = TRUE,
            exit = 'excel')
{


   ##################################################
   #    Arguments verification / transformation     #
   ##################################################

   ## Dataframe
   if (is.data.frame(DF) || is.matrix(DF) || is.tbl(DF))
   {
      DF <- as.data.frame(DF,row.names = NULL)
      if (make.names(colnames(DF)) != colnames(DF))
      {
         message("column names are not valid, 'make.names()' is used to have valid colnames")
         make.names(colnames(DF)) -> colnames(DF)
         make.names(y) -> y
      }
   } else
   {
      stop("No dataframe has been provided. Make sure 'DF' is a dataframe, a tibble or a matrix")
   }

   if (!is.character(y) || !(y %in% colnames(DF)))
      stop("y must be a character variable, part of DF")

   DF[, y] <- as.factor(DF[[y]])
   levels_y <- length(levels(DF[, y]))

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
   #                First two lines                 #
   ##################################################
   tabf <- matrix(nrow = 1, ncol = levels_y + 2)
   if (tests)
   {
      tabf <- c("characteristics", ynames, "p-value")
      ligne2 <- rep("", levels_y + 2)
   } else {
      tabf <- c("characteristics", ynames)
      ligne2 <- rep("", levels_y + 1)
   }

   for (k in 1:levels_y)
   {
      ligne2[k + 1] <- paste0("N = ", table(DF[, y])[k]) #number of observations for each levels of y
   }
   numbers_observation <- ligne2[1:levels_y + 1]
   tabf <- rbind(tabf, ligne2)
   ##################################################


   y_index <- match(y, colnames(DF))
   DF_without_y <- DF[, -y_index]
   i <- 0
   num_variables <- vector()

   for (column in colnames(DF))
   {#pour mise en page flextable
      i + 1 -> i
      is.numeric(DF[,column]) -> num
      if (num)
         num_variables <- c(num_variables,i)
   }


   i <- 0

   ### loop for each characteristics (var) ###
   for (var in DF_without_y)
   {
      i <- i + 1
      varname <- colnames(DF)[i]
      progressbar(total = length(DF),i,variable = varname)
      ligne1 <- varname
      ligne2 <- "\t \t Mean (SD)"
      ligne3 <- "\t \t Median [min - max]"
      sign <- NULL

      if (is.numeric(var))
      {
         mean_vars <- aggregate(var ~ DF[, y], FUN = "mean")
         median_vars <- aggregate(var ~ DF[, y], FUN = "median")
         sd_vars <- aggregate(var ~ DF[, y], FUN = "sd")
         min_vars <- aggregate(var ~ DF[, y], FUN = "min")
         max_vars <- aggregate(var ~ DF[, y], FUN = "max")
         length_vars <- aggregate(var ~ DF[, y], FUN = "length")
         for (j in 1:levels_y)
         {
            # for each modality
            mean_vars_level <- round(mean_vars[j, 2], 2)
            sd_vars_level <- round(sd_vars[j, 2], 2)
            median_vars_level <- round(median_vars[j, 2], 2)
            min_vars_level <- round(min_vars[j, 2], 2)
            max_vars_level <- round(max_vars[j, 2], 2)
            ligne1 <- c(ligne1," ")
            ligne2 <- c(ligne2,paste0(mean_vars_level, " (", sd_vars_level,") "))
            ligne3 <- c(ligne3,paste0(median_vars_level," [",min_vars_level," - ",max_vars_level,"] "))
         }
         if (tests)
         {
            # 1. Verification des conditions d'application
            verif <- TRUE
            ncst <- TRUE
            p <- "-"
            if (nrow(mean_vars) == levels_y)
            {
               for (var_mod in 1:levels_y)
               {
                  if (length_vars[var_mod,2] < 8)
                  {
                     ncst <- FALSE
                  }
               }
               if (ncst & !(NA %in% sd_vars[,2]))
               {
                  for (var_mod in 1:levels_y)
                  {
                     if (length_vars[var_mod,2] < 31 & length_vars[var_mod,2] > 7 & sd_vars[var_mod,2] != 0)
                     {
                        p_shapiro <- shapiro.test(var[DF[,y] == levels(DF[, y])[var_mod]])$p.value
                        if (p_shapiro < 0.05)
                           verif <- FALSE
                     }
                  }
                  variance_equal <- ifelse(var.test(var ~ DF[, y])$p.value > 0.05,TRUE,FALSE)
               }
               if (ncst & sd(var, na.rm = TRUE) != 0)
               {
                  # Conditions d'application = loi normale ou n > 30 respectées si verif == TRUE
                  if (verif)
                  {
                     if (variance_equal)
                     {
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


      else{#if non numeric
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


         ## Variable with 2 levels
         if (length(levels(var)) == 2) {

            if (levels(var)[1] == "non" || levels(var)[1] == "NON")
            {
               if (levels(var)[2] == "oui")
                  var <- relevel(var, "oui")
               if (levels(var)[2] == "OUI")
                  var <- relevel(var, "OUI")
            }
            if (levels(var)[1] == "no" & levels(var)[2] == "yes")
            {
               var <- relevel(var, "yes")
            }

            ligne <- paste0(ligne1, " (", levels(var)[1], ") - no. (%)")

            for (j in 1:levels_y) {
               no <- tb[j, 1]
               pn <- signif(100 * no / tbm[j], 3)
               ligne <- c(ligne, paste0(no, " (", pn, "%)"))
            }
            ligne <- c(ligne, paste0(clig, sign))
            tabf <- rbind(tabf, ligne) #ajout de la ligne au tableau
         } else{
            ligne <- c(paste0(varname, " - no. (%)"),
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

   if ("html" %in% exit)
   {
      rslt <- as.data.frame(rslt[-1,])
      colsy <- vector()
      for (n in seq(ynames))
      {
         colsy[n] <- paste(ynames[n],numbers_observation[n],sep="\n")
      }
      colnames(rslt) <- c("characteristics",colsy,"p value")

      rslt <- flextable(rslt, col_keys = c("characteristics",colsy,"p value"))

      rslt <- add_footer(rslt, characteristics = "p-values have been obtained form a two-sided student test for continuous variables and from a khi-2 test for categorical variables, unless specified otherwise : \n
                         - a : Student test with Welch approximation
                         - b : Fisher's exact test
                         - c : Wilcoxon test" )
      rslt <- merge_at(rslt, j = 1:(levels_y+2), part = "footer")
      rslt <- valign(rslt, valign = "bottom", part = "footer")
      rslt <- add_header_lines(rslt,"Table 1. Patients baseline characteristics by study group")
      rslt %>% fontsize(i = 1, part = "header", size = 24) %>%
         bold(i = 1, part = "header", bold = TRUE) -> rslt
      rslt %>% fontsize(i = 2, part = "header", size = 20) %>%
         bold(i = 2, part = "header", bold = TRUE) -> rslt


      #### Mise en gras des variables (- a faire)
      #ligne=1
      #var = 1
      #while (var < length(DF))
      #{
      #   rslt %>% bold(i = ligne,j=1, part = "body", bold = TRUE) -> rslt
      #   if (ligne %in% num_variables)
      #   {
      #      ligne <- ligne + 3
      #   } else {
      #      ligne <- ligne + 1
      #   }
      #   var <- var+1
      #}

      rslt <- theme_booktabs(rslt)
      rslt <- autofit(rslt)
      rslt %>% align(j=2:4,align='center') -> rslt
      rslt %>% align(j=2:4,align='center',part = "header") -> rslt
   }

   return(rslt)

   }

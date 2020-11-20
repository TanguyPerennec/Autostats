survivor <- function(
   DF,
   time="time",
   event="event",
   explicatives = colnames(DF)[colnames(DF) != c(time,event)],
   verbose = TRUE,
   round=2,
   keep = FALSE)
{



   #To ignore warnings during usage
   options(warn = -1)
   options("getSymbols.warning4.0" = FALSE)

   as.data.frame(DF) -> DF
   DF_expl <- DF[,c(explicatives,time,event)]


   NA_rm_for_glm(DF_expl,method_NA = "lessNA",EPV=1) -> DF
   DF[,"surv"] <- Surv(as.numeric(DF[ ,time]),as.numeric(DF[ ,event]))


   # Constructing 'vect_explicative'
   #####
   vect_explicative <- vector()
   as.data.frame(DF) -> DF
   n = 1
   for (var in explicatives) {#making a vector with the name of the variable displayed as many times as (levels - 1)
      if (is.numeric(DF[, var])) {
         vect_explicative[n] <- var
         n <- n + 1
      } else{
         as.factor(DF[, var]) -> DF[,var]
         levels_var <- length(levels(DF[, var]))
         vect_explicative[n:(n + levels_var - 2)] <- rep(var, (levels_var - 1))
         n <- n + levels_var - 1
      }
   }
   #####

   if (verbose) cat(
      "\n
\n
-----+-----------------------------+--------------------------------------
     |                             |
     |    1) UNIVARIATE MODEL      |
     |                             |
     +-----------------------------+\n
")


   rslt <- matrix(ncol = 7, nrow = (length(vect_explicative) + 1))
   rownames(rslt) <- c("",vect_explicative)

   getinfo_cox <- function(mod,k,var,cat) {
      HR <- round(exp(as.numeric(mod$coefficients[k + 1])), round)
      pval <- summary(mod)$coefficients[k + 1, 5]
      IC <- paste0(round(suppressMessages(exp(confint(mod)))[k + 1, ], round),collapse = ";")
      IC <- paste0("[",IC,"]")
      name_var <- name_level<- var
      if (cat != 'num') {
         name_level <- stringr::str_split(rownames(summary(mod)$coefficients)[k+1], var)[[1]][2]
         name_var <- ifelse(name_level == "",name_var,(paste0(name_var," (",name_level ,")")))
      }
      return(c(name_var,HR,IC,pval,name_level))
   }

   i = 0
   for (var_uni in explicatives) {
      progressbar(total = length(vect_explicative)-1,i,variable = var_uni)
      mod_uni <- survival::coxph(as.formula(DF[,c("surv",var_uni)]),data=DF)
      vector_var = vector()
      k = 0
      if (is.numeric(DF[, var_uni])) {
         i <- i + 1
         ligneR <- getinfo_cox(mod_uni,k,var_uni,"num")
         vector_var[i] <- var_uni
         rslt[i + 1, ] <- c(ligneR[1:4], "-", "-", "-")
         row.names(rslt)[i + 1] <- paste0(var_uni,ligneR[5])
      } else {
         while (k + 1 < length(levels(DF[, var_uni]))) {
            i <- i + 1
            k <- k + 1
            ligneR <- getinfo_cox(mod_uni,k-1,var_uni,'cat')
            vector_var[i] <- var_uni
            rslt[i + 1, ] <- c(ligneR[1:4], "-", "-", "-")
            row.names(rslt)[i + 1] <- paste0(var_uni,ligneR[5])
         }
      } #else of is.numeric
   } # end of loop

   ##################################################



   ##################################################
   #               MULTIVARIATE MODEL               #
   ##################################################
   if (verbose) cat("
\n
\n
-----+-----------------------------+--------------------------------------
     |                             |
     |    3) MULTIVARIATE MODEL    |
     |                             |
     +-----------------------------+\n")


   # Variable selection
   #explicatives_multi <- multivariate_selection(DF[,c(y,explicatives)],y,keep = keep2)
   #explicatives_multi <- explicatives_multi$vars_multi
   # Definitive model
   step(survival::coxph(formula = as.formula(DF[,c("surv",explicatives)]), data = DF)) -> mod_multi

   survival::coxph(as.formula(DF[,c("surv",attr(mod_multi$terms,"term.labels"))]),data=DF) -> mod_multi

   ##################################################


   ##################################################
   #              MATRICE DE RESULTATS              #
   ##################################################
   HR <- exp(mod_multi$coefficients)
   pval <- summary(mod_multi)$coefficients[,5]
   IC <- round(suppressMessages(exp(confint(mod_multi))),round)
   i <- 0

   for (HR_var in names(HR)) {
      i <- i + 1
      n_ligne <- match(HR_var,rownames(rslt))
      p <- round(as.numeric(pval[i]), round)
      p <- ifelse(p == 0, "<0.001", p)
      IC_paste <- paste0("[",IC[i, 1], ";", IC[i, 2],"]")
      rslt[n_ligne, 5:7] <- c(signif(HR[i], round), IC_paste, p)
   }

   for (n in 1:length(rslt[-1, 4])) {
      p = as.numeric(rslt[n + 1, 4])
      round(p, round) -> p
      ifelse(p == 0, "<0.001", p) -> rslt[n + 1, 4]
   }

   rslt[1,] <- c("","HR","IC","pval","HR","IC","pval")

   row.names(rslt) <- NULL






   rslt <- as.data.frame(rslt[-1,])
   colnames(rslt) <-  c("variables","HR_univariate","IC_univariate","pval_univariate","HR_multivariate","IC_multivariate","pval_multivariate")
   rslt <- flextable(rslt, col_keys =  c("variables","HR_univariate","IC_univariate","pval_univariate","HR_multivariate","IC_multivariate","pval_multivariate"))



   rslt <- delete_part(rslt,part = "header")
   rslt <- add_header(x = rslt, variable = "variables",HR_univariate = "HR",IC_univariate = "IC95%",pval_univariate = "p-value",HR_multivariate = "HR",IC_multivariate = "IC95%",pval_multivariate = "p-value", top = FALSE)
   rslt <- add_header(x = rslt, variable = "variables",HR_univariate = "univariate model",IC_univariate = "univariate model",pval_univariate = "univariate model",HR_multivariate = "multivariate model",IC_multivariate = "multivariate model",pval_multivariate = "multivariate model", top = TRUE)

   rslt <- merge_at(rslt, part = "header",i = 1:1,j = 2:4)
   rslt <- merge_at(rslt, part = "header",i = 1:1,j = 5:7)


   rslt <- theme_booktabs(rslt)
   rslt <- autofit(rslt)

rslt

}


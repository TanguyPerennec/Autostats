#match.arg
#missing

dropterm <- function(object, scale = 0, test = c("none", "Chisq", "F"), k = 2, sorted = FALSE, verbose = FALSE){
      x <- model.matrix(object)
      n <- nrow(x)
      asgn <- attr(x, "assign")
      tl <- attr(object$terms, "term.labels")
      scope <- tl
      ns <- length(scope)

      ndrop <- match(scope, tl)
      rdf <- object$df.residual
      chisq <- object$deviance
      dfs <- numeric(ns)
      dev <- numeric(ns)
      y <- object$y
      wt <- object$prior.weights
      for(i in seq_len(ns)) {
         if(verbose) cat("\rtrying .......", scope[i])
         ii <- seq_along(asgn)[asgn == ndrop[i]]
         jj <- setdiff(seq(ncol(x)), ii)
         z <-  glm.fit(x[, jj, drop = FALSE], y, wt, offset=object$offset,family="binomial", control=object$control)
         dfs[i] <- z$rank
         dev[i] <- z$deviance
      }
      scope <- c("<none>", scope)
      dfs <- c(object$rank, dfs)
      dev <- c(chisq, dev)
      dispersion <-
         if (is.null(scale) || scale == 0){summary(object, dispersion = NULL)$dispersion}else{scale}
      fam <- object$family$family
      loglik <-
         if(fam == "gaussian") {
            if(scale > 0) dev/scale - n else n * log(dev/n)
         } else dev/dispersion
      aic <- loglik + k * dfs
      dfs <- dfs[1L] - dfs
      dfs[1L] <- NA
      aic <- aic + (extractAIC(object, k = k)[2L] - aic[1L])
      aod <- data.frame(Df = dfs, Deviance = dev, AIC = aic,
                        row.names = scope, check.names = FALSE)
      o <- if(sorted) order(aod$AIC) else seq_along(aod$AIC)
      if(all(is.na(aic))) aod <- aod[, -3]
      test <- match.arg(test)
      if(test == "Chisq") {
         dev <- pmax(0, loglik - loglik[1L])
         dev[1L] <- NA
         nas <- !is.na(dev)
         LRT <- if(dispersion == 1) "LRT" else "scaled dev."
         aod[, LRT] <- dev
         dev[nas] <- safe_pchisq(dev[nas], aod$Df[nas], lower.tail=FALSE)
         aod[, "Pr(Chi)"] <- dev
      } else if(test == "F") {
         if(fam == "binomial" || fam == "poisson")
            warning(gettextf("F test assumes 'quasi%s' family", fam),
                    domain = NA)
         dev <- aod$Deviance
         rms <- dev[1L]/rdf
         dev <- pmax(0, dev - dev[1L])
         dfs <- aod$Df
         rdf <- object$df.residual
         Fs <- (dev/dfs)/rms
         Fs[dfs < 1e-4] <- NA
         P <- Fs
         nas <- !is.na(Fs)
         P[nas] <- safe_pf(Fs[nas], dfs[nas], rdf, lower.tail=FALSE)
         aod[, c("F value", "Pr(F)")] <- list(Fs, P)
      }
      aod <- aod[o, ]
      head <- c("Single term deletions", "\nModel:", deparse(formula(object)))
      if(scale > 0) head <- c(head, paste("\nscale: ", format(scale), "\n"))
      class(aod) <- c("anova", "data.frame")
      attr(aod, "heading") <- head
      aod
   }



###################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
###################################################################################################


stepAIC(model)


stepAIC <- function(object, scope=NULL, scale = 0, method = c("backward"),verbose = 1, keep = NULL, steps = 1000, k = 2, ...){

   mydeviance <- function(x, ...){
      dev <- deviance(x)
      if(!is.null(dev)) dev else extractAIC(x, k=0)[2]
      }

   cut.string <- function(string){
      if(length(string) > 1L)
         string[-1L] <- paste("\n", string[-1], sep = "")
      string
      }

   re.arrange <- function(keep){
      namr <- names(k1 <- keep[[1]])
      namc <- names(keep)
      nc <- length(keep)
      nr <- length(k1)
      array(unlist(keep, recursive = FALSE), c(nr, nc), list(namr, namc))
      }




   step.results <- function(models, fit, object, usingCp=FALSE){
      change <- sapply(models, "[[", "change")
      rd <- sapply(models, "[[", "deviance")
      dd <- c(NA, abs(diff(rd)))
      rdf <- sapply(models, "[[", "df.resid")
      ddf <- c(NA, abs(diff(rdf)))
      AIC <- sapply(models, "[[", "AIC")
      heading <- c("Stepwise Model Path \nAnalysis of Deviance Table",
                   "\nInitial Model:", deparse(formula(object)),
                   "\nFinal Model:", deparse(formula(fit)),
                   "\n")
      aod <- data.frame(Step = change, Df = ddf, Deviance = dd,
                      "Resid. Df" = rdf, "Resid. Dev" = rd,
                      AIC = AIC, check.names = FALSE)
      attr(aod, "heading") <- heading
      class(aod) <- c("Anova", "data.frame")
      fit$anova <- aod
      fit
   }



   Terms <- terms(object)
   object$formula <- Terms
   object$call$formula <- Terms

   # SCOPE
   ######
   if(is.null(scope)) {
      fdrop <- numeric()
      fadd <- attr(Terms, "factors")
   }else{
      if(is.list(scope)){
         fdrop <- if(!is.null(fdrop <- scope$lower))
            attr(terms(update.formula(object, fdrop)), "factors")
         else numeric()
         fadd <- if(!is.null(fadd <- scope$upper))
            attr(terms(update.formula(object, fadd)), "factors")
      }else {
         fadd <- if(!is.null(fadd <- scope))
            attr(terms(update.formula(object, scope)), "factors")
         fdrop <- numeric()
      }
   }
   ######

      models <- vector("list", steps)

      if(!is.null(keep)) keep.list <- vector("list", steps)

      n <- nobs(object, use.fallback = TRUE)  # might be NA

      fit <- object

      bAIC <- extractAIC(fit, scale, k = k)
      edf <- bAIC[[1]]
      bAIC <- bAIC[2]
      if(is.na(bAIC)) stop("AIC is not defined for this model, so 'stepAIC' cannot proceed")
      if(bAIC == -Inf) stop("AIC is -infinity for this model, so 'stepAIC' cannot proceed")

      nm <- 1
      Terms <- terms(fit)
      if(verbose) cat("Start:  AIC=", format(round(bAIC, 2)), "\n",cut.string(deparse(formula(fit))), "\n\n", sep='')
      models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - edf,change = "", AIC = bAIC)
      if(!is.null(keep)) keep.list[[nm]] <- keep(fit, bAIC)

      while(steps > 0) {
         steps <- steps - 1
         AIC <- bAIC
         ffac <- attr(Terms, "factors")
         ## don't drop strata terms
         if(!is.null(sp <- attr(Terms, "specials")) && !is.null(st <- sp$strata)) ffac <- ffac[-st,]
         scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
         aod <- NULL
         change <- NULL

         # BACKWARD
         if("backward" %in% method && length(scope$drop)) {
            aod <- dropterm(fit, scope$drop, scale = scale, k = k)
            rn <- row.names(aod)
            row.names(aod) <- c(rn[1L], paste("-", rn[-1L], sep=" "))

            ## drop all zero df terms first.
            if(any(aod$Df == 0, na.rm=TRUE)) {
               zdf <- aod$Df == 0 & !is.na(aod$Df)
               nc <- match("AIC", names(aod))
               ch <- abs(aod[zdf, nc] - aod[1, nc]) > 0.01
               if(any(is.finite(ch) & ch)) {
                  warning("0 df terms are changing AIC")
                  zdf <- zdf[!ch]
               }
               ## drop zero df terms first: one at time since they
               ## may mask each other
               if(length(zdf) > 0L)
                  change <- rev(rownames(aod)[zdf])[1L]
            }
         }


         if(is.null(change) || is.na(change)) {
            if("forward" %in% method && length(scope$add)) {
               aodf <- addterm(fit, scope$add, scale = scale, k = k)
               rn <- row.names(aodf)
               row.names(aodf) <- c(rn[1L], paste("+", rn[-1L], sep=" "))
               aod <-
                  if(is.null(aod)) aodf
               else rbind(aod, aodf[-1, , drop=FALSE])
            }

            attr(aod, "heading") <- NULL
            if(is.null(aod) || ncol(aod) == 0) break
            ## need to remove any terms with zero df from consideration
            nzdf <- if(!is.null(aod$Df)) aod$Df != 0 | is.na(aod$Df)
            aod <- aod[nzdf, ]
            if(is.null(aod) || ncol(aod) == 0) break
            nc <- match("AIC", names(aod))
            o <- order(aod[, nc])
            if(verbose) print(aod[o,  ])
            if(o[1L] == 1) break
            change <- rownames(aod)[o[1]]
         }


         DF_fit <- DF[,c(y,colnames(DF)[colnames(DF)])]
         fit <- logit(DF[,c(y,var_uni)])
         nnew <- nobs(fit, use.fallback = TRUE)

         if(all(is.finite(c(n, nnew))) && nnew != n)
            stop("number of rows in use has changed: remove missing values?")
         Terms <- terms(fit)
         bAIC <- extractAIC(fit, scale, k = k)
         edf <- bAIC[1L]
         bAIC <- bAIC[2L]
         if(verbose)
            cat("\nStep:  AIC=", format(round(bAIC, 2)), "\n",cut.string(deparse(formula(fit))), "\n\n", sep='')


         ## add a tolerance as dropping 0-df terms might increase AIC slightly
         if(bAIC >= AIC + 1e-7) break

         nm <- nm + 1
         models[[nm]] <-list(deviance = mydeviance(fit), df.resid = n - edf,change = change, AIC = bAIC)
         if(!is.null(keep)) keep.list[[nm]] <- keep(fit, bAIC)
      }
      if(!is.null(keep)) fit$keep <- re.arrange(keep.list[seq(nm)])

      step.results(models = models[seq(nm)], fit, object, usingCp)
   }

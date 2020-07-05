multivariate_selection <- function(method="backward"){












}












complete_separation <- FALSE
complete_separation <- tryCatch(glm(DF_uni, family = binomial, data = DF_uni,separation="find") -> mod_uni,
                                error = function(e) {# if "fitted probabilities numerically 0 or 1 occurred"
                                   msg <- paste0(var_uni," is causing perfect separation")
                                   msg
                                   firth <- TRUE
                                   return(firth)
                                },
                                finally={}
)
complete_separation -> firth
Clogg = FALSE

if(complete_separation){
   # If there is a perfect separation : (Heinz et Al)
   #1. Omission of NV from the model : provides no information about the effect of this unusually strong and therefore important risk factor and furthermore does not allow adjusting effects of the other risk factors for the effect of NV. Therefore, this option is totally inappropriate.
   #2. Changing to a different type of model : Models whose parameters have di􏰃erent interpretations that are not risk-related (option 2) may be less appealing.
   #3. Use of an ad hoc adjustment (data manipulation) : While simple adjustments of cell frequencies can have undesirable properties (Agresti and Yang), Clogg et al. pursued a more elaborate approach: creat p = sum(yi/n) with y=(0,1) ; add pk/g artifficial responses and (1−p)k/g artifficial non-responses to each of the g groups of distinct risk factor patterns, and then to do a standard analysis on the augmented data set. k is the number of parameters to estimate
   #4. Exact logistic regression : permits replacement of the unsuitable maximum likelihood estimate by a median unbiased estimate [4]: let xir denote the value of the rth risk factor for individual i (16i6n; 26r6k) and let xi1=1 for all i. Then the median unbiased estimate of a parameter 􏰁r as well as corresponding inference are based on the exact null distribution of the su􏰂cient statistic Tr = 􏰊ni=1 yixir of 􏰁r, conditional on the observed values of the other su􏰂cient statistics Tr′ ; r′ ̸= r. An e􏰂cient algorithm is available to evaluate these conditional distributions [16] which should contain a su􏰂cient number of elements. This requirement may be violated with a single continuous risk factor but also with multiple dichotomous risk factors. In the endometrial cancer study we cannot apply exact logistic regression because there are two continuous risk factors in the model leading to degenerate distributions of all su􏰂cient stat 5) im
   #5. Standard analysis with BettaˆNV set to a ‘high’ value (for example, the value of BettaˆNV of that iteration at which the log-likelihood changed by less than 10−6).permits replacement of the unsuitable maximum likelihood estimate by a median unbiased estimate [4]: let xir denote the value of the rth risk factor for individual i (16i6n; 26r6k) and let xi1=1 for all i. Then the median unbiased estimate of a parameter 􏰁r as well as corresponding inference are based on the exact null distribution of the su􏰂cient statistic Tr = 􏰊ni=1 yixir of 􏰁r, conditional on the observed values of the other su􏰂cient statistics Tr′ ; r′ ̸= r. An e􏰂cient algorithm is available to evaluate these conditional distributions [16] which should contain a su􏰂cient number of elements. This requirement may be violated with a single continuous risk factor but also with multiple dichotomous risk factors. In the endometrial cancer study we cannot apply exact logistic regression because there are two continuous risk factors in the model leading to degenerate distributions of all su􏰂cient statistics.permits replacement of the unsuitable maximum likelihood estimate by a median unbiased estimate [4]: let xir denote the value of the rth risk factor for individual i (16i6n; 26r6k) and let xi1=1 for all i. Then the median unbiased estimate of a parameter 􏰁r as well as corresponding inference are based on the exact null distribution of the su􏰂cient statistic Tr = 􏰊ni=1 yixir of 􏰁r, conditional on the observed values of the other su􏰂cient statistics Tr′ ; r′ ̸= r. An e􏰂cient algorithm is available to evaluate these conditional distributions [16] which should contain a su􏰂cient number of elements. This requirement may be violated with a single continuous risk factor but also with multiple dichotomous risk factors. In the endometrial cancer study we cannot apply exact logistic regression because there are two continuous risk factors in the model leading to degenerate distributions of all su􏰂cient statistics.
   #6. Firth's method
   if(firth) logistf::logistf(DF[,y]~DF[,var_uni], data = DF_uni, pl = FALSE, firth = TRUE) -> mod_uni
   vector_firth <- c(vector_firth,var_uni)
   # 7. re-cast the model
}

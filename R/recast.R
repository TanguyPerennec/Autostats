recast <-function(DF){

}

library(tidyverse)
library(questionr)

#
nb_breaks=5
nb_entier = TRUE
max_break = 9
#

var <- DF$Age

trunc(max(var)) + 1 -> max_int
trunc(min(var)) -> min_int
range = max_int - min_int


possible_breaks <- 1+ seq(1:min(max_break-1,range-1))
for(b in possible_breaks[possible_breaks<=range]){
   if(range%%b != 0)
      possible_breaks <- possible_breaks[-(b-1)]
}

result_break <- as.data.frame(var)
i = 2
for(breaking in possible_breaks){
   step = (max_int - min_int)/breaking
   breaks_test = min_int
   for(m in 1:breaking){
      breaks_test[m+1] = breaks_test[m] + step
   }
   cut(var,breaks = breaks_test, include.lowest = TRUE) -> new_var
   result_break[,i] <- new_var
   i = i + 1
}


# Check du résultat
sum_sq_diff = vector()
for (u in 1:(length(result_break)-1)){
   mean(table(result_break$V2)) -> mean
   p = 1
   for (i in table(result_break$V2)){
      (mean - i)^2 -> sq_diff[p]
      p + 1 -> p
   }
   sum(sq_diff) -> sum_sq_diff[u]
   names(sum_sq_diff)[u] <- u+1
}

names(sum_sq_diff[order(sum_sq_diff)][1]) -> best_breaks


#final cut
final_cut <- result_break[,as.numeric(best_breaks)]


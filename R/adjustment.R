clogg_transformation <- function(){
   k = 2 #numberofparametre
   pi=vector()
   new_DF_uni <- as.matrix(data)
   row.names(new_DF_uni) <- NULL
   table(data) -> table_Clogg
   n_to_add = table(data)
   g_number <- length(colnames(table_Clogg))
   for (g in 1:length(colnames(table_Clogg))){
      n = sum(table_Clogg[,g])
      pi[g] = table_Clogg[2,g]/n
      n_to_add[1,g] <- (1-pi[g])*g_number/k
      n_to_add[2,g] <- (pi[g])*g_number/k
   }
}

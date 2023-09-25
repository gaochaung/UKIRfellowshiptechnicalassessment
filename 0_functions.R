#p value function
p_value_calculate_kruskal <- function(D) {
  if (dim(D)[1]!=0 & length(unique(D$crisis))>1 & length(unique(D$var2))>1){
    t <- kruskal.test(D$crisis ~ D$var2, data = D)
    if (t[["p.value"]]<0.05) {
      return("< 0.05") 
    } else {
      return(round(t[["p.value"]],3))
    }} else {
      return("NA")
    }
}

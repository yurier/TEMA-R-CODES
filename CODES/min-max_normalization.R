Min_max_nrmlztn <- function(data){
  for (i in 1:ncol(data)){
    maxi<-max(data[,i])
    mini<-min(data[,i])
    data[,i]=(data[,i]-mini)/(maxi-mini)
  }

return(data)
  }
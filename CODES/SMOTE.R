SMOTE <- function(t,N,k){
library(grid)
library(lattice)
library(modeltools)
library(stats4)
library(flexclust)

synth<-data.frame()
index<-sample(seq_len(nrow(t)), size =N,replace=TRUE)
dista<-dist2(x=t,y=t,method="minkowski",p=2)
rownames(dista)<-1:nrow(dista)
  
for (i in index){
  knear<-t[order(dista[,i]),][2:(k+1),]
  rand<-runif(1,0,1)
  aux<-(1-rand)*t[i,]+rand*knear[sample(seq_len(k),1),]
  synth<-rbind(aux,synth)
}

  return(synth) 
}
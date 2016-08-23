library(class)

#for (s in 1:nrow(lista_best_normal_wthoutdef)){
  plasma<-df_MERGE_BIO_complete[complete.cases(df_MERGE_BIO_complete),]
  plasma$NOISE<-rnorm(nrow(plasma))
  PLASMA<-plasma[plasma$VISCODE=='bl',]
  dement<-PLASMA[PLASMA$DX=='Dementia',]
  mci<-PLASMA[PLASMA$DX=='MCI',]
  normal<-PLASMA[PLASMA$DX=='NL',]
  dement$DX<-"AD"
  mci$DX<-"MCI"
  normal$DX<-"CN"
  PLASMA<-rbind(dement,mci,normal)
  row.names(PLASMA)<-1:nrow(PLASMA)
  PLASMA$DX<-as.factor(as.character(PLASMA$DX))
  dement<-PLASMA[PLASMA$DX=='AD',]
  mci<-PLASMA[PLASMA$DX=='MCI',]
  normal<-PLASMA[PLASMA$DX=='CN',]
  PLASMA<-subset(PLASMA, select=c("FDG","AV45","CDRSB","ADAS11","MMSE","RAVLT.perc.forgetting","ABETA","PTAU","NOISE","DX"))
  PLASMA[,1:(ncol(PLASMA)-1)]<-Min_max_nrmlztn(PLASMA[,1:(ncol(PLASMA)-1)])
  
  modres<-NULL
  modcomp<-NULL
  hit<-NULL
  mean_hit<-0
  table_it<-data.frame()
for (i in 2:(ncol(PLASMA)-1)){
  combs<-combn(1:9,i)
  for(ii in 1:ncol(combs)){
    ptm <- proc.time()
  for (K in c(1:25)){
    for(sds in c(1:3)){
      
    for (j in c(1:nrow(PLASMA))){
      
      train<-PLASMA[-j,c(combs[,ii],10)]
      test<-PLASMA[j,c(combs[,ii],10)]
      
      cltra<-train[,ncol(train)]
      train<-train[,-ncol(train)]
      
      cltes<-test[,ncol(test)]
      test<-test[,-ncol(test)]
      
      ans1 <- knn(train,test,cltra,K,use.all = FALSE)
      modres<-c(as.character(ans1),modres)
      modcomp<-c(as.character(cltes),modcomp)}
    hit<-c(hit,as.data.frame(t(summary(as.integer(modcomp==modres))))[4,3])
    modres<-NULL
    modcomp<-NULL}
    print(hit)
    if (mean(hit)>mean_hit){
      mean_hit<-mean(hit)
      sd_hit<-sd(hit)
      k_opt<-K}
      hit<-NULL
    
  }
    table_it<-rbind(table_it,data.frame(k_opt,mean_hit,sd_hit,i,ii,(proc.time() - ptm)[][3][[1]]))
    mean_hit<-0
    print(table_it)}

}
  
table_it_normal<-table_it
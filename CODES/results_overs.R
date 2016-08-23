library(data.table)
library(class)

lista_best_overs<-as.data.frame(NULL)
for (i in 2:7){
  lista_best_overs<-rbind(table_it_overs_nondef[table_it_overs_nondef[,4]==i,][1,],lista_best_overs)
}

lista_best_overs<-lista_best_overs[order(lista_best_overs[,6]),]

for (s in 1:nrow(lista_best_overs)){
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
  PLASMA1<-subset(PLASMA, select=c("FDG","AV45","CDRSB","ADAS11","MMSE","RAVLT.perc.forgetting","ABETA","PTAU","NOISE"))
  #PLASMA1<-PLASMA1[,c(combn(1:9,lista_best_overs[s,4])[,lista_best_overs[s,5]])]
  PLASMA1<-PLASMA1[,c(3,5)]
  PLASMA<-cbind(DX=PLASMA$DX,PLASMA1)
  PLASMA[,2:ncol(PLASMA)]<-Min_max_nrmlztn(PLASMA[,2:ncol(PLASMA)])
  
  CV<-10
  pnorm<-2
  
  #perc<-seq(0,1.0,0.1)
  perc<-seq(1)
  conf<-matrix(0,3,3,dimnames = list(c("AD","MCI","CN"),c("AD","MCI","CN")))
  matrix_aux<-matrix(0,3,3,dimnames = list(c("AD","MCI","CN"),c("AD","MCI","CN")))
  trains<-as.data.frame(c(1:length(perc)))
  tests<-as.data.frame(c(1:length(perc)))

  indexN<-matrix(0,ncol=CV,nrow=floor(nrow(normal)/CV)); normal_list<-as.numeric(rownames(normal))
  indexD<-matrix(0,ncol=CV,nrow=floor(nrow(dement)/CV)); dement_list<-as.numeric(rownames(dement)) 
  indexM<-matrix(0,ncol=CV,nrow=floor(nrow(mci)/CV)); mci_list<-as.numeric(rownames(mci))
  
  indexN[,1]<-sample(normal_list, size = floor(nrow(normal)/CV))
  indexD[,1]<-sample(dement_list, size = floor(nrow(dement)/CV))
  indexM[,1]<-sample(mci_list, size = floor(nrow(mci)/CV))
  
  for (i in 1:(CV-1)){
    indexN[,i+1]<-sample(normal_list[!normal_list %in% indexN[,1:i]], size = floor(nrow(normal)/CV))
    indexD[,i+1]<-sample(dement_list[!dement_list%in%indexD[,1:i]], size = floor(nrow(dement)/CV))
    indexM[,i+1]<-sample(mci_list[!mci_list%in%indexM[,1:i]], size = floor(nrow(mci)/CV))
  }
  
  i<-1
  for (K in 1:25){
    for (j in c(1:CV)){
      plasma<-PLASMA
      
      Dtest <-plasma[indexD[,j], ]
      classD <- plasma[indexD[,-j], ]
      Mtest <-plasma[indexM[,j], ]
      classM <- plasma[indexM[,-j], ]
      Ntest <-plasma[indexN[,j], ]
      classN <- plasma[indexN[,-j], ]
      
      test<-rbind(Dtest,Mtest,Ntest)
      cltes<-test[,1]
      test<-test[,2:(ncol(test))]
      
      classD<-rbind(classD,data.frame(DX='AD',SMOTE(classD[,-1],nrow(classM)-nrow(classD),5)))
      classN<-rbind(classN,data.frame(DX='CN',SMOTE(classN[,-1],nrow(classM)-nrow(classN),5)))
      
      train<-rbind(classD,classM,classN)
      cltra<-train[,1]
      train<-train[,2:(ncol(train))]
      
      ans1 <- knn(train,test,cltra,K)
      modtest<-ans1
      
      modtest<-as.character(modtest)
      cltes<-as.character(cltes)
      
      datag<-data.frame(cbind(as.vector(modtest),as.vector(cltes)))
      colnames(datag)<-c('predict','correct')
      conf <- data.frame()
      for (ii in c("AD","MCI","CN")){
        for(jj in c("AD","MCI","CN")){
          aux1<-datag[datag$correct==ii,]
          aux2<-aux1[aux1$predict==jj,]
          conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
      tests[K,j]<-(conf[1,1]+conf[2,2]+conf[3,3])/3
      
      #tests[K,j]<-as.data.frame(t(summary(as.integer(modtest==cltes))))[4,3]
      print(c(tests[K,j],j,K))
    }
  }
  
  test_mean<-NULL
  test_sd<-NULL
  
  for (i in c(1:K)){
    test_mean[i]<-mean(t(tests[i,1:CV]))
    test_sd[i]<-sd(t(tests[i,1:CV]))
  }
  
  lista_best_overs[s,8]<-which.max(test_mean)
  lista_best_overs[s,9]<-test_mean[which.max(test_mean)]
  lista_best_overs[s,10]<-test_sd[which.max(test_mean)]
  print(lista_best_overs)
}
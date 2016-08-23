library(data.table)
library(class)

library(data.table)
library(class)
lista_best_normal<-as.data.frame(NULL)
for (i in 2:7){
  lista_best_normal<-rbind(table_it_normal_nondef[table_it_normal_nondef[,4]==i,][1,],lista_best_normal)
}

lista_best_normal<-lista_best_normal[order(lista_best_normal[,6]),]

for (s in 1:nrow(lista_best_normal)){
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
  #PLASMA1<-PLASMA1[,c(combn(1:9,lista_best_normal[s,4])[,lista_best_normal[s,5]])]
  PLASMA1<-PLASMA1[,c(combn(1:9,2)[,34])]
  PLASMA<-cbind(DX=PLASMA$DX,PLASMA1)
  PLASMA[,2:ncol(PLASMA)]<-Min_max_nrmlztn(PLASMA[,2:ncol(PLASMA)])

  CV<-10
  pnorm<-2
  
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
  i<-0
  for (K in 1:25){
    for (j in c(1:CV)){
      
      plasma<-PLASMA
      dement<-PLASMA[PLASMA$DX=='AD',]
      mci<-PLASMA[PLASMA$DX=='MCI',]
      normal<-PLASMA[PLASMA$DX=='CN',]
      
      #indexM<-sample(seq_len(nrow(mci)), size = floor(nrow(mci)*i))
      index_M<-sample(as.vector(indexM[,-j]), size = floor((1-i)*(length(as.vector(indexM[,-j])))+(i)*(length(as.vector(indexD[,-j])))))
      index_N<-sample(as.vector(indexN[,-j]), size = floor((1-i)*(length(as.vector(indexN[,-j])))+(i)*(length(as.vector(indexD[,-j])))))
      
      Dtest <-plasma[indexD[,j], ]
      classD <- plasma[indexD[,-j], ]
      
      Mtest <-plasma[indexM[,j], ]
      classM <- plasma[index_M, ]
      
      Ntest <-plasma[indexN[,j], ]
      classN <- plasma[index_N, ]

      train<-rbind(classD,classM,classN)
      cltra<-train[,1]
      train<-train[,2:(ncol(train))]
      
      test<-rbind(Dtest,Mtest,Ntest)
      cltes<-test[,1]
      test<-test[,2:(ncol(test))]
      
      ans1 <- knn(train,test,cltra,K)
      modtest<-ans1
      
      modtest<-as.character(modtest)
      cltes<-as.character(cltes)
      #tests[K,j]<-as.data.frame(t(summary(as.integer(modtest==cltes))))[4,3]
      datag<-data.frame(cbind(as.vector(modtest),as.vector(cltes)))
      colnames(datag)<-c('predict','correct')
      conf <- data.frame()
      for (ii in c("AD","MCI","CN")){
        for(jj in c("AD","MCI","CN")){
          aux1<-datag[datag$correct==ii,]
          aux2<-aux1[aux1$predict==jj,]
          conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
      tests[K,j]<-(conf[1,1]+conf[2,2]+conf[3,3])/3
      }
}
  
  test_mean<-NULL
  test_sd<-NULL
  
  for (i in c(1:K)){
    test_mean[i]<-mean(t(tests[i,1:CV]))
    test_sd[i]<-sd(t(tests[i,1:CV]))
  }
  lista_best_normal[s,8]<-which.max(test_mean)
  lista_best_normal[s,9]<-test_mean[which.max(test_mean)]
  lista_best_normal[s,10]<-test_sd[which.max(test_mean)]
  print(lista_best_normal)
}
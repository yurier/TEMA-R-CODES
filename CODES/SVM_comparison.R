s<-8
library(e1071)
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
  PLASMA1<-PLASMA1[,c(combn(1:9,lista_best_normal[s,4])[,lista_best_normal[s,5]])]
  PLASMA<-cbind(PLASMA$DX,PLASMA1)
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
      
      test<-rbind(Dtest,Mtest,Ntest)
      cltes<-test[,1]
      test<-test[,2:(ncol(test))]
      train
      model <- svm( train$DX~., train )
      res <- predict( model, newdata=test )

      
      datag<-data.frame(cbind(as.vector(res),as.vector(cltes)))
      colnames(datag)<-c('predict','correct')
      conf <- data.frame()
      for (ii in c("AD","MCI","CN")){
        for(jj in c("AD","MCI","CN")){
          aux1<-datag[datag$correct==ii,]
          aux2<-aux1[aux1$predict==jj,]
          conf[ii,jj]<-as.numeric(nrow(aux2)/nrow(aux1))}}
      matrix_aux<-matrix_aux+conf
      tests[1,j]<-(conf[1,1]+conf[2,2]+conf[3,3])/3
      print(j)      
    }
    matrix_aux<-matrix_aux/CV
    #print(nrow(classD))
    #print(nrow(classM))
    #print(nrow(classN))
    #cairo_ps(width=5,height = 5 ,file=sprintf('confunder_%g.eps',s))
    #corrplot(as.matrix(matrix_aux),is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 1),tl.col="black")
    #dev.off()
    conf<-matrix(0,3,3,dimnames = list(c("AD","MCI","CN"),c("AD","MCI","CN")))
    matrix_aux<-matrix(0,3,3,dimnames = list(c("AD","MCI","CN"),c("AD","MCI","CN")))
  
  test_mean<-NULL
  test_sd<-NULL
  
 for (i in c(1)){
    test_mean[i]<-mean(t(tests[i,1:CV]))
    test_sd[i]<-sd(t(tests[i,1:CV]))
  }
#  lista_best_normal[s,7]<-which.max(test_mean)
#  lista_best_normal[s,8]<-test_mean[which.max(test_mean)]
#  lista_best_normal[s,9]<-test_sd[which.max(test_mean)]
library(kknn)
t<-NULL
s<-1
for (i in 1:10){
  nameee<-rownames(as.data.frame(answer_normal[[1]][lista_best_normal[,6][s]]))
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
  data<-subset(PLASMA, select=c("DX","ABETA","PTAU"))
  data[,2:ncol(data)]<-Min_max_nrmlztn(data[,2:ncol(data)])
  
  
  m <- dim(data)[1]
  val <- sample(1:m, size = round(m/3), replace = FALSE,
                prob = rep(1/m, m))
  data.learn <- data[-val,]
  data.valid <- data[val,]
  data.kknn <- kknn(DX~., data.learn, data.valid, distance = 1,
                    kernel = "triangular")
  summary(data.kknn)
  fit <- fitted(data.kknn)
  matrix<-table(data.valid$DX, fit)
  value<-rowSums(matrix)
  matrix<-matrix/value
  
  t[i]<-(matrix[1,1]+matrix[2,2]+matrix[3,3])/3
}
mean(t); sd(t)
combination_overs <- function(data,synth,cl_synth,bignun,neigh){
  library(class)
  ##Organizando a informa?ao
  ptm <- proc.time() #contagem do tempo
  dx1<-data[which(data$DXCHANGE=='CN'),]  #subseting para Normal
  dx2<-data[which(data$DXCHANGE=='MCI'),]  #subseting para MCI
  dx3<-data[which(data$DXCHANGE=="AD"),]  #subseting para Alzheimer
  #min_size_class<-min(nrow(dx1),nrow(dx2),nrow(dx3)) #menor classe entre as 3
  #cl<-c(rep(1,min_size_class),rep(2,min_size_class),rep(3,min_size_class)) #vetor com classes num?rico a principio
  cl<-c(rep(1,nrow(dx1)),rep(2,nrow(dx2)),rep(3,nrow(dx3))) #vetor com classes num?rico a principio
  #dx1<-dx1[sample(nrow(dx1), min_size_class), ] #sampling for normal lesser size 
  #dx2<-dx2[sample(nrow(dx2), min_size_class), ] #sampling for MCI lesser size
  #dx3<-dx3[sample(nrow(dx3), min_size_class), ] #sampling for Alzheimer lesser size
  rownames(dx1)<-NULL #remover nome das linhas
  rownames(dx2)<-NULL #remover nome das linhas
  rownames(dx3)<-NULL #remover nome das linhas
  data<-rbind(dx1,dx2,dx3) #unir conjunto de treino redimensionado
  data$DXCHANGE<-NULL #remover coluna com informa??es sobre a classe
  ##Ranking
  test<-abs(cor(data,cl,method="spearman")) #ranking das variaveis
  test<-cbind(test,1:length(test)) #coluna com ordenamento das variaveis na posi?ao do data.frame
  test<-test[order(-test[,1]),] #ordenamento pelo valor do ranking
  cl<-factor(cl) #transformando as classes em valores n?o numericos
  vec<-test[,2]
  
  ##Evaluating
  
  table_it<-NULL
  for (j in c(2:nrow(test))){
    for (i in c(1:ncol(combn(c(1:nrow(test)),j)))){
      print(c(i,j))
      user<-combn(c(1:nrow(test)),j)
      aux<-NULL
      aux2<-NULL
      aux1<-NULL
      for (K in neigh){#seq(1,9,2)
        a1<-0
        a2<-0
        a3<-0
        aux2<-NULL
        for (s in 1:bignun){
          res<-KNN_cv_smote(data[vec[user[,i]]],cl,synth[vec[user[,i]]],cl_synth,K,2,5)
          #res<-knn(data[vec[user[,i]]],data[vec[user[,i]]], cl,K)
          p1<-sum(res[1:nrow(dx1)]==1, na.rm=TRUE)/nrow(dx1)
          p2<-sum(res[(nrow(dx1)+1):(nrow(dx1)+nrow(dx2))]==2, na.rm=TRUE)/nrow(dx2)
          p3<-sum(res[(nrow(dx1)+nrow(dx2)+1):(nrow(dx1)+nrow(dx2)+nrow(dx3))]==3, na.rm=TRUE)/nrow(dx3)
          a1<-a1+p1
          a2<-a2+p2
          a3<-a3+p3
          aux2<-rbind(aux2,c(p1,p2,p3,(p1+p2+p3)/(3),K))
        }
        aux1<-c(a1/bignun,a2/bignun,a3/bignun,(a1+a2+a3)/(3*bignun),sd(aux2[,1]),sd(aux2[,2]),sd(aux2[,3]),sd(aux2[,4]),K)
        aux<-rbind(aux,aux1)
        row.names(aux)<-NULL
        colnames(aux)<-c("N","MCI","ALZ","MEAN","sdN","sdMCI","sdALZ","sdMEAN","K")
      }
      aux<-data.frame(aux)
      pos_opt<-which.max(aux$MEAN)
      optim<-c(pos_opt,aux$MEAN[pos_opt],i,j,aux$sdMEAN[pos_opt])
      table_it<-rbind(table_it,optim)
      row.names(table_it)<-NULL
      colnames(table_it)<-c("k","max_hit","i","j","sdmax_hit")
      print(table_it)
      }}
  
  tab<-data.frame(table_it)
  tab<-tab[order(-tab[,2]),]
  max<-tab[3:4]
  vec1<-vec[combn(c(1:nrow(test)),max[1,2])[,max[1,1]]]
  vari<-list(vec1)
  for (i in 2:nrow(max)){
    vari[[length(vari)+1]] <- vec[combn(c(1:nrow(test)),max[i,2])[,max[i,1]]]  
  }
  tab<-cbind(tab,1:nrow(tab))
  return(list(vari,tab,proc.time() - ptm))
  
}
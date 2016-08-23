hllclmb <- function(data,bignun,neigh){
  ##Organizando a informa?ao
  ptm <- proc.time() #contagem do tempo
  dx1<-data[which(data$DXCHANGE=="CN"),]  #subseting para Normal
  dx2<-data[which(data$DXCHANGE=="MCI"),]  #subseting para MCI
  dx3<-data[which(data$DXCHANGE=="AD"),]  #subseting para Alzheimer
  min_size_class<-min(nrow(dx1),nrow(dx2),nrow(dx3)) #menor classe entre as 3
  cl<-c(rep(1,min_size_class),rep(2,min_size_class),rep(3,min_size_class)) #vetor com classes num?rico a principio
  dx1<-dx1[sample(nrow(dx1), min_size_class), ] #sampling for normal lesser size 
  dx2<-dx2[sample(nrow(dx2), min_size_class), ] #sampling for MCI lesser size
  dx3<-dx3[sample(nrow(dx3), min_size_class), ] #sampling for Alzheimer lesser size
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
  test_view<-test #salvando a variavel de teste
  vec<-c(test[1,2]) #inicio do vetor de variaveis selecionado.
  ncol(data)
  ##Evaluating
  ###big numbers and class and first iteration
  bst<-0
  library(class)
  axsrch<-c(1:ncol(data))
  search1<-c(1)
  search0<-2:nrow(test)
  crit<-1
  while(crit==1){
  if(length(vec)!=1){
    for (s in search1){
      aux<-NULL
      aux2<-NULL
      for (j in neigh){
        a1<-0
        a2<-0
        a3<-0
        for (i in 1:bignun){
          res<-knn.cv(data[vec[vec!=test[s,2]]], cl,k = j)
          p1<-sum(res[1:min_size_class]==1, na.rm=TRUE)/min_size_class
          p2<-sum(res[(min_size_class+1):(2*min_size_class)]==2, na.rm=TRUE)/min_size_class
          p3<-sum(res[(2*min_size_class+1):(3*min_size_class)]==3, na.rm=TRUE)/min_size_class
          a1<-a1+p1
          a2<-a2+p2
          a3<-a3+p3
          aux2<-rbind(aux2,c(p1,p2,p3,(p1+p2+p3)/(3),j))
        }
        aux1<-c(a1/bignun,a2/bignun,a3/bignun,(a1+a2+a3)/(3*bignun),sd(aux2[,1]),sd(aux2[,2]),sd(aux2[,3]),sd(aux2[,4]),j)
        aux<-rbind(aux,aux1)
        row.names(aux)<-NULL
        colnames(aux)<-c("N","MCI","ALZ","MEAN","sdN","sdMCI","sdALZ","sdMEAN","K")
      }
      aux<-data.frame(aux)
      pos_opt<-which.max(aux$MEAN)
      optim<-c(pos_opt,aux$MEAN[pos_opt],s,aux$sdMEAN[pos_opt],1)
      table_it<-rbind(table_it,optim)
      row.names(table_it)<-NULL
      colnames(table_it)<-c("k","max_hit","ord","sd_max_hit","fb")
    } #backward
  } else {table_it<-NULL}
  
    for (s in search0){
      aux<-NULL
      aux1<-NULL
      for (j in neigh){
        a1<-0
        a2<-0
        a3<-0
        aux2<-NULL
        for (i in 1:bignun){
          res<-knn.cv(data[c(vec,test[s,2])], cl,k = j)
          p1<-sum(res[1:min_size_class]==1, na.rm=TRUE)/min_size_class
          p2<-sum(res[(min_size_class+1):(2*min_size_class)]==2, na.rm=TRUE)/min_size_class
          p3<-sum(res[(2*min_size_class+1):(3*min_size_class)]==3, na.rm=TRUE)/min_size_class
          a1<-a1+p1
          a2<-a2+p2
          a3<-a3+p3
          aux2<-rbind(aux2,c(p1,p2,p3,(p1+p2+p3)/(3),j))
        }
        aux1<-c(a1/bignun,a2/bignun,a3/bignun,(a1+a2+a3)/(3*bignun),sd(aux2[,1]),sd(aux2[,2]),sd(aux2[,3]),sd(aux2[,4]),j)
        aux<-rbind(aux,aux1)
        row.names(aux)<-NULL
        colnames(aux)<-c("N","MCI","ALZ","MEAN","sdN","sdMCI","sdALZ","sdMEAN","K")
      }
      aux<-data.frame(aux)
      pos_opt<-which.max(aux$MEAN)
      optim<-c(pos_opt,aux$MEAN[pos_opt],s,aux$sdMEAN[pos_opt],0)
      table_it<-rbind(table_it,optim)
      row.names(table_it)<-NULL
      colnames(table_it)<-c("k","max_hit","ord","sd_max_hit","fb")
    } #forward
    
    tab<-data.frame(table_it)
    

    if(tab[which.max(tab$max_hit),2]>=bst) 
    {
      tab2<-table_it
      table_it<-NULL
      bst<-tab[which.max(tab$max_hit),2]
      if(tab[which.max(tab[,2]),5]==1){
        vec<-vec[vec!=test[tab[which.max(tab[,2]),3],2]] #backward elimination
        search1<-search1[search1!=tab[which.max(tab[,2]),3]]
        search0<-setdiff(axsrch,search1)
      } else {
        vec<-c(vec,test[tab[which.max(tab[,2]),3],2]) #forward add
        search0<-search0[search0!=tab[which.max(tab[,2]),3]] #nova lista de procura 
        search1<-setdiff(axsrch,search0)
      }
    } else {crit<-0}
  }
  
  return(list(bst,vec,proc.time() - ptm,tab,tab2))
  
}
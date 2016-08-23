forward_normal <- function(data,bignun,neigh){
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
  test_view<-test #salvando a variavel de teste
  vec<-c(test[1,2]) #inicio do vetor de variaveis selecionado.
  
  ##Evaluating
  ###big numbers and class and first iteration
  bst<-0
  library(class)
  search<-2:nrow(test)
  crit<-1
  table_it<-NULL
  while(crit==1){
    for (s in search){
      aux<-NULL
      aux1<-NULL
      for (j in neigh){
        a1<-0
        a2<-0
        a3<-0
        aux2<-NULL
        for (i in 1:bignun){
          res<-knn.cv(data[c(vec,test[s,2])], cl,k = j)
          p1<-sum(res[1:nrow(dx1)]==1, na.rm=TRUE)/nrow(dx1)
          p2<-sum(res[(nrow(dx1)+1):(nrow(dx1)+nrow(dx2))]==2, na.rm=TRUE)/nrow(dx2)
          p3<-sum(res[(nrow(dx1)+nrow(dx2)+1):(nrow(dx1)+nrow(dx2)+nrow(dx3))]==3, na.rm=TRUE)/nrow(dx3)
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
      optim<-c(pos_opt,aux$MEAN[pos_opt],s,aux$sdMEAN[pos_opt])
      table_it<-rbind(table_it,optim)
      row.names(table_it)<-NULL
      colnames(table_it)<-c("k","max_hit","ord","sd_max_hit")
    }
    
    tab<-data.frame(table_it)
    
    if(tab[which.max(tab$max_hit),2]>bst)
    {
      tab2<-tab
      table_it<-NULL
      bst<-tab[which.max(tab$max_hit),2]
      vec<-c(vec,test[tab[which.max(tab[,2]),3],2]) #nova lista de variaveis 
      search<-search[search!=tab[which.max(tab[,2]),3]] #nova lista de procura 
      if(length(search)<1){crit<-0}
    } else {crit<-0}
  }
  
  return(list(bst,vec,proc.time() - ptm,tab,tab2))
  
}
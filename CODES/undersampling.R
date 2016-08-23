undersample <- function(train){
  library(class)

  dx1<-train[which(train$DX=='CN'),]  #subseting para Normal
  dx2<-train[which(train$DX=='MCI'),]  #subseting para MCI
  dx3<-train[which(train$DX=="AD"),]  #subseting para Alzheimer
  min_size_class<-min(nrow(dx1),nrow(dx2),nrow(dx3)) #menor classe entre as 3
  dx1<-dx1[sample(nrow(dx1), min_size_class), ] #sampling for normal lesser size 
  dx2<-dx2[sample(nrow(dx2), min_size_class), ] #sampling for MCI lesser size
  dx3<-dx3[sample(nrow(dx3), min_size_class), ] #sampling for Alzheimer lesser size
  train<-rbind(dx1,dx2,dx3)
  return(train)
  
}
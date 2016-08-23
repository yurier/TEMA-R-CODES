batt<- function(d1,d2){
  library("MASS")
  D1<-as.matrix(d1)
  D2<-as.matrix(d2)
  m1<-c(mean(D1[,1]),mean(D1[,2]))
  m2<-c(mean(D2[,1]),mean(D2[,2]))
  s1<-cov(D1)
  s2<-cov(D2)
  S<-(s1+s2)/2
  D<-(1/8)*(m1-m2)%*%ginv(S)%*%(m1-m2)+(1/2)*(log(det(S)/(sqrt(det(s1)*det(s2)))))
  return(D)
}
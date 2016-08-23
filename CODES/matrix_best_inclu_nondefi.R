answer1<-table_it_overs_noncog

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


name<-c(1,2,7,8,9)
cont<-matrix(0,length(name),length(name))
library(corrplot)
library(ggplot2)

x<-1
y<-1
combn(1:9,7)[,20]
aux<-0
pos_i<-NULL
pos_j<-NULL
aux1<-0;aux2<-0;

for (i in name){
  for (j in name){
    for (ij in 1:nrow(answer1)){
      auuxx<-combn(1:9,answer1[ij,4])[,answer1[ij,5]]
      same_i<-as.integer(any(auuxx==i))
      same_j<-as.integer(any(auuxx==j))
      if ((same_i==1)&(same_j==0)){
        aux1<-1+aux1
        pos_i[aux1]<-ij}
      if ((same_i==0)&(same_j==1)){
        aux2<-1+aux2
        pos_j[aux2]<-ij}
    }
    for (ik in pos_i){
      value<-table(answer1[ik,2]>answer1[pos_j,2])["TRUE"][[1]]
      value<-ifelse(is.na(value),yes = 0,no = value) 
      print(value)
      aux<-aux+value}
    cont[x,y]<-aux/(length(pos_i)*length(pos_j))
    y<-y+1
    pos_i<-NULL
    pos_j<-NULL
    aux1<-0;aux2<-0; aux<-0
  }
  y<-1
  x<-x+1
}


cont[is.nan(cont)]<-0

ccont<-colSums(cont)/4
names(ccont)<-NULL
rcont<-rowSums(cont)/4
names(rcont)<-NULL
cont<-rbind(cont,ccont)
cont<-cbind(cont,c(rcont,0))
name<-c(1,2,7,8,"N","A")
rownames(cont)<-name
colnames(cont)<-name

#cairo_ps(width=6,height = 6 ,file="cont_under.eps")
corrplot(floor(cont*1000)/10,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 100),tl.col="black",tl.cex = 1.5,number.cex = 1.70,cl.pos="n",tl.srt=0)
#dev.off()
#cont_under<-cont
rownames(cont_under)<-rownames(cont)
colnames(cont_under)<-rownames(cont)
colnames(cont_overs)<-rownames(cont)
rownames(cont_overs)<-rownames(cont)
colnames(cont_normal)<-rownames(cont)
rownames(cont_normal)<-rownames(cont)

cairo_ps(width=8,height = 2.9 ,file="cont.eps")
par(mfrow = c(1,3),oma = c(0,0,0,0),mar = c(0,0,0,0))
corrplot(floor(cont_normal*1000)/10,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 100),tl.col="black",tl.cex = 1.5,number.cex = 1.6,cl.pos="n",tl.srt=0)
corrplot(floor(cont_under*1000)/10,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 100),tl.col="black",tl.cex = 1.5,number.cex = 1.6,cl.pos="n",tl.srt=0)
corrplot(floor(cont_overs*1000)/10,is.corr = FALSE,method='color',addCoef.col="white",col=colorRampPalette(c("white","grey",'black'))(250),outline=TRUE,cl.lim=c(0, 100),tl.col="black",tl.cex = 1.5,number.cex = 1.6,cl.pos="n",tl.srt=0)
dev.off()




#cairo_ps(width=6,height =2.5  ,file='ujn.eps')
#ggplot()+theme_bw()+
#  geom_line(data=result,aes(x=j, y=validation_S,colour=clasS),size=1)+
#  geom_line(data=result,aes(x=j, y=validation,colour=clasU),size=1)+
#  geom_errorbar(data=result,aes(colour=clasS,x=j,y=validation_S,ymax=validation_S+sd_validation_S,ymin=validation_S-sd_validation_S),size=1)+
#  geom_errorbar(data=result,aes(colour=clasU,x=j,y=validation,ymax=validation+sd_validation,ymin=validation-sd_validation),size=1)+
#  scale_color_manual(values=c("#999999","#E69F00"),name="")+scale_x_continuous(breaks=c(2,3,4,5,6,7,8,9))+
#  labs(x='Number of features',y='Classification rate')
#dev.off()
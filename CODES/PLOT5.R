library(ggplot2)
library(gridExtra)
library(grid)
library(mvtnorm)
library(class)
library(MASS)

plasma<-df_MERGE_BIO_complete[complete.cases(df_MERGE_BIO_complete),]
PLASMA<-plasma[plasma$VISCODE=='bl',]
AD<-PLASMA[PLASMA$DX=='Dementia',]
MCI<-PLASMA[PLASMA$DX=='MCI',]
CN<-PLASMA[PLASMA$DX=='NL',]
AD$DX<-"AD"
MCI$DX<-"MCI"
CN$DX<-"CN"
min_size_class<-min(nrow(CN),nrow(MCI),nrow(AD)) 
CN<-CN[sample(nrow(CN), min_size_class), ] #sampling for normal lesser size 
MCI<-MCI[sample(nrow(MCI), min_size_class), ] #sampling for MCI lesser size
AD<-AD[sample(nrow(AD), min_size_class), ]


x<-rbind(CN,MCI,AD)[,15:16]
rownames(x)<-NULL
class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))

xnew<-expand.grid(x=seq(75-1,310+1,
                        by=2),
                  y=seq(0-1, 150+1, 
                        by=2))

mod <- knn(x, xnew, class, k=5, prob=TRUE)

prob <- attr(mod, "prob")

prob_CN<-ifelse(mod=="CN",prob,0)
prob_MCI<-ifelse(mod=="MCI",prob,0)
prob_AD<-ifelse(mod=="AD",prob,0)

prob_CN1<-ifelse(mod=="CN",1,0)
prob_MCI1<-ifelse(mod=="MCI",1,0)
prob_AD1<-ifelse(mod=="AD",1,0)


px1<-seq(75-1,310+1,by=2)
px2<-seq(0-1, 150+1,by=2)

probCN <- matrix(prob_CN, length(px1), length(px2))
probMCI<-matrix(prob_MCI, length(px1), length(px2))
probAD <- matrix(prob_AD, length(px1), length(px2))
probCN1 <- matrix(prob_CN1, length(px1), length(px2))
probMCI1<-matrix(prob_MCI1, length(px1), length(px2))
probAD1 <- matrix(prob_AD1, length(px1), length(px2))

par(mar=rep(2,4))

dataf<-cbind(xnew,as.vector(probCN),as.vector(probMCI),as.vector(probAD),as.vector(probCN1),as.vector(probMCI1),as.vector(probAD1))
colnames(dataf)<-c('x','y','probCN','probMCI','probAD','probCN1','probMCI1','probAD1')

A<-ggplot()+theme_bw()+
  geom_point(data=dataf,aes(x=x, y=y, size=probCN),fill = "dark grey", colour = "black", alpha = 1/9)+
  geom_contour(data=dataf,aes(x=x, y=y, z=probCN1), bins=1, colour = "black", alpha = 1,size=1)+
  scale_size(range=c(-1, 3))+xlim(75,310)+ylim(0,150)+
  geom_point(aes(x=x, y=y, shape=class), size=2,data=data.frame(x=x[(1:nrow(CN)),1], y=x[(1:nrow(CN)),2], class=class[1:nrow(CN)]),alpha = 1)+
  labs(x=expression(paste("A",beta[1-42]," pg/mL")),y=expression(paste('p-tau'[181]," pg/mL")))+scale_shape_manual(values = c(16))+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))

B<-ggplot()+theme_bw()+
  geom_point(data=dataf,aes(x=x, y=y, size=probMCI),fill = "dark grey", colour = "black", alpha = 1/9)+
  geom_contour(data=dataf,aes(x=x, y=y, z=probMCI1), bins=1, colour = "black", alpha = 1,size=1)+
  scale_size(range=c(-1, 3))+xlim(75,310)+ylim(0,150)+
  geom_point(aes(x=x, y=y, shape=class), size=2,data=data.frame(x=x[(nrow(CN)+1):(nrow(CN)+nrow(MCI)),1], y=x[(nrow(CN)+1):(nrow(CN)+nrow(MCI)),2], class=class[(nrow(CN)+1):(nrow(CN)+nrow(MCI))]),alpha = 1)+
  labs(x=expression(paste("A",beta[1-42]," pg/mL")),y=expression(paste('p-tau'[181]," pg/mL")))+scale_shape_manual(values = c(15))+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))

C<-ggplot()+theme_bw()+
  geom_point(data=dataf,aes(x=x, y=y, size=probAD),fill = "dark grey", colour = "black", alpha = 1/9)+
  geom_contour(data=dataf,aes(x=x, y=y, z=probAD1), bins=1, colour = "black", alpha = 1,size=1)+
  scale_size(range=c(-1, 3))+xlim(75,310)+ylim(0,150)+
  geom_point(aes(x=x, y=y, shape=class), size=2,data=data.frame(x=x[(nrow(CN)+nrow(MCI)+1):(nrow(CN)+nrow(MCI)+nrow(AD)),1], y=x[(nrow(CN)+nrow(MCI)+1):(nrow(CN)+nrow(MCI)+nrow(AD)),2], class=class[(nrow(MCI)+1):(nrow(MCI)+nrow(AD))]),alpha = 1)+
  labs(x=expression(paste("A",beta[1-42]," pg/mL")),y=expression(paste('p-tau'[181]," pg/mL")))+scale_shape_manual(values = c(17))+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))

D<-ggplot()+theme_bw()+
  geom_point(data=dataf,aes(x=x, y=y, size=probAD),fill = "dark grey", colour = "black", alpha = 1/9)+
  geom_contour(data=dataf,aes(x=x, y=y, z=probAD1), bins=1, colour = "black", alpha = 1,size=1)+
  scale_size(name = "probability",range=c(-1, 3))+xlim(75,310)+ylim(0,150)+
  labs(x=expression(paste("A",beta[1-42]," pg/mL")),y=expression(paste('p-tau'[181]," pg/mL")))+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))

E<-ggplot()+theme_bw()+
  geom_point(aes(x=x, y=y, shape=class), size=2,data=data.frame(x=x[,1], y=x[,2], class=class),alpha = 1)+
  labs(x=expression(paste("A",beta[1-42]," pg/mL")),y=expression(paste('p-tau'[181]," pg/mL")))+scale_shape_manual(values = c(17,16,15))+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))


mylegend1<-g_legend(D)
mylegend2<-g_legend(E)

cairo_ps(width=13,height = 3,file='plot5.eps')
grid.arrange(arrangeGrob(A + theme(legend.position="none"),
                         B + theme(legend.position="none"),
                         C + theme(legend.position="none"),
                         nrow=1),
             arrangeGrob(mylegend1,mylegend2),
             widths = c(5,0.5))
dev.off()



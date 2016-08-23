plasma<-df_MERGE_BIO_complete[complete.cases(df_MERGE_BIO_complete),]
PLASMA<-plasma[plasma$VISCODE=='bl',]
AD<-PLASMA[PLASMA$DX=='Dementia',]
MCI<-PLASMA[PLASMA$DX=='MCI',]
CN<-PLASMA[PLASMA$DX=='NL',]
AD$DX<-"AD"
MCI$DX<-"MCI"
CN$DX<-"CN"

library(ggplot2)
library(gridExtra)
library(grid)
library(mvtnorm)
library(class)
library(MASS)

x<-rbind(CN,AD)[,15:16]
class<-c(rep("CN",nrow(CN)),rep("AD",nrow(AD)))
g<-c(rep(0,nrow(CN)),rep(1,nrow(AD)))

xnew<-expand.grid(x=seq(75-1,310+1,
                        by=7),
                  y=seq(0-1, 150+1, 
                        by=7))

mod <- knn(x, xnew, g, k=3, prob=TRUE)
prob <- attr(mod, "prob")
prob1 <- ifelse(mod=="1", prob, 1-prob)
prob <- ifelse(mod=="1", 0, 1)

px1<-seq(75-1,310+1,by=7)
px2<-seq(0-1, 150+1,by=7)
prob15 <- matrix(prob, length(px1), length(px2))
prob25<-matrix(prob1, length(px1), length(px2))
#par(mar=rep(2,4))


dataf<-cbind(xnew,as.vector(prob15),as.vector(prob25))
colnames(dataf)<-c('x','y','prob','probability')

#cairo_ps(width=7,height = 4,file='ex4.eps')

D<-ggplot(dataf)+geom_contour(aes(x=x, y=y, z=prob), bins=1,data=dataf, colour = "black", alpha = 1,size=1)+
  theme_bw()+scale_size(range=c(-1, 3))+
  geom_point(aes(x=x, y=y,shape=class), size=2.5,data=data.frame(x=x[,1], y=x[,2]),alpha = 1)+
  scale_shape_manual(values = c(17,16))+
  labs(x=expression(paste("A",beta[1-42]," pg/mL")),y=expression(paste('p-tau'[181]," pg/mL")))+xlim(75,310)+ylim(0,150)+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))
#dev.off()


A<-ggplot(data=data.frame(AD,class='AD'),aes(x=ABETA,y=PTAU))+ 
  stat_density2d(aes(color = class,alpha=..level..),geom="polygon",size=0.4, bins=12)+
  scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.05))+
  scale_color_manual(values=c("AD"="gray"),guide=FALSE)+
  geom_point(aes(shape=class),size=3) + xlim(75,310)+ylim(0,150)+theme_bw()+
  scale_shape_manual(values = c(17),guide=FALSE)+
  labs(x=expression(paste("A",beta[1-42]," pg/mL")),y=expression(paste('p-tau'[181]," pg/mL")))+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))

B<-ggplot(data=data.frame(CN,class="CN"),aes(x=ABETA,y=PTAU))+ 
stat_density2d(aes(color = class,alpha=..level..),geom="polygon",size=0.4, bins=12)+
  scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.05))+
  scale_color_manual(values=c("CN"="white"),guide=FALSE) +
  geom_point(aes(shape=class),size=3) +
  labs(x=expression(paste("A",beta[1-42]," pg/mL")),y=expression(paste('p-tau'[181]," pg/mL")))+
  xlim(75,310)+ylim(0,150)+theme_bw()+scale_shape_manual(values = c(16),guide=FALSE)+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))

C<-ggplot(data=rbind(data.frame(AD,class='AD'),data.frame(CN,class="CN")),aes(x=ABETA,y=PTAU))+ 
  stat_density2d(aes(color = class,alpha=..level..),geom="polygon",size=0.4, bins=12)+
  scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.05))+
  labs(x=expression(paste("A",beta[1-42]," pg/mL")),y=expression(paste('p-tau'[181]," pg/mL")))+
  scale_color_manual(values=c("CN"="white", "AD"="gray"),guide=FALSE)+ xlim(75,310)+ylim(0,150)+theme_bw()+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))

mylegend1<-g_legend(B)
mylegend2<-g_legend(D)


cairo_ps(width=13,height = 3,file='plot1.eps')
grid.arrange(arrangeGrob(A + theme(legend.position="none"),
                         B + theme(legend.position="none"),
                         C + theme(legend.position="none"),
                         nrow=1),
             arrangeGrob(mylegend1,mylegend2),
             widths = c(5,0.5))
dev.off()

class<-c(rep("CN",nrow(CN)),rep("AD",nrow(AD)))
g<-c(rep(0,nrow(CN)),rep(1,nrow(AD)))

xnew<-expand.grid(x=seq(75-1,310+1,
                        by=2.7),
                  y=seq(0-1, 150+1, 
                        by=2.7))

mod <- knn(x, xnew, g, k=3, prob=TRUE)
prob <- attr(mod, "prob")
prob1 <- ifelse(mod=="1", prob, 1-prob)
prob <- ifelse(mod=="1", 0, 1)

px1<-seq(75-1,310+1,by=2.7)
px2<-seq(0-1, 150+1,by=2.7)
prob15 <- matrix(prob, length(px1), length(px2))
prob25<-matrix(prob1, length(px1), length(px2))

dataf<-cbind(xnew,as.vector(prob15),as.vector(prob25))
colnames(dataf)<-c('x','y','prob','probability')
cairo_ps(width=7.5,height = 4.6,file='plot12.eps')
ggplot(dataf)+geom_contour(aes(x=x, y=y, z=prob), bins=1,data=dataf, colour = "black", alpha = 1,size=1)+
  theme_bw()+geom_point(aes(x=x, y=y, size=probability), colour = "black", alpha = 1/3)+
  scale_size(range=c(-1, 3))+
  geom_point(aes(x=x, y=y, shape=class), size=3.5,data=data.frame(x=x[,1], y=x[,2], class=class),alpha = 1)+
  scale_colour_manual(values=c( "#E69F00","#56B4E9"))+labs(x=expression(paste("A",beta[1-42]," pg/mL")),y=expression(paste('p-tau'[181]," pg/mL")))+
  xlim(75,310)+ylim(0,150)+scale_shape_manual(values = c(17,16))+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))
dev.off()



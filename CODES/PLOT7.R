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

x<-rbind(AD)[,15:16]
class<-c(rep("AD",nrow(AD)))
dataa<-data.frame(AD,class='AD')

Ad<-rbind(SMOTE(x,floor((nrow(MCI)-nrow(AD))),7),x)
batt(x,Ad)
dataa<-data.frame(Ad,class='AD')
class<-c(rep("AD",nrow(dataa)))

A<-ggplot(data=dataa,aes(x=ABETA,y=PTAU))+ 
  stat_density2d(aes(color = class,alpha=..level..),geom="polygon",size=0.4, bins=14)+
  scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.05))+
  scale_color_manual(values=c("AD"="gray"),guide=FALSE)+
  geom_point(aes(shape=class),size=1) + xlim(75,300)+ylim(0,150)+theme_bw()+
  scale_shape_manual(values = c(17),guide=FALSE)+
  labs(x=expression(paste("A",beta[1-42]," pg/mL")),y=expression(paste('p-tau'[181]," pg/mL")))+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))

Ad<-rbind(SMOTE(x,floor((nrow(MCI)-nrow(AD))),15),x)
batt(x,Ad)
dataa<-data.frame(Ad,class='AD')
class<-c(rep("AD",nrow(dataa)))


B<-ggplot(data=dataa,aes(x=ABETA,y=PTAU))+ 
  stat_density2d(aes(color = class,alpha=..level..),geom="polygon",size=0.4, bins=14)+
  scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.05))+
  scale_color_manual(values=c("AD"="gray"),guide=FALSE)+
  geom_point(aes(shape=class),size=1) + xlim(75,300)+ylim(0,150)+theme_bw()+
  scale_shape_manual(values = c(17),guide=FALSE)+
  labs(x=expression(paste("A",beta[1-42]," pg/mL")),y=expression(paste('p-tau'[181]," pg/mL")))+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))

Ad<-rbind(SMOTE(x,floor((nrow(MCI)-nrow(AD))),30),x)
batt(x,Ad)
dataa<-data.frame(Ad,class='AD')
class<-c(rep("AD",nrow(dataa)))

C<-ggplot(data=dataa,aes(x=ABETA,y=PTAU))+ 
  stat_density2d(aes(color = class,alpha=..level..),geom="polygon",size=0.4, bins=14)+
  scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.05))+
  scale_color_manual(values=c("AD"="gray"),guide=FALSE)+
  geom_point(aes(shape=class),size=1) + xlim(75,300)+ylim(0,150)+theme_bw()+
  scale_shape_manual(values = c(17),guide=FALSE)+
  labs(x=expression(paste("A",beta[1-42]," pg/mL")),y=expression(paste('p-tau'[181]," pg/mL")))+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))

D<-ggplot(data=dataa,aes(x=ABETA,y=PTAU))+ 
  stat_density2d(aes(color = class,alpha=..level..),geom="polygon",size=0.4, bins=14)+
  scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.05))+
  scale_color_manual(values=c("AD"="gray"),guide=FALSE)+
  geom_point(aes(shape=class),size=2) + xlim(75,300)+ylim(0,150)+theme_bw()+
  scale_shape_manual(values = c(17))+
  labs(x=expression(paste("A",beta[1-42]," pg/mL")),y=expression(paste('p-tau'[181]," pg/mL")))+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))

mylegend1<-g_legend(D)


cairo_ps(width=13,height = 3,file='plot7.eps')
grid.arrange(arrangeGrob(A + theme(legend.position="none"),
                         B + theme(legend.position="none"),
                         C + theme(legend.position="none"),
                         nrow=1),
             arrangeGrob(mylegend1),
             widths = c(5,0.5))
dev.off()

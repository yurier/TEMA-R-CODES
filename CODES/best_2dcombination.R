library(ggplot2)
library(gridExtra)
library(grid)
library(mvtnorm)
library(class)
library(MASS)

list2<-combn(1:9,2)
table_it_2d<-table_it_normal[table_it_normal$i==2,]
table_it_2d<-table_it_2d[order(table_it_2d$ii,decreasing = FALSE),]

plasma<-df_MERGE_BIO_complete[complete.cases(df_MERGE_BIO_complete),]
plasma$NOISE<-rnorm(nrow(plasma))
PLASMA<-plasma[plasma$VISCODE=='bl',]

AD<-PLASMA[PLASMA$DX=='Dementia',]
MCI<-PLASMA[PLASMA$DX=='MCI',]
CN<-PLASMA[PLASMA$DX=='NL',]
AD$DX<-"AD"
MCI$DX<-"MCI"
CN$DX<-"CN"
PLASMA<-rbind(CN,MCI,AD)
PLASMA<-undersample(PLASMA)
AD<-PLASMA[PLASMA$DX=='AD',]
MCI<-PLASMA[PLASMA$DX=='MCI',]
CN<-PLASMA[PLASMA$DX=='CN',]

for (j in 1:nrow(table_it_2d)){
PLASMA1<-subset(PLASMA, select=c("FDG","AV45","CDRSB","ADAS11","MMSE","RAVLT.perc.forgetting","ABETA","PTAU","NOISE"))
#PLASMA1<-PLASMA1[,c(combn(1:9,lista_best_normal[s,4])[,lista_best_normal[s,5]])]
PLASMA1<-PLASMA1[,list2[,j]]
neime<-colnames(PLASMA1)

x<-Min_max_nrmlztn(PLASMA1)
rownames(x)<-NULL
class<-PLASMA$DX
scale1=(max(x[,1])-min(x[,1]))/100
scale2=(max(x[,2])-min(x[,2]))/100
print(scale1,scale2)

xnew<-expand.grid(x=seq(min(x[,1]), max(x[,1]),
                        by=scale1),
                  y=seq(min(x[,2]), max(x[,2]), 
                        by=scale2))

mod <- knn(x, xnew, class, k=table_it_2d[j,1], prob=TRUE)

prob <- attr(mod, "prob")

prob_CN<-ifelse(mod=="CN",prob,0)
prob_MCI<-ifelse(mod=="MCI",prob,0)
prob_AD<-ifelse(mod=="AD",prob,0)

prob_CN1<-ifelse(mod=="CN",1,0)
prob_MCI1<-ifelse(mod=="MCI",1,0)
prob_AD1<-ifelse(mod=="AD",1,0)


px1<-seq(min(x[,1]), max(x[,1]),by=scale1)
px2<-seq(min(x[,2]), max(x[,2]),by=scale2)

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
  scale_size(range=c(-1, 3))+#xlim(75,310)+ylim(0,270)+
  geom_point(aes(x=x, y=y, shape=class), size=2,data=data.frame(x=x[(1:nrow(CN)),1], y=x[(1:nrow(CN)),2], class=class[1:nrow(CN)]),alpha = 1)+
  labs(x=neime[1],y=neime[2])+scale_shape_manual(values = c(16))+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))

B<-ggplot()+theme_bw()+
  geom_point(data=dataf,aes(x=x, y=y, size=probMCI),fill = "dark grey", colour = "black", alpha = 1/9)+
  geom_contour(data=dataf,aes(x=x, y=y, z=probMCI1), bins=1, colour = "black", alpha = 1,size=1)+
  scale_size(range=c(-1, 3))+#xlim(75,310)+ylim(0,270)+
  geom_point(aes(x=x, y=y, shape=class), size=2,data=data.frame(x=x[(nrow(CN)+1):(nrow(CN)+nrow(MCI)),1], y=x[(nrow(CN)+1):(nrow(CN)+nrow(MCI)),2], class=class[(nrow(CN)+1):(nrow(CN)+nrow(MCI))]),alpha = 1)+
  labs(x=neime[1],y=neime[2])+scale_shape_manual(values = c(15))+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))

C<-ggplot()+theme_bw()+
  geom_point(data=dataf,aes(x=x, y=y, size=probAD),fill = "dark grey", colour = "black", alpha = 1/9)+
  geom_contour(data=dataf,aes(x=x, y=y, z=probAD1), bins=1, colour = "black", alpha = 1,size=1)+
  scale_size(range=c(-1, 3))+#xlim(75,310)+ylim(0,270)+
  geom_point(aes(x=x, y=y, shape=class), size=2,data=data.frame(x=x[(nrow(CN)+nrow(MCI)+1):(nrow(CN)+nrow(MCI)+nrow(AD)),1], y=x[(nrow(CN)+nrow(MCI)+1):(nrow(CN)+nrow(MCI)+nrow(AD)),2], class=class[(nrow(CN)+nrow(MCI)+1):(nrow(CN)+nrow(MCI)+nrow(AD))]),alpha = 1)+
  labs(x=neime[1],y=neime[2])+scale_shape_manual(values = c(17))+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))

D<-ggplot()+theme_bw()+
  geom_point(data=dataf,aes(x=x, y=y, size=probAD),fill = "dark grey", colour = "black", alpha = 1/9)+
  geom_contour(data=dataf,aes(x=x, y=y, z=probAD1), bins=1, colour = "black", alpha = 1,size=1)+
  scale_size(name = "probability",range=c(-1, 3))+#xlim(75,310)+ylim(0,270)+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))

E<-ggplot()+theme_bw()+
  geom_point(aes(x=x, y=y, shape=class), size=2,data=data.frame(x=x[,1], y=x[,2], class=class),alpha = 1)+
  labs(x=neime[1],y=neime[2])+scale_shape_manual(values = c(17,16,15))+
  theme(legend.text = element_text(size = 15),axis.text=element_text(size=15),legend.title = element_text(size = 15),axis.title.y=element_text(size=15),axis.title.x=element_text(size=15))


mylegend1<-g_legend(D)
mylegend2<-g_legend(E)


png(filename = sprintf('plot2d_%s.png',paste0(as.character(list2[1,j]),'_',as.character(list2[2,j]))),
    width = 1300, height = 300, units = "px", pointsize = 12,
    bg = "white",  res = NA,
    type = c("cairo", "cairo-png", "Xlib", "quartz"))

grid.arrange(arrangeGrob(A + theme(legend.position="none"),
                         B + theme(legend.position="none"),
                         C + theme(legend.position="none"),
                         nrow=1),
             arrangeGrob(mylegend1,mylegend2),
             widths = c(5,0.5))
dev.off()
}


aux<-subset(df_MERGE_BIO_complete_incorrect_version[df_MERGE_BIO_complete_incorrect_version$DX=='NL',], MMSE >= 24 & MMSE <= 30)
aux<-subset(aux, CDRSB==0)
aux_CN<-aux
df_MERGE_BIO_complete<-rbind(aux_CN,df_MERGE_BIO_complete_incorrect_version[df_MERGE_BIO_complete_incorrect_version$DX=="MCI",],df_MERGE_BIO_complete_incorrect_version[df_MERGE_BIO_complete_incorrect_version$DX=="Dementia",])
#____________________________________________________________


plasma<-df_MERGE_BIO_complete[complete.cases(df_MERGE_BIO_complete),]
PLASMA<-plasma[plasma$VISCODE=='bl',]
AD<-PLASMA[PLASMA$DX=='Dementia',]
MCI<-PLASMA[PLASMA$DX=='MCI',]
CN<-PLASMA[PLASMA$DX=='NL',]
AD$DX<-"AD"
MCI$DX<-"MCI"
CN$DX<-"CN"
PLASMA<-rbind(CN,MCI,AD)
CN<-data.frame(CN[,9:16],NOISE=rnorm(nrow(CN)))
MCI<-data.frame(MCI[,9:16],NOISE=rnorm(nrow(MCI)))
AD<-data.frame(AD[,9:16],NOISE=rnorm(nrow(AD)))
class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
data<-data.frame(rbind(CN,MCI,AD),DXCHANGE=class)
data[,1:(ncol(data)-1)]<-Min_max_nrmlztn(data[,1:(ncol(data)-1)])

answer_under<-combination_unders(data,5,c(1:25)) #with undersampling
set.seed(1)

data<-data[,-c(3,5)] #removing the definition

hllclmb(data,20,c(1:25))
backward(data,20,c(1:25))
forward(data,20,c(1:25))

#______________________________________________________________________
plasma<-df_MERGE_BIO_complete[complete.cases(df_MERGE_BIO_complete),]
PLASMA<-plasma[plasma$VISCODE=='bl',]
AD<-PLASMA[PLASMA$DX=='Dementia',]
MCI<-PLASMA[PLASMA$DX=='MCI',]
CN<-PLASMA[PLASMA$DX=='NL',]
AD$DX<-"AD"
MCI$DX<-"MCI"
CN$DX<-"CN"
PLASMA<-rbind(CN,MCI,AD)
CN<-data.frame(CN[,9:16],NOISE=rnorm(nrow(CN)))
MCI<-data.frame(MCI[,9:16],NOISE=rnorm(nrow(MCI)))
AD<-data.frame(AD[,9:16],NOISE=rnorm(nrow(AD)))
class<-c(rep("CN",nrow(CN)),rep("MCI",nrow(MCI)),rep("AD",nrow(AD)))
data<-data.frame(rbind(CN,MCI,AD),DXCHANGE=class)
data[,1:(ncol(data)-1)]<-Min_max_nrmlztn(data[,1:(ncol(data)-1)])

answer_normal<-combination_normal(data,5,c(1:25)) #with normal

data<-data[,-c(3,5)] #removing the definition
set.seed(1)
hllclmb_normal(data,20,c(1:25))
backward_normal(data,20,c(1:25))
forward_normal(data,20,c(1:25))

#______________________________________________________________________
plasma<-df_MERGE_BIO_complete[complete.cases(df_MERGE_BIO_complete),]
PLASMA<-plasma[plasma$VISCODE=='bl',]
AD<-PLASMA[PLASMA$DX=='Dementia',]
MCI<-PLASMA[PLASMA$DX=='MCI',]
CN<-PLASMA[PLASMA$DX=='NL',]
AD$DX<-"AD"
MCI$DX<-"MCI"
CN$DX<-"CN"
PLASMA<-rbind(CN,MCI,AD)
CN<-data.frame(CN[,9:16],NOISE=rnorm(nrow(CN)))
MCI<-data.frame(MCI[,9:16],NOISE=rnorm(nrow(MCI)))
AD<-data.frame(AD[,9:16],NOISE=rnorm(nrow(AD)))

CN<-rbind(CN,SMOTE(CN,nrow(MCI)-nrow(CN),5))
AD<-rbind(AD,SMOTE(AD,nrow(MCI)-nrow(AD),5))

class<-c(rep("CN",nrow(MCI)),rep("MCI",nrow(MCI)),rep("AD",nrow(MCI)))
data<-data.frame(rbind(CN,MCI,AD),DXCHANGE=class)
data[,1:(ncol(data)-1)]<-Min_max_nrmlztn(data[,1:(ncol(data)-1)])

#CN_synth<-data.frame(SMOTE(CN,nrow(MCI)-nrow(CN),5))
#AD_synth<-data.frame(SMOTE(CN,nrow(MCI)-nrow(AD),5))
#synth<-rbind(CN_synth,AD_synth)
#aux<-Min_max_nrmlztn(rbind(synth,data[,1:(ncol(data)-1)]))
#synth<-aux[1:nrow(synth),]
#data[,1:(ncol(data)-1)]<-aux[-c(1:nrow(synth)),]
#cl_synth<-c(rep("1",nrow(MCI)-nrow(CN)),rep("3",nrow(MCI)-nrow(AD)))
#answer_overs<-combination_overs(data,synth,cl_synth,5,c(1:25)) #with oversampling]]
answer_overs<-combination_normal(data,5,c(1:25))

data<-data[,-c(3,5)] #removing the definition
set.seed(1)
hllclmb(data,1,c(1:25))
backward(data,20,c(1:25))
forward(data,20,c(1:25))
source('~/Dropbox/Projeto Alzheimer/Team Projects/TEMA - Feature selection in Alzheimer\'s biomarkers .../TEMA_R_CODES/results_overs.R')
source('~/Dropbox/Projeto Alzheimer/Team Projects/TEMA - Feature selection in Alzheimer\'s biomarkers .../TEMA_R_CODES/results_under.R')
source('~/Dropbox/Projeto Alzheimer/Team Projects/TEMA - Feature selection in Alzheimer\'s biomarkers .../TEMA_R_CODES/results_normal.R')


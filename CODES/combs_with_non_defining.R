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

aux2<-NULL
aux1<-NULL
count<-0
for (i in 2:(ncol(PLASMA)-1)){
  combs<-combn(1:9,i)
  for(ii in 1:ncol(combs)){
    x1<-combs[,ii]==3
    x2<-combs[,ii]==5
    count<-count+1
    if (all(!any(x1),!any(x2))){
      aux1<-rbind(aux,c(i,ii))
      aux2<-c(aux2,count)
    }
}
}

table_it_normal_nondef<-table_it_normal[row.names(table_it_normal) %in% aux2, ]
table_it_overs_nondef<-table_it_overs[row.names(table_it_overs) %in% aux2, ]
table_it_under_nondef<-table_it_under[row.names(table_it_under) %in% aux2, ]

table_it_overs_nondef<-table_it_overs_nondef[order(table_it_overs_nondef[,2],decreasing = TRUE),]
table_it_under_nondef<-table_it_under_nondef[order(table_it_under_nondef[,2],decreasing = TRUE),]
table_it_normal_nondef<-table_it_normal_nondef[order(table_it_normal_nondef[,2],decreasing = TRUE),]

table_it_normal_nondef[,7]<-1:nrow(table_it_normal_nondef)
table_it_overs_nondef[,7]<-1:nrow(table_it_overs_nondef)
table_it_under_nondef[,7]<-1:nrow(table_it_under_nondef)

table_it_overs<-table_it_overs[order(table_it_overs[,2],decreasing = TRUE),]
table_it_under<-table_it_under[order(table_it_under[,2],decreasing = TRUE),]
table_it_normal<-table_it_normal[order(table_it_normal[,2],decreasing = TRUE),]

aux2<-NULL
aux1<-NULL
count<-0
for (i in 2:(ncol(PLASMA)-1)){
  combs<-combn(1:9,i)
  for(ii in 1:ncol(combs)){
    x1<-combs[,ii]==3
    x2<-combs[,ii]==4
    x3<-combs[,ii]==5
    x4<-combs[,ii]==6
    count<-count+1
    if (all(!any(x1),!any(x2),!any(x3),!any(x4))){
      aux1<-rbind(aux,c(i,ii))
      aux2<-c(aux2,count)
    }
  }
}

table_it_normal_noncog<-table_it_normal[row.names(table_it_normal) %in% aux2, ]
table_it_overs_noncog<-table_it_overs[row.names(table_it_overs) %in% aux2, ]
table_it_under_noncog<-table_it_under[row.names(table_it_under) %in% aux2, ]

table_it_overs_noncog<-table_it_overs_noncog[order(table_it_overs_noncog[,2],decreasing = TRUE),]
table_it_under_noncog<-table_it_under_noncog[order(table_it_under_noncog[,2],decreasing = TRUE),]
table_it_normal_noncog<-table_it_normal_noncog[order(table_it_normal_noncog[,2],decreasing = TRUE),]
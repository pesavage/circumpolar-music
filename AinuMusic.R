#This code was used on 2013-11-12 to perform the analyses of 680 traditional songs from 35 populations published in:
#Savage, P. E., Matsumae, H., Oota, H., Stoneking, M., Currie, T. E., Tajima, A., Gillan, M., & Brown, S. (2015). How “circumpolar” is Ainu music? Musical and genetic perspectives on the history of the Japanese archipelago. Ethnomusicology Forum, 24(3), 443–467. https://doi.org/10.1080/17411912.2015.1084236

#The code to create musical distance matrices is based on code developed for:
#Rzeszutek, T., Savage, P. E., & Brown, S. (2012). The structure of cross-cultural musical diversity. Proceedings of the Royal Society B: Biological Sciences, 279(1733), 1606–1612. https://doi.org/10.1098/rspb.2011.1750

#Please cite these publications when adapting this code

#This code requires the "ade4" package

#This code also uses separate software (Arlequin) to convert song-level distance matrices into population-level distance matrices. The results of this conversion are provided in case you want/need to skip that step.

#First, check spreadsheet for errors, save as .csv spreadsheet with first row=Song No. and rows 2-42= CC1-26+CMPerformance (sorted by region: Ainu, E. Asia, then Circumpolar)


#open R, change workspace to correct folder
setwd("/Users/pesavage/Documents/Research/Papers/Published/Savage et al (2015) Ethnomusicology Forum")

#import spreadsheet (including multi-state codings):
japan<-as.matrix(read.csv("Savage_Et_Al_Ainu_Music_Supplementary.csv",header=TRUE,row.names=1))

#add distance matrix algorithm (this algorithm calculates average distances among all songs using the method described in Rzeszutek et al. 2012 to account for nominal and ordinal characters and for single- and multi-coded characters)

#add distance matrix algorithm
ordinal.fn<-function (x,y) {
if(is.na(x)|is.na(y))"NA" else
abs(x-y)}
weightedv6.dist<-function(d,ord,nom) {
d[d==""]<-NA
nominal<-cbind(d[,nom])
nominal.fields<-vector("list",length=length(nominal[1,]))
for (i in 1:length(nominal[1,])) {nominal.fields[[i]]<-matrix(nrow=length(nominal[,1]),ncol=13)}
for (i in 1:length(nominal[1,])) {rownames(nominal.fields[[i]])<-rownames(nominal)}
for (i in 1:length(nominal[1,])) {colnames(nominal.fields[[i]])<-c("a","b","c","d","e","f","g","h","i","j","k","l","m")}
for (j in 1:13){
for (k in 1:length(nominal[,1])){
for (i in 1:length(nominal[1,])){nominal.fields[[i]][k,j]<-(if(is.na(nominal[k,i]))"NA" else if(substr(nominal[k,i],1,1)==colnames(nominal.fields[[i]])[j] | substr(nominal[k,i],2,2)==colnames(nominal.fields[[i]])[j] |substr(nominal[k,i],3,3)==colnames(nominal.fields[[i]])[j] |substr(nominal[k,i],4,4)==colnames(nominal.fields[[i]])[j] |substr(nominal[k,i],5,5)==colnames(nominal.fields[[i]])[j] |substr(nominal[k,i],6,6)==colnames(nominal.fields[[i]])[j] |substr(nominal[k,i],7,7)==colnames(nominal.fields[[i]])[j] |substr(nominal[k,i],8,8)==colnames(nominal.fields[[i]])[j] |substr(nominal[k,i],9,9)==colnames(nominal.fields[[i]])[j] |substr(nominal[k,i],10,10)==colnames(nominal.fields[[i]])[j] |substr(nominal[k,i],11,11)==colnames(nominal.fields[[i]])[j] |substr(nominal[k,i],12,12)==colnames(nominal.fields[[i]])[j] |substr(nominal[k,i],13,13)==colnames(nominal.fields[[i]])[j])1 else 0) }}}
suppressWarnings(for (i in 1:length(nominal[1,])){storage.mode(nominal.fields[[i]])<-"numeric"})
nominal.dist<-vector("list",length=length(nominal[1,]))
for (i in 1:length(nominal[1,])) {nominal.dist[[i]]<-matrix(nrow=length(nominal[,1]),ncol=length(nominal[,1]))}
nominal.result<-matrix(nrow=length(d[,1]),ncol=length(d[,1]),c(rep(0,(length(d[,1])*length(d[,1])))))
ordinal<-cbind(d[,ord])
suppressWarnings(storage.mode(ordinal)<-"numeric")
ordinal.dist<-vector("list",length=length(ordinal[1,]))
for (i in 1:length(ordinal[1,])) {ordinal.dist[[i]]<-matrix(nrow=length(ordinal[,1]),ncol=length(ordinal[,1]))}
ordinal.result<-matrix(nrow=length(d[,1]),ncol=length(d[,1]),c(rep(0,(length(d[,1])*length(d[,1])))))
result<-matrix(nrow=length(d[,1]),ncol=length(d[,1]))
for (k in 1:length(nominal[,1])){
for (j in 1:length(nominal[,1])){
for (i in 1:length(nominal[1,])){nominal.dist[[i]][k,j]<-if(is.na(nominal.fields[[i]][k,1])|is.na(nominal.fields[[i]][j,1]))"NA" else ((if(nominal.fields[[i]][k,1]==nominal.fields[[i]][j,1])0 else 1)+(if(nominal.fields[[i]][k,2]==nominal.fields[[i]][j,2])0 else 1)+(if(nominal.fields[[i]][k,3]==nominal.fields[[i]][j,3])0 else 1)+(if(nominal.fields[[i]][k,4]==nominal.fields[[i]][j,4])0 else 1)+(if(nominal.fields[[i]][k,5]==nominal.fields[[i]][j,5])0 else 1)+(if(nominal.fields[[i]][k,6]==nominal.fields[[i]][j,6])0 else 1)+(if(nominal.fields[[i]][k,7]==nominal.fields[[i]][j,7])0 else 1)+(if(nominal.fields[[i]][k,8]==nominal.fields[[i]][j,8])0 else 1)+(if(nominal.fields[[i]][k,9]==nominal.fields[[i]][j,9])0 else 1)+(if(nominal.fields[[i]][k,10]==nominal.fields[[i]][j,10])0 else 1)+(if(nominal.fields[[i]][k,11]==nominal.fields[[i]][j,11])0 else 1)+(if(nominal.fields[[i]][k,12]==nominal.fields[[i]][j,12])0 else 1))/((if(nominal.fields[[i]][k,1]==1 | nominal.fields[[i]][[j,1]]==1)1 else 0)+(if(nominal.fields[[i]][k,2]==1 | nominal.fields[[i]][[j,2]]==1)1 else 0)+(if(nominal.fields[[i]][k,3]==1 | nominal.fields[[i]][[j,3]]==1)1 else 0)+(if(nominal.fields[[i]][k,4]==1 | nominal.fields[[i]][[j,4]]==1)1 else 0)+(if(nominal.fields[[i]][k,5]==1 | nominal.fields[[i]][[j,5]]==1)1 else 0)+(if(nominal.fields[[i]][k,6]==1 | nominal.fields[[i]][[j,6]]==1)1 else 0)+(if(nominal.fields[[i]][k,7]==1 | nominal.fields[[i]][[j,7]]==1)1 else 0)+(if(nominal.fields[[i]][k,8]==1 | nominal.fields[[i]][[j,8]]==1)1 else 0)+(if(nominal.fields[[i]][k,9]==1 | nominal.fields[[i]][[j,9]]==1)1 else 0)+(if(nominal.fields[[i]][k,10]==1 | nominal.fields[[i]][[j,10]]==1)1 else 0)+(if(nominal.fields[[i]][k,11]==1 | nominal.fields[[i]][[j,11]]==1)1 else 0)+(if(nominal.fields[[i]][k,12]==1 | nominal.fields[[i]][[j,12]]==1)1 else 0))}}}
suppressWarnings(for (i in 1:length(nominal[1,])){storage.mode(nominal.dist[[i]])<-"numeric"})
vnom<-vector(mode="numeric",length=length(nominal[1,]))
for (k in 1:length(nominal[,1])){
for (j in 1:length(nominal[,1])){
for (i in 1:length(nominal[1,])){
  vnom[i]<-nominal.dist[[i]][k,j]
  }
  nominal.result[k,j]<-mean(suppressWarnings(as.numeric(vnom)),na.rm=TRUE)
  }}
for (k in 1:length(ordinal[,1])){
for (j in 1:length(ordinal[,1])){
for (i in 1:length(ordinal[1,])){ordinal.dist[[i]][k,j]<-ordinal.fn(x=ordinal[k,i],y=ordinal[j,i]) }}}
suppressWarnings(for (i in 1:length(ordinal[1,])){storage.mode(ordinal.dist[[i]])<-"numeric"})
vord<-vector(mode="numeric",length=length(ordinal[1,]))
for (k in 1:length(ordinal[,1])){
for (j in 1:length(ordinal[,1])){
for (i in 1:length(ordinal[1,])){
  vord[i]<-ordinal.dist[[i]][k,j]
  }
  ordinal.result[k,j]<-mean(suppressWarnings(as.numeric(vord)),na.rm=TRUE)
  }}
for (k in 1:length(nominal[,1])){
for (j in 1:length(nominal[,1])){
result[k,j]<-if(is.na(nominal.result[k,j]))ordinal.result[k,j] else if(is.na(ordinal.result[k,j])) nominal.result[k,j] else (ordinal.result[k,j]*length(ord) + nominal.result[k,j]*length(nom))/(length(ord)+length(nom))
}}
row.names(result)<-row.names(d)
colnames(result)<-row.names(d)
as.dist(result)
}
#create distance matrix of 680 songs from Savage et al. (2015)
japan.dist<-weightedv6.dist(japan,c(5:7,10:13,15:17,19,21:23,26:41),c(1:4,8:9,14,18,20,24:25))

#K-means:
japan.mds<-cmdscale(japan.dist)
wss <- (nrow(japan.mds)-1)*sum(apply(japan.mds,2,var))
for (i in 2:25) wss[i] <- sum(kmeans(japan.mds,centers=i)$withinss)
plot(1:25, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#K=5
dev.new()
fit <- kmeans(japan.mds, 5)
aggregate(japan.mds,by=list(fit$cluster),FUN=mean)

japancol.MDS<- data.frame(japan.mds, fit$cluster)
japancol.MDS$japancol.MDS[japancol.MDS$fit.cluster ==4]<- "gray0"
japancol.MDS$japancol.MDS[japancol.MDS$fit.cluster ==2]<- "gray25"
japancol.MDS$japancol.MDS[japancol.MDS$fit.cluster ==3]<- "gray50"
japancol.MDS$japancol.MDS[japancol.MDS$fit.cluster ==5]<- "gray75"
japancol.MDS$japancol.MDS[japancol.MDS$fit.cluster ==1]<- "gray100"
colnames(japancol.MDS)<-c("X1",  "X2", "fit.cluster", "col")
plot(japancol.MDS $X1, japancol.MDS $X2,xlab="Dimension 1", ylab="Dimension 2", main="K=5 Solution", pch=c(rep(21,60),rep(24,240),rep(22,380)),bg= japancol.MDS $col)
write.csv(japancol.MDS,"kmeanssoln5.csv")
table(japan[,44],japancol.MDS$col)

#Pies:
K5<-table(japan[,44],japancol.MDS$col)
write.csv(K5,"Japan680K5Freq.csv")

#rearrange columns from gray0-gray100, delete column names, resave

K5<-read.csv("Japan680K5Freq.csv",header=FALSE,row.names=1)
par(lwd = 10)
 with(K5[1,],pie(c(V2,V3,V4,V5,V6),clockwise=TRUE,radius=1,labels=NA,col=c("gray0","gray25","gray50","gray75","gray100")))

#And so on for K5[2,], K8[3,], etc….


#With colour
japancol.MDS<- data.frame(japan.mds, fit$cluster)
japancol.MDS$japancol.MDS[japancol.MDS$fit.cluster ==5]<- "red"
japancol.MDS$japancol.MDS[japancol.MDS$fit.cluster ==3]<- "yellow"
japancol.MDS$japancol.MDS[japancol.MDS$fit.cluster ==2]<- "green"
japancol.MDS$japancol.MDS[japancol.MDS$fit.cluster ==4]<- "blue"
japancol.MDS$japancol.MDS[japancol.MDS$fit.cluster ==1]<- "purple"
colnames(japancol.MDS)<-c("X1",  "X2", "fit.cluster", "col")
plot(japancol.MDS $X1, japancol.MDS $X2,xlab="Dimension 1", ylab="Dimension 2", main="K=5 Solution", pch=c(rep(21,60),rep(24,240),rep(22,381)),bg= japancol.MDS $col)


#Transform distance matrix to be Euclidean

library(ade4)
japan.euc.dist<-lingoes(japan.dist)
japan.sq.euc.dist<-(japan.euc.dist)^2
japan.sq.euc.frame<-as.data.frame(as.matrix(japan.sq.euc.dist))
japan.sq.euc.frame[upper.tri(japan.sq.euc.frame)] <- NA  write.table(japan.sq.euc.frame,"JapanCCCMEucDist.txt",na="", row.names=FALSE)

#Open the .txt file, remove “” marks using find/replace, then copy this distance matrix into the .arp file, and modify the samples lists to make it consistent.

#After saving this .arp (or other files, e.g., .txt) MAKE SURE TO OPEN IT ONCE WITH EXCEL IN WINDOWS AND RE-SAVE IT AS .TXT, OTHERWISE THE FORMATTING GETS ALL MESSED UP!!! (OR, POSSIBLY SAVING AS WINDOWS FORMATTED TEXT (.txt) MAY WORK EVEN BETTER)

#It doesn’t work to simply make the .arp in Mac, open the .arp in Excel and re-save it, because the formatting still gets messed up. You need to open the distance matrix and the sample labels separately, then merge them with the .arp in Windows (you’ll need to delete some extra “” marks that appear).

#Try to avoid using parentheses in names, because it messes up things in SplitsTree!

#Save as .nex file: 
#First, copy Arlequin print-out to txt file, add [] and ‘’ to numbers and labels, replace negative PhiST values with 0, then copy resulting sheet into a .nex template (remove any parentheses from taxa labels)

#open with SplitsTree.


#Correlations

## THEN COPY RESULTING FST TABLES AND SAVE AS FOLLOWING .CSV FILES

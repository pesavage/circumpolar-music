#This code was used in 2015 to convert the distance matrix of 680 traditional songs from 35 populations published in Savage et al. (2015) into pairwise PhiST distances between populations using Arlequin and the methods/theories in Rzeszutek et al. Please cite the following papers in any resulting publications:
#Savage, P. E., Matsumae, H., Oota, H., Stoneking, M., Currie, T. E., Tajima, A., Gillan, M., & Brown, S. (2015). How “circumpolar” is Ainu music? Musical and genetic perspectives on the history of the Japanese archipelago. Ethnomusicology Forum, 24(3), 443–467. https://doi.org/10.1080/17411912.2015.1084236

#Rzeszutek, T., Savage, P. E., & Brown, S. (2012). The structure of cross-cultural musical diversity. Proceedings of the Royal Society B: Biological Sciences, 279(1733), 1606–1612. https://doi.org/10.1098/rspb.2011.1750

#This code requires the "ade4" package

#This code also uses separate software (Arlequin) to convert song-level distance matrices into population-level distance matrices. The results of this conversion are provided ("Japan680Music.csv") in case you want/need to skip that step.

#open R, change workspace to correct folder
setwd("/Users/pesavage/Documents/Research/Papers/Unpublished/Matsumae Savage et al JapanMusicGeneMigrationContinued")

#Import 680-song distance matrix:
japan.dist<-as.dist(read.csv("Japan680SongDist.csv",header=TRUE)) 

################
###############Double-check to make sure this re-importing correctly

#Transform distance matrix to be squared Euclidean distances for AMOVA 

library(ade4)
japan.euc.dist<-lingoes(japan.dist)
japan.sq.euc.dist<-(japan.euc.dist)^2
japan.sq.euc.frame<-as.data.frame(as.matrix(japan.sq.euc.dist))
japan.sq.euc.frame[upper.tri(japan.sq.euc.frame)] <- NA 
write.table(japan.sq.euc.frame,"JapanCCCMEucDist.txt",na="", row.names=FALSE)

#Open the .txt file, remove “” marks using find/replace, then copy this distance matrix into the .arp file, and modify the samples lists to make it consistent.

#After saving this .arp (or other files, e.g., .txt) MAKE SURE TO OPEN IT ONCE WITH EXCEL IN WINDOWS AND RE-SAVE IT AS .TXT, OTHERWISE THE FORMATTING GETS ALL MESSED UP!!! (OR, POSSIBLY SAVING AS WINDOWS FORMATTED TEXT (.txt) MAY WORK EVEN BETTER)

#It doesn’t work to simply make the .arp in Mac, open the .arp in Excel and re-save it, because the formatting still gets messed up. You need to open the distance matrix and the sample labels separately, then merge them with the .arp in Windows (you’ll need to delete some extra “” marks that appear).

#Try to avoid using parentheses in names, because it messes up things in SplitsTree!

#Save as .nex file: 
#First, copy Arlequin print-out to txt file, add [] and ‘’ to numbers and labels, replace negative PhiST values with 0, then copy resulting sheet into a .nex template (remove any parentheses from taxa labels)

#open with SplitsTree.

## THEN COPY RESULTING FST TABLES AND SAVE AS "Japan680Music.csv"

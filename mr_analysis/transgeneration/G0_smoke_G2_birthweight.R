#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#import clean data
grandmother_bw<-read.csv(paste(Sys.getenv('Mydata'),'maternal_smoking_women_participants_clean.csv',sep=''), sep=',')
head(grandmother_bw)
grandmother_bw<-grandmother_bw[c(2,4,7:17,20,27)]


#convert unit of G2 birthweight from pounds to kg
grandmother_bw$bwchild_use<-grandmother_bw$bwchild*0.45359237
summary(grandmother_bw)

#mean & SD 
mean(grandmother_bw$bwchild_use)
sd(grandmother_bw$bwchild_use)

#function, extract beta, se, lci and uci
linearregression <- function(database,modelname) {
  fit = lm(database[,"bwchild_use"] ~., data=database[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
  sumx = summary(fit)
  beta = sumx$coefficients["SNP1","Estimate"]
  se = sumx$coefficients["SNP1","Std. Error"]
  cis = confint(fit, level=0.95)
  lower = cis["SNP1", "2.5 %"]
  upper = cis["SNP1", "97.5 %"]
  write.table(cbind(modelname,beta,se,lower,upper),file=paste(Sys.getenv('Myresults'),'mini project3_plot/bw_grandmum_grandchild.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
}

##in all participants
###G1 SNP~G2 bw in G1=all
linearregression(grandmother_bw,"g0all_g1all")
###G1 SNP~G2 bw in G1=non-smoker
g1nonsmoke<-grandmother_bw[which(grandmother_bw$smoke_preg==0),]
linearregression(g1nonsmoke,"g0all_g1nonsmoke")
###G1 SNP~G2 bw in G1=smoker
g1smoke<-grandmother_bw[which(grandmother_bw$smoke_preg==1),]
linearregression(g1smoke,"g0all_g1smoke")

##subset participants: G0 smoking=No
g0nonsmoke_g1all<-grandmother_bw[which(grandmother_bw$mumsmoke==0),]
linearregression(g0nonsmoke_g1all,"g0nonsmoke_g1all")
##subset participants: G0 smoking=Yes
g0smoke_g1all<-grandmother_bw[which(grandmother_bw$mumsmoke==1),]
linearregression(g0smoke_g1all,"g0smoke_g1all")

#analyze SNP~G2 birthweight in non-smoker(G0+G1)
g0g1nonsmoke<-grandmother_bw[which(grandmother_bw$mumsmoke==0 & grandmother_bw$smoke_preg==0),]
linearregression(g0g1nonsmoke,"g0nonsmoke_g1nonsmoke")

#analyze SNP~G2 birthweight in non-smoker(G1 only)
g0smoke_g1nonsmoke<-grandmother_bw[which(grandmother_bw$mumsmoke==1 & grandmother_bw$smoke_preg==0),]
linearregression(g0smoke_g1nonsmoke,"g0smoke_g1nonsmoke")

#analyze SNP~G2 birthweight in non-smoker(G0 only)
g0nonsmoke_g1smoke<-grandmother_bw[which(grandmother_bw$mumsmoke==0 & grandmother_bw$smoke_preg==1),]
linearregression(g0nonsmoke_g1smoke,"g0nonsmoke_g1smoke")

#analyze SNP~G2 birthweight in smoker(G0+G1)
g0smoke_g1smoke<-grandmother_bw[which(grandmother_bw$mumsmoke==1 & grandmother_bw$smoke_preg==1),]
linearregression(g0smoke_g1smoke,"g0smoke_g1smoke")
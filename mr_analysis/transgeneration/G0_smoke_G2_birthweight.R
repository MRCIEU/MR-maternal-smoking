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


##in all participants
###G1 SNP~G2 bw in G1=all
est_all<-lm(grandmother_bw$bwchild_use~.,data=grandmother_bw[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
###G1 SNP~G2 bw in G1=non-smoker
g1nonsmoke<-grandmother_bw[which(grandmother_bw$smoke_preg==0),]
est_g1nonsmoke<-lm(g1nonsmoke$bwchild_use~.,data=g1nonsmoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
###G1 SNP~G2 bw in G1=smoker
g1smoke<-grandmother_bw[which(grandmother_bw$smoke_preg==1),]
est_g1smoke<-lm(g1smoke$bwchild_use~.,data=g1smoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
summary(est_all)
summary(est_g1nonsmoke)
summary(est_g1smoke)

##subset participants: G0 smoking=No
g0nonsmoke_g1all<-grandmother_bw[which(grandmother_bw$mumsmoke==0),]
##subset participants: G0 smoking=Yes
g0smoke_g1all<-grandmother_bw[which(grandmother_bw$mumsmoke==1),]
##analysis
est_g0nonsmoke_g1all<-lm(g0nonsmoke_g1all$bwchild_use~.,data=g0nonsmoke_g1all[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_g0smoke_g1all<-lm(g0smoke_g1all$bwchild_use~.,data=g0smoke_g1all[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#analyze SNP~G2 birthweight in non-smoker(G0+G1)
g0g1nonsmoke<-grandmother_bw[which(grandmother_bw$mumsmoke==0 & grandmother_bw$smoke_preg==0),]
est_g0g1_nonsmoke<-lm(g0g1nonsmoke$bwchild_use~.,data=g0g1nonsmoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#analyze SNP~G2 birthweight in non-smoker(G1 only)
g0smoke_g1nonsmoke<-grandmother_bw[which(grandmother_bw$mumsmoke==1 & grandmother_bw$smoke_preg==0),]
est_g0smoke_g1nonsmoke<-lm(g0smoke_g1nonsmoke$bwchild_use~.,data=g0smoke_g1nonsmoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#analyze SNP~G2 birthweight in non-smoker(G0 only)
g0nonsmoke_g1smoke<-grandmother_bw[which(grandmother_bw$mumsmoke==0 & grandmother_bw$smoke_preg==1),]
est_g0nonsmoke_g1smoke<-lm(g0nonsmoke_g1smoke$bwchild_use~.,data=g0nonsmoke_g1smoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#analyze SNP~G2 birthweight in smoker(G0+G1)
g0smoke_g1smoke<-grandmother_bw[which(grandmother_bw$mumsmoke==1 & grandmother_bw$smoke_preg==1),]
est_g0smoke_g1smoke<-lm(g0smoke_g1smoke$bwchild_use~.,data=g0smoke_g1smoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#save results in a file for plot
#creat a matrix for results
vars<-c("g0smoke_g1smoke", "g0smoke_g1nonsmoke", "g0smoke_g1all", "g0nonsmoke_g1smoke", "g0nonsmoke_g1nonsmoke", "g0nonsmoke_g1all","all","g0all_g1nonsmoke","g0all_g1smoke")
col.names <- c("supp","exposure","bw","se")
covar <- matrix(, ncol=4, nrow=length(vars))
dimnames(covar) <- list(vars, col.names)

#in the 1st colomn of matrix("supp"), "1" means both non-smokers and smokers in G0,"2" means G0 is non-smoker,"3" means G0 is smoker
covar[,1] <- c(3,3,3,2,2,2,1,1,1)

#in the 2nd colomn of matrix("exposure"), "1" means G1 is smoker, "3" means G1 is non-smoker, "5" means all participants
covar[,2] <- c(1,3,5,1,3,5,5,3,1)

#extract beta 
covar[1,3] <- est_g0smoke_g1smoke$coefficients['SNP1']
covar[2,3] <- est_g0smoke_g1nonsmoke$coefficients['SNP1']
covar[3,3] <- est_g0smoke_g1all$coefficients['SNP1']
covar[4,3] <- est_g0nonsmoke_g1smoke$coefficients['SNP1']
covar[5,3] <- est_g0g1_nonsmoke$coefficients['SNP1']
covar[6,3] <- est_g0nonsmoke_g1all$coefficients['SNP1']
covar[7,3] <- est_all$coefficients['SNP1']
covar[8,3] <- est_g1nonsmoke$coefficients['SNP1']
covar[9,3] <- est_g1smoke$coefficients['SNP1']

#extract SE
covar[1,4] <- coef(summary(est_g0smoke_g1smoke))['SNP1', "Std. Error"]
covar[2,4] <- coef(summary(est_g0smoke_g1nonsmoke))['SNP1', "Std. Error"]
covar[3,4] <- coef(summary(est_g0smoke_g1all))['SNP1', "Std. Error"]
covar[4,4] <- coef(summary(est_g0nonsmoke_g1smoke))['SNP1', "Std. Error"]
covar[5,4] <- coef(summary(est_g0g1_nonsmoke))['SNP1', "Std. Error"]
covar[6,4] <- coef(summary(est_g0nonsmoke_g1all))['SNP1', "Std. Error"]
covar[7,4] <- coef(summary(est_all))['SNP1', "Std. Error"]
covar[8,4] <- coef(summary(est_g1nonsmoke))['SNP1', "Std. Error"]
covar[9,4] <- coef(summary(est_g1smoke))['SNP1', "Std. Error"]

write.csv(covar, file=paste(Sys.getenv('Myresults'),'mini project3_plot/bw_grandmum_grandchild.csv',sep=''))
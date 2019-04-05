#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#import IQ data 
iq<-read.csv(paste(Sys.getenv('Mydata'),'iq.csv',sep=''), sep=',')
head(iq)
colnames(iq)<-c("app16729","iq_t1","iq_t2","iq_t3","iq_online")

#import previous clean dataset
proof2<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''), sep=',')
head(proof2)
proof2<-proof2[c(2:4,6:20)]

#combine new outcome data with my previous dataset
iq_use<-merge(proof2,iq,by="app16729")
summary(iq_use)

###################################################################################################
###################################################################################################
#the following analyses use iq_use database
###################################################################################################
###################################################################################################
#Part 1
#If participants took the VNR test at multiple time points, only the earliest was used (PMID 29326435)
iq_use$iq<-iq_use$iq_online
iq3ix<-which(!is.na(iq_use$iq_t3))
iq_use$iq[iq3ix]<-iq_use$iq_t3[iq3ix]
iq2ix<-which(!is.na(iq_use$iq_t2))
iq_use$iq[iq2ix]<-iq_use$iq_t2[iq2ix]
iq1ix<-which(!is.na(iq_use$iq_t1))
iq_use$iq[iq1ix]<-iq_use$iq_t1[iq1ix]

#mean & SD by sex
summary(iq_use)
aggregate(iq_use,by=list(iq_use$sex),FUN = mean,na.rm=TRUE)
aggregate(iq_use,by=list(iq_use$sex),FUN = sd,na.rm=TRUE)


#Part 2
#note: use baseline & follow-up "Fluid intelligence score"
#note: unweighted sum of the number of correct answers given to the 13 fluid intelligence questions
#in all participants
##subset participants: G0 smoking=No
mumnonsmoke_allchild<-iq_use[which(iq_use$mumsmoke==0),]
##subset participants: G0 smoking=Yes
mumsmoke_allchild<-iq_use[which(iq_use$mumsmoke==1),]
est_mumnonsmoke_allchild<-lm(mumnonsmoke_allchild$iq~.,data=mumnonsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_mumsmoke_allchild<-lm(mumsmoke_allchild$iq~.,data=mumsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#in non-smoker(G0 & G1)
nonsmoke<-iq_use[which(iq_use$mumsmoke==0 & iq_use$smoke==0),]
est_both_nonsmoke<-lm(nonsmoke$iq~.,data=nonsmoke[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=never smoker
smoke_mum<-iq_use[which(iq_use$mumsmoke==1 & iq_use$smoke==0),]
est_only_mumsmoke<-lm(smoke_mum$iq~.,data=smoke_mum[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=former smoker
former_child<-iq_use[which(iq_use$mumsmoke==0 & iq_use$smoke==1),]
est_only_child_former<-lm(former_child$iq~.,data=former_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=current smoker
current_child<-iq_use[which(iq_use$mumsmoke==0 & iq_use$smoke==2),]
est_only_child_current<-lm(current_child$iq~.,data=current_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=former smoker
smoke_both_former<-iq_use[which(iq_use$mumsmoke==1 & iq_use$smoke==1),]
est_both_former<-lm(smoke_both_former$iq~.,data=smoke_both_former[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=current smoker
smoke_both_current<-iq_use[which(iq_use$mumsmoke==1 & iq_use$smoke==2),]
est_both_current<-lm(smoke_both_current$iq~.,data=smoke_both_current[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#Part 3
#save results in a file for plot
#creat a matrix for results
vars<-c("mumsmoke_current","mumsmoke_former","mumsmoke_never","mumsmoke_all","mumnot_current","mumnot_former","mumnot_never","mumnot_all")
col.names <- c("supp","exposure","iq","se")
covar <- matrix(, ncol=4, nrow=length(vars))
dimnames(covar) <- list(vars, col.names)

#in the 1st colomn of matrix("supp"), "1" subgroup of "G0 smoking=No", "2" subgroup of "G0 smoking=Yes",
covar[,1] <- c(2,2,2,2,1,1,1,1)

#in the 2nd colomn of matrix("exposure"), "1" subgroup of current smoker, "3" subgroup of former smoker, "5" subgroup of never smoker, "7" all UKBB participants
covar[,2] <- c(1,3,5,7,1,3,5,7)

#extract beta 
covar[1,3] <- est_both_current$coefficients['SNP1']
covar[2,3] <- est_both_former$coefficients['SNP1']
covar[3,3] <- est_only_mumsmoke$coefficients['SNP1']
covar[4,3] <- est_mumsmoke_allchild$coefficients['SNP1']
covar[5,3] <- est_only_child_current$coefficients['SNP1']
covar[6,3] <- est_only_child_former$coefficients['SNP1']
covar[7,3] <- est_both_nonsmoke$coefficients['SNP1']
covar[8,3] <- est_mumnonsmoke_allchild$coefficients['SNP1']

#extract SE
covar[1,4] <- coef(summary(est_both_current))['SNP1', "Std. Error"]
covar[2,4] <- coef(summary(est_both_former))['SNP1', "Std. Error"]
covar[3,4] <- coef(summary(est_only_mumsmoke))['SNP1', "Std. Error"]
covar[4,4] <- coef(summary(est_mumsmoke_allchild))['SNP1', "Std. Error"]
covar[5,4] <- coef(summary(est_only_child_current))['SNP1', "Std. Error"]
covar[6,4] <- coef(summary(est_only_child_former))['SNP1', "Std. Error"]
covar[7,4] <- coef(summary(est_both_nonsmoke))['SNP1', "Std. Error"]
covar[8,4] <- coef(summary(est_mumnonsmoke_allchild))['SNP1', "Std. Error"]

write.csv(covar, file=paste(Sys.getenv('Myresults'),'mini project3_plot/iq.csv',sep=''))
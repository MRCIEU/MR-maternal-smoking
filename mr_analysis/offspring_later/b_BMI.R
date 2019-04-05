#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#import BMI data
outcome<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi_lung.csv',sep=''), sep=',')
summary(outcome)
outcome<-outcome[c(1,4,5)]
colnames(outcome)<-c("app16729","smoke","bmi")

#import previous clean dataset
proof<-read.csv(paste(Sys.getenv('Mydata'),'bw_only_correct.csv',sep=''), sep=',')
summary(proof)
proof<-proof[c(2:17)]

#import "age" of UKBB participants
age<-read.csv(paste(Sys.getenv('Mydata'),'height_education.csv',sep=''), sep=',')
summary(age)
age<-age[c(1,11)]
colnames(age)<-c("app16729","age")

#combine new outcome data with my previous dataset
outcome_proof<-merge(outcome,proof,by="app16729")
proof2<-merge(age,outcome_proof,by="app16729")
summary(proof2)

write.csv(proof2,file=paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''))
###################################################################################################
###################################################################################################
#the following analyses use proof2 database
###################################################################################################
###################################################################################################
#read in database
proof2<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''), sep=',')
summary(proof2)

#mean & SD by sex
aggregate(proof2,by=list(proof2$sex),FUN = mean,na.rm=TRUE)
aggregate(proof2,by=list(proof2$sex),FUN = sd,na.rm=TRUE)
table(proof2$smoke,proof2$sex)


#in all participants
##subset participants: G0 smoking=NO
mumnonsmoke_allchild<-proof2[which(proof2$mumsmoke==0),]
##subset participants: G0 smoking=YES
mumsmoke_allchild<-proof2[which(proof2$mumsmoke==1),]
##analysis
est_mumnonsmoke_allchild<-lm(mumnonsmoke_allchild$bmi~.,data=mumnonsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_mumsmoke_allchild<-lm(mumsmoke_allchild$bmi~.,data=mumsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])

#in non-smoker(G0 & G1)
nonsmoke<-proof2[which(proof2$mumsmoke=="0" & proof2$smoke==0),]
est_both_nonsmoke<-lm(nonsmoke$bmi~.,data=nonsmoke[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=never smoker
smoke_mum<-proof2[which(proof2$mumsmoke=="1" & proof2$smoke==0),]
est_only_mumsmoke<-lm(smoke_mum$bmi~.,data=smoke_mum[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=former smoker
former_child<-proof2[which(proof2$mumsmoke=="0" & proof2$smoke==1),]
est_only_child_former<-lm(former_child$bmi~.,data=former_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=current smoker
current_child<-proof2[which(proof2$mumsmoke=="0" & proof2$smoke==2),]
est_only_child_current<-lm(current_child$bmi~.,data=current_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=former smoker
smoke_both_former<-proof2[which(proof2$mumsmoke=="1" & proof2$smoke==1),]
est_both_former<-lm(smoke_both_former$bmi~.,data=smoke_both_former[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=current smoker
smoke_both_current<-proof2[which(proof2$mumsmoke=="1" & proof2$smoke==2),]
est_both_current<-lm(smoke_both_current$bmi~.,data=smoke_both_current[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#save results in a file for plot
#creat a matrix for results
vars<-c("mumsmoke_current","mumsmoke_former","mumsmoke_never","mumsmoke_all","mumnot_current","mumnot_former","mumnot_never","mumnot_all")
col.names <- c("supp","exposure","bmi","se")
covar <- matrix(, ncol=4, nrow=length(vars))
dimnames(covar) <- list(vars, col.names)

#in the 1st colomn of matrix("supp"), "1" subgroup of "G0 smoking=NO", "2" subgroup of "G0 smoking=YES",
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

write.csv(covar, file=paste(Sys.getenv('Myresults'),'mini project3_plot/bmi.csv',sep=''))
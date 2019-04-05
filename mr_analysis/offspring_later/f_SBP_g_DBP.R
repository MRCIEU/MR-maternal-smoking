#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#import blood pressure data
bp<-read.csv(paste(Sys.getenv('Mydata'),'bp.csv',sep=''), sep=',')
summary(bp)
colnames(bp)<-c("app16729","sbp_manual1","sbp_manual2","dbp_manual1","dbp_manual2","dbp_auto1","dbp_auto2","sbp_auto1","sbp_auto2")
##mean of 2 readings
bp$sbp_auto<-rowMeans(bp[,c("sbp_auto1","sbp_auto2")],na.rm = TRUE)
bp$sbp_manual<-rowMeans(bp[,c("sbp_manual1","sbp_manual2")],na.rm = TRUE)
bp$dbp_auto<-rowMeans(bp[,c("dbp_auto1","dbp_auto2")],na.rm = TRUE)
bp$dbp_manual<-rowMeans(bp[,c("dbp_manual1","dbp_manual2")],na.rm = TRUE)
##manual sphygmomanometer was used if digital monitor could not be employed
bp$sbp<-bp$sbp_auto
sbpix<-which(!is.na(bp$sbp_manual))
bp$sbp[sbpix]<-bp$sbp_manual[sbpix]
bp$dbp<-bp$dbp_auto
dbpix<-which(!is.na(bp$dbp_manual))
bp$dbp[dbpix]<-bp$dbp_manual[dbpix]
summary(bp)

#import previous clean dataset
proof2<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''), sep=',')
head(proof2)
proof2<-proof2[c(2:4,6:20)]


#combine new outcome data with my previous dataset
proof3<-merge(proof2,bp,by="app16729")
summary(proof3)

#mean & SD by sex
aggregate(proof3,by=list(proof3$sex),FUN = mean,na.rm=TRUE)
aggregate(proof3,by=list(proof3$sex),FUN = sd,na.rm=TRUE)

###################################################################################################
###################################################################################################
#the following analyses use proof3 database
###################################################################################################
###################################################################################################
#SBP
#in all participants
##subset participants: G0 smoking=NO
mumnonsmoke_allchild<-proof3[which(proof3$mumsmoke==0),]
##subset participants: G0 smoking=YES
mumsmoke_allchild<-proof3[which(proof3$mumsmoke==1),]
est_mumnonsmoke_allchild<-lm(mumnonsmoke_allchild$sbp~.,data=mumnonsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_mumsmoke_allchild<-lm(mumsmoke_allchild$sbp~.,data=mumsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#in non-smoker(G0 & G1)
nonsmoke<-proof3[which(proof3$mumsmoke==0 & proof3$smoke==0),]
est_both_nonsmoke<-lm(nonsmoke$sbp~.,data=nonsmoke[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=never smoker
smoke_mum<-proof3[which(proof3$mumsmoke==1 & proof3$smoke==0),]
est_only_mumsmoke<-lm(smoke_mum$sbp~.,data=smoke_mum[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=former smoker
former_child<-proof3[which(proof3$mumsmoke==0 & proof3$smoke==1),]
est_only_child_former<-lm(former_child$sbp~.,data=former_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=current smoker
current_child<-proof3[which(proof3$mumsmoke==0 & proof3$smoke==2),]
est_only_child_current<-lm(current_child$sbp~.,data=current_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=former smoker
smoke_both_former<-proof3[which(proof3$mumsmoke==1 & proof3$smoke==1),]
est_both_former<-lm(smoke_both_former$sbp~.,data=smoke_both_former[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=current smoker
smoke_both_current<-proof3[which(proof3$mumsmoke==1 & proof3$smoke==2),]
est_both_current<-lm(smoke_both_current$sbp~.,data=smoke_both_current[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#save results in a file for plot
#creat a matrix for results
vars<-c("mumsmoke_current","mumsmoke_former","mumsmoke_never","mumsmoke_all","mumnot_current","mumnot_former","mumnot_never","mumnot_all")
col.names <- c("supp","exposure","sbp","se")
covar_sbp <- matrix(, ncol=4, nrow=length(vars))
dimnames(covar_sbp) <- list(vars, col.names)

#in the 1st colomn of matrix("supp"), "1" subgroup of "G0 smoking=No", "2" subgroup of "G0 smoking=Yes"
covar_sbp[,1] <- c(2,2,2,2,1,1,1,1)

#in the 2nd colomn of matrix("exposure"), "1" subgroup of current smoker, "3" subgroup of former smoker, "5" subgroup of never smoker, "7" all UKBB participants
covar_sbp[,2] <- c(1,3,5,7,1,3,5,7)

#extract beta 
covar_sbp[1,3] <- est_both_current$coefficients['SNP1']
covar_sbp[2,3] <- est_both_former$coefficients['SNP1']
covar_sbp[3,3] <- est_only_mumsmoke$coefficients['SNP1']
covar_sbp[4,3] <- est_mumsmoke_allchild$coefficients['SNP1']
covar_sbp[5,3] <- est_only_child_current$coefficients['SNP1']
covar_sbp[6,3] <- est_only_child_former$coefficients['SNP1']
covar_sbp[7,3] <- est_both_nonsmoke$coefficients['SNP1']
covar_sbp[8,3] <- est_mumnonsmoke_allchild$coefficients['SNP1']

#extract SE
covar_sbp[1,4] <- coef(summary(est_both_current))['SNP1', "Std. Error"]
covar_sbp[2,4] <- coef(summary(est_both_former))['SNP1', "Std. Error"]
covar_sbp[3,4] <- coef(summary(est_only_mumsmoke))['SNP1', "Std. Error"]
covar_sbp[4,4] <- coef(summary(est_mumsmoke_allchild))['SNP1', "Std. Error"]
covar_sbp[5,4] <- coef(summary(est_only_child_current))['SNP1', "Std. Error"]
covar_sbp[6,4] <- coef(summary(est_only_child_former))['SNP1', "Std. Error"]
covar_sbp[7,4] <- coef(summary(est_both_nonsmoke))['SNP1', "Std. Error"]
covar_sbp[8,4] <- coef(summary(est_mumnonsmoke_allchild))['SNP1', "Std. Error"]

write.csv(covar_sbp, file=paste(Sys.getenv('Myresults'),'mini project3_plot/sbp.csv',sep=''))
###########################################################################################################################
#DBP
#in all participants
est_mumnonsmoke_allchild<-lm(mumnonsmoke_allchild$dbp~.,data=mumnonsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_mumsmoke_allchild<-lm(mumsmoke_allchild$dbp~.,data=mumsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#in non-smoker(G0 & G1)
est_both_nonsmoke<-lm(nonsmoke$dbp~.,data=nonsmoke[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=never smoker
est_only_mumsmoke<-lm(smoke_mum$dbp~.,data=smoke_mum[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=former smoker
est_only_child_former<-lm(former_child$dbp~.,data=former_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=current smoker
est_only_child_current<-lm(current_child$dbp~.,data=current_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=former smoker
est_both_former<-lm(smoke_both_former$dbp~.,data=smoke_both_former[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=current smoker
est_both_current<-lm(smoke_both_current$dbp~.,data=smoke_both_current[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#save results in a file for plot
#creat a matrix for results
vars<-c("mumsmoke_current","mumsmoke_former","mumsmoke_never","mumsmoke_all","mumnot_current","mumnot_former","mumnot_never","mumnot_all")
col.names <- c("supp","exposure","dbp","se")
covar_dbp <- matrix(, ncol=4, nrow=length(vars))
dimnames(covar_dbp) <- list(vars, col.names)

#in the 1st colomn of matrix("supp"), "1" subgroup of "G0 smoking=No", "2" subgroup of "G0 smoking=Yes"
covar_dbp[,1] <- c(2,2,2,2,1,1,1,1)

#in the 2nd colomn of matrix("exposure"), "1" subgroup of current smoker, "3" subgroup of former smoker, "5" subgroup of never smoker, "7" all UKBB participants
covar_dbp[,2] <- c(1,3,5,7,1,3,5,7)

#extract beta 
covar_dbp[1,3] <- est_both_current$coefficients['SNP1']
covar_dbp[2,3] <- est_both_former$coefficients['SNP1']
covar_dbp[3,3] <- est_only_mumsmoke$coefficients['SNP1']
covar_dbp[4,3] <- est_mumsmoke_allchild$coefficients['SNP1']
covar_dbp[5,3] <- est_only_child_current$coefficients['SNP1']
covar_dbp[6,3] <- est_only_child_former$coefficients['SNP1']
covar_dbp[7,3] <- est_both_nonsmoke$coefficients['SNP1']
covar_dbp[8,3] <- est_mumnonsmoke_allchild$coefficients['SNP1']

#extract SE
covar_dbp[1,4] <- coef(summary(est_both_current))['SNP1', "Std. Error"]
covar_dbp[2,4] <- coef(summary(est_both_former))['SNP1', "Std. Error"]
covar_dbp[3,4] <- coef(summary(est_only_mumsmoke))['SNP1', "Std. Error"]
covar_dbp[4,4] <- coef(summary(est_mumsmoke_allchild))['SNP1', "Std. Error"]
covar_dbp[5,4] <- coef(summary(est_only_child_current))['SNP1', "Std. Error"]
covar_dbp[6,4] <- coef(summary(est_only_child_former))['SNP1', "Std. Error"]
covar_dbp[7,4] <- coef(summary(est_both_nonsmoke))['SNP1', "Std. Error"]
covar_dbp[8,4] <- coef(summary(est_mumnonsmoke_allchild))['SNP1', "Std. Error"]

write.csv(covar_dbp, file=paste(Sys.getenv('Myresults'),'mini project3_plot/dbp.csv',sep=''))
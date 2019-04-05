#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#import lung function (best measure) data
best_measure<-read.csv(paste(Sys.getenv('Mydata'),'lung_function_best_measures.csv',sep=''), sep=',')
head(best_measure)
colnames(best_measure)<-c("app16729","fev1_best","fvc_best")


#import previous clean dataset
proof2<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''), sep=',')
head(proof2)
proof2<-proof2[c(2:4,6:20)]


#combine new outcome data with my previous dataset
proof3<-merge(proof2,best_measure,by="app16729")
summary(proof3)


#mean & SD by sex
aggregate(proof3,by=list(proof3$sex),FUN = mean,na.rm=TRUE)
aggregate(proof3,by=list(proof3$sex),FUN = sd,na.rm=TRUE)

###################################################################################################
###################################################################################################
#the following analyses use proof3 database
###################################################################################################
###################################################################################################
#FVC
#in all participants
##subset participants: G0 smoking=No
mumnonsmoke_allchild<-proof3[which(proof3$mumsmoke==0),]
##subset participants: G0 smoking=Yes
mumsmoke_allchild<-proof3[which(proof3$mumsmoke==1),]
##analysis
est_mumnonsmoke_allchild<-lm(mumnonsmoke_allchild$fvc_best~.,data=mumnonsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_mumsmoke_allchild<-lm(mumsmoke_allchild$fvc_best~.,data=mumsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#in non-smoker(G0 & G1)
nonsmoke<-proof3[which(proof3$mumsmoke==0 & proof3$smoke==0),]
est_both_nonsmoke<-lm(nonsmoke$fvc_best~.,data=nonsmoke[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=never smoker
smoke_mum<-proof3[which(proof3$mumsmoke==1 & proof3$smoke==0),]
est_only_mumsmoke<-lm(smoke_mum$fvc_best~.,data=smoke_mum[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=former smoker
former_child<-proof3[which(proof3$mumsmoke==0 & proof3$smoke==1),]
est_only_child_former<-lm(former_child$fvc_best~.,data=former_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=current smoker
current_child<-proof3[which(proof3$mumsmoke==0 & proof3$smoke==2),]
est_only_child_current<-lm(current_child$fvc_best~.,data=current_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=former smoker
smoke_both_former<-proof3[which(proof3$mumsmoke==1 & proof3$smoke==1),]
est_both_former<-lm(smoke_both_former$fvc_best~.,data=smoke_both_former[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=current smoker
smoke_both_current<-proof3[which(proof3$mumsmoke==1 & proof3$smoke==2),]
est_both_current<-lm(smoke_both_current$fvc_best~.,data=smoke_both_current[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#save results in a file for plot
#creat a matrix for results
vars<-c("mumsmoke_current","mumsmoke_former","mumsmoke_never","mumsmoke_all","mumnot_current","mumnot_former","mumnot_never","mumnot_all")
col.names <- c("supp","exposure","fvc","se")
covar_fvc <- matrix(, ncol=4, nrow=length(vars))
dimnames(covar_fvc) <- list(vars, col.names)

#in the 1st colomn of matrix("supp"), "1" subgroup of "G0 smoking=No", "2" subgroup of "G0 smoking=Yes"
covar_fvc[,1] <- c(2,2,2,2,1,1,1,1)

#in the 2nd colomn of matrix("exposure"), "1" subgroup of current smoker, "3" subgroup of former smoker, "5" subgroup of never smoker, "7" all UKBB participants
covar_fvc[,2] <- c(1,3,5,7,1,3,5,7)

#extract beta 
covar_fvc[1,3] <- est_both_current$coefficients['SNP1']
covar_fvc[2,3] <- est_both_former$coefficients['SNP1']
covar_fvc[3,3] <- est_only_mumsmoke$coefficients['SNP1']
covar_fvc[4,3] <- est_mumsmoke_allchild$coefficients['SNP1']
covar_fvc[5,3] <- est_only_child_current$coefficients['SNP1']
covar_fvc[6,3] <- est_only_child_former$coefficients['SNP1']
covar_fvc[7,3] <- est_both_nonsmoke$coefficients['SNP1']
covar_fvc[8,3] <- est_mumnonsmoke_allchild$coefficients['SNP1']

#extract SE
covar_fvc[1,4] <- coef(summary(est_both_current))['SNP1', "Std. Error"]
covar_fvc[2,4] <- coef(summary(est_both_former))['SNP1', "Std. Error"]
covar_fvc[3,4] <- coef(summary(est_only_mumsmoke))['SNP1', "Std. Error"]
covar_fvc[4,4] <- coef(summary(est_mumsmoke_allchild))['SNP1', "Std. Error"]
covar_fvc[5,4] <- coef(summary(est_only_child_current))['SNP1', "Std. Error"]
covar_fvc[6,4] <- coef(summary(est_only_child_former))['SNP1', "Std. Error"]
covar_fvc[7,4] <- coef(summary(est_both_nonsmoke))['SNP1', "Std. Error"]
covar_fvc[8,4] <- coef(summary(est_mumnonsmoke_allchild))['SNP1', "Std. Error"]

write.csv(covar_fvc, file=paste(Sys.getenv('Myresults'),'mini project3_plot/fvc_best.csv',sep=''))
###########################################################################################################################
#FEV1
#in all participants
est_mumnonsmoke_allchild<-lm(mumnonsmoke_allchild$fev1_best~.,data=mumnonsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_mumsmoke_allchild<-lm(mumsmoke_allchild$fev1_best~.,data=mumsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#in non-smoker(G0 & G1)
est_both_nonsmoke<-lm(nonsmoke$fev1_best~.,data=nonsmoke[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])

#G0=smoker, G1=never smoker
est_only_mumsmoke<-lm(smoke_mum$fev1_best~.,data=smoke_mum[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=former smoker
est_only_child_former<-lm(former_child$fev1_best~.,data=former_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=current smoker
est_only_child_current<-lm(current_child$fev1_best~.,data=current_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=former smoker
est_both_former<-lm(smoke_both_former$fev1_best~.,data=smoke_both_former[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=current smoker                                                                                                                  
est_both_current<-lm(smoke_both_current$fev1_best~.,data=smoke_both_current[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#save results in a file for plot
#creat a matrix for results
vars<-c("mumsmoke_current","mumsmoke_former","mumsmoke_never","mumsmoke_all","mumnot_current","mumnot_former","mumnot_never","mumnot_all")
col.names <- c("supp","exposure","fev1","se")
covar_fev1 <- matrix(, ncol=4, nrow=length(vars))
dimnames(covar_fev1) <- list(vars, col.names)

#in the 1st colomn of matrix("supp"), "1" subgroup of "G0 smoking=No", "2" subgroup of "G0 smoking=Yes"
covar_fev1[,1] <- c(2,2,2,2,1,1,1,1)

#in the 2nd colomn of matrix("exposure"), "1" subgroup of current smoker, "3" subgroup of former smoker, "5" subgroup of never smoker, "7" all UKBB participants
covar_fev1[,2] <- c(1,3,5,7,1,3,5,7)

#extract beta 
covar_fev1[1,3] <- est_both_current$coefficients['SNP1']
covar_fev1[2,3] <- est_both_former$coefficients['SNP1']
covar_fev1[3,3] <- est_only_mumsmoke$coefficients['SNP1']
covar_fev1[4,3] <- est_mumsmoke_allchild$coefficients['SNP1']
covar_fev1[6,3] <- est_only_child_former$coefficients['SNP1']
covar_fev1[7,3] <- est_both_nonsmoke$coefficients['SNP1']
covar_fev1[8,3] <- est_mumnonsmoke_allchild$coefficients['SNP1']

#extract SE
covar_fev1[1,4] <- coef(summary(est_both_current))['SNP1', "Std. Error"]
covar_fev1[2,4] <- coef(summary(est_both_former))['SNP1', "Std. Error"]
covar_fev1[3,4] <- coef(summary(est_only_mumsmoke))['SNP1', "Std. Error"]
covar_fev1[4,4] <- coef(summary(est_mumsmoke_allchild))['SNP1', "Std. Error"]
covar_fev1[5,4] <- coef(summary(est_only_child_current))['SNP1', "Std. Error"]
covar_fev1[6,4] <- coef(summary(est_only_child_former))['SNP1', "Std. Error"]
covar_fev1[7,4] <- coef(summary(est_both_nonsmoke))['SNP1', "Std. Error"]
covar_fev1[8,4] <- coef(summary(est_mumnonsmoke_allchild))['SNP1', "Std. Error"]

write.csv(covar_fev1, file=paste(Sys.getenv('Myresults'),'mini project3_plot/fev1_best.csv',sep=''))
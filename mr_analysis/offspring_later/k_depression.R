#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())


#import depression data (questionnaire)
depression_ques<-read.csv(paste(Sys.getenv('Mydata'),'depression.csv',sep=''), sep=',')

#import depression data (ICD-10)
depression_icd_main<-read.csv(paste(Sys.getenv('Mydata'),'icd10_main_depression.csv',sep=''), sep=',')
depression_icd_second<-read.csv(paste(Sys.getenv('Mydata'),'icd10_second_depression.csv',sep=''), sep=',')

#combine depression data to derive depression_diag
depression_icd<-merge(depression_icd_main,depression_icd_second,by="eid")
depression<-merge(depression_ques,depression_icd,by="eid")
colnames(depression)<-c("app16729","gp1","gp2","gp3","psych1","psych2","psych3","depression_diag_main","depression_diag_second")

#code value < 0 as missing
depression$gp1[depression$gp1<0]<-NA
depression$gp2[depression$gp2<0]<-NA
depression$gp3[depression$gp3<0]<-NA
depression$psych1[depression$psych1<0]<-NA
depression$psych2[depression$psych2<0]<-NA
depression$psych3[depression$psych3<0]<-NA

#code 0 means no depression, 1 means any depression in any question/assessment/ICD-code
depression$diag_sum<-rowSums(depression[,c(2:9)],na.rm=TRUE)
depression$diag_sum[is.na(depression$gp1) & is.na(depression$gp2) & is.na(depression$gp3) & is.na(depression$psych1) & is.na(depression$psych2) & is.na(depression$psych3) & is.na(depression$depression_diag_main) & is.na(depression$depression_diag_second)]<-NA
depression$diag_sum[depression$diag_sum>0]<-1

depression<-depression[c("app16729","diag_sum")]

###################################################################################################
#import previous clean dataset
proof2<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''), sep=',')
head(proof2)
proof2<-proof2[c(2:4,6:20)]

#combine new outcome data with my previous dataset
dep_use<-merge(proof2,depression,by="app16729")
summary(dep_use)

#N(%) by sex
table(dep_use$diag_sum,dep_use$sex)

###################################################################################################
###################################################################################################
#the following analyses use dep_use database
###################################################################################################
###################################################################################################
#in all participants
##subset participants: G0 smoking=No
mumnonsmoke_allchild<-dep_use[which(dep_use$mumsmoke==0),]
##subset participants: G0 smoking=Yes
mumsmoke_allchild<-dep_use[which(dep_use$mumsmoke==1),]
##analysis
est_mumnonsmoke_allchild<-glm(mumnonsmoke_allchild$diag_sum~.,binomial(link = "logit"),data=mumnonsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_mumsmoke_allchild<-glm(mumsmoke_allchild$diag_sum~.,binomial(link = "logit"),data=mumsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#in non-smoker(G0 & G1)
nonsmoke<-dep_use[which(dep_use$mumsmoke==0 & dep_use$smoke==0),]
est_both_nonsmoke<-glm(nonsmoke$diag_sum~.,binomial(link = "logit"),data=nonsmoke[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=never smoker
smoke_mum<-dep_use[which(dep_use$mumsmoke==1 & dep_use$smoke==0),]
est_only_mumsmoke<-glm(smoke_mum$diag_sum~.,binomial(link = "logit"),data=smoke_mum[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=former smoker
former_child<-dep_use[which(dep_use$mumsmoke==0 & dep_use$smoke==1),]
est_only_child_former<-glm(former_child$diag_sum~.,binomial(link = "logit"),data=former_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=current smoker
current_child<-dep_use[which(dep_use$mumsmoke==0 & dep_use$smoke==2),]
est_only_child_current<-glm(current_child$diag_sum~.,binomial(link = "logit"),data=current_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=former smoker
smoke_both_former<-dep_use[which(dep_use$mumsmoke==1 & dep_use$smoke==1),]
est_both_former<-glm(smoke_both_former$diag_sum~.,binomial(link = "logit"),data=smoke_both_former[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=current smoker
smoke_both_current<-dep_use[which(dep_use$mumsmoke==1 & dep_use$smoke==2),]
est_both_current<-glm(smoke_both_current$diag_sum~.,binomial(link = "logit"),data=smoke_both_current[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#save results in a file for plot
#creat a matrix for results
vars<-c("mumsmoke_current","mumsmoke_former","mumsmoke_never","mumsmoke_all","mumnot_current","mumnot_former","mumnot_never","mumnot_all")
col.names <- c("supp","exposure","depress","se")
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

write.csv(covar, file=paste(Sys.getenv('Myresults'),'mini project3_plot/depression.csv',sep=''))
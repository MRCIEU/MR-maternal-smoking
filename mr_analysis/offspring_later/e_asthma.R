#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#import asthma data
asthma<-read.csv(paste(Sys.getenv('Mydata'),'asthma.csv',sep=''), sep=',')
summary(asthma)
colnames(asthma)<-c("app16729","asthma_age","asthma_q")
asthma$asthma_age[asthma$asthma_age<0]<-NA
asthma$asthma_d[as.numeric(asthma$asthma_q)==-7 | as.numeric(asthma$asthma_q)>0]<-0 #define controls
asthma$asthma_d[as.numeric(asthma$asthma_q)==8]<-1 #define asthma cases


#import age start smoking
smoking_check<-read.csv(paste(Sys.getenv('Mydata'),'maternal_smoking_women_participants.csv',sep=''), sep=',')
head(smoking_check)
colnames(smoking_check)<-c("app16729","live_b_no","bwchild","age_live_b","age_start_former","age_stop","age_start_current","smoking","age_stop_medical")
smoking_check<-smoking_check[c(1,5,7,8)]

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
asthma_smoking<-merge(asthma,smoking_check,by="app16729")
asthma_smoking2<-merge(asthma_smoking,proof,by="app16729")
asthma_use<-merge(asthma_smoking2,age,by="app16729")
summary(asthma_use)
table(asthma_use$asthma_d,asthma_use$sex)

###################################################################################################
###################################################################################################
#the following analyses use asthma_use database
###################################################################################################
###################################################################################################
#ever smoker vs never smoker in UKBB participants
#in all participants
##subset participants: G0 smoking=No
mumnonsmoke_allchild<-asthma_use[which(asthma_use$mumsmoke==0),]
##subset participants: G0 smoking=Yes
mumsmoke_allchild<-asthma_use[which(asthma_use$mumsmoke==1),]
##analysis
est_mumnonsmoke_allchild<-glm(mumnonsmoke_allchild$asthma_d~.,binomial(link = "logit"),data=mumnonsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_mumsmoke_allchild<-glm(mumsmoke_allchild$asthma_d~.,binomial(link = "logit"),data=mumsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#in non-smoker(G0 & G1)
nonsmoke<-asthma_use[which(asthma_use$mumsmoke==0 & asthma_use$smoking==0),]
est_both_nonsmoke<-glm(nonsmoke$asthma_d~.,binomial(link = "logit"),data=nonsmoke[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=never smoker
smoke_mum<-asthma_use[which(asthma_use$mumsmoke==1 & asthma_use$smoking==0),]
est_only_mumsmoke<-glm(smoke_mum$asthma_d~.,binomial(link = "logit"),data=smoke_mum[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=ever smoker
smoke_child<-asthma_use[which(asthma_use$mumsmoke==0 & asthma_use$smoking>=1),]
est_only_childsmoke<-glm(smoke_child$asthma_d~.,binomial(link = "logit"),data=smoke_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=ever smoker
smoke_both<-asthma_use[which(asthma_use$mumsmoke==1 & asthma_use$smoking>=1),]
est_both_smoke<-glm(smoke_both$asthma_d~.,binomial(link = "logit"),data=smoke_both[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#save results in a file for plot
#creat a matrix for results
vars<-c("mumsmoke_ever", "mumsmoke_never", "mumsmoke_all", "mumnot_ever", "mumnot_never", "mumnot_all")
col.names <- c("supp","exposure","asthma","se")
covar <- matrix(, ncol=4, nrow=length(vars))
dimnames(covar) <- list(vars, col.names)

#in the 1st colomn of matrix("supp"), "1" subgroup of "G0 smoking=NO", "2" subgroup of "G0 smoking=YES"
covar[,1] <- c(2,2,2,1,1,1)

#in the 2nd colomn of matrix("exposure"), "1" subgroup of ever smoker, "3" subgroup of never smoker, "5" all UKBB participants
covar[,2] <- c(1,3,5,1,3,5)

#extract beta 
covar[1,3] <- est_both_smoke$coefficients['SNP1']
covar[2,3] <- est_only_mumsmoke$coefficients['SNP1']
covar[3,3] <- est_mumsmoke_allchild$coefficients['SNP1']
covar[4,3] <- est_only_childsmoke$coefficients['SNP1']
covar[5,3] <- est_both_nonsmoke$coefficients['SNP1']
covar[6,3] <- est_mumnonsmoke_allchild$coefficients['SNP1']

#extract SE
covar[1,4] <- coef(summary(est_both_smoke))['SNP1', "Std. Error"]
covar[2,4] <- coef(summary(est_only_mumsmoke))['SNP1', "Std. Error"]
covar[3,4] <- coef(summary(est_mumsmoke_allchild))['SNP1', "Std. Error"]
covar[4,4] <- coef(summary(est_only_childsmoke))['SNP1', "Std. Error"]
covar[5,4] <- coef(summary(est_both_nonsmoke))['SNP1', "Std. Error"]
covar[6,4] <- coef(summary(est_mumnonsmoke_allchild))['SNP1', "Std. Error"]

write.csv(covar, file=paste(Sys.getenv('Myresults'),'mini project3_plot/asthma_ever_never.csv',sep=''))
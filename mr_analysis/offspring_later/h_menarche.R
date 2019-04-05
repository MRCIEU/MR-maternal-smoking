#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#import menarche data
menarche<-read.csv(paste(Sys.getenv('Mydata'),'menarche.csv',sep=''), sep=',')
summary(menarche)
colnames(menarche)<-c("app16729","menarche")
table(menarche$menarche,exclude=NULL)
menarche$menarche[menarche$menarche<0]<-NA

#import age start smoking
smoking_check<-read.csv(paste(Sys.getenv('Mydata'),'maternal_smoking_women_participants.csv',sep=''), sep=',')
head(smoking_check)
colnames(smoking_check)<-c("app16729","live_b_no","bwchild","age_live_b","age_start_former","age_stop","age_start_current","smoking","age_stop_medical")
smoking_check<-smoking_check[c(1,5,7,8)]

#import previous clean dataset
proof<-read.csv(paste(Sys.getenv('Mydata'),'bw_only_correct.csv',sep=''), sep=',')
summary(proof)
proof<-proof[c(2:17)]

#combine new outcome data with my previous dataset
menarche_smoking<-merge(menarche,smoking_check,by="app16729")
menarche_use<-merge(menarche_smoking,proof,by="app16729")
menarche_use <- menarche_use[ which(menarche_use$sex=='F'), ]
summary(menarche_use)

#mean & SD by sex
aggregate(menarche_use,by=list(menarche_use$sex),FUN = mean,na.rm=TRUE)
aggregate(menarche_use,by=list(menarche_use$sex),FUN = sd,na.rm=TRUE)

###################################################################################################
###################################################################################################
#the following analyses use menarche_use database
###################################################################################################
###################################################################################################
#ever smoker vs never smoker in UKBB participants
#in all participants
##subset participants: G0 smoking=No
mumnonsmoke_allchild<-menarche_use[which(menarche_use$mumsmoke==0),]
##subset participants: G0 smoking=Yes
mumsmoke_allchild<-menarche_use[which(menarche_use$mumsmoke==1),]
est_mumnonsmoke_allchild<-lm(mumnonsmoke_allchild$menarche~.,data=mumnonsmoke_allchild[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_mumsmoke_allchild<-lm(mumsmoke_allchild$menarche~.,data=mumsmoke_allchild[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#in non-smoker(G0 & G1)
nonsmoke<-menarche_use[which(menarche_use$mumsmoke==0 & menarche_use$smoking==0),]
est_both_nonsmoke<-lm(nonsmoke$menarche~.,data=nonsmoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=never smoker
smoke_mum<-menarche_use[which(menarche_use$mumsmoke==1 & menarche_use$smoking==0),]
est_only_mumsmoke<-lm(smoke_mum$menarche~.,data=smoke_mum[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=ever smoker
smoke_child<-menarche_use[which(menarche_use$mumsmoke==0 & menarche_use$smoking>=1),]
est_only_childsmoke<-lm(smoke_child$menarche~.,data=smoke_child[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=ever smoker
smoke_both<-menarche_use[which(menarche_use$mumsmoke==1 & menarche_use$smoking>=1),]
est_both_smoke<-lm(smoke_both$menarche~.,data=smoke_both[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#save results in a file for plot
#creat a matrix for results
vars<-c("mumsmoke_ever", "mumsmoke_never", "mumsmoke_all", "mumnot_ever", "mumnot_never", "mumnot_all")
col.names <- c("supp","exposure","menarche","se")
covar <- matrix(, ncol=4, nrow=length(vars))
dimnames(covar) <- list(vars, col.names)

#in the 1st colomn of matrix("supp"), "1" subgroup of "maternal smoking=NO", "2" subgroup of "maternal smoking=YES"
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

write.csv(covar, file=paste(Sys.getenv('Myresults'),'mini project3_plot/menarche_ever_never.csv',sep=''))
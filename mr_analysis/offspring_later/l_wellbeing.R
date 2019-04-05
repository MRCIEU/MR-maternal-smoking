#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#load packages
require(foreign)
require(MASS)
require(Hmisc)
require(reshape2)

#import wellbeing data 
well<-read.csv(paste(Sys.getenv('Mydata'),'wellbeing.csv',sep=''), sep=',')
head(well)
well<-well[c(1,2)]
colnames(well)<-c("app16729","happy")

#code value < 0 as missing
well$happy[well$happy<0]<-NA

#Reverse scores: e.g extremely happy=6 and extremely unhappy=1
well$happy<-7-well$happy

###################################################################################################
#import previous clean dataset
proof2<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''), sep=',')
head(proof2)
proof2<-proof2[c(2:4,6:20)]

#combine new outcome data with my previous dataset
well_use<-merge(proof2,well,by="app16729")
summary(well_use)

#N(%)
table(well_use$happy,well_use$sex)

###################################################################################################
###################################################################################################
#the following analyses use well_use database
###################################################################################################
###################################################################################################
#Happiness
well_use$happy<-as.factor(well_use$happy)
#ever smoker vs never smoker in UKBB participants, given relatively small sample size
#in all participants
##subset participants: G0 smoking=No
mumnonsmoke_allchild<-well_use[which(well_use$mumsmoke==0),]
##subset participants: G0 smoking=YES
mumsmoke_allchild<-well_use[which(well_use$mumsmoke==1),]
##analysis
est_mumnonsmoke_allchild<-polr(mumnonsmoke_allchild$happy~.,Hess=TRUE,data=mumnonsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_mumsmoke_allchild<-polr(mumsmoke_allchild$happy~., Hess=TRUE,data=mumsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#in non-smoker(G0 & G1)
nonsmoke<-well_use[which(well_use$mumsmoke==0 & well_use$smoke==0),]
est_both_nonsmoke<-polr(nonsmoke$happy~., Hess=TRUE,data=nonsmoke[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=never smoker
smoke_mum<-well_use[which(well_use$mumsmoke==1 & well_use$smoke==0),]
est_only_mumsmoke<-polr(smoke_mum$happy~., Hess=TRUE,data=smoke_mum[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=ever smoker
smoke_child<-well_use[which(well_use$mumsmoke==0 & well_use$smoke>=1),]
est_only_childsmoke<-polr(smoke_child$happy~., Hess=TRUE,data=smoke_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])

#G0=smoker, G1=ever smoker
smoke_both<-well_use[which(well_use$mumsmoke==1 & well_use$smoke>=1),]
est_both_smoke<-polr(smoke_both$happy~., Hess=TRUE,data=smoke_both[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#save results in a file for plot
#creat a matrix for results
vars<-c("mumsmoke_ever", "mumsmoke_never", "mumsmoke_all", "mumnot_ever", "mumnot_never", "mumnot_all")
col.names <- c("supp","exposure","happy","happy_se")
covar <- matrix(, ncol=4, nrow=length(vars))
dimnames(covar) <- list(vars, col.names)

#in the 1st colomn of matrix("supp"), "1" subgroup of "G0 smoking=No", "2" subgroup of "G0 smoking=Yes"
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

write.csv(covar, file=paste(Sys.getenv('Myresults'),'mini project3_plot/wellbeing.csv',sep=''))
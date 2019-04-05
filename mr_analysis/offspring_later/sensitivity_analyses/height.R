#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#import height data
height<-read.csv(paste(Sys.getenv('Mydata'),'height_education.csv',sep=''), sep=',')
summary(height)
colnames(height)<-c("app16729","height","ageedu","country","edulevel1","edulevel2","edulevel3","edulevel4","edulevel5","edulevel6","age")
height<-height[c(1,2,11)]

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
height_smoking<-merge(height,smoking_check,by="app16729")
height_use<-merge(height_smoking,proof,by="app16729")
summary(height_use)

###################################################################################################
###################################################################################################
#the following analyses use height_use database
###################################################################################################
###################################################################################################
#ever smoker vs never smoker in UKBB participants, considering age started smoking
#Part 1, define smoker/non-smoker before completing puberty
#non-smoker before completing puberty
no_pub1<-height_use[ which(height_use$smoking==0),]
no_pub2<-height_use[ which(height_use$sex=="F" & height_use$age_start_former > 15),]
no_pub3<-height_use[ which(height_use$sex=="F" & height_use$age_start_current > 15),]
no_pub4<-height_use[ which(height_use$sex=="M" & height_use$age_start_former > 17),]
no_pub5<-height_use[ which(height_use$sex=="M" & height_use$age_start_current > 17),]
no_pub<-rbind(no_pub1,no_pub2,no_pub3,no_pub4,no_pub5)

#smoker before completing puberty
yes_pub1<-height_use[ which(height_use$sex=="F" & height_use$age_start_former < 15 & height_use$age_start_former > 0),]
yes_pub2<-height_use[ which(height_use$sex=="F" & height_use$age_start_current < 15 & height_use$age_start_current > 0),]
yes_pub3<-height_use[ which(height_use$sex=="M" & height_use$age_start_former < 17 & height_use$age_start_former > 0),]
yes_pub4<-height_use[ which(height_use$sex=="M" & height_use$age_start_current < 17 & height_use$age_start_current > 0),]
yes_pub<-rbind(yes_pub1,yes_pub2,yes_pub3,yes_pub4)

#we did not include edge cases in our analyses given the uncertainty 


#Part 2, analyses
#in all participants (same as previous height results)
##subset participants: G0 smoking=NO
mumnonsmoke_allchild<-height_use[which(height_use$mumsmoke==0),]
##subset participants: G1 smoking=YES
mumsmoke_allchild<-height_use[which(height_use$mumsmoke==1),]
##analysis
est_mumnonsmoke_allchild<-lm(mumnonsmoke_allchild$height~.,data=mumnonsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_mumsmoke_allchild<-lm(mumsmoke_allchild$height~.,data=mumsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#in non-smoker(G0 & G1)
nonsmoke<-no_pub[ which(no_pub$mumsmoke==0),]
est_both_nonsmoke<-lm(nonsmoke$height~.,data=nonsmoke[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=never smoker
smoke_mum<-no_pub[ which(no_pub$mumsmoke==1),]
est_only_mumsmoke<-lm(smoke_mum$height~.,data=smoke_mum[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=ever smoker
smoke_child<-yes_pub[ which(yes_pub$mumsmoke==0),]
est_only_childsmoke<-lm(smoke_child$height~.,data=smoke_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=ever smoker
smoke_both<-yes_pub[ which(yes_pub$mumsmoke==1),]
est_both_smoke<-lm(smoke_both$height~.,data=smoke_both[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#Part 3
#save results in a file for plot
#creat a matrix for results
vars<-c("mumsmoke_ever", "mumsmoke_never", "mumsmoke_all", "mumnot_ever", "mumnot_never", "mumnot_all")
col.names <- c("supp","exposure","height","se")
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

write.csv(covar, file=paste(Sys.getenv('Myresults'),'mini project3_plot/height_age.csv',sep=''))
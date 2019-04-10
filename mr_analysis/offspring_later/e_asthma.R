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
#function, extract logOR, se, loglci and loguci
#calculate loglci & loguci manually due to confint is very slow
logisticregression <- function(database,modelname) {
  fit = glm(database[,"asthma_d"] ~.,binomial(link = "logit"), data=database[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
  sumx = summary(fit)
  logor = sumx$coefficients["SNP1","Estimate"]
  se = sumx$coefficients["SNP1","Std. Error"]
  lower = logor-1.96*se
  upper = logor+1.96*se
  write.table(cbind(modelname,logor,se,lower,upper),file=paste(Sys.getenv('Myresults'),'mini project3_plot/asthma_ever_never.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
}
#in all participants
##subset participants: G0 smoking=No
mumnonsmoke_allchild<-asthma_use[which(asthma_use$mumsmoke==0),]
logisticregression(mumnonsmoke_allchild,"mumnonsmoke_childall")
##subset participants: G0 smoking=Yes
mumsmoke_allchild<-asthma_use[which(asthma_use$mumsmoke==1),]
logisticregression(mumsmoke_allchild,"mumsmoke_childall")

#in non-smoker(G0 & G1)
nonsmoke<-asthma_use[which(asthma_use$mumsmoke==0 & asthma_use$smoking==0),]
logisticregression(nonsmoke,"mumnonsmoke_childnever")

#G0=smoker, G1=never smoker
smoke_mum<-asthma_use[which(asthma_use$mumsmoke==1 & asthma_use$smoking==0),]
logisticregression(smoke_mum,"mumsmoke_childnever")

#G0=non-smoker, G1=ever smoker
smoke_child<-asthma_use[which(asthma_use$mumsmoke==0 & asthma_use$smoking>=1),]
logisticregression(smoke_child,"mumnonsmoke_childever")

#G0=smoker, G1=ever smoker
smoke_both<-asthma_use[which(asthma_use$mumsmoke==1 & asthma_use$smoking>=1),]
logisticregression(smoke_both,"mumsmoke_childever")
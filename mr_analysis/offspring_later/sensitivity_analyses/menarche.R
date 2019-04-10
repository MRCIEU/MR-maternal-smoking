#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#import height data
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

###################################################################################################
###################################################################################################
#the following analyses use menarche_use database
###################################################################################################
###################################################################################################
#ever smoker vs never smoker in UKBB participants, considering age started smoking
#Part 1, define smoker/non-smoker based on age at menarche
#non-smoker
no_menarche1<-menarche_use[ which(menarche_use$smoking==0),]
no_menarche2<-menarche_use[ which(menarche_use$age_start_former > menarche_use$menarche),]
no_menarche3<-menarche_use[ which(menarche_use$age_start_current > menarche_use$menarche),]
no_menarche<-rbind(no_menarche1,no_menarche2,no_menarche3)

#smoker before menarche
yes_menarche1<-menarche_use[ which(menarche_use$age_start_former < menarche_use$menarche & menarche_use$age_start_former > 0),]
yes_menarche2<-menarche_use[ which(menarche_use$age_start_current < menarche_use$menarche & menarche_use$age_start_current > 0),]
yes_menarche<-rbind(yes_menarche1,yes_menarche2)

#we did not include edge cases in our analyses given the uncertainty 


#Part 2, analyses
#function, extract beta, se, lci and uci
linearregression <- function(database,modelname) {
  fit = lm(database[,"menarche"] ~., data=database[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
  sumx = summary(fit)
  beta = sumx$coefficients["SNP1","Estimate"]
  se = sumx$coefficients["SNP1","Std. Error"]
  cis = confint(fit, level=0.95)
  lower = cis["SNP1", "2.5 %"]
  upper = cis["SNP1", "97.5 %"]
  write.table(cbind(modelname,beta,se,lower,upper),file=paste(Sys.getenv('Myresults'),'mini project3_plot/menarche_age.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
}
#in all participants (same as previous menarche results)
##subset participants: G0 smoking=NO
mumnonsmoke_allchild<-menarche_use[which(menarche_use$mumsmoke==0),]
linearregression(mumnonsmoke_allchild,"mumnonsmoke_childall")
##subset participants: G0 smoking=YES
mumsmoke_allchild<-menarche_use[which(menarche_use$mumsmoke==1),]
linearregression(mumsmoke_allchild,"mumsmoke_childall")

#in non-smoker(G0 & G1)
nonsmoke<-no_menarche[ which(no_menarche$mumsmoke==0),]
linearregression(nonsmoke,"mumnonsmoke_childnever")

#G0=smoker, G1=never smoker
smoke_mum<-no_menarche[ which(no_menarche$mumsmoke==1),]
linearregression(smoke_mum,"mumsmoke_childnever")

#G0=non-smoker, G1=ever smoker
smoke_child<-yes_menarche[ which(yes_menarche$mumsmoke==0),]
linearregression(smoke_child,"mumnonsmoke_childever")

#G0=smoker, G1=ever smoker
smoke_both<-yes_menarche[ which(yes_menarche$mumsmoke==1),]
linearregression(smoke_both,"mumsmoke_childever")
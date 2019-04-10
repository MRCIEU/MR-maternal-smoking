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
#function, extract beta, se, lci and uci
linearregression <- function(database,modelname) {
  fit = lm(database[,"height"] ~., data=database[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
  sumx = summary(fit)
  beta = sumx$coefficients["SNP1","Estimate"]
  se = sumx$coefficients["SNP1","Std. Error"]
  cis = confint(fit, level=0.95)
  lower = cis["SNP1", "2.5 %"]
  upper = cis["SNP1", "97.5 %"]
  write.table(cbind(modelname,beta,se,lower,upper),file=paste(Sys.getenv('Myresults'),'mini project3_plot/height_age.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
}
#in all participants (same as previous height results)
##subset participants: G0 smoking=NO
mumnonsmoke_allchild<-height_use[which(height_use$mumsmoke==0),]
linearregression(mumnonsmoke_allchild,"mumnonsmoke_childall")
##subset participants: G1 smoking=YES
mumsmoke_allchild<-height_use[which(height_use$mumsmoke==1),]
linearregression(mumsmoke_allchild,"mumsmoke_childall")

#in non-smoker(G0 & G1)
nonsmoke<-no_pub[ which(no_pub$mumsmoke==0),]
linearregression(nonsmoke,"mumnonsmoke_childnever")

#G0=smoker, G1=never smoker
smoke_mum<-no_pub[ which(no_pub$mumsmoke==1),]
linearregression(smoke_mum,"mumsmoke_childnever")

#G0=non-smoker, G1=ever smoker
smoke_child<-yes_pub[ which(yes_pub$mumsmoke==0),]
linearregression(smoke_child,"mumnonsmoke_childever")

#G0=smoker, G1=ever smoker
smoke_both<-yes_pub[ which(yes_pub$mumsmoke==1),]
linearregression(smoke_both,"mumsmoke_childever")
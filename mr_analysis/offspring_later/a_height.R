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

#mean & SD by sex
aggregate(height_use,by=list(height_use$sex),FUN = mean,na.rm=TRUE)
aggregate(height_use,by=list(height_use$sex),FUN = sd,na.rm=TRUE)

###################################################################################################
###################################################################################################
#the following analyses use height_use database
###################################################################################################
###################################################################################################
#ever smoker vs never smoker in UKBB participants
#function, extract beta, se, lci and uci
linearregression <- function(database,modelname) {
  fit = lm(database[,"height"] ~., data=database[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
  sumx = summary(fit)
  beta = sumx$coefficients["SNP1","Estimate"]
  se = sumx$coefficients["SNP1","Std. Error"]
  cis = confint(fit, level=0.95)
  lower = cis["SNP1", "2.5 %"]
  upper = cis["SNP1", "97.5 %"]
  write.table(cbind(modelname,beta,se,lower,upper),file=paste(Sys.getenv('Myresults'),'mini project3_plot/height_ever_never.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
}

#in all participants
##subset participants: G0 smoking=NO
mumnonsmoke_allchild<-height_use[which(height_use$mumsmoke==0),]
linearregression(mumnonsmoke_allchild,"mumnonsmoke_childall")
##subset participants: G0 smoking=YES
mumsmoke_allchild<-height_use[which(height_use$mumsmoke==1),]
linearregression(mumsmoke_allchild,"mumsmoke_childall")

#in non-smoker(G0 & G1)
nonsmoke<-height_use[which(height_use$mumsmoke==0 & height_use$smoking==0),]
linearregression(nonsmoke,"mumnonsmoke_childnever")

#G0=smoker, G1=never smoker
smoke_mum<-height_use[which(height_use$mumsmoke==1 & height_use$smoking==0),]
linearregression(smoke_mum,"mumsmoke_childnever")

#G0=non-smoker, G1=ever smoker
smoke_child<-height_use[which(height_use$mumsmoke==0 & height_use$smoking>=1),]
linearregression(smoke_child,"mumnonsmoke_childever")

#G0=smoker, G1=ever smoker
smoke_both<-height_use[which(height_use$mumsmoke==1 & height_use$smoking>=1),]
linearregression(smoke_both,"mumsmoke_childever")
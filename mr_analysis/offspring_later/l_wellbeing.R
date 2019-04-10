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
#function, extract logOR, se, loglci and loguci
#calculate loglci & loguci manually due to confint is very slow
ordinal <- function(database,modelname) {
  fit = polr(database[,"happy"] ~.,Hess=TRUE, data=database[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
  sumx = summary(fit)
  logor = sumx$coefficients["SNP1","Value"]
  se = sumx$coefficients["SNP1","Std. Error"]
  lower = logor-1.96*se
  upper = logor+1.96*se
  write.table(cbind(modelname,logor,se,lower,upper),file=paste(Sys.getenv('Myresults'),'mini project3_plot/wellbeing.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
}
#in all participants
##subset participants: G0 smoking=No
mumnonsmoke_allchild<-well_use[which(well_use$mumsmoke==0),]
ordinal(mumnonsmoke_allchild,"mumnonsmoke_childall")
##subset participants: G0 smoking=YES
mumsmoke_allchild<-well_use[which(well_use$mumsmoke==1),]
ordinal(mumsmoke_allchild,"mumsmoke_childall")

#in non-smoker(G0 & G1)
nonsmoke<-well_use[which(well_use$mumsmoke==0 & well_use$smoke==0),]
ordinal(nonsmoke,"mumnonsmoke_childnever")

#G0=smoker, G1=never smoker
smoke_mum<-well_use[which(well_use$mumsmoke==1 & well_use$smoke==0),]
ordinal(smoke_mum,"mumsmoke_childnever")

#G0=non-smoker, G1=ever smoker
smoke_child<-well_use[which(well_use$mumsmoke==0 & well_use$smoke>=1),]
ordinal(smoke_child,"mumnonsmoke_childever")

#G0=smoker, G1=ever smoker
smoke_both<-well_use[which(well_use$mumsmoke==1 & well_use$smoke>=1),]
ordinal(smoke_both,"mumsmoke_childever")
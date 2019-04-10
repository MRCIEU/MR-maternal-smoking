#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#import IQ data 
iq<-read.csv(paste(Sys.getenv('Mydata'),'iq.csv',sep=''), sep=',')
head(iq)
colnames(iq)<-c("app16729","iq_t1","iq_t2","iq_t3","iq_online")

#import previous clean dataset
proof2<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''), sep=',')
head(proof2)
proof2<-proof2[c(2:4,6:20)]

#combine new outcome data with my previous dataset
iq_use<-merge(proof2,iq,by="app16729")
summary(iq_use)

###################################################################################################
###################################################################################################
#the following analyses use iq_use database
###################################################################################################
###################################################################################################
#Part 1
#If participants took the VNR test at multiple time points, only the earliest was used (PMID 29326435)
iq_use$iq<-iq_use$iq_online
iq3ix<-which(!is.na(iq_use$iq_t3))
iq_use$iq[iq3ix]<-iq_use$iq_t3[iq3ix]
iq2ix<-which(!is.na(iq_use$iq_t2))
iq_use$iq[iq2ix]<-iq_use$iq_t2[iq2ix]
iq1ix<-which(!is.na(iq_use$iq_t1))
iq_use$iq[iq1ix]<-iq_use$iq_t1[iq1ix]

#mean & SD by sex
summary(iq_use)
aggregate(iq_use,by=list(iq_use$sex),FUN = mean,na.rm=TRUE)
aggregate(iq_use,by=list(iq_use$sex),FUN = sd,na.rm=TRUE)


#Part 2
#note: use baseline & follow-up "Fluid intelligence score"
#note: unweighted sum of the number of correct answers given to the 13 fluid intelligence questions
#function, extract beta, se, lci and uci
linearregression <- function(database,modelname) {
  fit = lm(database[,"iq"] ~., data=database[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
  sumx = summary(fit)
  beta = sumx$coefficients["SNP1","Estimate"]
  se = sumx$coefficients["SNP1","Std. Error"]
  cis = confint(fit, level=0.95)
  lower = cis["SNP1", "2.5 %"]
  upper = cis["SNP1", "97.5 %"]
  write.table(cbind(modelname,beta,se,lower,upper),file=paste(Sys.getenv('Myresults'),'mini project3_plot/iq.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
}
#in all participants
##subset participants: G0 smoking=No
mumnonsmoke_allchild<-iq_use[which(iq_use$mumsmoke==0),]
linearregression(mumnonsmoke_allchild,"mumnonsmoke_childall")
##subset participants: G0 smoking=Yes
mumsmoke_allchild<-iq_use[which(iq_use$mumsmoke==1),]
linearregression(mumsmoke_allchild,"mumsmoke_childall")

#in non-smoker(G0 & G1)
nonsmoke<-iq_use[which(iq_use$mumsmoke==0 & iq_use$smoke==0),]
linearregression(nonsmoke,"mumnonsmoke_childnever")

#G0=smoker, G1=never smoker
smoke_mum<-iq_use[which(iq_use$mumsmoke==1 & iq_use$smoke==0),]
linearregression(smoke_mum,"mumsmoke_childnever")

#G0=non-smoker, G1=former smoker
former_child<-iq_use[which(iq_use$mumsmoke==0 & iq_use$smoke==1),]
linearregression(former_child,"mumnonsmoke_childformer")

#G0=non-smoker, G1=current smoker
current_child<-iq_use[which(iq_use$mumsmoke==0 & iq_use$smoke==2),]
linearregression(current_child,"mumnonsmoke_childcurrent")

#G0=smoker, G1=former smoker
smoke_both_former<-iq_use[which(iq_use$mumsmoke==1 & iq_use$smoke==1),]
linearregression(smoke_both_former,"mumsmoke_childformer")

#G0=smoker, G1=current smoker
smoke_both_current<-iq_use[which(iq_use$mumsmoke==1 & iq_use$smoke==2),]
linearregression(smoke_both_current,"mumsmoke_childcurrent")
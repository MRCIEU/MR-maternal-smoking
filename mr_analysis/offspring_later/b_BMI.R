#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#import BMI data
outcome<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi_lung.csv',sep=''), sep=',')
summary(outcome)
outcome<-outcome[c(1,4,5)]
colnames(outcome)<-c("app16729","smoke","bmi")

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
outcome_proof<-merge(outcome,proof,by="app16729")
proof2<-merge(age,outcome_proof,by="app16729")
summary(proof2)

write.csv(proof2,file=paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''))
###################################################################################################
###################################################################################################
#the following analyses use proof2 database
###################################################################################################
###################################################################################################
#read in database
proof2<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''), sep=',')
summary(proof2)

#mean & SD by sex
aggregate(proof2,by=list(proof2$sex),FUN = mean,na.rm=TRUE)
aggregate(proof2,by=list(proof2$sex),FUN = sd,na.rm=TRUE)
table(proof2$smoke,proof2$sex)

#function, extract beta, se, lci and uci
linearregression <- function(database,modelname) {
  fit = lm(database[,"bmi"] ~., data=database[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
  sumx = summary(fit)
  beta = sumx$coefficients["SNP1","Estimate"]
  se = sumx$coefficients["SNP1","Std. Error"]
  cis = confint(fit, level=0.95)
  lower = cis["SNP1", "2.5 %"]
  upper = cis["SNP1", "97.5 %"]
  write.table(cbind(modelname,beta,se,lower,upper),file=paste(Sys.getenv('Myresults'),'mini project3_plot/bmi.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
}

#in all participants
##subset participants: G0 smoking=NO
mumnonsmoke_allchild<-proof2[which(proof2$mumsmoke==0),]
linearregression(mumnonsmoke_allchild,"mumnonsmoke_childall")
##subset participants: G0 smoking=YES
mumsmoke_allchild<-proof2[which(proof2$mumsmoke==1),]
linearregression(mumsmoke_allchild,"mumsmoke_childall")

#in non-smoker(G0 & G1)
nonsmoke<-proof2[which(proof2$mumsmoke=="0" & proof2$smoke==0),]
linearregression(nonsmoke,"mumnonsmoke_childnever")

#G0=smoker, G1=never smoker
smoke_mum<-proof2[which(proof2$mumsmoke=="1" & proof2$smoke==0),]
linearregression(smoke_mum,"mumsmoke_childnever")

#G0=non-smoker, G1=former smoker
former_child<-proof2[which(proof2$mumsmoke=="0" & proof2$smoke==1),]
linearregression(former_child,"mumnonsmoke_childformer")

#G0=non-smoker, G1=current smoker
current_child<-proof2[which(proof2$mumsmoke=="0" & proof2$smoke==2),]
linearregression(current_child,"mumnonsmoke_childcurrent")

#G0=smoker, G1=former smoker
smoke_both_former<-proof2[which(proof2$mumsmoke=="1" & proof2$smoke==1),]
linearregression(smoke_both_former,"mumsmoke_childformer")

#G0=smoker, G1=current smoker
smoke_both_current<-proof2[which(proof2$mumsmoke=="1" & proof2$smoke==2),]
linearregression(smoke_both_current,"mumsmoke_childcurrent")
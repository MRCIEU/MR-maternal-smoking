#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())


#import depression data (questionnaire)
depression_ques<-read.csv(paste(Sys.getenv('Mydata'),'depression.csv',sep=''), sep=',')

#import depression data (ICD-10)
depression_icd_main<-read.csv(paste(Sys.getenv('Mydata'),'icd10_main_depression.csv',sep=''), sep=',')
depression_icd_second<-read.csv(paste(Sys.getenv('Mydata'),'icd10_second_depression.csv',sep=''), sep=',')

#combine depression data to derive depression_diag
depression_icd<-merge(depression_icd_main,depression_icd_second,by="eid")
depression<-merge(depression_ques,depression_icd,by="eid")
colnames(depression)<-c("app16729","gp1","gp2","gp3","psych1","psych2","psych3","depression_diag_main","depression_diag_second")

#code value < 0 as missing
depression$gp1[depression$gp1<0]<-NA
depression$gp2[depression$gp2<0]<-NA
depression$gp3[depression$gp3<0]<-NA
depression$psych1[depression$psych1<0]<-NA
depression$psych2[depression$psych2<0]<-NA
depression$psych3[depression$psych3<0]<-NA

#code 0 means no depression, 1 means any depression in any question/assessment/ICD-code
depression$diag_sum<-rowSums(depression[,c(2:9)],na.rm=TRUE)
depression$diag_sum[is.na(depression$gp1) & is.na(depression$gp2) & is.na(depression$gp3) & is.na(depression$psych1) & is.na(depression$psych2) & is.na(depression$psych3) & is.na(depression$depression_diag_main) & is.na(depression$depression_diag_second)]<-NA
depression$diag_sum[depression$diag_sum>0]<-1

depression<-depression[c("app16729","diag_sum")]

###################################################################################################
#import previous clean dataset
proof2<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''), sep=',')
head(proof2)
proof2<-proof2[c(2:4,6:20)]

#combine new outcome data with my previous dataset
dep_use<-merge(proof2,depression,by="app16729")
summary(dep_use)

#N(%) by sex
table(dep_use$diag_sum,dep_use$sex)

###################################################################################################
###################################################################################################
#the following analyses use dep_use database
###################################################################################################
###################################################################################################
#function, extract logOR, se, loglci and loguci
#calculate loglci & loguci manually due to confint is very slow
logisticregression <- function(database,modelname) {
  fit = glm(database[,"diag_sum"] ~.,binomial(link = "logit"), data=database[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
  sumx = summary(fit)
  logor = sumx$coefficients["SNP1","Estimate"]
  se = sumx$coefficients["SNP1","Std. Error"]
  lower = logor-1.96*se
  upper = logor+1.96*se
  write.table(cbind(modelname,logor,se,lower,upper),file=paste(Sys.getenv('Myresults'),'mini project3_plot/depression.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
}
#in all participants
##subset participants: G0 smoking=No
mumnonsmoke_allchild<-dep_use[which(dep_use$mumsmoke==0),]
logisticregression(mumnonsmoke_allchild,"mumnonsmoke_childall")
##subset participants: G0 smoking=Yes
mumsmoke_allchild<-dep_use[which(dep_use$mumsmoke==1),]
logisticregression(mumsmoke_allchild,"mumsmoke_childall")

#in non-smoker(G0 & G1)
nonsmoke<-dep_use[which(dep_use$mumsmoke==0 & dep_use$smoke==0),]
logisticregression(nonsmoke,"mumnonsmoke_childnever")

#G0=smoker, G1=never smoker
smoke_mum<-dep_use[which(dep_use$mumsmoke==1 & dep_use$smoke==0),]
logisticregression(smoke_mum,"mumsmoke_childnever")

#G0=non-smoker, G1=former smoker
former_child<-dep_use[which(dep_use$mumsmoke==0 & dep_use$smoke==1),]
logisticregression(former_child,"mumnonsmoke_childformer")

#G0=non-smoker, G1=current smoker
current_child<-dep_use[which(dep_use$mumsmoke==0 & dep_use$smoke==2),]
logisticregression(current_child,"mumnonsmoke_childcurrent")

#G0=smoker, G1=former smoker
smoke_both_former<-dep_use[which(dep_use$mumsmoke==1 & dep_use$smoke==1),]
logisticregression(smoke_both_former,"mumsmoke_childformer")

#G0=smoker, G1=current smoker
smoke_both_current<-dep_use[which(dep_use$mumsmoke==1 & dep_use$smoke==2),]
logisticregression(smoke_both_current,"mumsmoke_childcurrent")
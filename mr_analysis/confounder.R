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
require(metafor)

#import previous clean datasets
proof2<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''), sep=',')
head(proof2)
proof2<-proof2[c(2:4,6:20)]

grandmother_bw<-read.csv(paste(Sys.getenv('Mydata'),'maternal_smoking_women_participants_clean.csv',sep=''), sep=',')
head(grandmother_bw)
grandmother_bw<-grandmother_bw[c(2,27)]

smoking_check<-read.csv(paste(Sys.getenv('Mydata'),'maternal_smoking_women_participants.csv',sep=''), sep=',')
head(smoking_check)
colnames(smoking_check)<-c("app16729","live_b_no","bwchild","age_live_b","age_start_former","age_stop","age_start_current","smoking","age_stop_medical")
smoking_check<-smoking_check[c(1,4)]

#import confounder dataset
confounder<-read.csv(paste(Sys.getenv('Mydata'),'confounder.csv',sep=''), sep=',')
head(confounder)
colnames(confounder)<-c("app16729","deprivation","income")
#code income < 0 as missing
confounder$income[confounder$income<0]<-NA
#rs16969968~education is in https://github.com/MRCIEU/MR-maternal-smoking/blob/master/mr_analysis/smoke8_education.R

#combine new outcome data with my previous datasets
confounder_use<-merge(confounder,smoking_check,by="app16729")
confounder_use<-merge(proof2,confounder_use,by="app16729")
confounder_use<-merge(confounder_use,grandmother_bw,by="app16729",all=TRUE)
summary(confounder_use)
confounder_use$smoke[confounder_use$smoke<0]<-NA
confounder_use$income<-as.factor(confounder_use$income)


#mean & SD by sex
confounder_use$age_live_b[confounder_use$age_live_b<0]<-NA
summary(confounder_use)
aggregate(confounder_use,by=list(confounder_use$sex),FUN = mean,na.rm=TRUE)
aggregate(confounder_use,by=list(confounder_use$sex),FUN = sd,na.rm=TRUE)

#N(%)
table(confounder_use$income,confounder_use$sex)
table(confounder_use$mumsmoke,confounder_use$sex)
table(confounder_use$smoke,confounder_use$sex)
table(confounder_use$smoke_preg,confounder_use$sex)

#SNP~smoking status
summary(glm(confounder_use$mumsmoke~.,data=confounder_use[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit")))
confounder_use$ever[confounder_use$smoke>0]<-1
confounder_use$ever[confounder_use$smoke==0]<-0
summary(glm(confounder_use$ever~.,data=confounder_use[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit")))
summary(glm(confounder_use$smoke_preg~.,data=confounder_use[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit")))


#################################################################################################################################
#################################################################################################################################
#SNP~confounder, by smoking status
#G1=all
all1<-confounder_use[which(confounder_use$mumsmoke==1),]
all2<-confounder_use[which(confounder_use$mumsmoke==0),]
#G1=current
current<-confounder_use[which(confounder_use$smoke==2),]
current1<-confounder_use[which(confounder_use$smoke==2 & confounder_use$mumsmoke==1),]
current2<-confounder_use[which(confounder_use$smoke==2 & confounder_use$mumsmoke==0),]
#G1=former
former<-confounder_use[which(confounder_use$smoke==1),]
former1<-confounder_use[which(confounder_use$smoke==1 & confounder_use$mumsmoke==1),]
former2<-confounder_use[which(confounder_use$smoke==1 & confounder_use$mumsmoke==0),]
#G1=ever
ever<-confounder_use[which(confounder_use$ever==1),]
ever1<-confounder_use[which(confounder_use$ever==1 & confounder_use$mumsmoke==1),]
ever2<-confounder_use[which(confounder_use$ever==1 & confounder_use$mumsmoke==0),]
#G1=never
never<-confounder_use[which(confounder_use$ever==0),]
never1<-confounder_use[which(confounder_use$ever==0 & confounder_use$mumsmoke==1),]
never2<-confounder_use[which(confounder_use$ever==0 & confounder_use$mumsmoke==0),]
#G1=women
women<-confounder_use[which(confounder_use$sex=='F'),]
women1<-confounder_use[which(confounder_use$sex=='F'& confounder_use$mumsmoke==1),]
women2<-confounder_use[which(confounder_use$sex=='F'& confounder_use$mumsmoke==0),]
#G1=smoker in pregnancy
smoke<-confounder_use[which(confounder_use$smoke_preg==1),]
smoke1<-confounder_use[which(confounder_use$smoke_preg==1 & confounder_use$mumsmoke==1),]
smoke2<-confounder_use[which(confounder_use$smoke_preg==1 & confounder_use$mumsmoke==0),]
#G1=non-smoker in pregnancy
nonsmoke<-confounder_use[which(confounder_use$smoke_preg==0),]
nonsmoke1<-confounder_use[which(confounder_use$smoke_preg==0 & confounder_use$mumsmoke==1),]
nonsmoke2<-confounder_use[which(confounder_use$smoke_preg==0 & confounder_use$mumsmoke==0),]
#################################################################################################################################
#G0=yes
#age
est_age_all<-lm(all1$age~.,all1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_current<-lm(current1$age~.,data=current1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_former<-lm(former1$age~.,data=former1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_ever<-lm(ever1$age~.,data=ever1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_never<-lm(never1$age~.,data=never1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_women<-lm(women1$age~.,data=women1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_smoke<-lm(smoke1$age~.,data=smoke1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_nonsmoke<-lm(nonsmoke1$age~.,data=nonsmoke1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#age at first live birth
est_ageflb_women<-lm(women1$age_live_b~.,data=women1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_ageflb_smoke<-lm(smoke1$age_live_b~.,data=smoke1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_ageflb_nonsmoke<-lm(nonsmoke1$age_live_b~.,data=nonsmoke1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#depriviation index
est_dep_all<-lm(all1$deprivation~.,data=all1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_current<-lm(current1$deprivation~.,data=current1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_former<-lm(former1$deprivation~.,data=former1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_ever<-lm(ever1$deprivation~.,data=ever1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_never<-lm(never1$deprivation~.,data=never1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_women<-lm(women1$deprivation~.,data=women1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_smoke<-lm(smoke1$deprivation~.,data=smoke1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_nonsmoke<-lm(nonsmoke1$deprivation~.,data=nonsmoke1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#household income
est_inc_all<-polr(all1$income~.,data=all1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_current<-polr(current1$income~.,data=current1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_former<-polr(former1$income~.,data=former1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_ever<-polr(ever1$income~.,data=ever1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_never<-polr(never1$income~.,data=never1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_women<-polr(women1$income~.,data=women1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_smoke<-polr(smoke1$income~.,data=smoke1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_nonsmoke<-polr(nonsmoke1$income~.,data=nonsmoke1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)


#sex
est_sex_all<-glm(all1$sex~.,data=all1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit"))
est_sex_current<-glm(current1$sex~.,data=current1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit"))
est_sex_former<-glm(former1$sex~.,data=former1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit"))
est_sex_ever<-glm(ever1$sex~.,data=ever1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit"))
est_sex_never<-glm(never1$sex~.,data=never1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit"))

#creat a matrix for results
vars<-c("age_all", "age_current","age_former","age_ever","age_never","age_female","age_smoke","age_nonsmoke","age_flb_female","age_flb_smoke","age_flb_nonsmoke","sex_all","sex_current","sex_former","sex_ever","sex_never","index_all","index_current","index_former","index_ever","index_never","index_female","index_smoke","index_nonsmoke","income_all","income_current","income_former","income_ever","income_never","income_female","income_smoke","income_nonsmoke")
col.names <- c("beta","lci","uci")
covar <- matrix(, ncol=3, nrow=length(vars))
dimnames(covar) <- list(vars, col.names)
#extract "beta"
covar[1,1] <- est_age_all$coefficients['SNP1']
covar[2,1] <- est_age_current$coefficients['SNP1']
covar[3,1] <- est_age_former$coefficients['SNP1']
covar[4,1] <- est_age_ever$coefficients['SNP1']
covar[5,1] <- est_age_never$coefficients['SNP1']
covar[6,1] <- est_age_women$coefficients['SNP1']
covar[7,1] <- est_age_smoke$coefficients['SNP1']
covar[8,1] <- est_age_nonsmoke$coefficients['SNP1']
covar[9,1] <- est_ageflb_women$coefficients['SNP1']
covar[10,1] <- est_ageflb_smoke$coefficients['SNP1']
covar[11,1] <- est_ageflb_nonsmoke$coefficients['SNP1']
covar[12,1] <- exp(est_sex_all$coefficients['SNP1'])
covar[13,1] <- exp(est_sex_current$coefficients['SNP1'])
covar[14,1] <- exp(est_sex_former$coefficients['SNP1'])
covar[15,1] <- exp(est_sex_ever$coefficients['SNP1'])
covar[16,1] <- exp(est_sex_never$coefficients['SNP1'])
covar[17,1] <- est_dep_all$coefficients['SNP1']
covar[18,1] <- est_dep_current$coefficients['SNP1']
covar[19,1] <- est_dep_former$coefficients['SNP1']
covar[20,1] <- est_dep_ever$coefficients['SNP1']
covar[21,1] <- est_dep_never$coefficients['SNP1']
covar[22,1] <- est_dep_women$coefficients['SNP1']
covar[23,1] <- est_dep_smoke$coefficients['SNP1']
covar[24,1] <- est_dep_nonsmoke$coefficients['SNP1']
covar[25,1] <- exp(est_inc_all$coefficients['SNP1'])
covar[26,1] <- exp(est_inc_current$coefficients['SNP1'])
covar[27,1] <- exp(est_inc_former$coefficients['SNP1'])
covar[28,1] <- exp(est_inc_ever$coefficients['SNP1'])
covar[29,1] <- exp(est_inc_never$coefficients['SNP1'])
covar[30,1] <- exp(est_inc_women$coefficients['SNP1'])
covar[31,1] <- exp(est_inc_smoke$coefficients['SNP1'])
covar[32,1] <- exp(est_inc_nonsmoke$coefficients['SNP1'])
#extract "lci"
covar[1,2] <- est_age_all$coefficients['SNP1']-1.96*coef(summary(est_age_all))['SNP1', "Std. Error"]
covar[2,2] <- est_age_current$coefficients['SNP1']-1.96*coef(summary(est_age_current))['SNP1', "Std. Error"]
covar[3,2] <- est_age_former$coefficients['SNP1']-1.96*coef(summary(est_age_former))['SNP1', "Std. Error"]
covar[4,2] <- est_age_ever$coefficients['SNP1']-1.96*coef(summary(est_age_ever))['SNP1', "Std. Error"]
covar[5,2] <- est_age_never$coefficients['SNP1']-1.96*coef(summary(est_age_never))['SNP1', "Std. Error"]
covar[6,2] <- est_age_women$coefficients['SNP1']-1.96*coef(summary(est_age_women))['SNP1', "Std. Error"]
covar[7,2] <- est_age_smoke$coefficients['SNP1']-1.96*coef(summary(est_age_smoke))['SNP1', "Std. Error"]
covar[8,2] <- est_age_nonsmoke$coefficients['SNP1']-1.96*coef(summary(est_age_nonsmoke))['SNP1', "Std. Error"]
covar[9,2] <- est_ageflb_women$coefficients['SNP1']-1.96*coef(summary(est_ageflb_women))['SNP1', "Std. Error"]
covar[10,2] <- est_ageflb_smoke$coefficients['SNP1']-1.96*coef(summary(est_ageflb_smoke))['SNP1', "Std. Error"]
covar[11,2] <- est_ageflb_nonsmoke$coefficients['SNP1']-1.96*coef(summary(est_ageflb_nonsmoke))['SNP1', "Std. Error"]
covar[12,2] <- exp(est_sex_all$coefficients['SNP1']-1.96*coef(summary(est_sex_all))['SNP1', "Std. Error"])
covar[13,2] <- exp(est_sex_current$coefficients['SNP1']-1.96*coef(summary(est_sex_current))['SNP1', "Std. Error"])
covar[14,2] <- exp(est_sex_former$coefficients['SNP1']-1.96*coef(summary(est_sex_former))['SNP1', "Std. Error"])
covar[15,2] <- exp(est_sex_ever$coefficients['SNP1']-1.96*coef(summary(est_sex_ever))['SNP1', "Std. Error"])
covar[16,2] <- exp(est_sex_never$coefficients['SNP1']-1.96*coef(summary(est_sex_never))['SNP1', "Std. Error"])
covar[17,2] <- est_dep_all$coefficients['SNP1']-1.96*coef(summary(est_dep_all))['SNP1', "Std. Error"]
covar[18,2] <- est_dep_current$coefficients['SNP1']-1.96*coef(summary(est_dep_current))['SNP1', "Std. Error"]
covar[19,2] <- est_dep_former$coefficients['SNP1']-1.96*coef(summary(est_dep_former))['SNP1', "Std. Error"]
covar[20,2] <- est_dep_ever$coefficients['SNP1']-1.96*coef(summary(est_dep_ever))['SNP1', "Std. Error"]
covar[21,2] <- est_dep_never$coefficients['SNP1']-1.96*coef(summary(est_dep_never))['SNP1', "Std. Error"]
covar[22,2] <- est_dep_women$coefficients['SNP1']-1.96*coef(summary(est_dep_women))['SNP1', "Std. Error"]
covar[23,2] <- est_dep_smoke$coefficients['SNP1']-1.96*coef(summary(est_dep_smoke))['SNP1', "Std. Error"]
covar[24,2] <- est_dep_nonsmoke$coefficients['SNP1']-1.96*coef(summary(est_dep_nonsmoke))['SNP1', "Std. Error"]
covar[25,2] <- exp(est_inc_all$coefficients['SNP1']-1.96*coef(summary(est_inc_all))['SNP1', "Std. Error"])
covar[26,2] <- exp(est_inc_current$coefficients['SNP1']-1.96*coef(summary(est_inc_current))['SNP1', "Std. Error"])
covar[27,2] <- exp(est_inc_former$coefficients['SNP1']-1.96*coef(summary(est_inc_former))['SNP1', "Std. Error"])
covar[28,2] <- exp(est_inc_ever$coefficients['SNP1']-1.96*coef(summary(est_inc_ever))['SNP1', "Std. Error"])
covar[29,2] <- exp(est_inc_never$coefficients['SNP1']-1.96*coef(summary(est_inc_never))['SNP1', "Std. Error"])
covar[30,2] <- exp(est_inc_women$coefficients['SNP1']-1.96*coef(summary(est_inc_women))['SNP1', "Std. Error"])
covar[31,2] <- exp(est_inc_smoke$coefficients['SNP1']-1.96*coef(summary(est_inc_smoke))['SNP1', "Std. Error"])
covar[32,2] <- exp(est_inc_nonsmoke$coefficients['SNP1']-1.96*coef(summary(est_inc_nonsmoke))['SNP1', "Std. Error"])
#extract "uci"
covar[1,3] <- est_age_all$coefficients['SNP1']+1.96*coef(summary(est_age_all))['SNP1', "Std. Error"]
covar[2,3] <- est_age_current$coefficients['SNP1']+1.96*coef(summary(est_age_current))['SNP1', "Std. Error"]
covar[3,3] <- est_age_former$coefficients['SNP1']+1.96*coef(summary(est_age_former))['SNP1', "Std. Error"]
covar[4,3] <- est_age_ever$coefficients['SNP1']+1.96*coef(summary(est_age_ever))['SNP1', "Std. Error"]
covar[5,3] <- est_age_never$coefficients['SNP1']+1.96*coef(summary(est_age_never))['SNP1', "Std. Error"]
covar[6,3] <- est_age_women$coefficients['SNP1']+1.96*coef(summary(est_age_women))['SNP1', "Std. Error"]
covar[7,3] <- est_age_smoke$coefficients['SNP1']+1.96*coef(summary(est_age_smoke))['SNP1', "Std. Error"]
covar[8,3] <- est_age_nonsmoke$coefficients['SNP1']+1.96*coef(summary(est_age_nonsmoke))['SNP1', "Std. Error"]
covar[9,3] <- est_ageflb_women$coefficients['SNP1']+1.96*coef(summary(est_ageflb_women))['SNP1', "Std. Error"]
covar[10,3] <- est_ageflb_smoke$coefficients['SNP1']+1.96*coef(summary(est_ageflb_smoke))['SNP1', "Std. Error"]
covar[11,3] <- est_ageflb_nonsmoke$coefficients['SNP1']+1.96*coef(summary(est_ageflb_nonsmoke))['SNP1', "Std. Error"]
covar[12,3] <- exp(est_sex_all$coefficients['SNP1']+1.96*coef(summary(est_sex_all))['SNP1', "Std. Error"])
covar[13,3] <- exp(est_sex_current$coefficients['SNP1']+1.96*coef(summary(est_sex_current))['SNP1', "Std. Error"])
covar[14,3] <- exp(est_sex_former$coefficients['SNP1']+1.96*coef(summary(est_sex_former))['SNP1', "Std. Error"])
covar[15,3] <- exp(est_sex_ever$coefficients['SNP1']+1.96*coef(summary(est_sex_ever))['SNP1', "Std. Error"])
covar[16,3] <- exp(est_sex_never$coefficients['SNP1']+1.96*coef(summary(est_sex_never))['SNP1', "Std. Error"])
covar[17,3] <- est_dep_all$coefficients['SNP1']+1.96*coef(summary(est_dep_all))['SNP1', "Std. Error"]
covar[18,3] <- est_dep_current$coefficients['SNP1']+1.96*coef(summary(est_dep_current))['SNP1', "Std. Error"]
covar[19,3] <- est_dep_former$coefficients['SNP1']+1.96*coef(summary(est_dep_former))['SNP1', "Std. Error"]
covar[20,3] <- est_dep_ever$coefficients['SNP1']+1.96*coef(summary(est_dep_ever))['SNP1', "Std. Error"]
covar[21,3] <- est_dep_never$coefficients['SNP1']+1.96*coef(summary(est_dep_never))['SNP1', "Std. Error"]
covar[22,3] <- est_dep_women$coefficients['SNP1']+1.96*coef(summary(est_dep_women))['SNP1', "Std. Error"]
covar[23,3] <- est_dep_smoke$coefficients['SNP1']+1.96*coef(summary(est_dep_smoke))['SNP1', "Std. Error"]
covar[24,3] <- est_dep_nonsmoke$coefficients['SNP1']+1.96*coef(summary(est_dep_nonsmoke))['SNP1', "Std. Error"]
covar[25,3] <- exp(est_inc_all$coefficients['SNP1']+1.96*coef(summary(est_inc_all))['SNP1', "Std. Error"])
covar[26,3] <- exp(est_inc_current$coefficients['SNP1']+1.96*coef(summary(est_inc_current))['SNP1', "Std. Error"])
covar[27,3] <- exp(est_inc_former$coefficients['SNP1']+1.96*coef(summary(est_inc_former))['SNP1', "Std. Error"])
covar[28,3] <- exp(est_inc_ever$coefficients['SNP1']+1.96*coef(summary(est_inc_ever))['SNP1', "Std. Error"])
covar[29,3] <- exp(est_inc_never$coefficients['SNP1']+1.96*coef(summary(est_inc_never))['SNP1', "Std. Error"])
covar[30,3] <- exp(est_inc_women$coefficients['SNP1']+1.96*coef(summary(est_inc_women))['SNP1', "Std. Error"])
covar[31,3] <- exp(est_inc_smoke$coefficients['SNP1']+1.96*coef(summary(est_inc_smoke))['SNP1', "Std. Error"])
covar[32,3] <- exp(est_inc_nonsmoke$coefficients['SNP1']+1.96*coef(summary(est_inc_nonsmoke))['SNP1', "Std. Error"])

covar <-round(covar,3)
write.csv(covar,file=paste(Sys.getenv('Myresults'),'results/confounder_g0yes.csv',sep=''))
#################################################################################################################################
#G0=no
#age
est_age_all<-lm(all2$age~.,all2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_current<-lm(current2$age~.,data=current2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_former<-lm(former2$age~.,data=former2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_ever<-lm(ever2$age~.,data=ever2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_never<-lm(never2$age~.,data=never2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_women<-lm(women2$age~.,data=women2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_smoke<-lm(smoke2$age~.,data=smoke2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_nonsmoke<-lm(nonsmoke2$age~.,data=nonsmoke2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#age at first live birth
est_ageflb_women<-lm(women2$age_live_b~.,data=women2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_ageflb_smoke<-lm(smoke2$age_live_b~.,data=smoke2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_ageflb_nonsmoke<-lm(nonsmoke2$age_live_b~.,data=nonsmoke2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#depriviation index
est_dep_all<-lm(all2$deprivation~.,data=all2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_current<-lm(current2$deprivation~.,data=current2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_former<-lm(former2$deprivation~.,data=former2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_ever<-lm(ever2$deprivation~.,data=ever2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_never<-lm(never2$deprivation~.,data=never2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_women<-lm(women2$deprivation~.,data=women2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_smoke<-lm(smoke2$deprivation~.,data=smoke2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_nonsmoke<-lm(nonsmoke2$deprivation~.,data=nonsmoke2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#household income
est_inc_all<-polr(all2$income~.,data=all2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_current<-polr(current2$income~.,data=current2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_former<-polr(former2$income~.,data=former2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_ever<-polr(ever2$income~.,data=ever2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_never<-polr(never2$income~.,data=never2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_women<-polr(women2$income~.,data=women2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_smoke<-polr(smoke2$income~.,data=smoke2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_nonsmoke<-polr(nonsmoke2$income~.,data=nonsmoke2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)


#sex
est_sex_all<-glm(all2$sex~.,data=all2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit"))
est_sex_current<-glm(current2$sex~.,data=current2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit"))
est_sex_former<-glm(former2$sex~.,data=former2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit"))
est_sex_ever<-glm(ever2$sex~.,data=ever2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit"))
est_sex_never<-glm(never2$sex~.,data=never2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit"))


#extract "beta"
covar[1,1] <- est_age_all$coefficients['SNP1']
covar[2,1] <- est_age_current$coefficients['SNP1']
covar[3,1] <- est_age_former$coefficients['SNP1']
covar[4,1] <- est_age_ever$coefficients['SNP1']
covar[5,1] <- est_age_never$coefficients['SNP1']
covar[6,1] <- est_age_women$coefficients['SNP1']
covar[7,1] <- est_age_smoke$coefficients['SNP1']
covar[8,1] <- est_age_nonsmoke$coefficients['SNP1']
covar[9,1] <- est_ageflb_women$coefficients['SNP1']
covar[10,1] <- est_ageflb_smoke$coefficients['SNP1']
covar[11,1] <- est_ageflb_nonsmoke$coefficients['SNP1']
covar[12,1] <- exp(est_sex_all$coefficients['SNP1'])
covar[13,1] <- exp(est_sex_current$coefficients['SNP1'])
covar[14,1] <- exp(est_sex_former$coefficients['SNP1'])
covar[15,1] <- exp(est_sex_ever$coefficients['SNP1'])
covar[16,1] <- exp(est_sex_never$coefficients['SNP1'])
covar[17,1] <- est_dep_all$coefficients['SNP1']
covar[18,1] <- est_dep_current$coefficients['SNP1']
covar[19,1] <- est_dep_former$coefficients['SNP1']
covar[20,1] <- est_dep_ever$coefficients['SNP1']
covar[21,1] <- est_dep_never$coefficients['SNP1']
covar[22,1] <- est_dep_women$coefficients['SNP1']
covar[23,1] <- est_dep_smoke$coefficients['SNP1']
covar[24,1] <- est_dep_nonsmoke$coefficients['SNP1']
covar[25,1] <- exp(est_inc_all$coefficients['SNP1'])
covar[26,1] <- exp(est_inc_current$coefficients['SNP1'])
covar[27,1] <- exp(est_inc_former$coefficients['SNP1'])
covar[28,1] <- exp(est_inc_ever$coefficients['SNP1'])
covar[29,1] <- exp(est_inc_never$coefficients['SNP1'])
covar[30,1] <- exp(est_inc_women$coefficients['SNP1'])
covar[31,1] <- exp(est_inc_smoke$coefficients['SNP1'])
covar[32,1] <- exp(est_inc_nonsmoke$coefficients['SNP1'])
#extract "lci"
covar[1,2] <- est_age_all$coefficients['SNP1']-1.96*coef(summary(est_age_all))['SNP1', "Std. Error"]
covar[2,2] <- est_age_current$coefficients['SNP1']-1.96*coef(summary(est_age_current))['SNP1', "Std. Error"]
covar[3,2] <- est_age_former$coefficients['SNP1']-1.96*coef(summary(est_age_former))['SNP1', "Std. Error"]
covar[4,2] <- est_age_ever$coefficients['SNP1']-1.96*coef(summary(est_age_ever))['SNP1', "Std. Error"]
covar[5,2] <- est_age_never$coefficients['SNP1']-1.96*coef(summary(est_age_never))['SNP1', "Std. Error"]
covar[6,2] <- est_age_women$coefficients['SNP1']-1.96*coef(summary(est_age_women))['SNP1', "Std. Error"]
covar[7,2] <- est_age_smoke$coefficients['SNP1']-1.96*coef(summary(est_age_smoke))['SNP1', "Std. Error"]
covar[8,2] <- est_age_nonsmoke$coefficients['SNP1']-1.96*coef(summary(est_age_nonsmoke))['SNP1', "Std. Error"]
covar[9,2] <- est_ageflb_women$coefficients['SNP1']-1.96*coef(summary(est_ageflb_women))['SNP1', "Std. Error"]
covar[10,2] <- est_ageflb_smoke$coefficients['SNP1']-1.96*coef(summary(est_ageflb_smoke))['SNP1', "Std. Error"]
covar[11,2] <- est_ageflb_nonsmoke$coefficients['SNP1']-1.96*coef(summary(est_ageflb_nonsmoke))['SNP1', "Std. Error"]
covar[12,2] <- exp(est_sex_all$coefficients['SNP1']-1.96*coef(summary(est_sex_all))['SNP1', "Std. Error"])
covar[13,2] <- exp(est_sex_current$coefficients['SNP1']-1.96*coef(summary(est_sex_current))['SNP1', "Std. Error"])
covar[14,2] <- exp(est_sex_former$coefficients['SNP1']-1.96*coef(summary(est_sex_former))['SNP1', "Std. Error"])
covar[15,2] <- exp(est_sex_ever$coefficients['SNP1']-1.96*coef(summary(est_sex_ever))['SNP1', "Std. Error"])
covar[16,2] <- exp(est_sex_never$coefficients['SNP1']-1.96*coef(summary(est_sex_never))['SNP1', "Std. Error"])
covar[17,2] <- est_dep_all$coefficients['SNP1']-1.96*coef(summary(est_dep_all))['SNP1', "Std. Error"]
covar[18,2] <- est_dep_current$coefficients['SNP1']-1.96*coef(summary(est_dep_current))['SNP1', "Std. Error"]
covar[19,2] <- est_dep_former$coefficients['SNP1']-1.96*coef(summary(est_dep_former))['SNP1', "Std. Error"]
covar[20,2] <- est_dep_ever$coefficients['SNP1']-1.96*coef(summary(est_dep_ever))['SNP1', "Std. Error"]
covar[21,2] <- est_dep_never$coefficients['SNP1']-1.96*coef(summary(est_dep_never))['SNP1', "Std. Error"]
covar[22,2] <- est_dep_women$coefficients['SNP1']-1.96*coef(summary(est_dep_women))['SNP1', "Std. Error"]
covar[23,2] <- est_dep_smoke$coefficients['SNP1']-1.96*coef(summary(est_dep_smoke))['SNP1', "Std. Error"]
covar[24,2] <- est_dep_nonsmoke$coefficients['SNP1']-1.96*coef(summary(est_dep_nonsmoke))['SNP1', "Std. Error"]
covar[25,2] <- exp(est_inc_all$coefficients['SNP1']-1.96*coef(summary(est_inc_all))['SNP1', "Std. Error"])
covar[26,2] <- exp(est_inc_current$coefficients['SNP1']-1.96*coef(summary(est_inc_current))['SNP1', "Std. Error"])
covar[27,2] <- exp(est_inc_former$coefficients['SNP1']-1.96*coef(summary(est_inc_former))['SNP1', "Std. Error"])
covar[28,2] <- exp(est_inc_ever$coefficients['SNP1']-1.96*coef(summary(est_inc_ever))['SNP1', "Std. Error"])
covar[29,2] <- exp(est_inc_never$coefficients['SNP1']-1.96*coef(summary(est_inc_never))['SNP1', "Std. Error"])
covar[30,2] <- exp(est_inc_women$coefficients['SNP1']-1.96*coef(summary(est_inc_women))['SNP1', "Std. Error"])
covar[31,2] <- exp(est_inc_smoke$coefficients['SNP1']-1.96*coef(summary(est_inc_smoke))['SNP1', "Std. Error"])
covar[32,2] <- exp(est_inc_nonsmoke$coefficients['SNP1']-1.96*coef(summary(est_inc_nonsmoke))['SNP1', "Std. Error"])
#extract "uci"
covar[1,3] <- est_age_all$coefficients['SNP1']+1.96*coef(summary(est_age_all))['SNP1', "Std. Error"]
covar[2,3] <- est_age_current$coefficients['SNP1']+1.96*coef(summary(est_age_current))['SNP1', "Std. Error"]
covar[3,3] <- est_age_former$coefficients['SNP1']+1.96*coef(summary(est_age_former))['SNP1', "Std. Error"]
covar[4,3] <- est_age_ever$coefficients['SNP1']+1.96*coef(summary(est_age_ever))['SNP1', "Std. Error"]
covar[5,3] <- est_age_never$coefficients['SNP1']+1.96*coef(summary(est_age_never))['SNP1', "Std. Error"]
covar[6,3] <- est_age_women$coefficients['SNP1']+1.96*coef(summary(est_age_women))['SNP1', "Std. Error"]
covar[7,3] <- est_age_smoke$coefficients['SNP1']+1.96*coef(summary(est_age_smoke))['SNP1', "Std. Error"]
covar[8,3] <- est_age_nonsmoke$coefficients['SNP1']+1.96*coef(summary(est_age_nonsmoke))['SNP1', "Std. Error"]
covar[9,3] <- est_ageflb_women$coefficients['SNP1']+1.96*coef(summary(est_ageflb_women))['SNP1', "Std. Error"]
covar[10,3] <- est_ageflb_smoke$coefficients['SNP1']+1.96*coef(summary(est_ageflb_smoke))['SNP1', "Std. Error"]
covar[11,3] <- est_ageflb_nonsmoke$coefficients['SNP1']+1.96*coef(summary(est_ageflb_nonsmoke))['SNP1', "Std. Error"]
covar[12,3] <- exp(est_sex_all$coefficients['SNP1']+1.96*coef(summary(est_sex_all))['SNP1', "Std. Error"])
covar[13,3] <- exp(est_sex_current$coefficients['SNP1']+1.96*coef(summary(est_sex_current))['SNP1', "Std. Error"])
covar[14,3] <- exp(est_sex_former$coefficients['SNP1']+1.96*coef(summary(est_sex_former))['SNP1', "Std. Error"])
covar[15,3] <- exp(est_sex_ever$coefficients['SNP1']+1.96*coef(summary(est_sex_ever))['SNP1', "Std. Error"])
covar[16,3] <- exp(est_sex_never$coefficients['SNP1']+1.96*coef(summary(est_sex_never))['SNP1', "Std. Error"])
covar[17,3] <- est_dep_all$coefficients['SNP1']+1.96*coef(summary(est_dep_all))['SNP1', "Std. Error"]
covar[18,3] <- est_dep_current$coefficients['SNP1']+1.96*coef(summary(est_dep_current))['SNP1', "Std. Error"]
covar[19,3] <- est_dep_former$coefficients['SNP1']+1.96*coef(summary(est_dep_former))['SNP1', "Std. Error"]
covar[20,3] <- est_dep_ever$coefficients['SNP1']+1.96*coef(summary(est_dep_ever))['SNP1', "Std. Error"]
covar[21,3] <- est_dep_never$coefficients['SNP1']+1.96*coef(summary(est_dep_never))['SNP1', "Std. Error"]
covar[22,3] <- est_dep_women$coefficients['SNP1']+1.96*coef(summary(est_dep_women))['SNP1', "Std. Error"]
covar[23,3] <- est_dep_smoke$coefficients['SNP1']+1.96*coef(summary(est_dep_smoke))['SNP1', "Std. Error"]
covar[24,3] <- est_dep_nonsmoke$coefficients['SNP1']+1.96*coef(summary(est_dep_nonsmoke))['SNP1', "Std. Error"]
covar[25,3] <- exp(est_inc_all$coefficients['SNP1']+1.96*coef(summary(est_inc_all))['SNP1', "Std. Error"])
covar[26,3] <- exp(est_inc_current$coefficients['SNP1']+1.96*coef(summary(est_inc_current))['SNP1', "Std. Error"])
covar[27,3] <- exp(est_inc_former$coefficients['SNP1']+1.96*coef(summary(est_inc_former))['SNP1', "Std. Error"])
covar[28,3] <- exp(est_inc_ever$coefficients['SNP1']+1.96*coef(summary(est_inc_ever))['SNP1', "Std. Error"])
covar[29,3] <- exp(est_inc_never$coefficients['SNP1']+1.96*coef(summary(est_inc_never))['SNP1', "Std. Error"])
covar[30,3] <- exp(est_inc_women$coefficients['SNP1']+1.96*coef(summary(est_inc_women))['SNP1', "Std. Error"])
covar[31,3] <- exp(est_inc_smoke$coefficients['SNP1']+1.96*coef(summary(est_inc_smoke))['SNP1', "Std. Error"])
covar[32,3] <- exp(est_inc_nonsmoke$coefficients['SNP1']+1.96*coef(summary(est_inc_nonsmoke))['SNP1', "Std. Error"])

covar <-round(covar,3)
write.csv(covar,file=paste(Sys.getenv('Myresults'),'results/confounder_g0no.csv',sep=''))
#################################################################################################################################
#G0=overall
#age
est_age_all<-lm(confounder_use$age~.,data=confounder_use[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_current<-lm(current$age~.,data=current[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_former<-lm(former$age~.,data=former[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_ever<-lm(ever$age~.,data=ever[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_never<-lm(never$age~.,data=never[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_women<-lm(women$age~.,data=women[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_smoke<-lm(smoke$age~.,data=smoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_age_nonsmoke<-lm(nonsmoke$age~.,data=nonsmoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#age at first live birth
est_ageflb_women<-lm(women$age_live_b~.,data=women[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_ageflb_smoke<-lm(smoke$age_live_b~.,data=smoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_ageflb_nonsmoke<-lm(nonsmoke$age_live_b~.,data=nonsmoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#depriviation index
est_dep_all<-lm(confounder_use$deprivation~.,data=confounder_use[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_current<-lm(current$deprivation~.,data=current[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_former<-lm(former$deprivation~.,data=former[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_ever<-lm(ever$deprivation~.,data=ever[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_never<-lm(never$deprivation~.,data=never[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_women<-lm(women$deprivation~.,data=women[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_smoke<-lm(smoke$deprivation~.,data=smoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_dep_nonsmoke<-lm(nonsmoke$deprivation~.,data=nonsmoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#household income
est_inc_all<-polr(confounder_use$income~.,data=confounder_use[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_current<-polr(current$income~.,data=current[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_former<-polr(former$income~.,data=former[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_ever<-polr(ever$income~.,data=ever[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_never<-polr(never$income~.,data=never[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_women<-polr(women$income~.,data=women[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_smoke<-polr(smoke$income~.,data=smoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)
est_inc_nonsmoke<-polr(nonsmoke$income~.,data=nonsmoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")], Hess=TRUE)


#sex
est_sex_all<-glm(confounder_use$sex~.,data=confounder_use[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit"))
est_sex_current<-glm(current$sex~.,data=current[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit"))
est_sex_former<-glm(former$sex~.,data=former[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit"))
est_sex_ever<-glm(ever$sex~.,data=ever[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit"))
est_sex_never<-glm(never$sex~.,data=never[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")],binomial(link = "logit"))


#extract "beta"
covar[1,1] <- est_age_all$coefficients['SNP1']
covar[2,1] <- est_age_current$coefficients['SNP1']
covar[3,1] <- est_age_former$coefficients['SNP1']
covar[4,1] <- est_age_ever$coefficients['SNP1']
covar[5,1] <- est_age_never$coefficients['SNP1']
covar[6,1] <- est_age_women$coefficients['SNP1']
covar[7,1] <- est_age_smoke$coefficients['SNP1']
covar[8,1] <- est_age_nonsmoke$coefficients['SNP1']
covar[9,1] <- est_ageflb_women$coefficients['SNP1']
covar[10,1] <- est_ageflb_smoke$coefficients['SNP1']
covar[11,1] <- est_ageflb_nonsmoke$coefficients['SNP1']
covar[12,1] <- exp(est_sex_all$coefficients['SNP1'])
covar[13,1] <- exp(est_sex_current$coefficients['SNP1'])
covar[14,1] <- exp(est_sex_former$coefficients['SNP1'])
covar[15,1] <- exp(est_sex_ever$coefficients['SNP1'])
covar[16,1] <- exp(est_sex_never$coefficients['SNP1'])
covar[17,1] <- est_dep_all$coefficients['SNP1']
covar[18,1] <- est_dep_current$coefficients['SNP1']
covar[19,1] <- est_dep_former$coefficients['SNP1']
covar[20,1] <- est_dep_ever$coefficients['SNP1']
covar[21,1] <- est_dep_never$coefficients['SNP1']
covar[22,1] <- est_dep_women$coefficients['SNP1']
covar[23,1] <- est_dep_smoke$coefficients['SNP1']
covar[24,1] <- est_dep_nonsmoke$coefficients['SNP1']
covar[25,1] <- exp(est_inc_all$coefficients['SNP1'])
covar[26,1] <- exp(est_inc_current$coefficients['SNP1'])
covar[27,1] <- exp(est_inc_former$coefficients['SNP1'])
covar[28,1] <- exp(est_inc_ever$coefficients['SNP1'])
covar[29,1] <- exp(est_inc_never$coefficients['SNP1'])
covar[30,1] <- exp(est_inc_women$coefficients['SNP1'])
covar[31,1] <- exp(est_inc_smoke$coefficients['SNP1'])
covar[32,1] <- exp(est_inc_nonsmoke$coefficients['SNP1'])
#extract "lci"
covar[1,2] <- est_age_all$coefficients['SNP1']-1.96*coef(summary(est_age_all))['SNP1', "Std. Error"]
covar[2,2] <- est_age_current$coefficients['SNP1']-1.96*coef(summary(est_age_current))['SNP1', "Std. Error"]
covar[3,2] <- est_age_former$coefficients['SNP1']-1.96*coef(summary(est_age_former))['SNP1', "Std. Error"]
covar[4,2] <- est_age_ever$coefficients['SNP1']-1.96*coef(summary(est_age_ever))['SNP1', "Std. Error"]
covar[5,2] <- est_age_never$coefficients['SNP1']-1.96*coef(summary(est_age_never))['SNP1', "Std. Error"]
covar[6,2] <- est_age_women$coefficients['SNP1']-1.96*coef(summary(est_age_women))['SNP1', "Std. Error"]
covar[7,2] <- est_age_smoke$coefficients['SNP1']-1.96*coef(summary(est_age_smoke))['SNP1', "Std. Error"]
covar[8,2] <- est_age_nonsmoke$coefficients['SNP1']-1.96*coef(summary(est_age_nonsmoke))['SNP1', "Std. Error"]
covar[9,2] <- est_ageflb_women$coefficients['SNP1']-1.96*coef(summary(est_ageflb_women))['SNP1', "Std. Error"]
covar[10,2] <- est_ageflb_smoke$coefficients['SNP1']-1.96*coef(summary(est_ageflb_smoke))['SNP1', "Std. Error"]
covar[11,2] <- est_ageflb_nonsmoke$coefficients['SNP1']-1.96*coef(summary(est_ageflb_nonsmoke))['SNP1', "Std. Error"]
covar[12,2] <- exp(est_sex_all$coefficients['SNP1']-1.96*coef(summary(est_sex_all))['SNP1', "Std. Error"])
covar[13,2] <- exp(est_sex_current$coefficients['SNP1']-1.96*coef(summary(est_sex_current))['SNP1', "Std. Error"])
covar[14,2] <- exp(est_sex_former$coefficients['SNP1']-1.96*coef(summary(est_sex_former))['SNP1', "Std. Error"])
covar[15,2] <- exp(est_sex_ever$coefficients['SNP1']-1.96*coef(summary(est_sex_ever))['SNP1', "Std. Error"])
covar[16,2] <- exp(est_sex_never$coefficients['SNP1']-1.96*coef(summary(est_sex_never))['SNP1', "Std. Error"])
covar[17,2] <- est_dep_all$coefficients['SNP1']-1.96*coef(summary(est_dep_all))['SNP1', "Std. Error"]
covar[18,2] <- est_dep_current$coefficients['SNP1']-1.96*coef(summary(est_dep_current))['SNP1', "Std. Error"]
covar[19,2] <- est_dep_former$coefficients['SNP1']-1.96*coef(summary(est_dep_former))['SNP1', "Std. Error"]
covar[20,2] <- est_dep_ever$coefficients['SNP1']-1.96*coef(summary(est_dep_ever))['SNP1', "Std. Error"]
covar[21,2] <- est_dep_never$coefficients['SNP1']-1.96*coef(summary(est_dep_never))['SNP1', "Std. Error"]
covar[22,2] <- est_dep_women$coefficients['SNP1']-1.96*coef(summary(est_dep_women))['SNP1', "Std. Error"]
covar[23,2] <- est_dep_smoke$coefficients['SNP1']-1.96*coef(summary(est_dep_smoke))['SNP1', "Std. Error"]
covar[24,2] <- est_dep_nonsmoke$coefficients['SNP1']-1.96*coef(summary(est_dep_nonsmoke))['SNP1', "Std. Error"]
covar[25,2] <- exp(est_inc_all$coefficients['SNP1']-1.96*coef(summary(est_inc_all))['SNP1', "Std. Error"])
covar[26,2] <- exp(est_inc_current$coefficients['SNP1']-1.96*coef(summary(est_inc_current))['SNP1', "Std. Error"])
covar[27,2] <- exp(est_inc_former$coefficients['SNP1']-1.96*coef(summary(est_inc_former))['SNP1', "Std. Error"])
covar[28,2] <- exp(est_inc_ever$coefficients['SNP1']-1.96*coef(summary(est_inc_ever))['SNP1', "Std. Error"])
covar[29,2] <- exp(est_inc_never$coefficients['SNP1']-1.96*coef(summary(est_inc_never))['SNP1', "Std. Error"])
covar[30,2] <- exp(est_inc_women$coefficients['SNP1']-1.96*coef(summary(est_inc_women))['SNP1', "Std. Error"])
covar[31,2] <- exp(est_inc_smoke$coefficients['SNP1']-1.96*coef(summary(est_inc_smoke))['SNP1', "Std. Error"])
covar[32,2] <- exp(est_inc_nonsmoke$coefficients['SNP1']-1.96*coef(summary(est_inc_nonsmoke))['SNP1', "Std. Error"])
#extract "uci"
covar[1,3] <- est_age_all$coefficients['SNP1']+1.96*coef(summary(est_age_all))['SNP1', "Std. Error"]
covar[2,3] <- est_age_current$coefficients['SNP1']+1.96*coef(summary(est_age_current))['SNP1', "Std. Error"]
covar[3,3] <- est_age_former$coefficients['SNP1']+1.96*coef(summary(est_age_former))['SNP1', "Std. Error"]
covar[4,3] <- est_age_ever$coefficients['SNP1']+1.96*coef(summary(est_age_ever))['SNP1', "Std. Error"]
covar[5,3] <- est_age_never$coefficients['SNP1']+1.96*coef(summary(est_age_never))['SNP1', "Std. Error"]
covar[6,3] <- est_age_women$coefficients['SNP1']+1.96*coef(summary(est_age_women))['SNP1', "Std. Error"]
covar[7,3] <- est_age_smoke$coefficients['SNP1']+1.96*coef(summary(est_age_smoke))['SNP1', "Std. Error"]
covar[8,3] <- est_age_nonsmoke$coefficients['SNP1']+1.96*coef(summary(est_age_nonsmoke))['SNP1', "Std. Error"]
covar[9,3] <- est_ageflb_women$coefficients['SNP1']+1.96*coef(summary(est_ageflb_women))['SNP1', "Std. Error"]
covar[10,3] <- est_ageflb_smoke$coefficients['SNP1']+1.96*coef(summary(est_ageflb_smoke))['SNP1', "Std. Error"]
covar[11,3] <- est_ageflb_nonsmoke$coefficients['SNP1']+1.96*coef(summary(est_ageflb_nonsmoke))['SNP1', "Std. Error"]
covar[12,3] <- exp(est_sex_all$coefficients['SNP1']+1.96*coef(summary(est_sex_all))['SNP1', "Std. Error"])
covar[13,3] <- exp(est_sex_current$coefficients['SNP1']+1.96*coef(summary(est_sex_current))['SNP1', "Std. Error"])
covar[14,3] <- exp(est_sex_former$coefficients['SNP1']+1.96*coef(summary(est_sex_former))['SNP1', "Std. Error"])
covar[15,3] <- exp(est_sex_ever$coefficients['SNP1']+1.96*coef(summary(est_sex_ever))['SNP1', "Std. Error"])
covar[16,3] <- exp(est_sex_never$coefficients['SNP1']+1.96*coef(summary(est_sex_never))['SNP1', "Std. Error"])
covar[17,3] <- est_dep_all$coefficients['SNP1']+1.96*coef(summary(est_dep_all))['SNP1', "Std. Error"]
covar[18,3] <- est_dep_current$coefficients['SNP1']+1.96*coef(summary(est_dep_current))['SNP1', "Std. Error"]
covar[19,3] <- est_dep_former$coefficients['SNP1']+1.96*coef(summary(est_dep_former))['SNP1', "Std. Error"]
covar[20,3] <- est_dep_ever$coefficients['SNP1']+1.96*coef(summary(est_dep_ever))['SNP1', "Std. Error"]
covar[21,3] <- est_dep_never$coefficients['SNP1']+1.96*coef(summary(est_dep_never))['SNP1', "Std. Error"]
covar[22,3] <- est_dep_women$coefficients['SNP1']+1.96*coef(summary(est_dep_women))['SNP1', "Std. Error"]
covar[23,3] <- est_dep_smoke$coefficients['SNP1']+1.96*coef(summary(est_dep_smoke))['SNP1', "Std. Error"]
covar[24,3] <- est_dep_nonsmoke$coefficients['SNP1']+1.96*coef(summary(est_dep_nonsmoke))['SNP1', "Std. Error"]
covar[25,3] <- exp(est_inc_all$coefficients['SNP1']+1.96*coef(summary(est_inc_all))['SNP1', "Std. Error"])
covar[26,3] <- exp(est_inc_current$coefficients['SNP1']+1.96*coef(summary(est_inc_current))['SNP1', "Std. Error"])
covar[27,3] <- exp(est_inc_former$coefficients['SNP1']+1.96*coef(summary(est_inc_former))['SNP1', "Std. Error"])
covar[28,3] <- exp(est_inc_ever$coefficients['SNP1']+1.96*coef(summary(est_inc_ever))['SNP1', "Std. Error"])
covar[29,3] <- exp(est_inc_never$coefficients['SNP1']+1.96*coef(summary(est_inc_never))['SNP1', "Std. Error"])
covar[30,3] <- exp(est_inc_women$coefficients['SNP1']+1.96*coef(summary(est_inc_women))['SNP1', "Std. Error"])
covar[31,3] <- exp(est_inc_smoke$coefficients['SNP1']+1.96*coef(summary(est_inc_smoke))['SNP1', "Std. Error"])
covar[32,3] <- exp(est_inc_nonsmoke$coefficients['SNP1']+1.96*coef(summary(est_inc_nonsmoke))['SNP1', "Std. Error"])

covar <-round(covar,3)
write.csv(covar,file=paste(Sys.getenv('Myresults'),'results/confounder_g0overall.csv',sep=''))
#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#read in database
#id, G1 birthweight
proof<-read.csv(paste(Sys.getenv('Mydata'),'bw_only_correct.csv',sep=''), sep=',')
proof<-proof[c(2,18)]
#id, age,G1smoking, bmi,sex,G0smoking
proof2<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''), sep=',')
proof2<-proof2[c(2,3,4,5,8,20)]
#id, fev1, fvc 
best_measure<-read.csv(paste(Sys.getenv('Mydata'),'lung_function_best_measures.csv',sep=''), sep=',')
colnames(best_measure)<-c("app16729","fev1_best","fvc_best")
#id, asthma
asthma<-read.csv(paste(Sys.getenv('Mydata'),'asthma.csv',sep=''), sep=',')
colnames(asthma)<-c("app16729","asthma_age","asthma_q")
asthma<-asthma[c(1,3)]
asthma$asthma_d[as.numeric(asthma$asthma_q)==-7 | as.numeric(asthma$asthma_q)>0]<-0 #define controls
asthma$asthma_d[as.numeric(asthma$asthma_q)==8]<-1 #define asthma cases
#id, sbp, dbp
bp<-read.csv(paste(Sys.getenv('Mydata'),'bp.csv',sep=''), sep=',')
colnames(bp)<-c("app16729","sbp_manual1","sbp_manual2","dbp_manual1","dbp_manual2","dbp_auto1","dbp_auto2","sbp_auto1","sbp_auto2")
##mean of 2 readings
bp$sbp_auto<-rowMeans(bp[,c("sbp_auto1","sbp_auto2")],na.rm = TRUE)
bp$sbp_manual<-rowMeans(bp[,c("sbp_manual1","sbp_manual2")],na.rm = TRUE)
bp$dbp_auto<-rowMeans(bp[,c("dbp_auto1","dbp_auto2")],na.rm = TRUE)
bp$dbp_manual<-rowMeans(bp[,c("dbp_manual1","dbp_manual2")],na.rm = TRUE)
##manual sphygmomanometer was used if digital monitor could not be employed
bp$sbp<-bp$sbp_auto
for (i in 1:502616){
  if(!is.na(bp$sbp_manual[i])){
    bp$sbp[i]<-bp$sbp_manual[i]
    i<-i+1
  }
}
bp$dbp<-bp$dbp_auto
for (i in 1:502616){
  if(!is.na(bp$dbp_manual[i])){
    bp$dbp[i]<-bp$dbp_manual[i]
    i<-i+1
  }
}
bp<-bp[c(1,14,15)]
#id, menarche
menarche<-read.csv(paste(Sys.getenv('Mydata'),'menarche.csv',sep=''), sep=',')
colnames(menarche)<-c("app16729","menarche")
menarche$menarche[menarche$menarche<0]<-NA
#id, height, edu
edu<-read.csv(paste(Sys.getenv('Mydata'),'height_education.csv',sep=''), sep=',')
colnames(edu)<-c("app16729","height","ageedu","country","edulevel1","edulevel2","edulevel3","edulevel4","edulevel5","edulevel6","age")
edu$eduyr[edu$edulevel1==-7]<-7
edu$eduyr[edu$edulevel1==1]<-20
edu$eduyr[is.na(edu$eduyr) & edu$edulevel1==5]<-19
edu$eduyr[is.na(edu$eduyr) & edu$edulevel2==5]<-19
edu$eduyr[is.na(edu$eduyr) & edu$edulevel3==5]<-19
edu$eduyr[is.na(edu$eduyr) & edu$edulevel4==5]<-19
edu$eduyr[is.na(edu$eduyr) & edu$edulevel5==5]<-19
edu$eduyr[is.na(edu$eduyr) & edu$edulevel1==6]<-15
edu$eduyr[is.na(edu$eduyr) & edu$edulevel2==6]<-15
edu$eduyr[is.na(edu$eduyr) & edu$edulevel3==6]<-15
edu$eduyr[is.na(edu$eduyr) & edu$edulevel4==6]<-15
edu$eduyr[is.na(edu$eduyr) & edu$edulevel5==6]<-15
edu$eduyr[is.na(edu$eduyr) & edu$edulevel6==6]<-15
edu$eduyr[is.na(edu$eduyr) & edu$edulevel1==2]<-13
edu$eduyr[is.na(edu$eduyr) & edu$edulevel2==2]<-13
edu$eduyr[is.na(edu$eduyr) & edu$edulevel1==3]<-10
edu$eduyr[is.na(edu$eduyr) & edu$edulevel2==3]<-10
edu$eduyr[is.na(edu$eduyr) & edu$edulevel3==3]<-10
edu$eduyr[is.na(edu$eduyr) & edu$edulevel1==4]<-10
edu$eduyr[is.na(edu$eduyr) & edu$edulevel2==4]<-10
edu$eduyr[is.na(edu$eduyr) & edu$edulevel3==4]<-10
edu$eduyr[is.na(edu$eduyr) & edu$edulevel4==4]<-10
summary(edu)
edu<-edu[c(1,2,12)]
#id, IQ
iq<-read.csv(paste(Sys.getenv('Mydata'),'iq.csv',sep=''), sep=',')
colnames(iq)<-c("app16729","iq_t1","iq_t2","iq_t3","iq_online")
iq$iq<-iq$iq_online
for (i in 1:337104){
  if(!is.na(iq$iq_t3[i])){
    iq$iq[i]<-iq$iq_t3[i]
    i<-i+1
  }
}
for (i in 1:337104){
  if(!is.na(iq$iq_t2[i])){
    iq$iq[i]<-iq$iq_t2[i]
    i<-i+1
  }
}
for (i in 1:337104){
  if(!is.na(iq$iq_t1[i])){
    iq$iq[i]<-iq$iq_t1[i]
    i<-i+1
  }
}
iq<-iq[c(1,6)]
#id, depression
depression_ques<-read.csv(paste(Sys.getenv('Mydata'),'depression.csv',sep=''), sep=',')
depression_icd_main<-read.csv(paste(Sys.getenv('Mydata'),'icd10_main_depression.csv',sep=''), sep=',')
depression_icd_second<-read.csv(paste(Sys.getenv('Mydata'),'icd10_second_depression.csv',sep=''), sep=',')
depression_icd<-merge(depression_icd_main,depression_icd_second,by="eid")
depression<-merge(depression_ques,depression_icd,by="eid")
colnames(depression)<-c("app16729","gp1","gp2","gp3","psych1","psych2","psych3","depression_diag_main","depression_diag_second")
depression$gp1[depression$gp1<0]<-NA
depression$gp2[depression$gp2<0]<-NA
depression$gp3[depression$gp3<0]<-NA
depression$psych1[depression$psych1<0]<-NA
depression$psych2[depression$psych2<0]<-NA
depression$psych3[depression$psych3<0]<-NA
depression$diag_sum<-rowSums(depression[,c(2:9)],na.rm=TRUE)
depression$diag_sum[is.na(depression$gp1) & is.na(depression$gp1) & is.na(depression$gp3) & is.na(depression$psych1) & is.na(depression$psych2) & is.na(depression$psych3) & is.na(depression$depression_diag_main) & is.na(depression$depression_diag_second)]<-NA
depression$diag_sum[depression$diag_sum>0]<-1
depression<-depression[c(1,10)]
#id, happiness
well<-read.csv(paste(Sys.getenv('Mydata'),'wellbeing.csv',sep=''), sep=',')
head(well)
well<-well[c(1,2)]
colnames(well)<-c("app16729","happy")
well$happy[well$happy<0]<-NA
well$happy<-7-well$happy
#id, G2 birthweight
grandmother_bw<-read.csv(paste(Sys.getenv('Mydata'),'maternal_smoking_women_participants_clean.csv',sep=''), sep=',')
head(grandmother_bw)
grandmother_bw<-grandmother_bw[c(2,20,27)]
grandmother_bw$bwchild_use<-grandmother_bw$bwchild*0.45359237
#id, confounder(SEP)
#import confounder dataset
confounder<-read.csv(paste(Sys.getenv('Mydata'),'confounder.csv',sep=''), sep=',')
colnames(confounder)<-c("app16729","deprivation","income")
confounder$income[confounder$income<0]<-NA
##################################################################################################
##################################################################################################
#combine databases
ob<-merge(proof,proof2,by="app16729")
ob<-merge(ob,best_measure,by="app16729")
ob<-merge(ob,asthma,by="app16729")
ob<-merge(ob,bp,by="app16729")
ob<-merge(ob,menarche,by="app16729")
ob<-merge(ob,edu,by="app16729")
ob<-merge(ob,iq,by="app16729")
ob<-merge(ob,depression,by="app16729")
ob<-merge(ob,well,by="app16729")
ob<-merge(ob,grandmother_bw,by="app16729",all.x=T)
ob<-merge(ob,confounder,by="app16729")
summary(ob)
##################################################################################################
##################################################################################################
#packages
require(foreign)
require(MASS)
require(Hmisc)
require(reshape2)
##################################################################################################
##################################################################################################
#observarional analyses, G0 smoking status~other variables
ob$smoke[ob$smoke<0]<-NA
ob$smoke[ob$smoke==2]<-1
#G1 smoking status
g1smoke<-glm(ob$smoke~ob$mumsmoke,binomial(link = "logit"))
g1smoke_p<-glm(ob$smoke_preg~ob$mumsmoke,binomial(link = "logit"))
#g1 birthweight
g1bw<-lm(ob$bw~ob$mumsmoke+ob$age)
g1bw_a<-lm(ob$bw~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
#g1 height
height<-lm(ob$height~ob$mumsmoke+ob$age)
height_a<-lm(ob$height~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
#g1 bmi
bmi<-lm(ob$bmi~ob$mumsmoke+ob$age)
bmi_a<-lm(ob$bmi~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
#g1 lung function
fev1<-lm(ob$fev1_best~ob$mumsmoke+ob$age)
fev1_a<-lm(ob$fev1_best~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
fvc<-lm(ob$fvc_best~ob$mumsmoke+ob$age)
fvc_a<-lm(ob$fvc_best~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
#g1 asthma
asthma<-glm(as.factor(ob$asthma_d)~ob$mumsmoke+ob$age,binomial(link = "logit"))
asthma_a<-glm(as.factor(ob$asthma_d)~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation,binomial(link = "logit"))
#g1 blood pressure
sbp<-lm(ob$sbp~ob$mumsmoke+ob$age)
sbp_a<-lm(ob$sbp~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
dbp<-lm(ob$dbp~ob$mumsmoke+ob$age)
dbp_a<-lm(ob$dbp~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
#g1 menarche
menarche<-lm(ob$menarche~ob$mumsmoke+ob$age)
menarche_a<-lm(ob$menarche~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
#g1 edu
edu<-lm(ob$eduyr~ob$mumsmoke+ob$age)
#g1 iq
iq<-lm(ob$iq~ob$mumsmoke+ob$age)
iq_a<-lm(ob$iq~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
#g1 depression/anxiety
depress<-glm(as.factor(ob$diag_sum)~ob$mumsmoke+ob$age,binomial(link = "logit"))
depress_a<-glm(as.factor(ob$diag_sum)~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation,binomial(link = "logit"))
#g1 happiness
happy<-polr(as.factor(ob$happy)~ob$mumsmoke+ob$age, Hess=TRUE)
happy_a<-polr(as.factor(ob$happy)~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation, Hess=TRUE)
#g2 birthweight
g2bw<-lm(ob$bwchild_use~ob$mumsmoke+ob$age)
g2bw_a<-lm(ob$bwchild_use~ob$mumsmoke+ob$age+ob$smoke_preg+ob$eduyr+ob$deprivation)
##################################################################################################
##################################################################################################
#save results in a file
#creat a matrix for results
vars<-c("g1smoke", "g1bw", "g1height", "g1bmi", "g1fev1", "g1fvc","g1asthma","g1sbp","g1dbp","g1menarche","g1edu","g1iq","g1depress","g1happy","g2bw")
col.names <- c("model1_beta","model1_lci","model1_uci","model1_p","model2_beta","model2_lci","model2_uci","model2_p")
covar <- matrix(, ncol=8, nrow=length(vars))
dimnames(covar) <- list(vars, col.names)

#extract beta 
covar[1,1] <- exp(g1smoke$coefficients["ob$mumsmoke"])
covar[2,1] <- g1bw$coefficients['ob$mumsmoke']
covar[3,1] <- height$coefficients['ob$mumsmoke']
covar[4,1] <- bmi$coefficients['ob$mumsmoke']
covar[5,1] <- fev1$coefficients['ob$mumsmoke']
covar[6,1] <- fvc$coefficients['ob$mumsmoke']
covar[7,1] <- exp(asthma$coefficients['ob$mumsmoke'])
covar[8,1] <- sbp$coefficients['ob$mumsmoke']
covar[9,1] <- dbp$coefficients['ob$mumsmoke']
covar[10,1] <- menarche$coefficients['ob$mumsmoke']
covar[11,1] <- edu$coefficients['ob$mumsmoke']
covar[12,1] <- iq$coefficients['ob$mumsmoke']
covar[13,1] <- exp(depress$coefficients['ob$mumsmoke'])
covar[14,1] <- exp(happy$coefficients['ob$mumsmoke'])
covar[15,1] <- g2bw$coefficients['ob$mumsmoke']

#extract lci
covar[1,2] <- exp(g1smoke$coefficients["ob$mumsmoke"]-1.96*coef(summary(g1smoke))['ob$mumsmoke', "Std. Error"])
covar[2,2] <- g1bw$coefficients['ob$mumsmoke']-1.96*coef(summary(g1bw))['ob$mumsmoke', "Std. Error"]
covar[3,2] <- height$coefficients['ob$mumsmoke']-1.96*coef(summary(height))['ob$mumsmoke', "Std. Error"]
covar[4,2] <- bmi$coefficients['ob$mumsmoke']-1.96*coef(summary(bmi))['ob$mumsmoke', "Std. Error"]
covar[5,2] <- fev1$coefficients['ob$mumsmoke']-1.96*coef(summary(fev1))['ob$mumsmoke', "Std. Error"]
covar[6,2] <- fvc$coefficients['ob$mumsmoke']-1.96*coef(summary(fvc))['ob$mumsmoke', "Std. Error"]
covar[7,2] <- exp(asthma$coefficients['ob$mumsmoke']-1.96*coef(summary(asthma))['ob$mumsmoke', "Std. Error"])
covar[8,2] <- sbp$coefficients['ob$mumsmoke']-1.96*coef(summary(sbp))['ob$mumsmoke', "Std. Error"]
covar[9,2] <- dbp$coefficients['ob$mumsmoke']-1.96*coef(summary(dbp))['ob$mumsmoke', "Std. Error"]
covar[10,2] <- menarche$coefficients['ob$mumsmoke']-1.96*coef(summary(menarche))['ob$mumsmoke', "Std. Error"]
covar[11,2] <- edu$coefficients['ob$mumsmoke']-1.96*coef(summary(edu))['ob$mumsmoke', "Std. Error"]
covar[12,2] <- iq$coefficients['ob$mumsmoke']-1.96*coef(summary(iq))['ob$mumsmoke', "Std. Error"]
covar[13,2] <- exp(depress$coefficients['ob$mumsmoke']-1.96*coef(summary(depress))['ob$mumsmoke', "Std. Error"])
covar[14,2] <- exp(happy$coefficients['ob$mumsmoke']-1.96*coef(summary(happy))['ob$mumsmoke', "Std. Error"])
covar[15,2] <- g2bw$coefficients['ob$mumsmoke']-1.96*coef(summary(g2bw))['ob$mumsmoke', "Std. Error"]

#extract uci
covar[1,3] <- exp(g1smoke$coefficients["ob$mumsmoke"]+1.96*coef(summary(g1smoke))['ob$mumsmoke', "Std. Error"])
covar[2,3] <- g1bw$coefficients['ob$mumsmoke']+1.96*coef(summary(g1bw))['ob$mumsmoke', "Std. Error"]
covar[3,3] <- height$coefficients['ob$mumsmoke']+1.96*coef(summary(height))['ob$mumsmoke', "Std. Error"]
covar[4,3] <- bmi$coefficients['ob$mumsmoke']+1.96*coef(summary(bmi))['ob$mumsmoke', "Std. Error"]
covar[5,3] <- fev1$coefficients['ob$mumsmoke']+1.96*coef(summary(fev1))['ob$mumsmoke', "Std. Error"]
covar[6,3] <- fvc$coefficients['ob$mumsmoke']+1.96*coef(summary(fvc))['ob$mumsmoke', "Std. Error"]
covar[7,3] <- exp(asthma$coefficients['ob$mumsmoke']+1.96*coef(summary(asthma))['ob$mumsmoke', "Std. Error"])
covar[8,3] <- sbp$coefficients['ob$mumsmoke']+1.96*coef(summary(sbp))['ob$mumsmoke', "Std. Error"]
covar[9,3] <- dbp$coefficients['ob$mumsmoke']+1.96*coef(summary(dbp))['ob$mumsmoke', "Std. Error"]
covar[10,3] <- menarche$coefficients['ob$mumsmoke']+1.96*coef(summary(menarche))['ob$mumsmoke', "Std. Error"]
covar[11,3] <- edu$coefficients['ob$mumsmoke']+1.96*coef(summary(edu))['ob$mumsmoke', "Std. Error"]
covar[12,3] <- iq$coefficients['ob$mumsmoke']+1.96*coef(summary(iq))['ob$mumsmoke', "Std. Error"]
covar[13,3] <- exp(depress$coefficients['ob$mumsmoke']+1.96*coef(summary(depress))['ob$mumsmoke', "Std. Error"])
covar[14,3] <- exp(happy$coefficients['ob$mumsmoke']+1.96*coef(summary(happy))['ob$mumsmoke', "Std. Error"])
covar[15,3] <- g2bw$coefficients['ob$mumsmoke']+1.96*coef(summary(g2bw))['ob$mumsmoke', "Std. Error"]

#extract p-value
covar[1,4] <- coef(summary(g1smoke))['ob$mumsmoke', "Pr(>|z|)"]
covar[2,4] <- coef(summary(g1bw))['ob$mumsmoke', "Pr(>|t|)"]
covar[3,4] <- coef(summary(height))['ob$mumsmoke', "Pr(>|t|)"]
covar[4,4] <- coef(summary(bmi))['ob$mumsmoke', "Pr(>|t|)"]
covar[5,4] <- coef(summary(fev1))['ob$mumsmoke', "Pr(>|t|)"]
covar[6,4] <- coef(summary(fvc))['ob$mumsmoke', "Pr(>|t|)"]
covar[7,4] <- coef(summary(asthma))['ob$mumsmoke', "Pr(>|z|)"]
covar[8,4] <- coef(summary(sbp))['ob$mumsmoke', "Pr(>|t|)"]
covar[9,4] <- coef(summary(dbp))['ob$mumsmoke', "Pr(>|t|)"]
covar[10,4] <- coef(summary(menarche))['ob$mumsmoke', "Pr(>|t|)"]
covar[11,4] <- coef(summary(edu))['ob$mumsmoke', "Pr(>|t|)"]
covar[12,4] <- coef(summary(iq))['ob$mumsmoke', "Pr(>|t|)"]
covar[13,4] <- coef(summary(depress))['ob$mumsmoke', "Pr(>|z|)"]
covar[14,4] <- pnorm(abs(coef(summary(happy))['ob$mumsmoke', "t value"]), lower.tail = FALSE) * 2
covar[15,4] <- coef(summary(g2bw))['ob$mumsmoke', "Pr(>|t|)"]

#extract beta, adjust model
covar[1,5] <- exp(g1smoke_p$coefficients["ob$mumsmoke"])
covar[2,5] <- g1bw_a$coefficients['ob$mumsmoke']
covar[3,5] <- height_a$coefficients['ob$mumsmoke']
covar[4,5] <- bmi_a$coefficients['ob$mumsmoke']
covar[5,5] <- fev1_a$coefficients['ob$mumsmoke']
covar[6,5] <- fvc_a$coefficients['ob$mumsmoke']
covar[7,5] <- exp(asthma_a$coefficients['ob$mumsmoke'])
covar[8,5] <- sbp_a$coefficients['ob$mumsmoke']
covar[9,5] <- dbp_a$coefficients['ob$mumsmoke']
covar[10,5] <- menarche_a$coefficients['ob$mumsmoke']
covar[12,5] <- iq_a$coefficients['ob$mumsmoke']
covar[13,5] <- exp(depress_a$coefficients['ob$mumsmoke'])
covar[14,5] <- exp(happy_a$coefficients['ob$mumsmoke'])
covar[15,5] <- g2bw_a$coefficients['ob$mumsmoke']

#extract lci
covar[1,6] <- exp(g1smoke_p$coefficients["ob$mumsmoke"]-1.96*coef(summary(g1smoke_p))['ob$mumsmoke', "Std. Error"])
covar[2,6] <- g1bw_a$coefficients['ob$mumsmoke']-1.96*coef(summary(g1bw_a))['ob$mumsmoke', "Std. Error"]
covar[3,6] <- height_a$coefficients['ob$mumsmoke']-1.96*coef(summary(height_a))['ob$mumsmoke', "Std. Error"]
covar[4,6] <- bmi_a$coefficients['ob$mumsmoke']-1.96*coef(summary(bmi_a))['ob$mumsmoke', "Std. Error"]
covar[5,6] <- fev1_a$coefficients['ob$mumsmoke']-1.96*coef(summary(fev1_a))['ob$mumsmoke', "Std. Error"]
covar[6,6] <- fvc_a$coefficients['ob$mumsmoke']-1.96*coef(summary(fvc_a))['ob$mumsmoke', "Std. Error"]
covar[7,6] <- exp(asthma_a$coefficients['ob$mumsmoke']-1.96*coef(summary(asthma_a))['ob$mumsmoke', "Std. Error"])
covar[8,6] <- sbp_a$coefficients['ob$mumsmoke']-1.96*coef(summary(sbp_a))['ob$mumsmoke', "Std. Error"]
covar[9,6] <- dbp_a$coefficients['ob$mumsmoke']-1.96*coef(summary(dbp_a))['ob$mumsmoke', "Std. Error"]
covar[10,6] <- menarche_a$coefficients['ob$mumsmoke']-1.96*coef(summary(menarche_a))['ob$mumsmoke', "Std. Error"]
covar[12,6] <- iq_a$coefficients['ob$mumsmoke']-1.96*coef(summary(iq_a))['ob$mumsmoke', "Std. Error"]
covar[13,6] <- exp(depress_a$coefficients['ob$mumsmoke']-1.96*coef(summary(depress_a))['ob$mumsmoke', "Std. Error"])
covar[14,6] <- exp(happy_a$coefficients['ob$mumsmoke']-1.96*coef(summary(happy_a))['ob$mumsmoke', "Std. Error"])
covar[15,6] <- g2bw_a$coefficients['ob$mumsmoke']-1.96*coef(summary(g2bw_a))['ob$mumsmoke', "Std. Error"]

#extract uci
covar[1,7] <- exp(g1smoke_p$coefficients["ob$mumsmoke"]+1.96*coef(summary(g1smoke_p))['ob$mumsmoke', "Std. Error"])
covar[2,7] <- g1bw_a$coefficients['ob$mumsmoke']+1.96*coef(summary(g1bw_a))['ob$mumsmoke', "Std. Error"]
covar[3,7] <- height_a$coefficients['ob$mumsmoke']+1.96*coef(summary(height_a))['ob$mumsmoke', "Std. Error"]
covar[4,7] <- bmi_a$coefficients['ob$mumsmoke']+1.96*coef(summary(bmi_a))['ob$mumsmoke', "Std. Error"]
covar[5,7] <- fev1_a$coefficients['ob$mumsmoke']+1.96*coef(summary(fev1_a))['ob$mumsmoke', "Std. Error"]
covar[6,7] <- fvc_a$coefficients['ob$mumsmoke']+1.96*coef(summary(fvc_a))['ob$mumsmoke', "Std. Error"]
covar[7,7] <- exp(asthma_a$coefficients['ob$mumsmoke']+1.96*coef(summary(asthma_a))['ob$mumsmoke', "Std. Error"])
covar[8,7] <- sbp_a$coefficients['ob$mumsmoke']+1.96*coef(summary(sbp_a))['ob$mumsmoke', "Std. Error"]
covar[9,7] <- dbp_a$coefficients['ob$mumsmoke']+1.96*coef(summary(dbp_a))['ob$mumsmoke', "Std. Error"]
covar[10,7] <- menarche_a$coefficients['ob$mumsmoke']+1.96*coef(summary(menarche_a))['ob$mumsmoke', "Std. Error"]
covar[12,7] <- iq_a$coefficients['ob$mumsmoke']+1.96*coef(summary(iq_a))['ob$mumsmoke', "Std. Error"]
covar[13,7] <- exp(depress_a$coefficients['ob$mumsmoke']+1.96*coef(summary(depress_a))['ob$mumsmoke', "Std. Error"])
covar[14,7] <- exp(happy_a$coefficients['ob$mumsmoke']+1.96*coef(summary(happy_a))['ob$mumsmoke', "Std. Error"])
covar[15,7] <- g2bw_a$coefficients['ob$mumsmoke']+1.96*coef(summary(g2bw_a))['ob$mumsmoke', "Std. Error"]

#extract p-value
covar[1,8] <- coef(summary(g1smoke_p))['ob$mumsmoke', "Pr(>|z|)"]
covar[2,8] <- coef(summary(g1bw_a))['ob$mumsmoke', "Pr(>|t|)"]
covar[3,8] <- coef(summary(height_a))['ob$mumsmoke', "Pr(>|t|)"]
covar[4,8] <- coef(summary(bmi_a))['ob$mumsmoke', "Pr(>|t|)"]
covar[5,8] <- coef(summary(fev1_a))['ob$mumsmoke', "Pr(>|t|)"]
covar[6,8] <- coef(summary(fvc_a))['ob$mumsmoke', "Pr(>|t|)"]
covar[7,8] <- coef(summary(asthma_a))['ob$mumsmoke', "Pr(>|z|)"]
covar[8,8] <- coef(summary(sbp_a))['ob$mumsmoke', "Pr(>|t|)"]
covar[9,8] <- coef(summary(dbp_a))['ob$mumsmoke', "Pr(>|t|)"]
covar[10,8] <- coef(summary(menarche_a))['ob$mumsmoke', "Pr(>|t|)"]
covar[12,8] <- coef(summary(iq_a))['ob$mumsmoke', "Pr(>|t|)"]
covar[13,8] <- coef(summary(depress_a))['ob$mumsmoke', "Pr(>|z|)"]
covar[14,8] <- pnorm(abs(coef(summary(happy_a))['ob$mumsmoke', "t value"]), lower.tail = FALSE) * 2
covar[15,8] <- coef(summary(g2bw_a))['ob$mumsmoke', "Pr(>|t|)"]

write.csv(covar, file=paste(Sys.getenv('Myresults'),'mini project3_plot/observational.csv',sep=''))

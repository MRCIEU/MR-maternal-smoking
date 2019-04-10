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
sbpix<-which(!is.na(bp$sbp_manual))
bp$sbp[sbpix]<-bp$sbp_manual[sbpix]
bp$dbp<-bp$dbp_auto
dbpix<-which(!is.na(bp$dbp_manual))
bp$dbp[dbpix]<-bp$dbp_manual[dbpix]      
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
iq3ix<-which(!is.na(iq$iq_t3))
iq$iq[iq3ix]<-iq$iq_t3[iq3ix]
iq2ix<-which(!is.na(iq$iq_t2))
iq$iq[iq2ix]<-iq$iq_t2[iq2ix]
iq1ix<-which(!is.na(iq$iq_t1))
iq$iq[iq1ix]<-iq$iq_t1[iq1ix]
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
depression$diag_sum[is.na(depression$gp1) & is.na(depression$gp2) & is.na(depression$gp3) & is.na(depression$psych1) & is.na(depression$psych2) & is.na(depression$psych3) & is.na(depression$depression_diag_main) & is.na(depression$depression_diag_second)]<-NA
depression$diag_sum[depression$diag_sum>0]<-1
depression<-depression[c("app16729","diag_sum")]
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
#calculate loglci & loguci manually due to confint is very slow
ob$smoke[ob$smoke<0]<-NA
ob$smoke[ob$smoke==2]<-1
#function, extract beta/OR, lci, uci and pvalue
observational <- function(fit,outcomename,linearmodel) {
  sumx = summary(fit)
  if(linearmodel=="linear"){
    beta = sumx$coefficients["ob$mumsmoke","Estimate"]
    pvalue = sumx$coefficients["ob$mumsmoke","Pr(>|t|)"]
    cis = confint(fit, level=0.95)
    lower = cis["ob$mumsmoke", "2.5 %"]
    upper = cis["ob$mumsmoke", "97.5 %"]
    write.table(cbind(outcomename,beta,lower,upper,pvalue),file=paste(Sys.getenv('Myresults'),'mini project3_plot/observational.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
  }else{
    if(linearmodel=="logistic"){
      logor = sumx$coefficients["ob$mumsmoke","Estimate"]
      se = sumx$coefficients["ob$mumsmoke","Std. Error"]
      pvalue = sumx$coefficients["ob$mumsmoke","Pr(>|z|)"]
      lower = exp(logor-1.96*se)
      upper = exp(logor+1.96*se)
      write.table(cbind(outcomename,exp(or),lower,upper,pvalue),file=paste(Sys.getenv('Myresults'),'mini project3_plot/observational.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
    }else{
      logor = sumx$coefficients["ob$mumsmoke","Value"]
      se = sumx$coefficients["ob$mumsmoke","Std. Error"]
      pvalue = pnorm(abs(coef(sumx)['ob$mumsmoke', "t value"]), lower.tail = FALSE) * 2
      lower = exp(logor-1.96*se)
      upper = exp(logor+1.96*se)
      write.table(cbind(outcomename,exp(logor),lower,upper,pvalue),file=paste(Sys.getenv('Myresults'),'mini project3_plot/observational.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
    }}
}
#G1 smoking status
g1smoke<-glm(ob$smoke~ob$mumsmoke,binomial(link = "logit"))
observational(g1smoke,"G1smoking_model1","logistic")
g1smoke_p<-glm(ob$smoke_preg~ob$mumsmoke,binomial(link = "logit"))
observational(g1smoke_p,"G1pregsmoking_model1","logistic")
#g1 birthweight
g1bw<-lm(ob$bw~ob$mumsmoke+ob$age)
observational(g1bw,"G1bw_model1","linear")
g1bw_a<-lm(ob$bw~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
observational(g1bw_a,"G1bw_model2","linear")
#g1 height
height<-lm(ob$height~ob$mumsmoke+ob$age)
observational(height,"G1height_model1","linear")
height_a<-lm(ob$height~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
observational(height_a,"G1height_model2","linear")
#g1 bmi
bmi<-lm(ob$bmi~ob$mumsmoke+ob$age)
observational(bmi,"G1bmi_model1","linear")
bmi_a<-lm(ob$bmi~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
observational(bmi_a,"G1bmi_model2","linear")
#g1 lung function
fev1<-lm(ob$fev1_best~ob$mumsmoke+ob$age)
observational(fev1,"G1fev1_model1","linear")
fev1_a<-lm(ob$fev1_best~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
observational(fev1_a,"G1fev1_model2","linear")
fvc<-lm(ob$fvc_best~ob$mumsmoke+ob$age)
observational(fvc,"G1fvc_model1","linear")
fvc_a<-lm(ob$fvc_best~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
observational(fvc_a,"G1fvc_model2","linear")
#g1 asthma
asthma<-glm(as.factor(ob$asthma_d)~ob$mumsmoke+ob$age,binomial(link = "logit"))
observational(asthma,"G1asthma_model1","logistic")
asthma_a<-glm(as.factor(ob$asthma_d)~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation,binomial(link = "logit"))
observational(asthma_a,"G1asthma_model2","logistic")
#g1 blood pressure
sbp<-lm(ob$sbp~ob$mumsmoke+ob$age)
observational(sbp,"G1sbp_model1","linear")
sbp_a<-lm(ob$sbp~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
observational(sbp_a,"G1sbp_model2","linear")
dbp<-lm(ob$dbp~ob$mumsmoke+ob$age)
observational(dbp,"G1dbp_model1","linear")
dbp_a<-lm(ob$dbp~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
observational(dbp_a,"G1dbp_model2","linear")
#g1 menarche
menarche<-lm(ob$menarche~ob$mumsmoke+ob$age)
observational(menarche,"G1menarche_model1","linear")
menarche_a<-lm(ob$menarche~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
observational(menarche_a,"G1menarche_model2","linear")
#g1 edu
edu<-lm(ob$eduyr~ob$mumsmoke+ob$age)
observational(edu,"G1edu_model1","linear")
#g1 iq
iq<-lm(ob$iq~ob$mumsmoke+ob$age)
observational(iq,"G1iq_model1","linear")
iq_a<-lm(ob$iq~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation)
observational(iq_a,"G1iq_model2","linear")
#g1 depression/anxiety
depress<-glm(as.factor(ob$diag_sum)~ob$mumsmoke+ob$age,binomial(link = "logit"))
observational(depress,"G1depress_model1","logistic")
depress_a<-glm(as.factor(ob$diag_sum)~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation,binomial(link = "logit"))
observational(depress_a,"G1depress_model2","logistic")
#g1 happiness
happy<-polr(as.factor(ob$happy)~ob$mumsmoke+ob$age, Hess=TRUE)
observational(happy,"G1wellbeing_model1","ordinal")
happy_a<-polr(as.factor(ob$happy)~ob$mumsmoke+ob$age+ob$smoke+ob$eduyr+ob$deprivation, Hess=TRUE)
observational(happy_a,"G1wellbeing_model2","ordinal")
#g2 birthweight
g2bw<-lm(ob$bwchild_use~ob$mumsmoke+ob$age)
observational(g2bw,"G2bw_model1","linear")
g2bw_a<-lm(ob$bwchild_use~ob$mumsmoke+ob$age+ob$smoke_preg+ob$eduyr+ob$deprivation)
observational(g2bw_a,"G2bw_model2","linear")
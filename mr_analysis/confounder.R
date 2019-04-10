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
#function, extract beta/OR, lci, uci and pvalue
#calculate loglci & loguci manually due to confint is very slow
confounder <- function(database,outcomename,linearmodel) {
  if(linearmodel=="linear"){
    fit = lm(database[,outcomename] ~., data=database[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
    sumx = summary(fit)
    beta = sumx$coefficients["SNP1","Estimate"]
    pvalue = sumx$coefficients["SNP1","Pr(>|t|)"]
    cis = confint(fit, level=0.95)
    lower = cis["SNP1", "2.5 %"]
    upper = cis["SNP1", "97.5 %"]
    write.table(cbind(outcomename,beta,lower,upper,pvalue),file=paste(Sys.getenv('Myresults'),'mini project3_plot/confounder.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
  }else{
    if(linearmodel=="logistic"){
      fit = glm(database[,outcomename] ~.,binomial(link = "logit"), data=database[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
      sumx = summary(fit)
      logor = sumx$coefficients["SNP1","Estimate"]
      se = sumx$coefficients["SNP1","Std. Error"]
      pvalue = sumx$coefficients["SNP1","Pr(>|z|)"]
      lower = exp(logor-1.96*se)
      upper = exp(logor+1.96*se)
      write.table(cbind(outcomename,exp(logor),lower,upper,pvalue),file=paste(Sys.getenv('Myresults'),'mini project3_plot/confounder.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
    }else{
      fit = polr(database[,outcomename] ~.,Hess=TRUE, data=database[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
      sumx = summary(fit)
      logor = sumx$coefficients["SNP1","Value"]
      se = sumx$coefficients["SNP1","Std. Error"]
      pvalue = pnorm(abs(coef(sumx)['SNP1', "t value"]), lower.tail = FALSE) * 2
      lower = exp(logor-1.96*se)
      upper = exp(logor+1.96*se)
      write.table(cbind(outcomename,exp(logor),lower,upper,pvalue),file=paste(Sys.getenv('Myresults'),'mini project3_plot/confounder.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
    }}
}

datasetnames<-c("confounder_use","all1","all2","current","current1","current2","former","former1","former2","ever","ever1","ever2","never","never1","never2","women","women1","women2","smoke","smoke1","smoke2","nonsmoke","nonsmoke1","nonsmoke2")
n<-length(datasetnames)
n
#age
for (i in 1:n){
  databaserun<-get(datasetnames[i])
  confounder(databaserun,"age","linear")
}
#age at first live birth (G1 female only)
for (i in 16:n){
  databaserun<-get(datasetnames[i])
  confounder(databaserun,"age_live_b","linear")
}
#deprivation index
for (i in 1:n){
  databaserun<-get(datasetnames[i])
  confounder(databaserun,"deprivation","linear")
}
#household income
for (i in 1:n){
  databaserun<-get(datasetnames[i])
  confounder(databaserun,"income","ordinal")
}
#sex (male vs female)
for (i in 1:15){
  databaserun<-get(datasetnames[i])
  confounder(databaserun,"sex","logistic")
}
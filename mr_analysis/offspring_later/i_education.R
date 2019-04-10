#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#import education data (UKBB ID 6138)
edu<-read.csv(paste(Sys.getenv('Mydata'),'height_education.csv',sep=''), sep=',')
summary(edu)
colnames(edu)<-c("app16729","height","ageedu","country","edulevel1","edulevel2","edulevel3","edulevel4","edulevel5","edulevel6","age")
edu<-edu[c(1,4:10)]


#import previous clean dataset
proof2<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''), sep=',')
head(proof2)
proof2<-proof2[c(2:4,6:20)]
grandmother_bw<-read.csv(paste(Sys.getenv('Mydata'),'maternal_smoking_women_participants_clean.csv',sep=''), sep=',')
head(grandmother_bw)
grandmother_bw<-grandmother_bw[c(2,27)]

#combine new outcome data with my previous dataset
edu_use<-merge(proof2,edu,by="app16729")
edu_use<-merge(edu_use,grandmother_bw,by="app16729",all = TRUE)
summary(edu_use)

###################################################################################################
###################################################################################################
#the following analyses use edu_use database
###################################################################################################
###################################################################################################
#Part1: derive year of education ("eduyr") using Okbay method (PMID: 27225129)
#note: "Which of the following qualifications do you have? (You can select more than one)"
# None of the above (7)
edu_use$eduyr[edu_use$edulevel1==-7]<-7

# College or University degree (20)
edu_use$eduyr[edu_use$edulevel1==1]<-20

# NVQ or HND or HNC or equivalent (19)
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel1==5]<-19
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel2==5]<-19
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel3==5]<-19
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel4==5]<-19
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel5==5]<-19

# Other professional qualifications eg: nursing, teaching (15)
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel1==6]<-15
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel2==6]<-15
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel3==6]<-15
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel4==6]<-15
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel5==6]<-15
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel6==6]<-15

# A levels/AS levels or equivalent (13)
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel1==2]<-13
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel2==2]<-13

# O levels/GCSEs or equivalent (10)
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel1==3]<-10
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel2==3]<-10
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel3==3]<-10

# CSEs or equivalent (10)
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel1==4]<-10
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel2==4]<-10
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel3==4]<-10
edu_use$eduyr[is.na(edu_use$eduyr) & edu_use$edulevel4==4]<-10

#mean & sd by sex
summary(edu_use)
aggregate(edu_use,by=list(edu_use$sex),FUN = mean,na.rm=TRUE)
aggregate(edu_use,by=list(edu_use$sex),FUN = sd,na.rm=TRUE)


#Part2: analyses
#ever smoker vs never smoker in UKBB participants
#function, extract beta, se, lci and uci
linearregression <- function(database,modelname) {
  fit = lm(database[,"eduyr"] ~., data=database[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
  sumx = summary(fit)
  beta = sumx$coefficients["SNP1","Estimate"]
  se = sumx$coefficients["SNP1","Std. Error"]
  cis = confint(fit, level=0.95)
  lower = cis["SNP1", "2.5 %"]
  upper = cis["SNP1", "97.5 %"]
  write.table(cbind(modelname,beta,se,lower,upper),file=paste(Sys.getenv('Myresults'),'mini project3_plot/eduyr_ever_never.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
}

#in all participants
##subset participants: G0 smoking=No
mumnonsmoke_allchild<-edu_use[which(edu_use$mumsmoke==0),]
linearregression(mumnonsmoke_allchild,"mumnonsmoke_childall")
##subset participants: G0 smoking=Yes
mumsmoke_allchild<-edu_use[which(edu_use$mumsmoke==1),]
linearregression(mumsmoke_allchild,"mumsmoke_childall")

#in non-smoker(G0 & G1)
nonsmoke<-edu_use[which(edu_use$mumsmoke==0 & edu_use$smoke==0),]
linearregression(nonsmoke,"mumnonsmoke_childnever")

#G0=smoker, G1=never smoker
smoke_mum<-edu_use[which(edu_use$mumsmoke==1 & edu_use$smoke==0),]
linearregression(smoke_mum,"mumsmoke_childnever")

#G0=non-smoker, G1=ever smoker
smoke_child<-edu_use[which(edu_use$mumsmoke==0 & edu_use$smoke>=1),]
linearregression(smoke_child,"mumnonsmoke_childever")

#G0=smoker, G1=ever smoker
smoke_both<-edu_use[which(edu_use$mumsmoke==1 & edu_use$smoke>=1),]
linearregression(smoke_both,"mumsmoke_childever")


#Part4: confounder
#G1=all
all1<-edu_use[which(edu_use$mumsmoke==1),]
all2<-edu_use[which(edu_use$mumsmoke==0),]
#G1=current
current<-edu_use[which(edu_use$smoke==2),]
current1<-edu_use[which(edu_use$smoke==2 & edu_use$mumsmoke==1),]
current2<-edu_use[which(edu_use$smoke==2 & edu_use$mumsmoke==0),]
#G1=former
former<-edu_use[which(edu_use$smoke==1),]
former1<-edu_use[which(edu_use$smoke==1 & edu_use$mumsmoke==1),]
former2<-edu_use[which(edu_use$smoke==1 & edu_use$mumsmoke==0),]
#G1=ever
ever<-edu_use[which(edu_use$smoke>=1),]
ever1<-edu_use[which(edu_use$smoke>=1 & edu_use$mumsmoke==1),]
ever2<-edu_use[which(edu_use$smoke>=1 & edu_use$mumsmoke==0),]
#G1=never
never<-edu_use[which(edu_use$smoke==0),]
never1<-edu_use[which(edu_use$smoke==0 & edu_use$mumsmoke==1),]
never2<-edu_use[which(edu_use$smoke==0 & edu_use$mumsmoke==0),]
#G1=women
women<-edu_use[which(edu_use$sex=="F"),]
women1<-edu_use[which(edu_use$sex=="F" & edu_use$mumsmoke==1),]
women2<-edu_use[which(edu_use$sex=="F" & edu_use$mumsmoke==0),]
#G1=smoker in pregnancy
smoke<-edu_use[which(edu_use$smoke_preg==1),]
smoke1<-edu_use[which(edu_use$smoke_preg==1 & edu_use$mumsmoke==1),]
smoke2<-edu_use[which(edu_use$smoke_preg==1 & edu_use$mumsmoke==0),]
#G1=non-smoker in pregnancy
nonsmoke<-edu_use[which(edu_use$smoke_preg==0),]
nonsmoke1<-edu_use[which(edu_use$smoke_preg==0 & edu_use$mumsmoke==1),]
nonsmoke2<-edu_use[which(edu_use$smoke_preg==0 & edu_use$mumsmoke==0),]

#year of education
#function, extract beta/OR, lci, uci and pvalue
confounder <- function(database,datasetname) {
    fit = lm(database$eduyr ~., data=database[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
    sumx = summary(fit)
    beta = sumx$coefficients["SNP1","Estimate"]
    pvalue = sumx$coefficients["SNP1","Pr(>|t|)"]
    cis = confint(fit, level=0.95)
    lower = cis["SNP1", "2.5 %"]
    upper = cis["SNP1", "97.5 %"]
    write.table(cbind(datasetname,beta,lower,upper,pvalue),file=paste(Sys.getenv('Myresults'),'mini project3_plot/confounder.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
}
datasetnames<-c("edu_use","all1","all2","current","current1","current2","former","former1","former2","ever","ever1","ever2","never","never1","never2","women","women1","women2","smoke","smoke1","smoke2","nonsmoke","nonsmoke1","nonsmoke2")
n<-length(datasetnames)
n
for (i in 1:n){
  databaserun<-get(datasetnames[i])
  confounder(databaserun,datasetnames[i])
  i<-i+1
}
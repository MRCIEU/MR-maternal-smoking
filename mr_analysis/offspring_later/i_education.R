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
#in all participants
##subset participants: G0 smoking=No
mumnonsmoke_allchild<-edu_use[which(edu_use$mumsmoke==0),]
##subset participants: G0 smoking=Yes
mumsmoke_allchild<-edu_use[which(edu_use$mumsmoke==1),]
est_mumnonsmoke_allchild<-lm(mumnonsmoke_allchild$eduyr~.,data=mumnonsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_mumsmoke_allchild<-lm(mumsmoke_allchild$eduyr~.,data=mumsmoke_allchild[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#in non-smoker(G0 & G1)
nonsmoke<-edu_use[which(edu_use$mumsmoke==0 & edu_use$smoke==0),]
est_both_nonsmoke<-lm(nonsmoke$eduyr~.,data=nonsmoke[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=never smoker
smoke_mum<-edu_use[which(edu_use$mumsmoke==1 & edu_use$smoke==0),]
est_only_mumsmoke<-lm(smoke_mum$eduyr~.,data=smoke_mum[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=non-smoker, G1=ever smoker
smoke_child<-edu_use[which(edu_use$mumsmoke==0 & edu_use$smoke>=1),]
est_only_childsmoke<-lm(smoke_child$eduyr~.,data=smoke_child[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#G0=smoker, G1=ever smoker
smoke_both<-edu_use[which(edu_use$mumsmoke==1 & edu_use$smoke>=1),]
est_both_smoke<-lm(smoke_both$eduyr~.,data=smoke_both[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])


#Part 3
#save results in a file for plot
#creat a matrix for results
vars<-c("mumsmoke_ever", "mumsmoke_never", "mumsmoke_all", "mumnot_ever", "mumnot_never", "mumnot_all")
col.names <- c("supp","exposure","eduyr","se")
covar <- matrix(, ncol=4, nrow=length(vars))
dimnames(covar) <- list(vars, col.names)

#in the 1st colomn of matrix("supp"), "1" subgroup of "G0 smoking=No", "2" subgroup of "G0 smoking=Yes"
covar[,1] <- c(2,2,2,1,1,1)

#in the 2nd colomn of matrix("exposure"), "1" subgroup of ever smoker, "3" subgroup of never smoker, "5" all UKBB participants
covar[,2] <- c(1,3,5,1,3,5)

#extract beta 
covar[1,3] <- est_both_smoke$coefficients['SNP1']
covar[2,3] <- est_only_mumsmoke$coefficients['SNP1']
covar[3,3] <- est_mumsmoke_allchild$coefficients['SNP1']
covar[4,3] <- est_only_childsmoke$coefficients['SNP1']
covar[5,3] <- est_both_nonsmoke$coefficients['SNP1']
covar[6,3] <- est_mumnonsmoke_allchild$coefficients['SNP1']

#extract SE
covar[1,4] <- coef(summary(est_both_smoke))['SNP1', "Std. Error"]
covar[2,4] <- coef(summary(est_only_mumsmoke))['SNP1', "Std. Error"]
covar[3,4] <- coef(summary(est_mumsmoke_allchild))['SNP1', "Std. Error"]
covar[4,4] <- coef(summary(est_only_childsmoke))['SNP1', "Std. Error"]
covar[5,4] <- coef(summary(est_both_nonsmoke))['SNP1', "Std. Error"]
covar[6,4] <- coef(summary(est_mumnonsmoke_allchild))['SNP1', "Std. Error"]

write.csv(covar, file=paste(Sys.getenv('Myresults'),'mini project3_plot/eduyr_ever_never.csv',sep=''))


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
est_edu_all<-lm(edu_use$eduyr~.,data=edu_use[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_current<-lm(current$eduyr~.,data=current[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_former<-lm(former$eduyr~.,data=former[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_ever<-lm(ever$eduyr~.,data=ever[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_never<-lm(never$eduyr~.,data=never[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_women<-lm(women$eduyr~.,data=women[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_smoke<-lm(smoke$eduyr~.,data=smoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_nonsmoke<-lm(nonsmoke$eduyr~.,data=nonsmoke[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])

#creat a matrix for results
vars<-c("edu_all", "edu_current","edu_former","edu_ever","edu_never","edu_women","edu_smoke","edu_nonsmoke")
col.names <- c("beta","lci","uci")
covar <- matrix(, ncol=3, nrow=length(vars))
dimnames(covar) <- list(vars, col.names)
#extract "beta"
covar[1,1] <- est_edu_all$coefficients['SNP1']
covar[2,1] <- est_edu_current$coefficients['SNP1']
covar[3,1] <- est_edu_former$coefficients['SNP1']
covar[4,1] <- est_edu_ever$coefficients['SNP1']
covar[5,1] <- est_edu_never$coefficients['SNP1']
covar[6,1] <- est_edu_women$coefficients['SNP1']
covar[7,1] <- est_edu_smoke$coefficients['SNP1']
covar[8,1] <- est_edu_nonsmoke$coefficients['SNP1']
#extract "lci"
covar[1,2] <- est_edu_all$coefficients['SNP1']-1.96*coef(summary(est_edu_all))['SNP1', "Std. Error"]
covar[2,2] <- est_edu_current$coefficients['SNP1']-1.96*coef(summary(est_edu_current))['SNP1', "Std. Error"]
covar[3,2] <- est_edu_former$coefficients['SNP1']-1.96*coef(summary(est_edu_former))['SNP1', "Std. Error"]
covar[4,2] <- est_edu_ever$coefficients['SNP1']-1.96*coef(summary(est_edu_ever))['SNP1', "Std. Error"]
covar[5,2] <- est_edu_never$coefficients['SNP1']-1.96*coef(summary(est_edu_never))['SNP1', "Std. Error"]
covar[6,2] <- est_edu_women$coefficients['SNP1']-1.96*coef(summary(est_edu_women))['SNP1', "Std. Error"]
covar[7,2] <- est_edu_smoke$coefficients['SNP1']-1.96*coef(summary(est_edu_smoke))['SNP1', "Std. Error"]
covar[8,2] <- est_edu_nonsmoke$coefficients['SNP1']-1.96*coef(summary(est_edu_nonsmoke))['SNP1', "Std. Error"]
#extract "uci"
covar[1,3] <- est_edu_all$coefficients['SNP1']+1.96*coef(summary(est_edu_all))['SNP1', "Std. Error"]
covar[2,3] <- est_edu_current$coefficients['SNP1']+1.96*coef(summary(est_edu_current))['SNP1', "Std. Error"]
covar[3,3] <- est_edu_former$coefficients['SNP1']+1.96*coef(summary(est_edu_former))['SNP1', "Std. Error"]
covar[4,3] <- est_edu_ever$coefficients['SNP1']+1.96*coef(summary(est_edu_ever))['SNP1', "Std. Error"]
covar[5,3] <- est_edu_never$coefficients['SNP1']+1.96*coef(summary(est_edu_never))['SNP1', "Std. Error"]
covar[6,3] <- est_edu_women$coefficients['SNP1']+1.96*coef(summary(est_edu_women))['SNP1', "Std. Error"]
covar[7,3] <- est_edu_smoke$coefficients['SNP1']+1.96*coef(summary(est_edu_smoke))['SNP1', "Std. Error"]
covar[8,3] <- est_edu_nonsmoke$coefficients['SNP1']+1.96*coef(summary(est_edu_nonsmoke))['SNP1', "Std. Error"]

covar <-round(covar,3)
write.csv(covar, file=paste(Sys.getenv('Myresults'),'results/confounder_edu.csv',sep=''))

#year of education, G0 smoking =yes
est_edu_all<-lm(all1$eduyr~.,data=all1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_current<-lm(current1$eduyr~.,data=current1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_former<-lm(former1$eduyr~.,data=former1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_ever<-lm(ever1$eduyr~.,data=ever1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_never<-lm(never1$eduyr~.,data=never1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_women<-lm(women1$eduyr~.,data=women1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_smoke<-lm(smoke1$eduyr~.,data=smoke1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_nonsmoke<-lm(nonsmoke1$eduyr~.,data=nonsmoke1[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])

#extract "beta"
covar[1,1] <- est_edu_all$coefficients['SNP1']
covar[2,1] <- est_edu_current$coefficients['SNP1']
covar[3,1] <- est_edu_former$coefficients['SNP1']
covar[4,1] <- est_edu_ever$coefficients['SNP1']
covar[5,1] <- est_edu_never$coefficients['SNP1']
covar[6,1] <- est_edu_women$coefficients['SNP1']
covar[7,1] <- est_edu_smoke$coefficients['SNP1']
covar[8,1] <- est_edu_nonsmoke$coefficients['SNP1']
#extract "lci"
covar[1,2] <- est_edu_all$coefficients['SNP1']-1.96*coef(summary(est_edu_all))['SNP1', "Std. Error"]
covar[2,2] <- est_edu_current$coefficients['SNP1']-1.96*coef(summary(est_edu_current))['SNP1', "Std. Error"]
covar[3,2] <- est_edu_former$coefficients['SNP1']-1.96*coef(summary(est_edu_former))['SNP1', "Std. Error"]
covar[4,2] <- est_edu_ever$coefficients['SNP1']-1.96*coef(summary(est_edu_ever))['SNP1', "Std. Error"]
covar[5,2] <- est_edu_never$coefficients['SNP1']-1.96*coef(summary(est_edu_never))['SNP1', "Std. Error"]
covar[6,2] <- est_edu_women$coefficients['SNP1']-1.96*coef(summary(est_edu_women))['SNP1', "Std. Error"]
covar[7,2] <- est_edu_smoke$coefficients['SNP1']-1.96*coef(summary(est_edu_smoke))['SNP1', "Std. Error"]
covar[8,2] <- est_edu_nonsmoke$coefficients['SNP1']-1.96*coef(summary(est_edu_nonsmoke))['SNP1', "Std. Error"]
#extract "uci"
covar[1,3] <- est_edu_all$coefficients['SNP1']+1.96*coef(summary(est_edu_all))['SNP1', "Std. Error"]
covar[2,3] <- est_edu_current$coefficients['SNP1']+1.96*coef(summary(est_edu_current))['SNP1', "Std. Error"]
covar[3,3] <- est_edu_former$coefficients['SNP1']+1.96*coef(summary(est_edu_former))['SNP1', "Std. Error"]
covar[4,3] <- est_edu_ever$coefficients['SNP1']+1.96*coef(summary(est_edu_ever))['SNP1', "Std. Error"]
covar[5,3] <- est_edu_never$coefficients['SNP1']+1.96*coef(summary(est_edu_never))['SNP1', "Std. Error"]
covar[6,3] <- est_edu_women$coefficients['SNP1']+1.96*coef(summary(est_edu_women))['SNP1', "Std. Error"]
covar[7,3] <- est_edu_smoke$coefficients['SNP1']+1.96*coef(summary(est_edu_smoke))['SNP1', "Std. Error"]
covar[8,3] <- est_edu_nonsmoke$coefficients['SNP1']+1.96*coef(summary(est_edu_nonsmoke))['SNP1', "Std. Error"]

covar <-round(covar,3)
write.csv(covar, file=paste(Sys.getenv('Myresults'),'results/confounder_edu_g0yes.csv',sep=''))


#year of education, G0 smoking =no
est_edu_all<-lm(all2$eduyr~.,data=all2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_current<-lm(current2$eduyr~.,data=current2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_former<-lm(former2$eduyr~.,data=former2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_ever<-lm(ever2$eduyr~.,data=ever2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_never<-lm(never2$eduyr~.,data=never2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_women<-lm(women2$eduyr~.,data=women2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_smoke<-lm(smoke2$eduyr~.,data=smoke2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])
est_edu_nonsmoke<-lm(nonsmoke2$eduyr~.,data=nonsmoke2[,c("SNP1","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])

#extract "beta"
covar[1,1] <- est_edu_all$coefficients['SNP1']
covar[2,1] <- est_edu_current$coefficients['SNP1']
covar[3,1] <- est_edu_former$coefficients['SNP1']
covar[4,1] <- est_edu_ever$coefficients['SNP1']
covar[5,1] <- est_edu_never$coefficients['SNP1']
covar[6,1] <- est_edu_women$coefficients['SNP1']
covar[7,1] <- est_edu_smoke$coefficients['SNP1']
covar[8,1] <- est_edu_nonsmoke$coefficients['SNP1']
#extract "lci"
covar[1,2] <- est_edu_all$coefficients['SNP1']-1.96*coef(summary(est_edu_all))['SNP1', "Std. Error"]
covar[2,2] <- est_edu_current$coefficients['SNP1']-1.96*coef(summary(est_edu_current))['SNP1', "Std. Error"]
covar[3,2] <- est_edu_former$coefficients['SNP1']-1.96*coef(summary(est_edu_former))['SNP1', "Std. Error"]
covar[4,2] <- est_edu_ever$coefficients['SNP1']-1.96*coef(summary(est_edu_ever))['SNP1', "Std. Error"]
covar[5,2] <- est_edu_never$coefficients['SNP1']-1.96*coef(summary(est_edu_never))['SNP1', "Std. Error"]
covar[6,2] <- est_edu_women$coefficients['SNP1']-1.96*coef(summary(est_edu_women))['SNP1', "Std. Error"]
covar[7,2] <- est_edu_smoke$coefficients['SNP1']-1.96*coef(summary(est_edu_smoke))['SNP1', "Std. Error"]
covar[8,2] <- est_edu_nonsmoke$coefficients['SNP1']-1.96*coef(summary(est_edu_nonsmoke))['SNP1', "Std. Error"]
#extract "uci"
covar[1,3] <- est_edu_all$coefficients['SNP1']+1.96*coef(summary(est_edu_all))['SNP1', "Std. Error"]
covar[2,3] <- est_edu_current$coefficients['SNP1']+1.96*coef(summary(est_edu_current))['SNP1', "Std. Error"]
covar[3,3] <- est_edu_former$coefficients['SNP1']+1.96*coef(summary(est_edu_former))['SNP1', "Std. Error"]
covar[4,3] <- est_edu_ever$coefficients['SNP1']+1.96*coef(summary(est_edu_ever))['SNP1', "Std. Error"]
covar[5,3] <- est_edu_never$coefficients['SNP1']+1.96*coef(summary(est_edu_never))['SNP1', "Std. Error"]
covar[6,3] <- est_edu_women$coefficients['SNP1']+1.96*coef(summary(est_edu_women))['SNP1', "Std. Error"]
covar[7,3] <- est_edu_smoke$coefficients['SNP1']+1.96*coef(summary(est_edu_smoke))['SNP1', "Std. Error"]
covar[8,3] <- est_edu_nonsmoke$coefficients['SNP1']+1.96*coef(summary(est_edu_nonsmoke))['SNP1', "Std. Error"]

covar <-round(covar,3)
write.csv(covar, file=paste(Sys.getenv('Myresults'),'results/confounder_edu_g0no.csv',sep=''))
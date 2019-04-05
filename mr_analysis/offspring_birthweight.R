#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#read in genetic data
genetic_afterQC<-read.csv(paste(Sys.getenv('Mydata'),'genetic_afterQC.csv',sep=''), sep=',')
genetic_afterQC<-genetic_afterQC[c(2,3)]
colnames(genetic_afterQC)<-c("id","SNP1")

#add sex and genetic array(do not use array in all main analyses)
sex<-read.table(paste(Sys.getenv('Mydata'),'data.covariates.txt',sep=''))
head(sex)
sex<-sex[c(2:4)]
colnames(sex)<-c("id","sex","array")
genetic_covariate<-merge(genetic_afterQC,sex,by="id")
table(genetic_covariate$SNP1,genetic_covariate$sex,exclude = NULL)

#add top 10pc
pc10<-read.table(paste(Sys.getenv('Mydata'),'pc10.txt',sep=''))
pc10<-pc10[c(2:12)]
colnames(pc10)<-c("id","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")
genetic_covariate_pc10<-merge(genetic_covariate,pc10,by="id")
colnames(genetic_covariate_pc10)[1]<-"app8786"

#add maternal smoking and participant bw
phenotype<-read.csv(paste(Sys.getenv('Mydata'),'positive_control.csv',sep=''), sep=',')
head(phenotype)
colnames(phenotype)<-c("app16729","mumsmoke","bw")

#add matching code
match_code<-read.csv(paste(Sys.getenv('Mydata'),'data.7341.csv',sep=''), sep=',')
head(match_code)
genetic_covariate_pc10_match<-merge(genetic_covariate_pc10,match_code,by="app8786")

#new withdrawn
w<-read.csv(paste(Sys.getenv('Mydata'),'w16729_20181016.csv',sep=''), sep=',', header=FALSE)
colnames(w)<-"app16729"
d<-merge(genetic_covariate_pc10_match,w,by="app16729")
genetic_covariate_pc10_match2<-genetic_covariate_pc10_match[ ! genetic_covariate_pc10_match$app16729 %in% d$app16729, ]

proof<-merge(genetic_covariate_pc10_match2,phenotype,by="app16729")
summary(proof)
proof<-proof[which(proof$mumsmoke>=0),]


write.csv(proof,file=paste(Sys.getenv('Mydata'),'bw_only_correct.csv',sep=''))

###################################################################################################
###################################################################################################
#the following analyses use proof database
###################################################################################################
###################################################################################################
#read in database
proof<-read.csv(paste(Sys.getenv('Mydata'),'bw_only_correct.csv',sep=''), sep=',')
summary(proof)

#mean & SD by sex
aggregate(proof,by=list(proof$sex),FUN = mean,na.rm=TRUE)
aggregate(proof,by=list(proof$sex),FUN = sd,na.rm=TRUE)
table(proof$mumsmoke,proof$sex)

##in all participants
est_all_cru<-lm(proof$bw~proof$SNP1)
est_all_adj<-lm(proof$bw~.,data=proof[,c("SNP1","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])

#in non-smoker(mum)
proof_nonsmoke<-proof[which(as.numeric(proof$mumsmoke)==0),]
summary(proof_nonsmoke)
est_nonsmoke_cru<-lm(proof_nonsmoke$bw~proof_nonsmoke$SNP1)
est_nonsmoke_adj<-lm(proof_nonsmoke$bw~.,data=proof_nonsmoke[,c("SNP1","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])

#in smoker(mum)
proof_smoke<-proof[which(as.numeric(proof$mumsmoke)==1),]
summary(proof_smoke)
est_smoke_cru<-lm(proof_smoke$bw~proof_smoke$SNP1)
est_smoke_adj<-lm(proof_smoke$bw~.,data=proof_smoke[,c("SNP1","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")])

###################################################################################################
###################################################################################################
#save results in a file for plot
###################################################################################################
###################################################################################################
#creat a matrix for results
vars<-c("adjust-no","adjust-yes","adjust-all","crude-no","crude-yes","crude-all")
col.names <- c("supp","exposure","bw","se")
covar <- matrix(, ncol=4, nrow=length(vars))
dimnames(covar) <- list(vars, col.names)

#in the 1st colomn of matrix("supp"), "1" means crude model, and "2" means adjust model 
covar[,1] <- c(2,2,2,1,1,1)

#in the 2nd colomn of matrix("exposure"), "1" = subgroup of "G0 smoking=NO", "3" = subgroup of "G0 smoking=YES", "5" = all participants
covar[,2] <- c(1,3,5,1,3,5)

#extract beta
covar[1,3] <- est_nonsmoke_adj$coefficients['SNP1']
covar[2,3] <- est_smoke_adj$coefficients['SNP1']
covar[3,3] <- est_all_adj$coefficients['SNP1']
covar[4,3] <- est_nonsmoke_cru$coefficients['SNP1']
covar[5,3] <- est_smoke_cru$coefficients['SNP1']
covar[6,3] <- est_all_cru$coefficients['SNP1']

#extract SE
covar[1,4] <- coef(summary(est_nonsmoke_adj))['SNP1', "Std. Error"]
covar[2,4] <- coef(summary(est_smoke_adj))['SNP1', "Std. Error"]
covar[3,4] <- coef(summary(est_all_adj))['SNP1', "Std. Error"]
covar[4,4] <- coef(summary(est_nonsmoke_cru))['SNP1', "Std. Error"]
covar[5,4] <- coef(summary(est_smoke_cru))['SNP1', "Std. Error"]
covar[6,4] <- coef(summary(est_all_cru))['SNP1', "Std. Error"]


write.csv(covar,file=paste(Sys.getenv('Myresults'),'mini project3_plot/bw_ever_never.csv',sep=''))
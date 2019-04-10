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

#function, extract beta, se, lci and uci
linearregression <- function(database,modelname) {
  if(substring(modelname,1,5)=="crude"){
    fit = lm(database[,"bw"] ~ SNP1,data=database)
  }else{
    fit = lm(database[,"bw"] ~., data=database[,c("SNP1","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
  }
  sumx = summary(fit)
  beta = sumx$coefficients["SNP1","Estimate"]
  se = sumx$coefficients["SNP1","Std. Error"]
  cis = confint(fit, level=0.95)
  lower = cis["SNP1", "2.5 %"]
  upper = cis["SNP1", "97.5 %"]
  write.table(cbind(modelname,beta,se,lower,upper),file=paste(Sys.getenv('Myresults'),'mini project3_plot/bw_ever_never.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
}

##in all participants
linearregression(proof,"crude_all")
linearregression(proof,"adjust_all")

#in non-smoker(mum)
proof_nonsmoke<-proof[which(as.numeric(proof$mumsmoke)==0),]
linearregression(proof_nonsmoke,"crude_nonsmoke")
linearregression(proof_nonsmoke,"adjust_nonsmoke")

#in smoker(mum)
proof_smoke<-proof[which(as.numeric(proof$mumsmoke)==1),]
linearregression(proof_smoke,"crude_smoke")
linearregression(proof_smoke,"adjust_smoke")
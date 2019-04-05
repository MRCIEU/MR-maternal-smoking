#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#import G1 smoking data
smoking_check<-read.csv(paste(Sys.getenv('Mydata'),'maternal_smoking_women_participants.csv',sep=''), sep=',')
head(smoking_check)
colnames(smoking_check)<-c("app16729","live_b_no","bwchild","age_live_b","age_start_former","age_stop","age_start_current","smoking","age_stop_medical")


#import previous clean dataset
proof<-read.csv(paste(Sys.getenv('Mydata'),'bw_only_correct.csv',sep=''), sep=',')
summary(proof)
proof<-proof[c(2:18)]

smoke_pregnancy<-merge(proof,smoking_check,by="app16729")
summary(smoke_pregnancy)


#QC: only female participants who had live birth and reported their 1st child birthweight 
table(smoke_pregnancy$sex,exclude=NULL)
smoke_pregnancy_women<-smoke_pregnancy[ which(smoke_pregnancy$sex=='F'),]

table(smoke_pregnancy_women$live_b_no,exclude = NULL)
smoke_pregnancy_women_b<-smoke_pregnancy_women[ which(smoke_pregnancy_women$live_b_no>0),]

table(smoke_pregnancy_women_b$bwchild,exclude = NULL)
smoke_pregnancy_women_b_bw<-smoke_pregnancy_women_b[ which(smoke_pregnancy_women_b$bwchild>0),]

#code age < 0 as missing
smoke_pregnancy_women_b_bw$age_live_b[smoke_pregnancy_women_b_bw$age_live_b<0]<-NA
smoke_pregnancy_women_b_bw$age_start_current[smoke_pregnancy_women_b_bw$age_start_current<0]<-NA
smoke_pregnancy_women_b_bw$age_start_former[smoke_pregnancy_women_b_bw$age_start_former<0]<-NA
smoke_pregnancy_women_b_bw$age_stop[smoke_pregnancy_women_b_bw$age_stop<0]<-NA


#define G1 smoking during pregnancy ("0"=No, "1"=Yes)
#we define the following as "0"
#a. never smoker
smoke_pregnancy_women_b_bw$smoke_preg[smoke_pregnancy_women_b_bw$smoking==0]<-0
#b. current smoker who started smoking after the first live birth
smoke_pregnancy_women_b_bw$smoke_preg[smoke_pregnancy_women_b_bw$age_start_current>smoke_pregnancy_women_b_bw$age_live_b]<-0
#c. former smoker who started smoking after the first live birth
smoke_pregnancy_women_b_bw$smoke_preg[smoke_pregnancy_women_b_bw$age_start_former>smoke_pregnancy_women_b_bw$age_live_b]<-0
#d. former smoker who stopped smoking at least 2 years before the first live birth
smoke_pregnancy_women_b_bw$smoke_preg[smoke_pregnancy_women_b_bw$age_stop<smoke_pregnancy_women_b_bw$age_live_b-1]<-0

#we define the following as "1"
#a. current smoker who started smoking at least 1 year before the first live birth
smoke_pregnancy_women_b_bw$smoke_preg[smoke_pregnancy_women_b_bw$age_start_current<=smoke_pregnancy_women_b_bw$age_live_b-1]<-1
#b. former smoker who started smoking at least 1 year before the first live birth and stopped smoking after the first live birth 
smoke_pregnancy_women_b_bw$smoke_preg[smoke_pregnancy_women_b_bw$age_start_former<=smoke_pregnancy_women_b_bw$age_live_b-1 & smoke_pregnancy_women_b_bw$age_stop>smoke_pregnancy_women_b_bw$age_live_b]<-1

#we did not include edge cases in our analyses given the uncertainty 

write.csv(smoke_pregnancy_women_b_bw, file=paste(Sys.getenv('Mydata'),'maternal_smoking_women_participants_clean.csv',sep=''))

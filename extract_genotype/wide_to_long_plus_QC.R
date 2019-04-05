#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

###################################################################################
###################################################################################
#convert wide format to long format
###################################################################################
###################################################################################

#read in data
genetic_all<-read.table(paste(Sys.getenv('Mydata'),'smoke-dosage.txt',sep=''), sep=',')
genetic_all_use<-genetic_all[c(-1,-2,-3,-4,-5,-6)]

#reshape data from wide to long
require(reshape2)
genetic_all_use_long<-dcast(melt(as.matrix(genetic_all_use)), Var2~paste0('r', Var1), value.var='value')
table(genetic_all_use_long$r1)

#combine .sample file (the ID colomun) with the long format genetic data
id<-read.table(paste(Sys.getenv('Mydata'),'userIds.txt',sep=''), sep=',')
genetic_all_id<-cbind(genetic_all_use_long,id)
genetic_all_id<-genetic_all_id[c(3,2)]
colnames(genetic_all_id)<-c("id","SNP1")
table(genetic_all_id$SNP1)

write.csv(genetic_all_id,file=paste(Sys.getenv('Mydata'),'genetic_all_id.csv',sep=''))

###################################################################################
###################################################################################
#QC
###################################################################################
###################################################################################

#read in genetic data

genetic_all_id<-read.csv(paste(Sys.getenv('Mydata'),'genetic_all_id.csv',sep=''), sep=',')
genetic_all_id<-genetic_all_id[c(2,3)]
colnames(genetic_all_id)<-c("V1","SNP1")

#delete non-white British
nonwb<-read.table(paste(Sys.getenv('UKB_DATA'),'genetic/variants/arrays/500k/imputed/hrc/released/2017-07-04/data/derived/ancestry/data.non_white_british.txt',sep=''), sep=',')
head(nonwb)
summary(genetic_all_id)
genetic_all_nonwb<-merge(genetic_all_id,nonwb,by="V1",all=TRUE)
genetic_all_exclude_nonwb<-genetic_all_nonwb[ which(is.na(genetic_all_nonwb$V2)),]
genetic_all_exclude_nonwb<-genetic_all_exclude_nonwb[c(1:2)]

#delete related participants
minimal<-read.table(paste(Sys.getenv('UKB_DATA'),'genetic/variants/arrays/500k/imputed/hrc/released/2017-07-04/data/derived/relateds_exclusions/data.minimal_relateds.txt',sep=''), sep=',')
head(minimal)
genetic_all_minimal<-merge(genetic_all_exclude_nonwb,minimal,by="V1",all=TRUE)
genetic_all_exclude_minimal<-genetic_all_minimal[ which(is.na(genetic_all_minimal$V2)),]
genetic_all_exclude_minimal<-genetic_all_exclude_minimal[c(1:2)]

high_related<-read.table(paste(Sys.getenv('UKB_DATA'),'genetic/variants/arrays/500k/imputed/hrc/released/2017-07-04/data/derived/relateds_exclusions/data.highly_relateds.txt',sep=''), sep=',')
head(high_related)
genetic_all_high<-merge(genetic_all_exclude_minimal,high_related,by="V1",all=TRUE)
genetic_all_exclude_high<-genetic_all_high[ which(is.na(genetic_all_high$V2)),]
genetic_all_exclude_high<-genetic_all_exclude_high[c(1:2)]

# number of participants is 337,115 now
write.csv(genetic_all_exclude_high,file=paste(Sys.getenv('Mydata'),'genetic_afterQC.csv',sep=''))

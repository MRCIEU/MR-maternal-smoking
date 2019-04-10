#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#import lung function (best measure) data
best_measure<-read.csv(paste(Sys.getenv('Mydata'),'lung_function_best_measures.csv',sep=''), sep=',')
head(best_measure)
colnames(best_measure)<-c("app16729","fev1_best","fvc_best")


#import previous clean dataset
proof2<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''), sep=',')
head(proof2)
proof2<-proof2[c(2:4,6:20)]


#combine new outcome data with my previous dataset
proof3<-merge(proof2,best_measure,by="app16729")
summary(proof3)


#mean & SD by sex
aggregate(proof3,by=list(proof3$sex),FUN = mean,na.rm=TRUE)
aggregate(proof3,by=list(proof3$sex),FUN = sd,na.rm=TRUE)

###################################################################################################
###################################################################################################
#the following analyses use proof3 database
###################################################################################################
###################################################################################################
#function, extract beta, se, lci and uci
linearregression <- function(database,outcomename,modelname) {
  fit = lm(database[,outcomename] ~., data=database[,c("SNP1","age","sex","pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10")]) 
  sumx = summary(fit)
  beta = sumx$coefficients["SNP1","Estimate"]
  se = sumx$coefficients["SNP1","Std. Error"]
  cis = confint(fit, level=0.95)
  lower = cis["SNP1", "2.5 %"]
  upper = cis["SNP1", "97.5 %"]
  if(outcomename=="fvc_best"){
    write.table(cbind(modelname,beta,se,lower,upper),file=paste(Sys.getenv('Myresults'),'mini project3_plot/fvc_best.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
  }else{
    write.table(cbind(modelname,beta,se,lower,upper),file=paste(Sys.getenv('Myresults'),'mini project3_plot/fev1_best.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
  }
}

#in all participants
##subset participants: G0 smoking=No
mumnonsmoke_allchild<-proof3[which(proof3$mumsmoke==0),]
linearregression(mumnonsmoke_allchild,"fvc_best","mumnonsmoke_childall")
linearregression(mumnonsmoke_allchild,"fev1_best","mumnonsmoke_childall")
##subset participants: G0 smoking=Yes
mumsmoke_allchild<-proof3[which(proof3$mumsmoke==1),]
linearregression(mumsmoke_allchild,"fvc_best","mumsmoke_childall")
linearregression(mumsmoke_allchild,"fev1_best","mumsmoke_childall")

#in non-smoker(G0 & G1)
nonsmoke<-proof3[which(proof3$mumsmoke==0 & proof3$smoke==0),]
linearregression(nonsmoke,"fvc_best","mumnonsmoke_childnever")
linearregression(nonsmoke,"fev1_best","mumnonsmoke_childnever")

#G0=smoker, G1=never smoker
smoke_mum<-proof3[which(proof3$mumsmoke==1 & proof3$smoke==0),]
linearregression(smoke_mum,"fvc_best","mumsmoke_childnever")
linearregression(smoke_mum,"fev1_best","mumsmoke_childnever")

#G0=non-smoker, G1=former smoker
former_child<-proof3[which(proof3$mumsmoke==0 & proof3$smoke==1),]
linearregression(former_child,"fvc_best","mumnonsmoke_childformer")
linearregression(former_child,"fev1_best","mumnonsmoke_childformer")

#G0=non-smoker, G1=current smoker
current_child<-proof3[which(proof3$mumsmoke==0 & proof3$smoke==2),]
linearregression(current_child,"fvc_best","mumnonsmoke_childcurrent")
linearregression(current_child,"fev1_best","mumnonsmoke_childcurrent")

#G0=smoker, G1=former smoker
smoke_both_former<-proof3[which(proof3$mumsmoke==1 & proof3$smoke==1),]
linearregression(smoke_both_former,"fvc_best","mumsmoke_childformer")
linearregression(smoke_both_former,"fev1_best","mumsmoke_childformer")

#G0=smoker, G1=current smoker
smoke_both_current<-proof3[which(proof3$mumsmoke==1 & proof3$smoke==2),]
linearregression(smoke_both_current,"fvc_best","mumsmoke_childcurrent")
linearregression(smoke_both_current,"fev1_best","mumsmoke_childcurrent")
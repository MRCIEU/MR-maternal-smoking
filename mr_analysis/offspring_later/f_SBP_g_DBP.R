#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#import blood pressure data
bp<-read.csv(paste(Sys.getenv('Mydata'),'bp.csv',sep=''), sep=',')
summary(bp)
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
summary(bp)

#import previous clean dataset
proof2<-read.csv(paste(Sys.getenv('Mydata'),'offspring_bmi.csv',sep=''), sep=',')
head(proof2)
proof2<-proof2[c(2:4,6:20)]


#combine new outcome data with my previous dataset
proof3<-merge(proof2,bp,by="app16729")
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
  if(outcomename=="sbp"){
    write.table(cbind(modelname,beta,se,lower,upper),file=paste(Sys.getenv('Myresults'),'mini project3_plot/sbp.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
  }else{
    write.table(cbind(modelname,beta,se,lower,upper),file=paste(Sys.getenv('Myresults'),'mini project3_plot/dbp.csv',sep=''), append=TRUE, quote=FALSE, sep=',',row.names=FALSE, col.names=FALSE)
  }
}

#in all participants
##subset participants: G0 smoking=NO
mumnonsmoke_allchild<-proof3[which(proof3$mumsmoke==0),]
linearregression(mumnonsmoke_allchild,"sbp","mumnonsmoke_childall")
linearregression(mumnonsmoke_allchild,"dbp","mumnonsmoke_childall")
##subset participants: G0 smoking=YES
mumsmoke_allchild<-proof3[which(proof3$mumsmoke==1),]
linearregression(mumsmoke_allchild,"sbp","mumsmoke_childall")
linearregression(mumsmoke_allchild,"dbp","mumsmoke_childall")

#in non-smoker(G0 & G1)
nonsmoke<-proof3[which(proof3$mumsmoke==0 & proof3$smoke==0),]
linearregression(nonsmoke,"sbp","mumnonsmoke_childnever")
linearregression(nonsmoke,"dbp","mumnonsmoke_childnever")

#G0=smoker, G1=never smoker
smoke_mum<-proof3[which(proof3$mumsmoke==1 & proof3$smoke==0),]
linearregression(smoke_mum,"sbp","mumsmoke_childnever")
linearregression(smoke_mum,"dbp","mumsmoke_childnever")

#G0=non-smoker, G1=former smoker
former_child<-proof3[which(proof3$mumsmoke==0 & proof3$smoke==1),]
linearregression(former_child,"sbp","mumnonsmoke_childformer")
linearregression(former_child,"dbp","mumnonsmoke_childformer")

#G0=non-smoker, G1=current smoker
current_child<-proof3[which(proof3$mumsmoke==0 & proof3$smoke==2),]
linearregression(current_child,"sbp","mumnonsmoke_childcurrent")
linearregression(current_child,"dbp","mumnonsmoke_childcurrent")

#G0=smoker, G1=former smoker
smoke_both_former<-proof3[which(proof3$mumsmoke==1 & proof3$smoke==1),]
linearregression(smoke_both_former,"sbp","mumsmoke_childformer")
linearregression(smoke_both_former,"dbp","mumsmoke_childformer")

#G0=smoker, G1=current smoker
smoke_both_current<-proof3[which(proof3$mumsmoke==1 & proof3$smoke==2),]
linearregression(smoke_both_current,"sbp","mumsmoke_childcurrent")
linearregression(smoke_both_current,"dbp","mumsmoke_childcurrent")
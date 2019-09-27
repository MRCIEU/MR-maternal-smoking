# Clear the work environment
rm(list = ls())

#require package
require(metafor)

#input simulation results (20 csv files)
#each file has 1000 rows which represent 1000 times simulations
#each row contains estimates for G0/G1 rs16969968 - G1 outcome associations in each strata
i<-1
for(i in 1:20){
  oname = paste("b", i, sep="")
  assign(oname, read.csv(paste(Sys.getenv('Mysimulation'),oname,".csv", sep=""), sep=','))}

#function to obtain P-value of Cochran's Q
gbye<-function(b_s,se_s,b_n,se_n,design){
  #get the dataframe ready for meta-analysis
  heterogeniety<-data.frame("strata"=c("smoker","nonsmoker"),"beta"=c(b_s,b_n),"se"=c(se_s,se_n))
  #conduct a meta-analysis to assess the heterogeniety between G0 smoking strata
  q<-rma(yi=beta, sei=se, slab=strata, method="FE", data=heterogeniety)
  #extract the p-value of the heterogeniety test
  pval<-q$QEp
  #output the p-value
  write.table(pval,file=paste(Sys.getenv('Mysimulation'),design,'.csv',sep=''),append=TRUE,quote=FALSE,sep=',',row.names=FALSE,col.names=FALSE)}

#function to prepare dataframes for Cochran's Q tests  
gepwr1<-function(database){
  colnames(database)<-c("No","b_s","se_s","b_n","se_n","b_s_proxy","se_s_proxy","b_n_proxy","se_n_proxy")
  m<-1
  for (m in 1:1000){
    #for GbyE MR
    gbye(database[,"b_s"][m],database[,"se_s"][m],database[,"b_n"][m],database[,"se_n"][m],"GbyE")
    #for proxy GbyE MR
    gbye(database[,"b_s_proxy"][m],database[,"se_s_proxy"][m],database[,"b_n_proxy"][m],database[,"se_n_proxy"][m],"proxy")}}
 
#calculate p-values
#each line adds 1000 p-values into GbyE.csv file, and 1000 p-values into proxy.csv file
#N=10^5, betaG=0.01
gepwr1(b1)
#N=5*10^5, betaG=0.01
gepwr1(b2)
#N=10^6, betaG=0.01
gepwr1(b3)
#N=5*10^6, betaG=0.01
gepwr1(b4)

#N=10^5, betaG=0.025
gepwr1(b5)
#N=5*10^5, betaG=0.025
gepwr1(b6)
#N=10^6, betaG=0.025
gepwr1(b7)
#N=5*10^6, betaG=0.025
gepwr1(b8)

#N=10^5, betaG=0.05
gepwr1(b9)
#N=5*10^5, betaG=0.05
gepwr1(b10)
#N=10^6, betaG=0.05
gepwr1(b11)
#N=5*10^6, betaG=0.05
gepwr1(b12)

#N=10^5, betaG=0.075
gepwr1(b13)
#N=5*10^5, betaG=0.075
gepwr1(b14)
#N=10^6, betaG=0.075
gepwr1(b15)
#N=5*10^6, betaG=0.075
gepwr1(b16)

#N=10^5, betaG=0.1
gepwr1(b17)
#N=5*10^5, betaG=0.1
gepwr1(b18)
#N=10^6, betaG=0.1
gepwr1(b19)
#N=5*10^6, betaG=0.1
gepwr1(b20)
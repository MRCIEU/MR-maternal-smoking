# Clear the work environment
rm(list = ls())

#require package
require(metafor)

#input simulation results
#each file has 1000 rows which represent 1000 times simulations
#each row contains estimates for G0/G1 rs16969968 - G1 outcome associations in each strata
n<-1
for(n in 1:20){
  oname = paste("d", n, sep="")
  assign(oname, read.csv(paste(Sys.getenv('Mysimulation'),oname,".csv", sep=""), sep=','))}

#function to obtain P-value of Cochran's Q
gbye<-function(b_s,se_s,b_n,se_n,design){
  #get the dataframe ready for meta-analysis
  heterogeniety<-data.frame("strata"=c("g0smoker","g0nonsmoker"),"beta"=c(b_s,b_n),"se"=c(se_s,se_n))
  #conduct a meta-analysis to assess the heterogeniety between G0 smoking strata
  q<-rma(yi=beta, sei=se, slab=strata, method="FE", data=heterogeniety)
  #extract the p-value of the heterogeniety test
  pval<-q$QEp
  #output the p-value
  write.table(pval,file=paste(Sys.getenv('Mysimulation'),design,'.csv',sep=''),append=TRUE,quote=FALSE,sep=',',row.names=FALSE,col.names=FALSE)}

#function to prepare dataframes for Cochran's Q tests 
gepwr2<-function(database){
  colnames(database)<-c("No","b_g0sg1s","se_g0sg1s","b_g0ng1s","se_g0ng1s","b_g0sg1n","se_g0sg1n","b_g0ng1n","se_g0ng1n","pb_g0sg1s","pse_g0sg1s","pb_g0ng1s","pse_g0ng1s","pb_g0sg1n","pse_g0sg1n","pb_g0ng1n","pse_g0ng1n")
  m<-1
  for (m in 1:1000){
    #for GbyE MR, in G1 smokers
    gbye(database[,"b_g0sg1s"][m],database[,"se_g0sg1s"][m],database[,"b_g0ng1s"][m],database[,"se_g0ng1s"][m],"GbyEG1smoker")
    #for GbyE MR, in G1 non-smokers
    gbye(database[,"b_g0sg1n"][m],database[,"se_g0sg1n"][m],database[,"b_g0ng1n"][m],database[,"se_g0ng1n"][m],"GbyEG1nonsmoker")
    #for proxy GbyE MR in G1 smokers
    gbye(database[,"pb_g0sg1s"][m],database[,"pse_g0sg1s"][m],database[,"pb_g0ng1s"][m],database[,"pse_g0ng1s"][m],"proxyG1smoker")
    #for proxy GbyE MR in G1 non-smokers
    gbye(database[,"pb_g0sg1n"][m],database[,"pse_g0sg1n"][m],database[,"pb_g0ng1n"][m],database[,"pse_g0ng1n"][m],"proxyG1nonsmoker")}}

#calculate p-values
#each line adds 2000 p-values into GbyE.csv file, and 2000 p-values into proxy.csv file
#N=10^5, betaG0=0.01, betaG1=0.1
gepwr2(d1)
#N=5*10^5, betaG0=0.01, betaG1=0.1
gepwr2(d2)
#N=10^6, betaG0=0.01, betaG1=0.1
gepwr2(d3)
#N=5*10^6, betaG0=0.01, betaG1=0.1
gepwr2(d4)

#N=10^5, betaG0=0.025, betaG1=0.1
gepwr2(d5)
#N=5*10^5, betaG0=0.025, betaG1=0.1
gepwr2(d6)
#N=10^6, betaG0=0.025, betaG1=0.1
gepwr2(d7)
#N=5*10^6, betaG0=0.025, betaG1=0.1
gepwr2(d8)

#N=10^5, betaG0=0.05, betaG1=0.1
gepwr2(d9)
#N=5*10^5, betaG0=0.05, betaG1=0.1
gepwr2(d10)
#N=10^6, betaG0=0.05, betaG1=0.1
gepwr2(d11)
#N=5*10^6, betaG0=0.05, betaG1=0.1
gepwr2(d12)

#N=10^5, betaG0=0.075, betaG1=0.1
gepwr2(d13)
#N=5*10^5, betaG0=0.075, betaG1=0.1
gepwr2(d14)
#N=10^6, betaG0=0.075, betaG1=0.1
gepwr2(d15)
#N=5*10^6, betaG0=0.075, betaG1=0.1
gepwr2(d16)

#N=10^5, betaG0=0.1, betaG1=0.1
gepwr2(d17)
#N=5*10^5, betaG0=0.1, betaG1=0.1
gepwr2(d18)
#N=10^6, betaG0=0.1, betaG1=0.1
gepwr2(d19)
#N=5*10^6, betaG0=0.1, betaG1=0.1
gepwr2(d20)
#############################################################################################################
#############################################################################################################
#create a function to do one simulation
#############################################################################################################
#############################################################################################################
#N=sample size
#betaG0=effect size for the smoking heaviness~outcomes association (SD in outcome/cigarette)
delution<-function(N,betaG0){
  #create one dataset
  data1m<-data.frame(matrix(ncol=0,nrow=N))
  data1m$id<-c(1:N)
  
  #generate maternal SNP (rs16969968 EAF = 0.33)
  data1m$g0snp<-sample(0:2,N, replace=T,prob=c(0.4489,0.4422,0.1089))
  #generate offspring SNP
  data1m$g1snp[data1m$g0snp==0]<-rbinom(length(data1m$g0snp[data1m$g0snp==0]),1,0.33)
  data1m$g1snp[data1m$g0snp==1]<-sample(0:2,length(data1m$g0snp[data1m$g0snp==1]), replace=T,prob=c(0.335,0.5,0.165))
  data1m$g1snp[data1m$g0snp==2]<-rbinom(length(data1m$g0snp[data1m$g0snp==2]),1,0.33)+1
  
  #maternal smoking status in pregnancy: 0=non-smokers, 1=smokers; 30.53% smokers in UKB
  data1m$g0smoke<-rbinom(N,1,0.3053)
  
  #maternal smoking heaviness in pregnancy
  #0 in non-smokers (not used in the code below)
  #>0 in smokers, 1 cigarette/day/allele (PMID: 22534784)
  data1m$g0heaviness[data1m$g0smoke==1]<-rlnorm(length(data1m$g0smoke[data1m$g0smoke==1]),2.8,0.5)+data1m$g0snp[data1m$g0smoke==1]*1
  #make it into integer
  data1m$g0heaviness<-floor(data1m$g0heaviness)
  
  #G1 continuous outcome 
  #in non-smokers  
  data1m$g1outcome1[data1m$g0smoke==0]<-rnorm(length(data1m$g0smoke[data1m$g0smoke==0]),0,1)
  #in smokers
  data1m$g1outcome1[data1m$g0smoke==1]<-rnorm(length(data1m$g0smoke[data1m$g0smoke==1]),0,1)+data1m$g0heaviness[data1m$g0smoke==1]*betaG0
  
  #regression
  #subset of smokers
  data1m_g0smoke<-data1m[which(data1m$g0smoke==1),]
  #subset of non-smokers
  data1m_g0nonsmoke<-data1m[which(data1m$g0smoke==0),]
  
  #G*E MR
  g0smoke_g0snp<-lm(g1outcome1~g0snp,data=data1m_g0smoke)
  g0nonsmoke_g0snp<-lm(g1outcome1~g0snp,data=data1m_g0nonsmoke)
  #G*E MR results
  beta_smoke = summary(g0smoke_g0snp)$coefficients["g0snp","Estimate"]
  se_smoke = summary(g0smoke_g0snp)$coefficients["g0snp","Std. Error"]
  beta_nonsmoke = summary(g0nonsmoke_g0snp)$coefficients["g0snp","Estimate"]
  se_nonsmoke = summary(g0nonsmoke_g0snp)$coefficients["g0snp","Std. Error"]
  
  #proxy G*E MR
  g0smoke_g1snp<-lm(g1outcome1~g1snp,data=data1m_g0smoke)
  g0nonsmoke_g1snp<-lm(g1outcome1~g1snp,data=data1m_g0nonsmoke)
  #Proxy G*E MR results
  beta_smoke_proxy = summary(g0smoke_g1snp)$coefficients["g1snp","Estimate"]
  se_smoke_proxy = summary(g0smoke_g1snp)$coefficients["g1snp","Std. Error"]
  beta_nonsmoke_proxy = summary(g0nonsmoke_g1snp)$coefficients["g1snp","Estimate"]
  se_nonsmoke_proxy = summary(g0nonsmoke_g1snp)$coefficients["g1snp","Std. Error"]
  
  #output all estimates in one row
  cbind(beta_smoke,se_smoke,beta_nonsmoke,se_nonsmoke,beta_smoke_proxy,se_smoke_proxy,beta_nonsmoke_proxy,se_nonsmoke_proxy)
}
#############################################################################################################
#############################################################################################################
#Simulate 1000 times
#############################################################################################################
#############################################################################################################
set.seed(123456)
Nrep<-1000

#N=10^5, betaG=0.01
a1<-replicate(n=Nrep,delution(10^5,0.01))
b1<-data.frame(matrix(unlist(a1), nrow=Nrep, byrow=T))
#N=5*10^5, betaG=0.01
a2<-replicate(n=Nrep,delution(5*10^5,0.01))
b2<-data.frame(matrix(unlist(a2), nrow=Nrep, byrow=T))
#N=10^6, betaG=0.01
a3<-replicate(n=Nrep,delution(10^6,0.01))
b3<-data.frame(matrix(unlist(a3), nrow=Nrep, byrow=T))
#N=5*10^6, betaG=0.01
a4<-replicate(n=Nrep,delution(5*10^6,0.01))
b4<-data.frame(matrix(unlist(a4), nrow=Nrep, byrow=T))

#N=10^5, betaG=0.025
a5<-replicate(n=Nrep,delution(10^5,0.025))
b5<-data.frame(matrix(unlist(a5), nrow=Nrep, byrow=T))
#N=5*10^5, betaG=0.025
a6<-replicate(n=Nrep,delution(5*10^5,0.025))
b6<-data.frame(matrix(unlist(a6), nrow=Nrep, byrow=T))
#N=10^6, betaG=0.025
a7<-replicate(n=Nrep,delution(10^6,0.025))
b7<-data.frame(matrix(unlist(a7), nrow=Nrep, byrow=T))
#N=5*10^6, betaG=0.025
a8<-replicate(n=Nrep,delution(5*10^6,0.025))
b8<-data.frame(matrix(unlist(a8), nrow=Nrep, byrow=T))

#N=10^5, betaG=0.05
a9<-replicate(n=Nrep,delution(10^5,0.05))
b9<-data.frame(matrix(unlist(a9), nrow=Nrep, byrow=T))
#N=5*10^5, betaG=0.05
a10<-replicate(n=Nrep,delution(5*10^5,0.05))
b10<-data.frame(matrix(unlist(a10), nrow=Nrep, byrow=T))
#N=10^6, betaG=0.05
a11<-replicate(n=Nrep,delution(10^6,0.05))
b11<-data.frame(matrix(unlist(a11), nrow=Nrep, byrow=T))
#N=5*10^6, betaG=0.05
a12<-replicate(n=Nrep,delution(5*10^6,0.05))
b12<-data.frame(matrix(unlist(a12), nrow=Nrep, byrow=T))

#N=10^5, betaG=0.075
a13<-replicate(n=Nrep,delution(10^5,0.075))
b13<-data.frame(matrix(unlist(a13), nrow=Nrep, byrow=T))
#N=5*10^5, betaG=0.075
a14<-replicate(n=Nrep,delution(5*10^5,0.075))
b14<-data.frame(matrix(unlist(a14), nrow=Nrep, byrow=T))
#N=10^6, betaG=0.075
a15<-replicate(n=Nrep,delution(10^6,0.075))
b15<-data.frame(matrix(unlist(a15), nrow=Nrep, byrow=T))
#N=5*10^6, betaG=0.075
a16<-replicate(n=Nrep,delution(5*10^6,0.075))
b16<-data.frame(matrix(unlist(a16), nrow=Nrep, byrow=T))

#N=10^5, betaG=0.1
a17<-replicate(n=Nrep,delution(10^5,0.1))
b17<-data.frame(matrix(unlist(a17), nrow=Nrep, byrow=T))
#N=5*10^5, betaG=0.1
a18<-replicate(n=Nrep,delution(5*10^5,0.1))
b18<-data.frame(matrix(unlist(a18), nrow=Nrep, byrow=T))
#N=10^6, betaG=0.1
a19<-replicate(n=Nrep,delution(10^6,0.1))
b19<-data.frame(matrix(unlist(a19), nrow=Nrep, byrow=T))
#N=5*10^6, betaG=0.1
a20<-replicate(n=Nrep,delution(5*10^6,0.1))
b20<-data.frame(matrix(unlist(a20), nrow=Nrep, byrow=T))

#save results
#each csv file has 1000 rows reflecting 1000 times simulations
write.csv(b1,file=paste(Sys.getenv('simulation_DATA'),'b1.csv',sep=''))
write.csv(b2,file=paste(Sys.getenv('simulation_DATA'),'b2.csv',sep=''))
write.csv(b3,file=paste(Sys.getenv('simulation_DATA'),'b3.csv',sep=''))
write.csv(b4,file=paste(Sys.getenv('simulation_DATA'),'b4.csv',sep=''))
write.csv(b5,file=paste(Sys.getenv('simulation_DATA'),'b5.csv',sep=''))
write.csv(b6,file=paste(Sys.getenv('simulation_DATA'),'b6.csv',sep=''))
write.csv(b7,file=paste(Sys.getenv('simulation_DATA'),'b7.csv',sep=''))
write.csv(b8,file=paste(Sys.getenv('simulation_DATA'),'b8.csv',sep=''))
write.csv(b9,file=paste(Sys.getenv('simulation_DATA'),'b9.csv',sep=''))
write.csv(b10,file=paste(Sys.getenv('simulation_DATA'),'b10.csv',sep=''))
write.csv(b11,file=paste(Sys.getenv('simulation_DATA'),'b11.csv',sep=''))
write.csv(b12,file=paste(Sys.getenv('simulation_DATA'),'b12.csv',sep=''))
write.csv(b13,file=paste(Sys.getenv('simulation_DATA'),'b13.csv',sep=''))
write.csv(b14,file=paste(Sys.getenv('simulation_DATA'),'b14.csv',sep=''))
write.csv(b15,file=paste(Sys.getenv('simulation_DATA'),'b15.csv',sep=''))
write.csv(b16,file=paste(Sys.getenv('simulation_DATA'),'b16.csv',sep=''))
write.csv(b17,file=paste(Sys.getenv('simulation_DATA'),'b17.csv',sep=''))
write.csv(b18,file=paste(Sys.getenv('simulation_DATA'),'b18.csv',sep=''))
write.csv(b19,file=paste(Sys.getenv('simulation_DATA'),'b19.csv',sep=''))
write.csv(b20,file=paste(Sys.getenv('simulation_DATA'),'b20.csv',sep=''))
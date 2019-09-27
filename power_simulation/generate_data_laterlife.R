#############################################################################################################
#############################################################################################################
#create a function to do one simulation
#############################################################################################################
#############################################################################################################
#N=sample size
#betaG0=effect size for G0 smoking heaviness~outcomes association (SD in outcome/cigarette)
#betaG1=effect size for G1 smoking heaviness~outcomes association (SD in outcome/cigarette)
delution<-function(N,betaG0,betaG1){
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
  ##offspring smoking status: 0=non-smokers, 1=smokers; 44.10% smokers in UKB
  data1m$g1smoke<-rbinom(N,1,0.4410)
  
  #maternal smoking heaviness in pregnancy
  #0 in non-smokers (not used in the code below)
  #>0 in smokers, 1 cigarette/day/allele (PMID: 22534784)
  data1m$g0heaviness[data1m$g0smoke==1]<-rlnorm(length(data1m$g0smoke[data1m$g0smoke==1]),2.8,0.5)+data1m$g0snp[data1m$g0smoke==1]*1
  #make it into integer
  data1m$g0heaviness<-floor(data1m$g0heaviness)
  
  #offspring smoking heaviness
  #0 in non-smokers (not used in the code below)
  #>0 in smokers, 1 cigarette/day/allele (PMID: 22534784)
  data1m$g1heaviness[data1m$g1smoke==1]<-rlnorm(length(data1m$g1smoke[data1m$g1smoke==1]),2.8,0.5)+data1m$g1snp[data1m$g1smoke==1]*1
  #make it into integer
  data1m$g1heaviness<-floor(data1m$g1heaviness)  
  
  #G1 continuous outcome 
  #in G0 & G1 non-smokers 
  data1m$g1outcome1[data1m$g0smoke==0 & data1m$g1smoke==0]<-rnorm(length(data1m$g0smoke[data1m$g0smoke==0 & data1m$g1smoke==0]),0,1) 
  #in G0 smokers/G1 non-smokers
  data1m$g1outcome1[data1m$g0smoke==1 & data1m$g1smoke==0]<-rnorm(length(data1m$g0smoke[data1m$g0smoke==1 & data1m$g1smoke==0]),0,1)+data1m$g0heaviness[data1m$g0smoke==1 & data1m$g1smoke==0]*betaG0
  #in G0 non-smokers/G1 smokers
  data1m$g1outcome1[data1m$g0smoke==0 & data1m$g1smoke==1]<-rnorm(length(data1m$g0smoke[data1m$g0smoke==0 & data1m$g1smoke==1]),0,1)+data1m$g1heaviness[data1m$g0smoke==0 & data1m$g1smoke==1]*betaG1
  #in G0 & G1 smokers
  data1m$g1outcome1[data1m$g0smoke==1 & data1m$g1smoke==1]<-rnorm(length(data1m$g0smoke[data1m$g0smoke==1 & data1m$g1smoke==1]),0,1)+data1m$g0heaviness[data1m$g0smoke==1 & data1m$g1smoke==1]*betaG0+data1m$g1heaviness[data1m$g0smoke==1 & data1m$g1smoke==1]*betaG1
  
  #regression
  #subset of G0 & G1 non-smokers
  data1m_g0nonsmoke_g1nonsmoke<-data1m[which(data1m$g0smoke==0 & data1m$g1smoke==0),]
  #subset of G0 smokers/G1 non-smokers
  data1m_g0smoke_g1nonsmoke<-data1m[which(data1m$g0smoke==1 & data1m$g1smoke==0),]
  #subset of G0 non-smokers/G1 smokers
  data1m_g0nonsmoke_g1smoke<-data1m[which(data1m$g0smoke==0 & data1m$g1smoke==1),]
  #subset of G0 & G1 smokers
  data1m_g0smoke_g1smoke<-data1m[which(data1m$g0smoke==1 & data1m$g1smoke==1),]
  
  #G*E MR
  g0smokeg1smoke_g0snp<-lm(g1outcome1~g0snp,data=data1m_g0smoke_g1smoke)
  g0nonsmokeg1smoke_g0snp<-lm(g1outcome1~g0snp,data=data1m_g0nonsmoke_g1smoke)
  g0smokeg1nonsmoke_g0snp<-lm(g1outcome1~g0snp,data=data1m_g0smoke_g1nonsmoke)
  g0nonsmokeg1nonsmoke_g0snp<-lm(g1outcome1~g0snp,data=data1m_g0nonsmoke_g1nonsmoke)
  #G*E MR results
  beta_g0smokeg1smoke = summary(g0smokeg1smoke_g0snp)$coefficients["g0snp","Estimate"]
  se_g0smokeg1smoke = summary(g0smokeg1smoke_g0snp)$coefficients["g0snp","Std. Error"]
  beta_g0nonsmokeg1smoke = summary(g0nonsmokeg1smoke_g0snp)$coefficients["g0snp","Estimate"]
  se_g0nonsmokeg1smoke = summary(g0nonsmokeg1smoke_g0snp)$coefficients["g0snp","Std. Error"]
  beta_g0smokeg1nonsmoke = summary(g0smokeg1nonsmoke_g0snp)$coefficients["g0snp","Estimate"]
  se_g0smokeg1nonsmoke = summary(g0smokeg1nonsmoke_g0snp)$coefficients["g0snp","Std. Error"]
  beta_g0nonsmokeg1nonsmoke = summary(g0nonsmokeg1nonsmoke_g0snp)$coefficients["g0snp","Estimate"]
  se_g0nonsmokeg1nonsmoke = summary(g0nonsmokeg1nonsmoke_g0snp)$coefficients["g0snp","Std. Error"]
  
  #proxy G*E MR
  g0smokeg1smoke_g1snp<-lm(g1outcome1~g1snp,data=data1m_g0smoke_g1smoke)
  g0nonsmokeg1smoke_g1snp<-lm(g1outcome1~g1snp,data=data1m_g0nonsmoke_g1smoke)
  g0smokeg1nonsmoke_g1snp<-lm(g1outcome1~g1snp,data=data1m_g0smoke_g1nonsmoke)
  g0nonsmokeg1nonsmoke_g1snp<-lm(g1outcome1~g1snp,data=data1m_g0nonsmoke_g1nonsmoke)
  #proxy G*E MR results
  proxy_beta_g0smokeg1smoke = summary(g0smokeg1smoke_g1snp)$coefficients["g1snp","Estimate"]
  proxy_se_g0smokeg1smoke = summary(g0smokeg1smoke_g1snp)$coefficients["g1snp","Std. Error"]
  proxy_beta_g0nonsmokeg1smoke = summary(g0nonsmokeg1smoke_g1snp)$coefficients["g1snp","Estimate"]
  proxy_se_g0nonsmokeg1smoke = summary(g0nonsmokeg1smoke_g1snp)$coefficients["g1snp","Std. Error"]
  proxy_beta_g0smokeg1nonsmoke = summary(g0smokeg1nonsmoke_g1snp)$coefficients["g1snp","Estimate"]
  proxy_se_g0smokeg1nonsmoke = summary(g0smokeg1nonsmoke_g1snp)$coefficients["g1snp","Std. Error"]
  proxy_beta_g0nonsmokeg1nonsmoke = summary(g0nonsmokeg1nonsmoke_g1snp)$coefficients["g1snp","Estimate"]
  proxy_se_g0nonsmokeg1nonsmoke = summary(g0nonsmokeg1nonsmoke_g1snp)$coefficients["g1snp","Std. Error"]
  
  #output all estimates in one row
  cbind(beta_g0smokeg1smoke,se_g0smokeg1smoke,beta_g0nonsmokeg1smoke,se_g0nonsmokeg1smoke,beta_g0smokeg1nonsmoke,se_g0smokeg1nonsmoke,beta_g0nonsmokeg1nonsmoke,se_g0nonsmokeg1nonsmoke,proxy_beta_g0smokeg1smoke,proxy_se_g0smokeg1smoke,proxy_beta_g0nonsmokeg1smoke,proxy_se_g0nonsmokeg1smoke,proxy_beta_g0smokeg1nonsmoke,proxy_se_g0smokeg1nonsmoke,proxy_beta_g0nonsmokeg1nonsmoke,proxy_se_g0nonsmokeg1nonsmoke)
}
#############################################################################################################
#############################################################################################################
#Simulate 1000 times
#############################################################################################################
#############################################################################################################
set.seed(123456)
Nrep<-1000

#N=10^5, betaG0=0.01, betaG1=0.1
c1<-replicate(n=Nrep,delution(10^5,0.01,0.1))
d1<-data.frame(matrix(unlist(c1), nrow=Nrep, byrow=T))
#N=5*10^5, betaG0=0.01, betaG1=0.1
c2<-replicate(n=Nrep,delution(5*10^5,0.01,0.1))
d2<-data.frame(matrix(unlist(c2), nrow=Nrep, byrow=T))
#N=10^6, betaG0=0.01, betaG1=0.1
c3<-replicate(n=Nrep,delution(10^6,0.01,0.1))
d3<-data.frame(matrix(unlist(c3), nrow=Nrep, byrow=T))
#N=5*10^6, betaG0=0.01, betaG1=0.1
c4<-replicate(n=Nrep,delution(5*10^6,0.01,0.1))
d4<-data.frame(matrix(unlist(c4), nrow=Nrep, byrow=T))

#N=10^5, betaG0=0.025, betaG1=0.1
c5<-replicate(n=Nrep,delution(10^5,0.025,0.1))
d5<-data.frame(matrix(unlist(c5), nrow=Nrep, byrow=T))
#N=5*10^5, betaG0=0.025, betaG1=0.1
c6<-replicate(n=Nrep,delution(5*10^5,0.025,0.1))
d6<-data.frame(matrix(unlist(c6), nrow=Nrep, byrow=T))
#N=10^6, betaG0=0.025, betaG1=0.1
c7<-replicate(n=Nrep,delution(10^6,0.025,0.1))
d7<-data.frame(matrix(unlist(c7), nrow=Nrep, byrow=T))
#N=5*10^6, betaG0=0.025, betaG1=0.1
c8<-replicate(n=Nrep,delution(5*10^6,0.025,0.1))
d8<-data.frame(matrix(unlist(c8), nrow=Nrep, byrow=T))

#N=10^5, betaG0=0.05, betaG1=0.1
c9<-replicate(n=Nrep,delution(10^5,0.05,0.1))
d9<-data.frame(matrix(unlist(c9), nrow=Nrep, byrow=T))
#N=5*10^5, betaG0=0.05, betaG1=0.1
c10<-replicate(n=Nrep,delution(5*10^5,0.05,0.1))
d10<-data.frame(matrix(unlist(c10), nrow=Nrep, byrow=T))
#N=10^6, betaG0=0.05, betaG1=0.1
c11<-replicate(n=Nrep,delution(10^6,0.05,0.1))
d11<-data.frame(matrix(unlist(c11), nrow=Nrep, byrow=T))
#N=5*10^6, betaG0=0.05, betaG1=0.1
c12<-replicate(n=Nrep,delution(5*10^6,0.05,0.1))
d12<-data.frame(matrix(unlist(c12), nrow=Nrep, byrow=T))

#N=10^5, betaG0=0.075, betaG1=0.1
c13<-replicate(n=Nrep,delution(10^5,0.075,0.1))
d13<-data.frame(matrix(unlist(c13), nrow=Nrep, byrow=T))
#N=5*10^5, betaG0=0.075, betaG1=0.1
c14<-replicate(n=Nrep,delution(5*10^5,0.075,0.1))
d14<-data.frame(matrix(unlist(c14), nrow=Nrep, byrow=T))
#N=10^6, betaG0=0.075, betaG1=0.1
c15<-replicate(n=Nrep,delution(10^6,0.075,0.1))
d15<-data.frame(matrix(unlist(c15), nrow=Nrep, byrow=T))
#N=5*10^6, betaG0=0.075, betaG1=0.1
c16<-replicate(n=Nrep,delution(5*10^6,0.075,0.1))
d16<-data.frame(matrix(unlist(c16), nrow=Nrep, byrow=T))

#N=10^5, betaG0=0.1, betaG1=0.1
c17<-replicate(n=Nrep,delution(10^5,0.1,0.1))
d17<-data.frame(matrix(unlist(c17), nrow=Nrep, byrow=T))
#N=5*10^5, betaG0=0.1, betaG1=0.1
c18<-replicate(n=Nrep,delution(5*10^5,0.1,0.1))
d18<-data.frame(matrix(unlist(c18), nrow=Nrep, byrow=T))
#N=10^6, betaG0=0.1, betaG1=0.1
c19<-replicate(n=Nrep,delution(10^6,0.1,0.1))
d19<-data.frame(matrix(unlist(c19), nrow=Nrep, byrow=T))
#N=5*10^6, betaG0=0.1, betaG1=0.1
c20<-replicate(n=Nrep,delution(5*10^6,0.1,0.1))
d20<-data.frame(matrix(unlist(c20), nrow=Nrep, byrow=T))

#save results
#each csv file has 1000 rows reflecting 1000 times simulations
write.csv(d1,file=paste(Sys.getenv('simulation_DATA'),'d1.csv',sep=''))
write.csv(d2,file=paste(Sys.getenv('simulation_DATA'),'d2.csv',sep=''))
write.csv(d3,file=paste(Sys.getenv('simulation_DATA'),'d3.csv',sep=''))
write.csv(d4,file=paste(Sys.getenv('simulation_DATA'),'d4.csv',sep=''))
write.csv(d5,file=paste(Sys.getenv('simulation_DATA'),'d5.csv',sep=''))
write.csv(d6,file=paste(Sys.getenv('simulation_DATA'),'d6.csv',sep=''))
write.csv(d7,file=paste(Sys.getenv('simulation_DATA'),'d7.csv',sep=''))
write.csv(d8,file=paste(Sys.getenv('simulation_DATA'),'d8.csv',sep=''))
write.csv(d9,file=paste(Sys.getenv('simulation_DATA'),'d9.csv',sep=''))
write.csv(d10,file=paste(Sys.getenv('simulation_DATA'),'d10.csv',sep=''))
write.csv(d11,file=paste(Sys.getenv('simulation_DATA'),'d11.csv',sep=''))
write.csv(d12,file=paste(Sys.getenv('simulation_DATA'),'d12.csv',sep=''))
write.csv(d13,file=paste(Sys.getenv('simulation_DATA'),'d13.csv',sep=''))
write.csv(d14,file=paste(Sys.getenv('simulation_DATA'),'d14.csv',sep=''))
write.csv(d15,file=paste(Sys.getenv('simulation_DATA'),'d15.csv',sep=''))
write.csv(d16,file=paste(Sys.getenv('simulation_DATA'),'d16.csv',sep=''))
write.csv(d17,file=paste(Sys.getenv('simulation_DATA'),'d17.csv',sep=''))
write.csv(d18,file=paste(Sys.getenv('simulation_DATA'),'d18.csv',sep=''))
write.csv(d19,file=paste(Sys.getenv('simulation_DATA'),'d19.csv',sep=''))
write.csv(d20,file=paste(Sys.getenv('simulation_DATA'),'d20.csv',sep=''))

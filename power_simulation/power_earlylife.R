# Clear the work environment
rm(list = ls())
#############################################################################################################
#############################################################################################################
#Gene-by-environment MR
#############################################################################################################
#############################################################################################################
#Input interaction P-value results
gbye<-read.csv(paste(Sys.getenv('Mysimulation'),"GbyE.csv", sep=""), sep=',',header=FALSE)
colnames(gbye)<-"pval"

#For each scenario, we simulate 1000 times.
gbye$no<-rep(1:1000,20)
#We assume 5 different effect sizes & 4 different sample sizes, and thus have 20 scenarios.
gbye$scenario<-c(rep(1,1000),rep(2,1000),rep(3,1000),rep(4,1000),rep(5,1000),rep(6,1000),rep(7,1000),rep(8,1000),rep(9,1000),rep(10,1000),rep(11,1000),rep(12,1000),rep(13,1000),rep(14,1000),rep(15,1000),rep(16,1000),rep(17,1000),rep(18,1000),rep(19,1000),rep(20,1000))
#reshape interaction P-value results: each column is for a scenario and has 1000 P-values.  
gbye<-reshape(gbye, idvar = "no", timevar = "scenario", direction = "wide")

#In each column, we count the proportion of P-values that are less than 0.05.
#N=10^5, betaG=0.01
sum(gbye$pval.1<0.05)/1000*100
#N=5*10^5, betaG=0.01
sum(gbye$pval.2<0.05)/1000*100
#N=10^6, betaG=0.01
sum(gbye$pval.3<0.05)/1000*100
#N=5*10^6, betaG=0.01
sum(gbye$pval.4<0.05)/1000*100

#N=10^5, betaG=0.025
sum(gbye$pval.5<0.05)/1000*100
#N=5*10^5, betaG=0.025
sum(gbye$pval.6<0.05)/1000*100
#N=10^6, betaG=0.025
sum(gbye$pval.7<0.05)/1000*100
#N=5*10^6, betaG=0.025
sum(gbye$pval.8<0.05)/1000*100

#N=10^5, betaG=0.05
sum(gbye$pval.9<0.05)/1000*100
#N=5*10^5, betaG=0.05
sum(gbye$pval.10<0.05)/1000*100
#N=10^6, betaG=0.05
sum(gbye$pval.11<0.05)/1000*100
#N=5*10^6, betaG=0.05
sum(gbye$pval.12<0.05)/1000*100
#N=10^5, betaG=0.075
sum(gbye$pval.13<0.05)/1000*100
#N=5*10^5, betaG=0.075
sum(gbye$pval.14<0.05)/1000*100
#N=10^6, betaG=0.075
sum(gbye$pval.15<0.05)/1000*100
#N=5*10^6, betaG=0.075
sum(gbye$pval.16<0.05)/1000*100
#N=10^5, betaG=0.1
sum(gbye$pval.17<0.05)/1000*100
#N=5*10^5, betaG=0.1
sum(gbye$pval.18<0.05)/1000*100
#N=10^6, betaG=0.1
sum(gbye$pval.19<0.05)/1000*100
#N=5*10^6, betaG=0.1
sum(gbye$pval.20<0.05)/1000*100

#############################################################################################################
#############################################################################################################
#proxy Gene-by-environment MR
#############################################################################################################
#############################################################################################################
#Input interaction P-value results 
proxy<-read.csv(paste(Sys.getenv('Mysimulation'),"proxy.csv", sep=""), sep=',',header=FALSE)
colnames(proxy)<-"pval"

#For each scenario, we simulate 1000 times.
proxy$no<-rep(1:1000,20)
#We assume 5 different effect sizes & 4 different sample sizes, and thus have 20 scenarios.
proxy$scenario<-c(rep(1,1000),rep(2,1000),rep(3,1000),rep(4,1000),rep(5,1000),rep(6,1000),rep(7,1000),rep(8,1000),rep(9,1000),rep(10,1000),rep(11,1000),rep(12,1000),rep(13,1000),rep(14,1000),rep(15,1000),rep(16,1000),rep(17,1000),rep(18,1000),rep(19,1000),rep(20,1000))
#reshape interaction P-value results: each column is for a scenario and has 1000 P-values.  
proxy<-reshape(proxy, idvar = "no", timevar = "scenario", direction = "wide")

#In each column, we count the proportion of P-values that are less than 0.05.
#N=10^5, betaG=0.01
sum(proxy$pval.1<0.05)/1000*100
#N=5*10^5, betaG=0.01
sum(proxy$pval.2<0.05)/1000*100
#N=10^6, betaG=0.01
sum(proxy$pval.3<0.05)/1000*100
#N=5*10^6, betaG=0.01
sum(proxy$pval.4<0.05)/1000*100

#N=10^5, betaG=0.025
sum(proxy$pval.5<0.05)/1000*100
#N=5*10^5, betaG=0.025
sum(proxy$pval.6<0.05)/1000*100
#N=10^6, betaG=0.025
sum(proxy$pval.7<0.05)/1000*100
#N=5*10^6, betaG=0.025
sum(proxy$pval.8<0.05)/1000*100

#N=10^5, betaG=0.05
sum(proxy$pval.9<0.05)/1000*100
#N=5*10^5, betaG=0.05
sum(proxy$pval.10<0.05)/1000*100
#N=10^6, betaG=0.05
sum(proxy$pval.11<0.05)/1000*100
#N=5*10^6, betaG=0.05
sum(proxy$pval.12<0.05)/1000*100

#N=10^5, betaG=0.075
sum(proxy$pval.13<0.05)/1000*100
#N=5*10^5, betaG=0.075
sum(proxy$pval.14<0.05)/1000*100
#N=10^6, betaG=0.075
sum(proxy$pval.15<0.05)/1000*100
#N=5*10^6, betaG=0.075
sum(proxy$pval.16<0.05)/1000*100

#N=10^5, betaG=0.1
sum(proxy$pval.17<0.05)/1000*100
#N=5*10^5, betaG=0.1
sum(proxy$pval.18<0.05)/1000*100
#N=10^6, betaG=0.1
sum(proxy$pval.19<0.05)/1000*100
#N=5*10^6, betaG=0.1
sum(proxy$pval.20<0.05)/1000*100

# Clear the work environment
rm(list = ls())
#############################################################################################################
#############################################################################################################
#Gene-by-environment MR
#############################################################################################################
#############################################################################################################
#Input interaction P-value results in the stratum of G1 smokers
gbyeg1smoker<-read.csv(paste(Sys.getenv('Mysimulation'),"GbyEG1smoker.csv", sep=""), sep=',',header=FALSE)
colnames(gbyeg1smoker)<-"pval"

#For each scenario, we simulate 1000 times.
gbyeg1smoker$no<-rep(1:1000,20)
#We assume 5 different effect sizes & 4 different sample sizes, and thus have 20 scenarios.
gbyeg1smoker$scenario<-c(rep(1,1000),rep(2,1000),rep(3,1000),rep(4,1000),rep(5,1000),rep(6,1000),rep(7,1000),rep(8,1000),rep(9,1000),rep(10,1000),rep(11,1000),rep(12,1000),rep(13,1000),rep(14,1000),rep(15,1000),rep(16,1000),rep(17,1000),rep(18,1000),rep(19,1000),rep(20,1000))
#reshape interaction P-value results: each column is for a scenario and has 1000 P-values. 
gbyeg1smoker<-reshape(gbyeg1smoker, idvar = "no", timevar = "scenario", direction = "wide")

#In each column, we count the proportion of P-values that are less than 0.05.
#N=10^5, betaG=0.01
sum(gbyeg1smoker$pval.1<0.05)/1000*100
#N=5*10^5, betaG=0.01
sum(gbyeg1smoker$pval.2<0.05)/1000*100
#N=10^6, betaG=0.01
sum(gbyeg1smoker$pval.3<0.05)/1000*100
#N=5*10^6, betaG=0.01
sum(gbyeg1smoker$pval.4<0.05)/1000*100

#N=10^5, betaG=0.025
sum(gbyeg1smoker$pval.5<0.05)/1000*100
#N=5*10^5, betaG=0.025
sum(gbyeg1smoker$pval.6<0.05)/1000*100
#N=10^6, betaG=0.025
sum(gbyeg1smoker$pval.7<0.05)/1000*100
#N=5*10^6, betaG=0.025
sum(gbyeg1smoker$pval.8<0.05)/1000*100

#N=10^5, betaG=0.05
sum(gbyeg1smoker$pval.9<0.05)/1000*100
#N=5*10^5, betaG=0.05
sum(gbyeg1smoker$pval.10<0.05)/1000*100
#N=10^6, betaG=0.05
sum(gbyeg1smoker$pval.11<0.05)/1000*100
#N=5*10^6, betaG=0.05
sum(gbyeg1smoker$pval.12<0.05)/1000*100

#N=10^5, betaG=0.075
sum(gbyeg1smoker$pval.13<0.05)/1000*100
#N=5*10^5, betaG=0.075
sum(gbyeg1smoker$pval.14<0.05)/1000*100
#N=10^6, betaG=0.075
sum(gbyeg1smoker$pval.15<0.05)/1000*100
#N=5*10^6, betaG=0.075
sum(gbyeg1smoker$pval.16<0.05)/1000*100

#N=10^5, betaG=0.1
sum(gbyeg1smoker$pval.17<0.05)/1000*100
#N=5*10^5, betaG=0.1
sum(gbyeg1smoker$pval.18<0.05)/1000*100
#N=10^6, betaG=0.1
sum(gbyeg1smoker$pval.19<0.05)/1000*100
#N=5*10^6, betaG=0.1
sum(gbyeg1smoker$pval.20<0.05)/1000*100

#############################################################################################################
#Input interaction P-value results in the stratum of G1 non-smokers
gbyeg1nonsmoker<-read.csv("C:/mini project 3/IJE minor revision/results/GbyEG1nonsmoker.csv",header=FALSE)
colnames(gbyeg1nonsmoker)<-"pval"

#For each scenario, we simulate 1000 times.
gbyeg1nonsmoker$no<-rep(1:1000,20)
#We assume 5 different effect sizes & 4 different sample sizes, and thus have 20 scenarios.
gbyeg1nonsmoker$scenario<-c(rep(1,1000),rep(2,1000),rep(3,1000),rep(4,1000),rep(5,1000),rep(6,1000),rep(7,1000),rep(8,1000),rep(9,1000),rep(10,1000),rep(11,1000),rep(12,1000),rep(13,1000),rep(14,1000),rep(15,1000),rep(16,1000),rep(17,1000),rep(18,1000),rep(19,1000),rep(20,1000))
#reshape interaction P-value results: each column is for a scenario and has 1000 P-values. 
gbyeg1nonsmoker<-reshape(gbyeg1nonsmoker, idvar = "no", timevar = "scenario", direction = "wide")

#In each column, we count the proportion of P-values that are less than 0.05.
#N=10^5, betaG=0.01
sum(gbyeg1nonsmoker$pval.1<0.05)/1000*100
#N=5*10^5, betaG=0.01
sum(gbyeg1nonsmoker$pval.2<0.05)/1000*100
#N=10^6, betaG=0.01
sum(gbyeg1nonsmoker$pval.3<0.05)/1000*100
#N=5*10^6, betaG=0.01
sum(gbyeg1nonsmoker$pval.4<0.05)/1000*100

#N=10^5, betaG=0.025
sum(gbyeg1nonsmoker$pval.5<0.05)/1000*100
#N=5*10^5, betaG=0.025
sum(gbyeg1nonsmoker$pval.6<0.05)/1000*100
#N=10^6, betaG=0.025
sum(gbyeg1nonsmoker$pval.7<0.05)/1000*100
#N=5*10^6, betaG=0.025
sum(gbyeg1nonsmoker$pval.8<0.05)/1000*100

#N=10^5, betaG=0.05
sum(gbyeg1nonsmoker$pval.9<0.05)/1000*100
#N=5*10^5, betaG=0.05
sum(gbyeg1nonsmoker$pval.10<0.05)/1000*100
#N=10^6, betaG=0.05
sum(gbyeg1nonsmoker$pval.11<0.05)/1000*100
#N=5*10^6, betaG=0.05
sum(gbyeg1nonsmoker$pval.12<0.05)/1000*100

#N=10^5, betaG=0.075
sum(gbyeg1nonsmoker$pval.13<0.05)/1000*100
#N=5*10^5, betaG=0.075
sum(gbyeg1nonsmoker$pval.14<0.05)/1000*100
#N=10^6, betaG=0.075
sum(gbyeg1nonsmoker$pval.15<0.05)/1000*100
#N=5*10^6, betaG=0.075
sum(gbyeg1nonsmoker$pval.16<0.05)/1000*100

#N=10^5, betaG=0.1
sum(gbyeg1nonsmoker$pval.17<0.05)/1000*100
#N=5*10^5, betaG=0.1
sum(gbyeg1nonsmoker$pval.18<0.05)/1000*100
#N=10^6, betaG=0.1
sum(gbyeg1nonsmoker$pval.19<0.05)/1000*100
#N=5*10^6, betaG=0.1
sum(gbyeg1nonsmoker$pval.20<0.05)/1000*100

#############################################################################################################
#############################################################################################################
#proxy Gene-by-environment MR
#############################################################################################################
#############################################################################################################
#Input interaction P-value results in the stratum of G1 smokers
proxyg1smoker<-read.csv("C:/mini project 3/IJE minor revision/results/proxyG1smoker.csv",header=FALSE)
colnames(proxyg1smoker)<-"pval"

#For each scenario, we simulate 1000 times.
proxyg1smoker$no<-rep(1:1000,20)
#We assume 5 different effect sizes & 4 different sample sizes, and thus have 20 scenarios.
proxyg1smoker$scenario<-c(rep(1,1000),rep(2,1000),rep(3,1000),rep(4,1000),rep(5,1000),rep(6,1000),rep(7,1000),rep(8,1000),rep(9,1000),rep(10,1000),rep(11,1000),rep(12,1000),rep(13,1000),rep(14,1000),rep(15,1000),rep(16,1000),rep(17,1000),rep(18,1000),rep(19,1000),rep(20,1000))
#reshape interaction P-value results: each column is for a scenario and has 1000 P-values. 
proxyg1smoker<-reshape(proxyg1smoker, idvar = "no", timevar = "scenario", direction = "wide")

#In each column, we count the proportion of P-values that are less than 0.05.
#N=10^5, betaG=0.01
sum(proxyg1smoker$pval.1<0.05)/1000*100
#N=5*10^5, betaG=0.01
sum(proxyg1smoker$pval.2<0.05)/1000*100
#N=10^6, betaG=0.01
sum(proxyg1smoker$pval.3<0.05)/1000*100
#N=5*10^6, betaG=0.01
sum(proxyg1smoker$pval.4<0.05)/1000*100

#N=10^5, betaG=0.025
sum(proxyg1smoker$pval.5<0.05)/1000*100
#N=5*10^5, betaG=0.025
sum(proxyg1smoker$pval.6<0.05)/1000*100
#N=10^6, betaG=0.025
sum(proxyg1smoker$pval.7<0.05)/1000*100
#N=5*10^6, betaG=0.025
sum(proxyg1smoker$pval.8<0.05)/1000*100

#N=10^5, betaG=0.05
sum(proxyg1smoker$pval.9<0.05)/1000*100
#N=5*10^5, betaG=0.05
sum(proxyg1smoker$pval.10<0.05)/1000*100
#N=10^6, betaG=0.05
sum(proxyg1smoker$pval.11<0.05)/1000*100
#N=5*10^6, betaG=0.05
sum(proxyg1smoker$pval.12<0.05)/1000*100

#N=10^5, betaG=0.075
sum(proxyg1smoker$pval.13<0.05)/1000*100
#N=5*10^5, betaG=0.075
sum(proxyg1smoker$pval.14<0.05)/1000*100
#N=10^6, betaG=0.075
sum(proxyg1smoker$pval.15<0.05)/1000*100
#N=5*10^6, betaG=0.075
sum(proxyg1smoker$pval.16<0.05)/1000*100

#N=10^5, betaG=0.1
sum(proxyg1smoker$pval.17<0.05)/1000*100
#N=5*10^5, betaG=0.1
sum(proxyg1smoker$pval.18<0.05)/1000*100
#N=10^6, betaG=0.1
sum(proxyg1smoker$pval.19<0.05)/1000*100
#N=5*10^6, betaG=0.1
sum(proxyg1smoker$pval.20<0.05)/1000*100

#############################################################################################################
#Input interaction P-value results in the stratum of G1 non-smokers
proxyg1nonsmoker<-read.csv("C:/mini project 3/IJE minor revision/results/proxyG1nonsmoker.csv",header=FALSE)
colnames(proxyg1nonsmoker)<-"pval"

#For each scenario, we simulate 1000 times.
proxyg1nonsmoker$no<-rep(1:1000,20)
proxyg1nonsmoker$scenario<-c(rep(1,1000),rep(2,1000),rep(3,1000),rep(4,1000),rep(5,1000),rep(6,1000),rep(7,1000),rep(8,1000),rep(9,1000),rep(10,1000),rep(11,1000),rep(12,1000),rep(13,1000),rep(14,1000),rep(15,1000),rep(16,1000),rep(17,1000),rep(18,1000),rep(19,1000),rep(20,1000))
#reshape interaction P-value results: each column is for a scenario and has 1000 P-values. 
proxyg1nonsmoker<-reshape(proxyg1nonsmoker, idvar = "no", timevar = "scenario", direction = "wide")

#In each column, we count the proportion of P-values that are less than 0.05.
#N=10^5, betaG=0.01
sum(proxyg1nonsmoker$pval.1<0.05)/1000*100
#N=5*10^5, betaG=0.01
sum(proxyg1nonsmoker$pval.2<0.05)/1000*100
#N=10^6, betaG=0.01
sum(proxyg1nonsmoker$pval.3<0.05)/1000*100
#N=5*10^6, betaG=0.01
sum(proxyg1nonsmoker$pval.4<0.05)/1000*100

#N=10^5, betaG=0.025
sum(proxyg1nonsmoker$pval.5<0.05)/1000*100
#N=5*10^5, betaG=0.025
sum(proxyg1nonsmoker$pval.6<0.05)/1000*100
#N=10^6, betaG=0.025
sum(proxyg1nonsmoker$pval.7<0.05)/1000*100
#N=5*10^6, betaG=0.025
sum(proxyg1nonsmoker$pval.8<0.05)/1000*100

#N=10^5, betaG=0.05
sum(proxyg1nonsmoker$pval.9<0.05)/1000*100
#N=5*10^5, betaG=0.05
sum(proxyg1nonsmoker$pval.10<0.05)/1000*100
#N=10^6, betaG=0.05
sum(proxyg1nonsmoker$pval.11<0.05)/1000*100
#N=5*10^6, betaG=0.05
sum(proxyg1nonsmoker$pval.12<0.05)/1000*100

#N=10^5, betaG=0.075
sum(proxyg1nonsmoker$pval.13<0.05)/1000*100
#N=5*10^5, betaG=0.075
sum(proxyg1nonsmoker$pval.14<0.05)/1000*100
#N=10^6, betaG=0.075
sum(proxyg1nonsmoker$pval.15<0.05)/1000*100
#N=5*10^6, betaG=0.075
sum(proxyg1nonsmoker$pval.16<0.05)/1000*100

#N=10^5, betaG=0.1
sum(proxyg1nonsmoker$pval.17<0.05)/1000*100
#N=5*10^5, betaG=0.1
sum(proxyg1nonsmoker$pval.18<0.05)/1000*100
#N=10^6, betaG=0.1
sum(proxyg1nonsmoker$pval.19<0.05)/1000*100
#N=5*10^6, betaG=0.1
sum(proxyg1nonsmoker$pval.20<0.05)/1000*100
#set enviromental variable
##################################################################################################
##################################################################################################
#use correct database
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#package
require(ggplot2)

pd<-position_dodge(0.5)

#height
#ever smoker vs never smoker in UKBB participants
height<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/height_ever_never.csv',sep=''), sep=',')
head(height)

a<-ggplot(height,aes(x=exposure,y=height,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=height-1.96*se,ymax=height+1.96*se),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("A. mean difference in height (cm)")+
  scale_shape_discrete(name="G0 smoking",
                       breaks=c("2","1"),
                       labels=c("Yes","No"))+
  scale_colour_discrete(name="G0 smoking",
                        breaks=c("2","1"),
                        labels=c("Yes","No"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y = element_blank())+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  annotate("text", x =1 , y = -0.25, label = "G1=ever",size=6)+
  annotate("text", x =3 , y = -0.25, label = "G1=never",size=6)+
  annotate("text", x =5 , y = -0.25, label = "G1=all",size=6)+
  scale_y_continuous(limits = c(-0.28,0.11),breaks = c(-0.2,-0.1,0,0.1))+
  scale_x_continuous(limits = c(0.5,5.5),breaks = c(1,3,5))+
  coord_flip()
a
############################################################################################################
#bmi
bmi<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/bmi.csv',sep=''), sep=',')
head(bmi)

b<-ggplot(bmi,aes(x=exposure,y=bmi,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=bmi-1.96*se,ymax=bmi+1.96*se),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("B. mean difference in BMI (kg/m2)")+
  scale_shape_discrete(name="G0 smoking",
                       breaks=c("2","1"),
                       labels=c("Yes","No"))+
  scale_colour_discrete(name="G0 smoking",
                        breaks=c("2","1"),
                        labels=c("Yes","No"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y = element_blank())+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  annotate("text", x =1 , y = -0.45, label = "G1=current",size=6)+
  annotate("text", x =3 , y = -0.45, label = "G1=former",size=6)+
  annotate("text", x =5 , y = -0.45, label = "G1=never",size=6)+
  annotate("text", x =7 , y = -0.45, label = "G1=all",size=6)+
  scale_y_continuous(limits = c(-0.5,0.11),breaks = c(-0.4,-0.3,-0.2,-0.1,0,0.1))+
  scale_x_continuous(limits = c(0.5,7.5),breaks = c(1,3,5,7))+
  coord_flip()
b
############################################################################################################
#FEV1
fev1<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/fev1_best.csv',sep=''), sep=',')
head(fev1)

c<-ggplot(fev1,aes(x=exposure,y=fev1,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=fev1-1.96*se,ymax=fev1+1.96*se),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("C. mean difference in FEV1 (L)")+
  scale_shape_discrete(name="G0 smoking",
                       breaks=c("2","1"),
                       labels=c("Yes","No"))+
  scale_colour_discrete(name="G0 smoking",
                        breaks=c("2","1"),
                        labels=c("Yes","No"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y = element_blank())+
  theme(legend.justification=c(1,1), legend.position=c(1,1))+
  theme(axis.text=element_text(size=14,face="bold"),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  annotate("text", x =1 , y = -0.078, label = "G1=current",size=6)+
  annotate("text", x =3 , y = -0.078, label = "G1=former",size=6)+
  annotate("text", x =5 , y = -0.078, label = "G1=never",size=6)+
  annotate("text", x =7 , y = -0.078, label = "G1=all",size=6)+
  scale_y_continuous(limits = c(-0.085,0.015),breaks = c(-0.06,-0.04,-0.02,0))+
  coord_flip()
c
############################################################################################################
#FVC
fvc<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/fvc_best.csv',sep=''), sep=',')
head(fvc)

d<-ggplot(fvc,aes(x=exposure,y=fvc,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=fvc-1.96*se,ymax=fvc+1.96*se),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("D. mean difference in FVC (L)")+
  scale_shape_discrete(name="G0 smoking",
                       breaks=c("2","1"),
                       labels=c("Yes","No"))+
  scale_colour_discrete(name="G0 smoking",
                        breaks=c("2","1"),
                        labels=c("Yes","No"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y = element_blank())+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=14,face="bold"),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  annotate("text", x =1 , y = -0.07, label = "G1=current",size=6)+
  annotate("text", x =3 , y = -0.07, label = "G1=former",size=6)+
  annotate("text", x =5 , y = -0.07, label = "G1=never",size=6)+
  annotate("text", x =7 , y = -0.07, label = "G1=all",size=6)+
  scale_y_continuous(limits = c(-0.075,0.01),breaks = c(-0.06,-0.04,-0.02,0,0.01))+
  coord_flip()
d
############################################################################################################
#asthma
asthma<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/asthma_ever_never.csv',sep=''), sep=',')
head(asthma)

e<-ggplot(asthma,aes(x=exposure,y=exp(asthma),shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=exp(asthma-1.96*se),ymax=exp(asthma+1.96*se)),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("E. Odds ratio of asthma")+
  scale_shape_discrete(name="G0 smoking",
                       breaks=c("2","1"),
                       labels=c("Yes","No"))+
  scale_colour_discrete(name="G0 smoking",
                        breaks=c("2","1"),
                        labels=c("Yes","No"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y = element_blank())+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=14,face="bold"),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 1, linetype = "dotted")+
  annotate("text", x =1 , y = 0.93, label = "G1=ever",size=6)+
  annotate("text", x =3 , y = 0.93, label = "G1=never",size=6)+
  annotate("text", x =5 , y = 0.93, label = "G1=all",size=6)+
  scale_y_continuous(limits = c(0.91,1.06383),breaks = c(0.95,1,1.05),trans = "log")+
  coord_flip()
e
############################################################################################################
#SBP
sbp<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/sbp.csv',sep=''), sep=',')
head(sbp)

f<-ggplot(sbp,aes(x=exposure,y=sbp,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=sbp-1.96*se,ymax=sbp+1.96*se),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("F. mean difference in SBP (mmHg)")+
  scale_shape_discrete(name="G0 smoking",
                       breaks=c("2","1"),
                       labels=c("Yes","No"))+
  scale_colour_discrete(name="G0 smoking",
                        breaks=c("2","1"),
                        labels=c("Yes","No"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y = element_blank())+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=14,face="bold"),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  annotate("text", x =1 , y = -0.63, label = "G1=current",size=6)+
  annotate("text", x =3 , y = -0.63, label = "G1=former",size=6)+
  annotate("text", x =5 , y = -0.63, label = "G1=never",size=6)+
  annotate("text", x =7 , y = -0.63, label = "G1=all",size=6)+
  scale_y_continuous(limits = c(-0.73,0.75),breaks = c(-0.5,-0.25,0,0.25,0.5,0.75))+
  coord_flip()
f
############################################################################################################
#DBP
dbp<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/dbp.csv',sep=''), sep=',')
head(dbp)

g<-ggplot(dbp,aes(x=exposure,y=dbp,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=dbp-1.96*se,ymax=dbp+1.96*se),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("G. mean difference in DBP (mmHg)")+
  scale_shape_discrete(name= "G0 smoking",
                       breaks=c("2","1"),
                       labels=c("Yes","No"))+
  scale_colour_discrete(name="G0 smoking",
                        breaks=c("2","1"),
                        labels=c("Yes","No"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y = element_blank())+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=14,face="bold"),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  annotate("text", x =1 , y = -0.6, label = "G1=current",size=6)+
  annotate("text", x =3 , y = -0.6, label = "G1=former",size=6)+
  annotate("text", x =5 , y = -0.6, label = "G1=never",size=6)+
  annotate("text", x =7 , y = -0.6, label = "G1=all",size=6)+
  scale_y_continuous(limits = c(-0.68,0.4),breaks = c(-0.4,-0.2,0,0.2,0.4))+
  coord_flip()
g
############################################################################################################
#menarche
#ever smoker vs never smoker in UKBB participants
menarche<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/menarche_ever_never.csv',sep=''), sep=',')
head(menarche)

h<-ggplot(menarche,aes(x=exposure,y=menarche,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=menarche-1.96*se,ymax=menarche+1.96*se),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("H. mean difference in age at menarche (year)")+
  scale_shape_discrete(name="G0 smoking",
                       breaks=c("2","1"),
                       labels=c("Yes","No"))+
  scale_colour_discrete(name="G0 smoking",
                        breaks=c("2","1"),
                        labels=c("Yes","No"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y = element_blank())+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  annotate("text", x =1 , y = -0.08, label = "G1=ever",size=6)+
  annotate("text", x =3 , y = -0.08, label = "G1=never",size=6)+
  annotate("text", x =5 , y = -0.08, label = "G1=all",size=6)+
  scale_y_continuous(limits = c(-0.085,0.005),breaks = c(-0.07,-0.06,-0.04,-0.02,0))+
  scale_x_continuous(limits = c(0.5,5.5),breaks = c(1,3,5))+
  coord_flip()
h
############################################################################################################
#education
edu<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/eduyr_ever_never.csv',sep=''), sep=',')
head(edu)

i<-ggplot(edu,aes(x=exposure,y=eduyr,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=eduyr-1.96*se,ymax=eduyr+1.96*se),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("I. mean difference in years of education")+
  scale_shape_discrete(name="G0 smoking",
                       breaks=c("2","1"),
                       labels=c("Yes","No"))+
  scale_colour_discrete(name="G0 smoking",
                        breaks=c("2","1"),
                        labels=c("Yes","No"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y = element_blank())+
  theme(legend.justification=c(1,1), legend.position=c(1,1))+
  theme(axis.text=element_text(size=14,face="bold"),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  annotate("text", x =1 , y = -0.08, label = "G1=ever",size=6)+
  annotate("text", x =3 , y = -0.08, label = "G1=never",size=6)+
  annotate("text", x =5 , y = -0.08, label = "G1=all",size=6)+
  scale_y_continuous(limits = c(-0.09,0.13),breaks = c(-0.08,-0.04,0,0.04,0.08,0.12))+
  coord_flip()
i
############################################################################################################
#fluid intelligence score
iq<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/iq.csv',sep=''), sep=',')
head(iq)

j<-ggplot(iq,aes(x=exposure,y=iq,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=iq-1.96*se,ymax=iq+1.96*se),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("J. mean difference in fluid intelligence score")+
  scale_shape_discrete(name="G0 smoking",
                       breaks=c("2","1"),
                       labels=c("Yes","No"))+
  scale_colour_discrete(name="G0 smoking",
                        breaks=c("2","1"),
                        labels=c("Yes","No"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y = element_blank())+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=14,face="bold"),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  annotate("text", x =1 , y = -0.14, label = "G1=current",size=6)+
  annotate("text", x =3 , y = -0.14, label = "G1=former",size=6)+
  annotate("text", x =5 , y = -0.14, label = "G1=never",size=6)+
  annotate("text", x =7 , y = -0.14, label = "G1=all",size=6)+
  scale_y_continuous(limits = c(-0.18,0.1),breaks = c(-0.1,-0.05,0,0.05,0.1))+
  coord_flip()
j
############################################################################################################
#depression/anxiety
depress<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/depression.csv',sep=''), sep=',')
head(depress)

k<-ggplot(depress,aes(x=exposure,y=exp(depress),shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=exp(depress-1.96*se),ymax=exp(depress+1.96*se)),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("K. Odds ratio of depression/anxiety")+
  scale_shape_discrete(name="G0 smoking",
                       breaks=c("2","1"),
                       labels=c("Yes","No"))+
  scale_colour_discrete(name="G0 smoking",
                        breaks=c("2","1"),
                        labels=c("Yes","No"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y = element_blank())+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=14,face="bold"),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 1, linetype = "dotted")+
  annotate("text", x =1 , y = 0.9, label = "G1=current",size=6)+
  annotate("text", x =3 , y = 0.9, label = "G1=former",size=6)+
  annotate("text", x =5 , y = 0.9, label = "G1=never",size=6)+
  annotate("text", x =7 , y = 0.9, label = "G1=all",size=6)+
  scale_y_continuous(limits = c(0.87,1.08),breaks = c(0.95,1,1.05),trans = "log")+
  coord_flip()
k
############################################################################################################
well<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/wellbeing.csv',sep=''), sep=',')
head(well)

#Happiness
l<-ggplot(well,aes(x=exposure,y=exp(happy),shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=exp(happy-1.96*happy_se),ymax=exp(happy+1.96*happy_se)),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("L. Odds ratio of happiness")+
  scale_shape_discrete(name="G1 smoking",
                       breaks=c("2","1"),
                       labels=c("Yes","No"))+
  scale_colour_discrete(name="G1 smoking",
                        breaks=c("2","1"),
                        labels=c("Yes","No"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y = element_blank())+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=14,face="bold"),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 1, linetype = "dotted")+
  annotate("text", x =1 , y = 0.9, label = "G1=ever",size=6)+
  annotate("text", x =3 , y = 0.9, label = "G1=never",size=6)+
  annotate("text", x =5 , y = 0.9, label = "G1=all",size=6)+
  scale_y_continuous(limits = c(0.88,1.06),breaks = c(0.95,1,1.05))+
  coord_flip()
l

#present in one figure
source("http://peterhaschke.com/Code/multiplot.R")
multiplot(a,d,b,e,c,f,cols=3)
multiplot(g,j,h,k,i,l,cols=3)
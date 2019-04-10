#set enviromental variable
##################################################################################################
##################################################################################################
#use correct database
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#package
require(ggplot2)
require(stringr)

pd<-position_dodge(0.5)

#height
#ever smoker vs never smoker in UKBB participants
height<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/height_ever_never.csv',sep=''), sep=',',header = FALSE)
head(height)
colnames(height)<-c("model","beta","se","lci","uci")
#add indices to plot
##G0 smoker=2, G0 non-smoker=1
height$supp[substring(height$model,1,8)=="mumsmoke"]<-2
height$supp[substring(height$model,1,11)=="mumnonsmoke"]<-1
##G1 ever smoker=1, G1 never smoker=3, G1 all participants=5
height$exposure[str_sub(height$model,-9,-1)=="childever"]<-1
height$exposure[str_sub(height$model,-10,-1)=="childnever"]<-3
height$exposure[str_sub(height$model,-8,-1)=="childall"]<-5

a<-ggplot(height,aes(x=exposure,y=beta,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.1,position=pd,size=1)+
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
bmi<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/bmi.csv',sep=''), sep=',',header = FALSE)
head(bmi)
colnames(bmi)<-c("model","beta","se","lci","uci")
#add indices to plot
##G0 smoker=2, G0 non-smoker=1
bmi$supp[substring(bmi$model,1,8)=="mumsmoke"]<-2
bmi$supp[substring(bmi$model,1,11)=="mumnonsmoke"]<-1
##G1 current smoker=1, G1 former smoker=3, G1 never smoker=5, G1 all participants=7
bmi$exposure[str_sub(bmi$model,-7,-1)=="current"]<-1
bmi$exposure[str_sub(bmi$model,-6,-1)=="former"]<-3
bmi$exposure[str_sub(bmi$model,-5,-1)=="never"]<-5
bmi$exposure[str_sub(bmi$model,-3,-1)=="all"]<-7

b<-ggplot(bmi,aes(x=exposure,y=beta,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.1,position=pd,size=1)+
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
fev1<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/fev1_best.csv',sep=''), sep=',',header = FALSE)
head(fev1)
colnames(fev1)<-c("model","beta","se","lci","uci")
#add indices to plot
##G0 smoker=2, G0 non-smoker=1
fev1$supp[substring(fev1$model,1,8)=="mumsmoke"]<-2
fev1$supp[substring(fev1$model,1,11)=="mumnonsmoke"]<-1
##G1 current smoker=1, G1 former smoker=3, G1 never smoker=5, G1 all participants=7
fev1$exposure[str_sub(fev1$model,-7,-1)=="current"]<-1
fev1$exposure[str_sub(fev1$model,-6,-1)=="former"]<-3
fev1$exposure[str_sub(fev1$model,-5,-1)=="never"]<-5
fev1$exposure[str_sub(fev1$model,-3,-1)=="all"]<-7

c<-ggplot(fev1,aes(x=exposure,y=beta,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.1,position=pd,size=1)+
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
fvc<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/fvc_best.csv',sep=''), sep=',',header = FALSE)
head(fvc)
colnames(fvc)<-c("model","beta","se","lci","uci")
#add indices to plot
##G0 smoker=2, G0 non-smoker=1
fvc$supp[substring(fvc$model,1,8)=="mumsmoke"]<-2
fvc$supp[substring(fvc$model,1,11)=="mumnonsmoke"]<-1
##G1 current smoker=1, G1 former smoker=3, G1 never smoker=5, G1 all participants=7
fvc$exposure[str_sub(fvc$model,-7,-1)=="current"]<-1
fvc$exposure[str_sub(fvc$model,-6,-1)=="former"]<-3
fvc$exposure[str_sub(fvc$model,-5,-1)=="never"]<-5
fvc$exposure[str_sub(fvc$model,-3,-1)=="all"]<-7

d<-ggplot(fvc,aes(x=exposure,y=beta,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.1,position=pd,size=1)+
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
asthma<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/asthma_ever_never.csv',sep=''), sep=',',header = FALSE)
head(asthma)
colnames(asthma)<-c("model","logor","se","loglci","loguci")
#add indices to plot
##G0 smoker=2, G0 non-smoker=1
asthma$supp[substring(asthma$model,1,8)=="mumsmoke"]<-2
asthma$supp[substring(asthma$model,1,11)=="mumnonsmoke"]<-1
##G1 ever smoker=1, G1 never smoker=3, G1 all participants=5
asthma$exposure[str_sub(asthma$model,-9,-1)=="childever"]<-1
asthma$exposure[str_sub(asthma$model,-10,-1)=="childnever"]<-3
asthma$exposure[str_sub(asthma$model,-8,-1)=="childall"]<-5

e<-ggplot(asthma,aes(x=exposure,y=exp(logor),shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=exp(loglci),ymax=exp(loguci)),width=0.1,position=pd,size=1)+
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
sbp<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/sbp.csv',sep=''), sep=',',header = FALSE)
head(sbp)
colnames(sbp)<-c("model","beta","se","lci","uci")
#add indices to plot
##G0 smoker=2, G0 non-smoker=1
sbp$supp[substring(sbp$model,1,8)=="mumsmoke"]<-2
sbp$supp[substring(sbp$model,1,11)=="mumnonsmoke"]<-1
##G1 current smoker=1, G1 former smoker=3, G1 never smoker=5, G1 all participants=7
sbp$exposure[str_sub(sbp$model,-7,-1)=="current"]<-1
sbp$exposure[str_sub(sbp$model,-6,-1)=="former"]<-3
sbp$exposure[str_sub(sbp$model,-5,-1)=="never"]<-5
sbp$exposure[str_sub(sbp$model,-3,-1)=="all"]<-7

f<-ggplot(sbp,aes(x=exposure,y=beta,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.1,position=pd,size=1)+
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
dbp<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/dbp.csv',sep=''), sep=',',header = FALSE)
head(dbp)
colnames(dbp)<-c("model","beta","se","lci","uci")
#add indices to plot
##G0 smoker=2, G0 non-smoker=1
dbp$supp[substring(dbp$model,1,8)=="mumsmoke"]<-2
dbp$supp[substring(dbp$model,1,11)=="mumnonsmoke"]<-1
##G1 current smoker=1, G1 former smoker=3, G1 never smoker=5, G1 all participants=7
dbp$exposure[str_sub(dbp$model,-7,-1)=="current"]<-1
dbp$exposure[str_sub(dbp$model,-6,-1)=="former"]<-3
dbp$exposure[str_sub(dbp$model,-5,-1)=="never"]<-5
dbp$exposure[str_sub(dbp$model,-3,-1)=="all"]<-7

g<-ggplot(dbp,aes(x=exposure,y=beta,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.1,position=pd,size=1)+
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
menarche<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/menarche_ever_never.csv',sep=''), sep=',',header = FALSE)
head(menarche)
colnames(menarche)<-c("model","beta","se","lci","uci")
#add indices to plot
##G0 smoker=2, G0 non-smoker=1
menarche$supp[substring(menarche$model,1,8)=="mumsmoke"]<-2
menarche$supp[substring(menarche$model,1,11)=="mumnonsmoke"]<-1
##G1 ever smoker=1, G1 never smoker=3, G1 all participants=5
menarche$exposure[str_sub(menarche$model,-9,-1)=="childever"]<-1
menarche$exposure[str_sub(menarche$model,-10,-1)=="childnever"]<-3
menarche$exposure[str_sub(menarche$model,-8,-1)=="childall"]<-5

h<-ggplot(menarche,aes(x=exposure,y=beta,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.1,position=pd,size=1)+
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
edu<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/eduyr_ever_never.csv',sep=''), sep=',',header = FALSE)
head(edu)
colnames(edu)<-c("model","beta","se","lci","uci")
#add indices to plot
##G0 smoker=2, G0 non-smoker=1
edu$supp[substring(edu$model,1,8)=="mumsmoke"]<-2
edu$supp[substring(edu$model,1,11)=="mumnonsmoke"]<-1
##G1 ever smoker=1, G1 never smoker=3, G1 all participants=5
edu$exposure[str_sub(edu$model,-9,-1)=="childever"]<-1
edu$exposure[str_sub(edu$model,-10,-1)=="childnever"]<-3
edu$exposure[str_sub(edu$model,-8,-1)=="childall"]<-5

i<-ggplot(edu,aes(x=exposure,y=beta,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.1,position=pd,size=1)+
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
iq<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/iq.csv',sep=''), sep=',',header = FALSE)
head(iq)
colnames(iq)<-c("model","beta","se","lci","uci")
#add indices to plot
##G0 smoker=2, G0 non-smoker=1
iq$supp[substring(iq$model,1,8)=="mumsmoke"]<-2
iq$supp[substring(iq$model,1,11)=="mumnonsmoke"]<-1
##G1 current smoker=1, G1 former smoker=3, G1 never smoker=5, G1 all participants=7
iq$exposure[str_sub(iq$model,-7,-1)=="current"]<-1
iq$exposure[str_sub(iq$model,-6,-1)=="former"]<-3
iq$exposure[str_sub(iq$model,-5,-1)=="never"]<-5
iq$exposure[str_sub(iq$model,-3,-1)=="all"]<-7

j<-ggplot(iq,aes(x=exposure,y=beta,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.1,position=pd,size=1)+
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
depress<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/depression.csv',sep=''), sep=',',header = FALSE)
head(depress)
colnames(depress)<-c("model","logor","se","loglci","loguci")
#add indices to plot
##G0 smoker=2, G0 non-smoker=1
depress$supp[substring(depress$model,1,8)=="mumsmoke"]<-2
depress$supp[substring(depress$model,1,11)=="mumnonsmoke"]<-1
##G1 current smoker=1, G1 former smoker=3, G1 never smoker=5, G1 all participants=7
depress$exposure[str_sub(depress$model,-7,-1)=="current"]<-1
depress$exposure[str_sub(depress$model,-6,-1)=="former"]<-3
depress$exposure[str_sub(depress$model,-5,-1)=="never"]<-5
depress$exposure[str_sub(depress$model,-3,-1)=="all"]<-7

k<-ggplot(depress,aes(x=exposure,y=exp(logor),shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=exp(loglci),ymax=exp(loguci)),width=0.1,position=pd,size=1)+
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
#Happiness
well<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/wellbeing.csv',sep=''), sep=',',header = FALSE)
head(well)
colnames(well)<-c("model","logor","se","loglci","loguci")
#add indices to plot
##G0 smoker=2, G0 non-smoker=1
well$supp[substring(well$model,1,8)=="mumsmoke"]<-2
well$supp[substring(well$model,1,11)=="mumnonsmoke"]<-1
##G1 ever smoker=1, G1 never smoker=3, G1 all participants=5
well$exposure[str_sub(well$model,-9,-1)=="childever"]<-1
well$exposure[str_sub(well$model,-10,-1)=="childnever"]<-3
well$exposure[str_sub(well$model,-8,-1)=="childall"]<-5

l<-ggplot(well,aes(x=exposure,y=exp(logor),shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=exp(loglci),ymax=exp(loguci)),width=0.1,position=pd,size=1)+
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
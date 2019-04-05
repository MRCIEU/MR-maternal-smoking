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

#ever smoker vs never smoker in UKBB participants, considering age started smoking
height_age<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/height_age.csv',sep=''), sep=',')
head(height_age)

a<-ggplot(height_age,aes(x=exposure,y=height,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=height-1.96*se,ymax=height+1.96*se),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("mean difference in height (cm)")+
  scale_shape_discrete(name="G0 smoking",
                       breaks=c("2","1"),
                       labels=c("Yes","No"))+
  scale_colour_discrete(name="G0 smoking",
                        breaks=c("2","1"),
                        labels=c("Yes","No"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(legend.justification=c(1,1), legend.position=c(1,1))+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  annotate("text", x =1 , y = 0.18, label = "Interaction P = 0.1198")+
  annotate("text", x =3 , y = 0.18, label = "Interaction P = 0.0395")+
  annotate("text", x =5 , y = 0.18, label = "Interaction P = 0.2415")+
  scale_y_continuous(limits = c(-0.28,0.3),breaks = c(-0.2,-0.1,0,0.1,0.2,0.3))+
  scale_x_continuous(limits = c(0.5,6),breaks = c(1,3,5),labels = c("G1:Ever","G1:Never","G1:All"))+
  coord_flip()
a

#ever smoker vs never smoker in UKBB participants, considering age started smoking
menarche_age<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/menarche_age.csv',sep=''), sep=',')
head(menarche_age)

b<-ggplot(menarche_age,aes(x=exposure,y=menarche,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=menarche-1.96*se,ymax=menarche+1.96*se),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("mean difference in age at menarche (year)")+
  scale_shape_discrete(name="G0 smoking",
                       breaks=c("2","1"),
                       labels=c("Yes","No"))+
  scale_colour_discrete(name="G0 smoking",
                        breaks=c("2","1"),
                        labels=c("Yes","No"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  annotate("text", x =1 , y = 0.1, label = "Interaction P = 0.0233")+
  annotate("text", x =3 , y = 0.1, label = "Interaction P = 0.8621")+
  annotate("text", x =5 , y = 0.1, label = "Interaction P = 0.7269")+
  scale_y_continuous(limits = c(-0.5,0.25),breaks = c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2))+
  scale_x_continuous(limits = c(0.5,5.5),breaks = c(1,3,5),labels = c("G1:Ever","G1:Never","G1:All"))+
  coord_flip()
b


#present in one figure
source("http://peterhaschke.com/Code/multiplot.R")
multiplot(a,b,cols=1)
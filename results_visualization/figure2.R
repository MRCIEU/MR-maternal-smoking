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

#input data
bw<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/bw_ever_never.csv',sep=''), sep=',',header = FALSE)
head(bw)
colnames(bw)<-c("model","beta","se","lci","uci")

#add indices to plot
##crude model=1, adjust model=2
bw$supp[substring(bw$model,1,5)=="crude"]<-1
bw$supp[substring(bw$model,1,6)=="adjust"]<-2
##non-smoker=1,smoker=3,all participants=5
bw$exposure[str_sub(bw$model,-8,-1)=="nonsmoke"]<-1
bw$exposure[str_sub(bw$model,-6,-1)=="_smoke"]<-3
bw$exposure[str_sub(bw$model,-3,-1)=="all"]<-5

#Dodge overlapping objects side-to-side
pd<-position_dodge(0.5)

#Draw the figure
a<-ggplot(bw,aes(x=exposure,y=beta,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("mean difference in birthweight (kg)")+
  scale_shape_discrete(name="Model:",
                       breaks=c("2","1"),
                       labels=c("Adjust","Crude"))+
  scale_colour_discrete(name="Model:",
                        breaks=c("2","1"),
                        labels=c("Adjust","Crude"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(legend.justification=c(1,0), legend.position=c(1,0))+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  annotate("text", x =1 , y = -0.03, label = "N=121 177")+
  annotate("text", x =3 , y = -0.03, label = "N=50 607")+
  annotate("text", x =5 , y = -0.03, label = "N=171 784")+
  annotate("text", x =1.8 , y = 0.004, label = "Interaction P (crude model) = 0.0028", colour = "red")+
  annotate("text", x =2.2 , y = 0.004, label = "Interaction P (adjust model) = 0.0039", colour = "blue")+
  scale_y_continuous(limits = c(-0.03,0.01),breaks = c(-0.03,-0.02,-0.01,0,0.01))+
  scale_x_continuous(limits = c(0,6),breaks = c(1,3,5),labels = c("G0 smoking=No","G0 smoking=Yes","All participants"))+
  coord_flip()
#See the figure in Plots
#Need to save the figure manually
print(a)
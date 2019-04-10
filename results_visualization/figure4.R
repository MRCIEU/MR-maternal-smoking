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

##input data
bw_g<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/bw_grandmum_grandchild.csv',sep=''), sep=',',header = FALSE)
head(bw_g)
colnames(bw_g)<-c("model","beta","se","lci","uci")

#add indices to plot
##G0 all participants=1, G0 nonsmokers=2, G0 smokers=3
bw_g$supp[substring(bw_g$model,1,5)=="g0all"]<-1
bw_g$supp[substring(bw_g$model,1,10)=="g0nonsmoke"]<-2
bw_g$supp[substring(bw_g$model,1,7)=="g0smoke"]<-3
##G1 smoker=1, G1 nonsmoker=3, G1 all participants=5
bw_g$exposure[str_sub(bw_g$model,-7,-1)=="g1smoke"]<-1
bw_g$exposure[str_sub(bw_g$model,-10,-1)=="g1nonsmoke"]<-3
bw_g$exposure[str_sub(bw_g$model,-5,-1)=="g1all"]<-5

#Dodge overlapping objects side-to-side
pd<-position_dodge(0.5)

#Draw the figure
a<-ggplot(bw_g,aes(x=exposure,y=beta,shape=factor(supp),colour=factor(supp)))+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.1,position=pd,size=1)+
  geom_point(position=pd,size=3)+
  xlab("")+
  ylab("mean difference in G2 birthweight (kg)")+
  scale_shape_discrete(name="G0 smoking",
                       breaks=c("3","2","1"),
                       labels=c("Yes","No","All"))+
  scale_colour_discrete(name="G0 smoking",
                        breaks=c("3","2","1"),
                        labels=c("Yes","No","All"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(legend.justification=c(1,1), legend.position=c(1,1))+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  annotate("text", x =0.8 , y = -0.06, label = "N=19 061")+
  annotate("text", x =1 , y = -0.06, label = "N=11 741")+
  annotate("text", x =1.2 , y = -0.06, label = "N=7320")+
  annotate("text", x =2.8 , y = -0.06, label = "N=80 552")+
  annotate("text", x =3 , y = -0.06, label = "N=56 837")+
  annotate("text", x =3.2 , y = -0.06, label = "N=23 715")+
  annotate("text", x =4.8 , y = -0.06, label = "N=126 122")+
  annotate("text", x =5 , y = -0.06, label = "N=87 811")+
  annotate("text", x =5.2 , y = -0.06, label = "N=38 311")+
  annotate("text", x =1.1 , y = 0.015, label = "Interaction P = 0.0914")+
  annotate("text", x =3.1 , y = 0.015, label = "Interaction P = 0.2447")+
  annotate("text", x =5.1 , y = 0.015, label = "Interaction P = 0.9925")+
  scale_y_continuous(limits = c(-0.06,0.025),breaks = c(-0.06,-0.04,-0.02,0,0.02))+
  scale_x_continuous(limits = c(0.5,6),breaks = c(1,3,5),labels = c("G1:smoker","G1:non-smoker", "G1:All women"))+
  coord_flip()

#See the figure in Plots
#Need to save the figure manually
print(a)

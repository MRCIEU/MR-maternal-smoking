#set enviromental variable
##################################################################################################
##################################################################################################
#use correct database
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

#package
require(ggplot2)

#input power results
powerdata<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/power.csv',sep=''), sep=',',header = TRUE)

#Power in Gene-by-environment MR versus power in proxy Gene-by-environment MR
a<-ggplot(powerdata,aes(x=n,y=power,colour=as.factor(effect),linetype=as.factor(group)))+
  geom_line(size=1)+
  geom_point(size=3)+
  theme_classic()+
  xlab('Total sample size')+
  ylab('Power')+
  scale_x_continuous(limits = c(1,4),breaks = c(1,2,3,4),labels = c("100 000","500 000","1 000 000","5 000 000"))+
  scale_colour_discrete(name="Effect size of G0 smoking heaviness*",
                          breaks=c("5","4","3","2","1"),
                          labels=c("0.1 SD/cigarette","0.075 SD/cigarette","0.05 SD/cigarette","0.025 SD/cigarette","0.01 SD/cigarette"))+
  scale_linetype_discrete((name="Study design"),
                          breaks=c("1","2"),
                          labels=c("G*E","proxy G*E"))+
  scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))+
  theme(legend.position="top",legend.direction="horizontal", legend.box="horizontal")+
  facet_grid(cols=vars(powergroup))+
  theme(strip.text.x=element_text(size=12, color="red", face="bold.italic"))+
  theme(axis.title=element_text(size=12, face="bold"),axis.text=element_text(size=12))+
  theme(legend.text = element_text(size=12, face="bold"),legend.title = element_text(size=12, face="bold"))
a


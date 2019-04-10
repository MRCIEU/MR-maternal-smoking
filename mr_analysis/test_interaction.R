#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

require(metafor)


#G1 birthweight
bw<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/bw_ever_never.csv',sep=''), sep=',',header = FALSE)
head(bw)
colnames(bw)<-c("model","beta","se","lci","uci")
#test heterogeneity in meta-analysis
bw_adjust<-bw[ which(bw$model=='adjust_nonsmoke'|bw$model=='adjust_smoke'),]
bw_crude<-bw[ which(bw$model=='crude_nonsmoke'|bw$model=='crude_smoke'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=bw_adjust)
rma(yi=beta, sei=se, slab=model, method="FE", data=bw_crude)


#G1 BMI
bmi<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/bmi.csv',sep=''), sep=',',header = FALSE)
head(bmi)
colnames(bmi)<-c("model","beta","se","lci","uci")
#test heterogeneity in meta-analysis
bmi_all<-bmi[which(bmi$model=='mumnonsmoke_childall'|bmi$model=='mumsmoke_childall'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=bmi_all)
bmi_never<-bmi[which(bmi$model=='mumnonsmoke_childnever'|bmi$model=='mumsmoke_childnever'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=bmi_never)
bmi_former<-bmi[which(bmi$model=='mumnonsmoke_childformer'|bmi$model=='mumsmoke_childformer'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=bmi_former)
bmi_current<-bmi[which(bmi$model=='mumnonsmoke_childcurrent'|bmi$model=='mumsmoke_childcurrent'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=bmi_current)


#G1 height
height<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/height_ever_never.csv',sep=''), sep=',',header = FALSE)
head(height)
colnames(height)<-c("model","beta","se","lci","uci")
#test heterogeneity in meta-analysis
height_all<-height[which(height$model=='mumnonsmoke_childall'|height$model=='mumsmoke_childall'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=height_all)
height_never<-height[which(height$model=='mumnonsmoke_childnever'|height$model=='mumsmoke_childnever'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=height_never)
height_ever<-height[which(height$model=='mumnonsmoke_childever'|height$model=='mumsmoke_childever'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=height_ever)


#G2 birthweight
bw_g<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/bw_grandmum_grandchild.csv',sep=''), sep=',',header = FALSE)
head(bw_g)
colnames(bw_g)<-c("model","beta","se","lci","uci")
#test heterogeneity in meta-analysis
bw_g_all<-bw_g[which(bw_g$model=="g0nonsmoke_g1all"|bw_g$model=="g0smoke_g1all"),]
rma(yi=beta, sei=se, slab=model, method="FE", data=bw_g_all)
bw_g_nonsmoke<-bw_g[which(bw_g$model=="g0nonsmoke_g1nonsmoke"|bw_g$model=="g0smoke_g1nonsmoke"),]
rma(yi=beta, sei=se, slab=model, method="FE", data=bw_g_nonsmoke)
bw_g_smoke<-bw_g[which(bw_g$model=="g0nonsmoke_g1smoke"|bw_g$model=="g0smoke_g1smoke"),]
rma(yi=beta, sei=se, slab=model, method="FE", data=bw_g_smoke)


#G1 menarche
menarche<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/menarche_ever_never.csv',sep=''), sep=',',header = FALSE)
head(menarche)
colnames(menarche)<-c("model","beta","se","lci","uci")
#test heterogeneity in meta-analysis
menarche_all<-menarche[which(menarche$model=="mumnonsmoke_childall"|menarche$model=="mumsmoke_childall"),]
rma(yi=beta, sei=se, slab=model, method="FE", data=menarche_all)
menarche_never<-menarche[which(menarche$model=="mumnonsmoke_childnever"|menarche$model=="mumsmoke_childnever"),]
rma(yi=beta, sei=se, slab=model, method="FE", data=menarche_never)
menarche_ever<-menarche[which(menarche$model=="mumnonsmoke_childever"|menarche$model=="mumsmoke_childever"),]
rma(yi=beta, sei=se, slab=model, method="FE", data=menarche_ever)


#G1 SBP
sbp<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/sbp.csv',sep=''), sep=',',header = FALSE)
head(sbp)
colnames(sbp)<-c("model","beta","se","lci","uci")
#test heterogeneity in meta-analysis
sbp_all<-sbp[which(sbp$model=="mumnonsmoke_childall"|sbp$model=="mumsmoke_childall"),]
rma(yi=beta, sei=se, slab=model, method="FE", data=sbp_all)
sbp_never<-sbp[which(sbp$model=="mumnonsmoke_childnever"|sbp$model=="mumsmoke_childnever"),]
rma(yi=beta, sei=se, slab=model, method="FE", data=sbp_never)
sbp_former<-sbp[which(sbp$model=="mumnonsmoke_childformer"|sbp$model=="mumsmoke_childformer"),]
rma(yi=beta, sei=se, slab=model, method="FE", data=sbp_former)
sbp_current<-sbp[which(sbp$model=="mumnonsmoke_childcurrent"|sbp$model=="mumsmoke_childcurrent"),]
rma(yi=beta, sei=se, slab=model, method="FE", data=sbp_current)


#G1 DBP
dbp<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/dbp.csv',sep=''), sep=',',header = FALSE)
head(dbp)
colnames(dbp)<-c("model","beta","se","lci","uci")
#test heterogeneity in meta-analysis
dbp_all<-dbp[which(dbp$model=="mumnonsmoke_childall"|dbp$model=="mumsmoke_childall"),]
rma(yi=beta, sei=se, slab=model, method="FE", data=dbp_all)
dbp_never<-dbp[which(dbp$model=="mumnonsmoke_childnever"|dbp$model=="mumsmoke_childnever"),]
rma(yi=beta, sei=se, slab=model, method="FE", data=dbp_never)
dbp_former<-dbp[which(dbp$model=="mumnonsmoke_childformer"|dbp$model=="mumsmoke_childformer"),]
rma(yi=beta, sei=se, slab=model, method="FE", data=dbp_former)
dbp_current<-dbp[which(dbp$model=="mumnonsmoke_childcurrent"|dbp$model=="mumsmoke_childcurrent"),]
rma(yi=beta, sei=se, slab=model, method="FE", data=dbp_current)


#G1 years of eduaction
edu<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/eduyr_ever_never.csv',sep=''), sep=',',header = FALSE)
head(edu)
colnames(edu)<-c("model","beta","se","lci","uci")
#test heterogeneity in meta-analysis
edu_all<-edu[which(edu$model=='mumnonsmoke_childall'|edu$model=='mumsmoke_childall'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=edu_all)
edu_never<-edu[which(edu$model=='mumnonsmoke_childnever'|edu$model=='mumsmoke_childnever'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=edu_never)
edu_ever<-edu[which(edu$model=='mumnonsmoke_childever'|edu$model=='mumsmoke_childever'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=edu_ever)


#G1 fluid intelligence
iq<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/iq.csv',sep=''), sep=',',header = FALSE)
head(iq)
colnames(iq)<-c("model","beta","se","lci","uci")
#test heterogeneity in meta-analysis
iq_all<-iq[which(iq$model=='mumnonsmoke_childall'|iq$model=='mumsmoke_childall'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=iq_all)
iq_never<-iq[which(iq$model=='mumnonsmoke_childnever'|iq$model=='mumsmoke_childnever'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=iq_never)
iq_former<-iq[which(iq$model=='mumnonsmoke_childformer'|iq$model=='mumsmoke_childformer'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=iq_former)
iq_current<-iq[which(iq$model=='mumnonsmoke_childcurrent'|iq$model=='mumsmoke_childcurrent'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=iq_current)


#G1 lung function, FVC
fvc<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/fvc_best.csv',sep=''), sep=',',header = FALSE)
head(fvc)
colnames(fvc)<-c("model","beta","se","lci","uci")
#test heterogeneity in meta-analysis
fvc_all<-fvc[which(fvc$model=='mumnonsmoke_childall'|fvc$model=='mumsmoke_childall'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=fvc_all)
fvc_never<-fvc[which(fvc$model=='mumnonsmoke_childnever'|fvc$model=='mumsmoke_childnever'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=fvc_never)
fvc_former<-fvc[which(fvc$model=='mumnonsmoke_childformer'|fvc$model=='mumsmoke_childformer'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=fvc_former)
fvc_current<-fvc[which(fvc$model=='mumnonsmoke_childcurrent'|fvc$model=='mumsmoke_childcurrent'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=fvc_current)


#G1 lung function, FEV1
fev1<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/fev1_best.csv',sep=''), sep=',',header = FALSE)
head(fev1)
colnames(fev1)<-c("model","beta","se","lci","uci")
#test heterogeneity in meta-analysis
fev1_all<-fev1[which(fev1$model=='mumnonsmoke_childall'|fev1$model=='mumsmoke_childall'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=fev1_all)
fev1_never<-fev1[which(fev1$model=='mumnonsmoke_childnever'|fev1$model=='mumsmoke_childnever'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=fev1_never)
fev1_former<-fev1[which(fev1$model=='mumnonsmoke_childformer'|fev1$model=='mumsmoke_childformer'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=fev1_former)
fev1_current<-fev1[which(fev1$model=='mumnonsmoke_childcurrent'|fev1$model=='mumsmoke_childcurrent'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=fev1_current)


#G1 asthma
asthma<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/asthma_ever_never.csv',sep=''), sep=',',header = FALSE)
head(asthma)
colnames(asthma)<-c("model","logor","se","loglci","loguci")
#test heterogeneity in meta-analysis
asthma_all<-asthma[which(asthma$model=='mumnonsmoke_childall'|asthma$model=='mumsmoke_childall'),]
rma(yi=logor, sei=se, slab=model, method="FE", data=asthma_all)
asthma_never<-asthma[which(asthma$model=='mumnonsmoke_childnever'|asthma$model=='mumsmoke_childnever'),]
rma(yi=logor, sei=se, slab=model, method="FE", data=asthma_never)
asthma_ever<-asthma[which(asthma$model=='mumnonsmoke_childever'|asthma$model=='mumsmoke_childever'),]
rma(yi=logor, sei=se, slab=model, method="FE", data=asthma_ever)


#G1 depression
depress<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/depression.csv',sep=''), sep=',',header = FALSE)
head(depress)
colnames(depress)<-c("model","logor","se","loglci","loguci")
#test heterogeneity in meta-analysis
depress_all<-depress[which(depress$model=='mumnonsmoke_childall'|depress$model=='mumsmoke_childall'),]
rma(yi=logor, sei=se, slab=model, method="FE", data=depress_all)
depress_never<-depress[which(depress$model=='mumnonsmoke_childnever'|depress$model=='mumsmoke_childnever'),]
rma(yi=logor, sei=se, slab=model, method="FE", data=depress_never)
depress_former<-depress[which(depress$model=='mumnonsmoke_childformer'|depress$model=='mumsmoke_childformer'),]
rma(yi=logor, sei=se, slab=model, method="FE", data=depress_former)
depress_current<-depress[which(depress$model=='mumnonsmoke_childcurrent'|depress$model=='mumsmoke_childcurrent'),]
rma(yi=logor, sei=se, slab=model, method="FE", data=depress_current)


#G1 wellbeing
well<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/wellbeing.csv',sep=''), sep=',',header = FALSE)
head(well)
colnames(well)<-c("model","logor","se","loglci","loguci")
#test heterogeneity in meta-analysis
well_all<-well[which(well$model=='mumnonsmoke_childall'|well$model=='mumsmoke_childall'),]
rma(yi=logor, sei=se, slab=model, method="FE", data=well_all)
well_never<-well[which(well$model=='mumnonsmoke_childnever'|well$model=='mumsmoke_childnever'),]
rma(yi=logor, sei=se, slab=model, method="FE", data=well_never)
well_ever<-well[which(well$model=='mumnonsmoke_childever'|well$model=='mumsmoke_childever'),]
rma(yi=logor, sei=se, slab=model, method="FE", data=well_ever)


#G1 height, sensitivity analysis
height_age<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/height_age.csv',sep=''), sep=',',header = FALSE)
head(height_age)
colnames(height_age)<-c("model","beta","se","loglci","loguci")
#test heterogeneity in meta-analysis
height_age_all<-height_age[which(height_age$model=='mumnonsmoke_childall'|height_age$model=='mumsmoke_childall'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=height_age_all)
height_age_never<-height_age[which(height_age$model=='mumnonsmoke_childnever'|height_age$model=='mumsmoke_childnever'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=height_age_never)
height_age_ever<-height_age[which(height_age$model=='mumnonsmoke_childever'|height_age$model=='mumsmoke_childever'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=height_age_ever)


#G1 age at menarche, sensitivity analysis
menarche_age<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/menarche_age.csv',sep=''), sep=',',header = FALSE)
head(menarche_age)
colnames(menarche_age)<-c("model","beta","se","loglci","loguci")
#test heterogeneity in meta-analysis
menarche_age_all<-menarche_age[which(menarche_age$model=='mumnonsmoke_childall'|menarche_age$model=='mumsmoke_childall'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=menarche_age_all)
menarche_age_never<-menarche_age[which(menarche_age$model=='mumnonsmoke_childnever'|menarche_age$model=='mumsmoke_childnever'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=menarche_age_never)
menarche_age_ever<-menarche_age[which(menarche_age$model=='mumnonsmoke_childever'|menarche_age$model=='mumsmoke_childever'),]
rma(yi=beta, sei=se, slab=model, method="FE", data=menarche_age_ever)
#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

require(metafor)


#G1 birthweight
bw<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/bw_ever_never.csv',sep=''), sep=',')
head(bw)
#test heterogeneity in meta-analysis
bw_adjust<-bw[c(1,2),]
bw_crude<-bw[c(4,5),]
rma(yi=bw, sei=se, slab=X, method="FE", data=bw_adjust)
rma(yi=bw, sei=se, slab=X, method="FE", data=bw_crude)


#G1 BMI
bmi<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/bmi.csv',sep=''), sep=',')
head(bmi)
#test heterogeneity in meta-analysis
bmi_all<-bmi[c(4,8),]
rma(yi=bmi, sei=se, slab=X, method="FE", data=bmi_all)
bmi_never<-bmi[c(3,7),]
rma(yi=bmi, sei=se, slab=X, method="FE", data=bmi_never)
bmi_former<-bmi[c(2,6),]
rma(yi=bmi, sei=se, slab=X, method="FE", data=bmi_former)
bmi_current<-bmi[c(1,5),]
rma(yi=bmi, sei=se, slab=X, method="FE", data=bmi_current)


#G1 height
height<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/height_ever_never.csv',sep=''), sep=',')
head(height)
#test heterogeneity in meta-analysis
height_all<-height[c(3,6),]
rma(yi=height, sei=se, slab=X, method="FE", data=height_all)
height_never<-height[c(2,5),]
rma(yi=height, sei=se, slab=X, method="FE", data=height_never)
height_ever<-height[c(1,4),]
rma(yi=height, sei=se, slab=X, method="FE", data=height_ever)


#G2 birthweight
bw_g<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/bw_grandmum_grandchild.csv',sep=''), sep=',')
head(bw_g)
#test heterogeneity in meta-analysis
bw_g_all<-bw_g[c(3,6),]
rma(yi=bw, sei=se, slab=X, method="FE", data=bw_g_all)
bw_g_nonsmoke<-bw_g[c(2,5),]
rma(yi=bw, sei=se, slab=X, method="FE", data=bw_g_nonsmoke)
bw_g_smoke<-bw_g[c(1,4),]
rma(yi=bw, sei=se, slab=X, method="FE", data=bw_g_smoke)


#G1 menarche
menarche<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/menarche_ever_never.csv',sep=''), sep=',')
head(menarche)
#test heterogeneity in meta-analysis
menarche_all<-menarche[c(3,6),]
rma(yi=menarche, sei=se, slab=X, method="FE", data=menarche_all)
menarche_never<-menarche[c(2,5),]
rma(yi=menarche, sei=se, slab=X, method="FE", data=menarche_never)
menarche_ever<-menarche[c(1,4),]
rma(yi=menarche, sei=se, slab=X, method="FE", data=menarche_ever)


#G1 SBP
sbp<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/sbp.csv',sep=''), sep=',')
head(sbp)
#test heterogeneity in meta-analysis
sbp_all<-sbp[c(4,8),]
rma(yi=sbp, sei=se, slab=X, method="FE", data=sbp_all)
sbp_never<-sbp[c(3,7),]
rma(yi=sbp, sei=se, slab=X, method="FE", data=sbp_never)
sbp_former<-sbp[c(2,6),]
rma(yi=sbp, sei=se, slab=X, method="FE", data=sbp_former)
sbp_current<-sbp[c(1,5),]
rma(yi=sbp, sei=se, slab=X, method="FE", data=sbp_current)


#G1 DBP
dbp<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/dbp.csv',sep=''), sep=',')
head(dbp)
#test heterogeneity in meta-analysis
dbp_all<-dbp[c(4,8),]
rma(yi=dbp, sei=se, slab=X, method="FE", data=dbp_all)
dbp_never<-dbp[c(3,7),]
rma(yi=dbp, sei=se, slab=X, method="FE", data=dbp_never)
dbp_former<-dbp[c(2,6),]
rma(yi=dbp, sei=se, slab=X, method="FE", data=dbp_former)
dbp_current<-dbp[c(1,5),]
rma(yi=dbp, sei=se, slab=X, method="FE", data=dbp_current)


#G1 years of eduaction
edu<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/eduyr_ever_never.csv',sep=''), sep=',')
head(edu)
#test heterogeneity in meta-analysis
edu_all<-edu[c(3,6),]
rma(yi=eduyr, sei=se, slab=X, method="FE", data=edu_all)
edu_never<-edu[c(2,5),]
rma(yi=eduyr, sei=se, slab=X, method="FE", data=edu_never)
edu_ever<-edu[c(1,4),]
rma(yi=eduyr, sei=se, slab=X, method="FE", data=edu_ever)


#G1 fluid intelligence
iq<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/iq.csv',sep=''), sep=',')
head(iq)
#test heterogeneity in meta-analysis
iq_all<-iq[c(4,8),]
rma(yi=iq, sei=se, slab=X, method="FE", data=iq_all)
iq_never<-iq[c(3,7),]
rma(yi=iq, sei=se, slab=X, method="FE", data=iq_never)
iq_former<-iq[c(2,6),]
rma(yi=iq, sei=se, slab=X, method="FE", data=iq_former)
iq_current<-iq[c(1,5),]
rma(yi=iq, sei=se, slab=X, method="FE", data=iq_current)


#G1 lung function, FVC
fvc<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/fvc_best.csv',sep=''), sep=',')
head(fvc)
#test heterogeneity in meta-analysis
fvc_all<-fvc[c(4,8),]
rma(yi=fvc, sei=se, slab=X, method="FE", data=fvc_all)
fvc_never<-fvc[c(3,7),]
rma(yi=fvc, sei=se, slab=X, method="FE", data=fvc_never)
fvc_former<-fvc[c(2,6),]
rma(yi=fvc, sei=se, slab=X, method="FE", data=fvc_former)
fvc_current<-fvc[c(1,5),]
rma(yi=fvc, sei=se, slab=X, method="FE", data=fvc_current)


#G1 lung function, FEV1
fev1<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/fev1_best.csv',sep=''), sep=',')
head(fev1)
#test heterogeneity in meta-analysis
fev1_all<-fev1[c(4,8),]
rma(yi=fev1, sei=se, slab=X, method="FE", data=fev1_all)
fev1_never<-fev1[c(3,7),]
rma(yi=fev1, sei=se, slab=X, method="FE", data=fev1_never)
fev1_former<-fev1[c(2,6),]
rma(yi=fev1, sei=se, slab=X, method="FE", data=fev1_former)
fev1_current<-fev1[c(1,5),]
rma(yi=fev1, sei=se, slab=X, method="FE", data=fev1_current)


#G1 asthma
asthma<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/asthma_ever_never.csv',sep=''), sep=',')
head(asthma)
#test heterogeneity in meta-analysis
asthma_all<-asthma[c(3,6),]
rma(yi=asthma, sei=se, slab=X, method="FE", data=asthma_all)
asthma_never<-asthma[c(2,5),]
rma(yi=asthma, sei=se, slab=X, method="FE", data=asthma_never)
asthma_ever<-asthma[c(1,4),]
rma(yi=asthma, sei=se, slab=X, method="FE", data=asthma_ever)


#G1 depression
depress<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/depression.csv',sep=''), sep=',')
head(depress)
#test heterogeneity in meta-analysis
depress_all<-depress[c(4,8),]
rma(yi=depress, sei=se, slab=X, method="FE", data=depress_all)
depress_never<-depress[c(3,7),]
rma(yi=depress, sei=se, slab=X, method="FE", data=depress_never)
depress_former<-depress[c(2,6),]
rma(yi=depress, sei=se, slab=X, method="FE", data=depress_former)
depress_current<-depress[c(1,5),]
rma(yi=depress, sei=se, slab=X, method="FE", data=depress_current)


#G1 wellbeing
well<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/wellbeing.csv',sep=''), sep=',')
head(well)
#test heterogeneity in meta-analysis
well_all<-well[c(3,6),]
rma(yi=happy, sei=happy_se, slab=X, method="FE", data=well_all)
well_never<-well[c(2,5),]
rma(yi=happy, sei=happy_se, slab=X, method="FE", data=well_never)
well_ever<-well[c(1,4),]
rma(yi=happy, sei=happy_se, slab=X, method="FE", data=well_ever)


#G1 height, sensitivity analysis
height_age<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/height_age.csv',sep=''), sep=',')
head(height_age)
#test heterogeneity in meta-analysis
height_age_all<-height_age[c(3,6),]
rma(yi=height, sei=se, slab=X, method="FE", data=height_age_all)
height_age_never<-height_age[c(2,5),]
rma(yi=height, sei=se, slab=X, method="FE", data=height_age_never)
height_age_ever<-height_age[c(1,4),]
rma(yi=height, sei=se, slab=X, method="FE", data=height_age_ever)


#G1 age at menarche, sensitivity analysis
menarche_age<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/menarche_age.csv',sep=''), sep=',')
head(menarche_age)
#test heterogeneity in meta-analysis
menarche_age_all<-menarche_age[c(3,6),]
rma(yi=menarche, sei=se, slab=X, method="FE", data=menarche_age_all)
menarche_age_never<-menarche_age[c(2,5),]
rma(yi=menarche, sei=se, slab=X, method="FE", data=menarche_age_never)
menarche_age_ever<-menarche_age[c(1,4),]
rma(yi=menarche, sei=se, slab=X, method="FE", data=menarche_age_ever)
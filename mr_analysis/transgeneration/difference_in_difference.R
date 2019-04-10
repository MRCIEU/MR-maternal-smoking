#set enviromental variable
##################################################################################################
##################################################################################################
#use correct path
.libPaths("C:/R/Library")

# Clear the work environment
rm(list = ls())

require(metafor)

#G2 birthweight
bw_g<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/bw_grandmum_grandchild.csv',sep=''), sep=',',header = FALSE)
head(bw_g)
colnames(bw_g)<-c("model","beta","se","lci","uci")
#G1 birthweight
bw<-read.csv(paste(Sys.getenv('Myresults'),'mini project3_plot/bw_ever_never.csv',sep=''), sep=',',header = FALSE)
head(bw)
colnames(bw)<-c("model","beta","se","lci","uci")

difference<-function(x1,x2,se1,se2){
  x<-x1-x2
  se<-sqrt(se1^2+se2^2)
  lci<-x-1.96*se
  uci<-x+1.96*se
  return(c(x,se,lci,uci))
}

#difference G0smoke-G1nonsmoke vs G0nonsmoke-G1nonsmoke
bw_g_nonsmoke<-bw_g[which(bw_g$model=="g0smoke_g1nonsmoke"|bw_g$model=="g0nonsmoke_g1nonsmoke"),]
difference1<-difference(bw_g_nonsmoke$beta[2],bw_g_nonsmoke$beta[1],bw_g_nonsmoke$se[2],bw_g_nonsmoke$se[1])
difference1
#difference G0smoke-G1smoke vs G0nonsmoke-G1smoke
bw_g_smoke<-bw_g[which(bw_g$model=="g0smoke_g1smoke"|bw_g$model=="g0nonsmoke_g1smoke"),]
difference2<-difference(bw_g_smoke$beta[2],bw_g_smoke$beta[1],bw_g_smoke$se[2],bw_g_smoke$se[1])
difference2
#difference between the two differences
difference(difference2[1],difference1[1],difference2[2],difference1[2])

diffbydiff <- data.frame("slab" = c("difference1","difference2"), "bw" = c(difference1[1],difference2[1]), "se" = c(difference1[2],difference2[2]))
head(diffbydiff)
rma(yi=bw, sei=se, slab=slab, method="FE", data=diffbydiff)

#difference G1SNP-G1bw(G0 smoker in pregnancy) vs G1SNP-G2bw(G1 smoker in pregnancy) reported in Discussion
rdb<-difference(bw_g$beta[3],bw$beta[6],bw_g$se[3],bw$se[6])
rdb
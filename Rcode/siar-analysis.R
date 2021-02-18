# ---- SIAR -----
# mixture models and wrymouth diet

# load packages
library(siar)
library(betareg)
library(lmtest)

# df
# Creat source file for analysis

graphics.off()
sources<-df%>%
  filter(name!="C. maculatus")%>%
  filter(clusterCut==5 | clusterCut==4 | clusterCut==2)%>%
  group_by(clusterCut)%>%
  summarise(Meand15N=mean(d15Navg),d15Nse=sd(d15Navg)/sqrt(n()),
            Meand13C=mean(d13Cavg),d13Cse=sd(d13Cavg)/sqrt(n()))%>%
  rename(Source=clusterCut)%>%
  as.data.frame()
consumers<-read.csv("./data/consumer.csv")
consumers<-as.matrix(consumers)
corrections<-sources%>%
  dplyr::select(Source)%>%
  mutate(Meand15N=3.4, 
         SDd15N=0.98,  # all corrections
         Mean13C=0.39,
         SDd13C=1.3)%>%
  as.data.frame()
concs<-0
model1 <- siarmcmcdirichletv4(consumers, sources,corrections,concs,iterations=200000)
siarplotdata(model1,iso=c(2,1))
siarhistograms(model1)

siarproportionbysourceplot(model1,grp=1)
siarproportionbygroupplot(model1,grp=1)
siarproportionbygroupplot(model1,grp=2)
siarproportionbygroupplot(model1,grp=3)
siarproportionbygroupplot(model1,grp=4)


hdrs<-siarhdrs(model1)
siarmatrixplot(model1)

model1$output

results<-read.csv("./data/hdrs.csv")
str(results)
results<-results%>%
  mutate(High95hdr=replace(High95hdr,High95hdr>1 & group==1 & parameter ==2,1.0))


# analysis of change in diet proportions

betaresults<-results%>%
  filter(parameter!="SD1")%>%
  filter(parameter!="SD2")%>%
  mutate(season="spring")%>%
  mutate(site="LAR")%>%
  mutate(season=replace(season,group==3,"summer"))%>%
  mutate(season=replace(season,group==4,"summer"))%>%
  mutate(site=replace(site,group==2,"MHC"))%>%
  mutate(site=replace(site,group==4,"MHC"))

bm<-betareg(mean~factor(parameter)+site+season,data=betaresults)
plot(bm)
hist(resid(bm))
plot(bm,which=1,type="pearson",caption = NULL)
plot(bm,which=4,type="pearson",caption=NULL)
plot(bm,which=5,type="deviance",caption = NULL)
plot(bm,which=2,type="pearson",caption = NULL)
summary(bm)

m.int<-betareg(mean~1,data=betaresults)
m.par<-betareg(mean~1+factor(parameter),data=betaresults)
m.site<-betareg(mean~1+factor(parameter)+site,data=betaresults)
m.season<-betareg(mean~1+factor(parameter)+site+season,data=betaresults)

ANODEV<-lrtest(m.int,m.par,m.site,m.season)
beta.model<-as.data.frame(ANODEV)%>%rename(numDf='#Df')

Anova.table<-beta.model%>%
  mutate(Deviance=2*LogLik)%>%
  mutate(dDeviance=Deviance-lag(Deviance))%>%
  mutate(LR=exp(dDeviance/2))%>%
  mutate(AIC=dDeviance-2*numDf)%>%
  mutate(LR_Df=LR/Df)%>%
  mutate(dAIC=AIC-lag(AIC))


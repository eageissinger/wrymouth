# ---- cluster analysis ----
# heirarchical cluster analysis for d13C and d15N of wrymouth
# required to run "data-format.R" script beforehand

# create dataframe of summary data for each species
isocluster<-data%>%
  group_by(type,name)%>%
  summarise(d15Navg=mean(d15N),d13Cavg=mean(d13C),
            d15Nse=sd(d15N)/sqrt(n()),d13Cse=sd(d13C)/sqrt(n()))%>%
  ungroup()%>%
  as.data.frame()

# full cluster (d15N and d13C)
clusters<-hclust(dist(isocluster[,3:4]),method='average')  
plot(clusters)

clusterCut<-cutree(clusters,6)
c.vector<-as.data.frame(clusterCut)
table(clusterCut,isocluster$name)
df<-cbind(isocluster,clusterCut)%>%
  filter(clusterCut!=7)
ggplot(df,aes(x=d13Cavg,y=d15Navg,col=as.factor(clusterCut)))+geom_point()+
  geom_text_repel(aes(x=d13Cavg,y=d15Navg,label=name))

#d13C clusters
Cclusters<-hclust(dist(isocluster[,4]),method='average')
plot(Cclusters)
clusterCutC<-cutree(Cclusters,5)
dfC<-cbind(isocluster,clusterCutC)
ggplot(df,aes(x=d13Cavg,y=d15Navg,col=as.factor(clusterCutC)))+
  geom_point()

dfC%>%
  group_by(clusterCutC)%>%
  summarise(min(d13Cavg),max(d13Cavg))

#d15N clusters
clustersN<-hclust(dist(isocluster[,3]),method='average')
plot(clustersN)
clusterCutN<-cutree(clustersN,4)
dfN<-cbind(isocluster,clusterCutN)
ggplot(dfN,aes(x=d13Cavg,y=d15Navg,col=as.factor(clusterCutN)))+
  geom_point()#+
# geom_text_repel(position="identity",aes(label=name))

dfN%>%
  group_by(clusterCutN)%>%
  summarise(mean(d15Navg))

# 1.6; 2.03; 2.48
# mean fractionation 2.29 (4 clusters)
N.frac<-c(1.6,2.03,2.48)
mean( N.frac)
sd(N.frac)


# ---- trophic structure ----
# Base: Mya Arenaria
# dn = 2.04
# TP = 1+ ([d15Con-d15NBase]/dn)

# prepare data for TP calculations
isoSUM<-data%>%
  group_by(type,site,season,name)%>%
  summarise(d15Navg=mean(d15N),d15Nse=sd(d15N)/sqrt(n()),
            d13Cavg=mean(d13C),d13Cse=sd(d13C)/sqrt(n()))%>%
  ungroup()%>%
  mutate(site=as.character(site))%>%
  mutate(base=8.06)%>%
  mutate(base=replace(base,site=="LAR" & season=="summer",7.55))%>%
  mutate(base=replace(base,site=="MHC" & season=="spring",8.87))%>%
  mutate(base=replace(base,site=="MHC" & season=="summer",7.34))%>%
  mutate(TP=2+((d15Navg-base)/3.4))%>%
  mutate(TPsd=(2+((d15Navg-base)/(3.4+.98)))-TP)%>%
  filter(name!="POM")

### Trophic position of inidivual wrymouth
wryTP<-data%>%
  dplyr::select(site, type, name, fish_number, d15N, d13C, season)%>%
  filter(type=="Cryptacanthodes maculatus")%>%
  mutate(site=as.character(site))%>%
  mutate(base=8.06)%>%
  mutate(base=replace(base,site=="LAR" & season=="summer",7.55))%>%
  mutate(base=replace(base,site=="MHC" & season=="spring",8.87))%>%
  mutate(base=replace(base,site=="MHC" & season=="summer",7.34))%>%
  mutate(TP=2+((d15N-base)/3.4))%>%
  mutate(TPsd=(2+((d15N-base)/(3.4+.98)))-TP)%>%
  filter(name!="POM")

# TP analysis for wrymouth
m.TP1<-lm(TP~season*site,data=wryTP)
plot(m.TP1)
summary(m.TP1)
Anova(m.TP1,type="III")

# create a dataframe with species name and cluster assignment
df2<-df%>%
  dplyr::select(name,clusterCut)

# combine cluster assignments to isotope data
iso<-left_join(isoSUM,df2)

# view M. arenaria (base)
iso%>%
  filter(type=="Mya arenaria")

# View wrymouth (predator)
iso%>%
  filter(type=="Cryptacanthodes maculatus")


# determine TP equation for each season and locaiton
ggplot(iso)+geom_point(aes(y=TP,x=d15Navg,colour=site,shape=season))

# LAR Spring
m5<-lm(TP~d15Navg,data=filter(iso,site=="LAR" & season == "spring"))
plot(m5)
Anova(m5,type="III")
summary(m5)
# LAR spring TP = -1.951 + 0.4902x
# TP = -0.3706 + 0.2941x

#LAR Summer
m6<-lm(TP~d15Navg,data=filter(iso,site=="LAR" & season=="summer"))
plot(m6)
summary(m6)
# LAR summer TP = -1.701 + 0.4902x
# TP = -.2206 +0.2941x

# MHC Spring
m7<-lm(TP~d15Navg,data=filter(iso,site=="MHC" & season == "spring"))
plot(m7)
summary(m7)

# MHC spring TP = -2.348 + 0.4902x
# TP = -0.6088 + 0.2941x

# MHC Summer
m8<-lm(TP~d15Navg,data=filter(iso,site=="MHC" & season == "summer"))
plot(m8)
summary(m8)


# wrymouth trophic position
iso%>%
  filter(type=="Cryptacanthodes maculatus")


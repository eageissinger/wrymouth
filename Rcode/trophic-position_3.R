# Trophic position analysis for wrymouth and all sampled species

# R script #3

# required to run "data-format.R" script beforehand

# load packages
library(car)
library(tidyverse)

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


# view M. arenaria (base)
isoSUM%>%
  filter(type=="Mya arenaria")

# View wrymouth (predator)
isoSUM%>%
  filter(type=="Cryptacanthodes maculatus")


# determine TP equation for each season and locaiton
ggplot(isoSUM)+geom_point(aes(y=TP,x=d15Navg,colour=site,shape=season))

# LAR Spring
m5<-lm(TP~d15Navg,data=filter(isoSUM,site=="LAR" & season == "spring"))
plot(m5)
anova(m5)
summary(m5)
# LAR spring TP = -1.951 + 0.4902x
# TP = -0.3706 + 0.2941x

#LAR Summer
m6<-lm(TP~d15Navg,data=filter(isoSUM,site=="LAR" & season=="summer"))
plot(m6)
summary(m6)
# LAR summer TP = -1.701 + 0.4902x
# TP = -.2206 +0.2941x

# MHC Spring
m7<-lm(TP~d15Navg,data=filter(isoSUM,site=="MHC" & season == "spring"))
plot(m7)
summary(m7)

# MHC spring TP = -2.348 + 0.4902x
# TP = -0.6088 + 0.2941x

# MHC Summer
m8<-lm(TP~d15Navg,data=filter(isoSUM,site=="MHC" & season == "summer"))
plot(m8)
summary(m8)


# wrymouth trophic position
isoSUM%>%
  filter(type=="Cryptacanthodes maculatus")


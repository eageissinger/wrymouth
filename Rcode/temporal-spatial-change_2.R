# ---- Temporal and Spatial variability ----

# R script #2

# load required package
library(car)
# required to run "data-format.R" script beforehand

# will use the data df from data-format.R

# changes in d13C and d15N
# changes in d13C and d15N for wrymouth only

# create dataframe that is just wrymouth with relevant facotrs
# length data
fishTL<-gut%>%
  select(fish_number,length_cm,weight_g,sex)%>%
  distinct()

wry<-data%>%
  filter(name=="C. maculatus")%>%
  select(fish_number,season,site,d13C,d15N)%>%
  left_join(fishTL)

wry0<-lm(d13C~length_cm,data=wry)
plot(wry0)
Anova(wry0,type="III")
summary(wry0)

wry1<-lm(d13C~site+season+length_cm,data=wry)
plot(wry1)
Anova(wry1,type="III")
summary(wry1)

wry2<-lm(d15N~site+season+length_cm,data=wry)
plot(wry2)
Anova(wry2,type="III")
summary(wry2)

# remaining species

# create list of species that have n > 1
check<-data%>%
  group_by(type,season,site)%>%
  summarise(n=n())%>%
  filter(n>1)
View(check)

# site x season
# C. maenas, L. littorina, M. arenaria

C.maenas1<-lm(d13C~site+season,data=filter(data,type=="Carcinus maenas"))
C.maenas1
plot(fitted(C.maenas1),resid(C.maenas1))
qqnorm(resid(C.maenas1))
qqline(resid(C.maenas1))
Anova(C.maenas1,type="III")
summary(C.maenas1)

C.maenas2<-lm(d15N~site+season,data=filter(data,type=="Carcinus maenas"))
C.maenas2
plot(fitted(C.maenas2),resid(C.maenas2))
qqnorm(resid(C.maenas2))
qqline(resid(C.maenas2))
Anova(C.maenas2,type="III")

littorina1<-lm(d15N~site+season,data=filter(data,type=="Littorina littorea"))
plot(fitted(littorina1),resid(littorina1))
qqnorm(resid(littorina1))
qqline(resid(littorina1))
Anova(littorina1,type="III")

littorina2<-lm(d13C~site*season,data=filter(data,type=="Littorina littorea"))
plot(fitted(littorina2),resid(littorina2))
qqnorm(resid(littorina2))
qqline(resid(littorina2))
Anova(littorina2,type="III")
summary(littorina2)

mya1<-lm(d13C~site+season,data=filter(data,type=="Mya arenaria"))
plot(fitted(mya1),resid(mya1))
qqnorm(resid(mya1))
qqline(resid(mya1))
Anova(mya1,type="III")
summary(mya1)

mya2<-lm(d15N~site+season,data=filter(data,type=="Mya arenaria"))
plot(fitted(mya2),resid(mya2))
qqnorm(resid(mya2))
qqline(resid(mya2))
Anova(mya2,type="III")
summary(mya2)

# Site only
virens1<-lm(d13C~site,data=filter(data,type=="Alitta virens" & season== "spring"))
plot(fitted(virens1),resid(virens1))
qqnorm(resid(virens1))
qqline(resid(virens1))
Anova(virens1,type="III")

virens2<-lm(d15N~site,data=filter(data,type=="Alitta virens" & season== "spring"))
plot(fitted(virens2),resid(virens2))
qqnorm(resid(virens2))
qqline(resid(virens2))
Anova(virens2,type="III")
summary(virens2)

edulis1<-lm(d13C~site,data=filter(data,type=="Mytilus edulis" & season=="spring"))
plot(fitted(edulis1),resid(edulis1))
qqnorm(resid(edulis1))
qqline(resid(edulis1))
Anova(edulis1,type="III")
summary(edulis1)

edulis2<-lm(d15N~site,data=filter(data,type=="Mytilus edulis" & season=="spring"))
plot(fitted(edulis2),resid(edulis2))
qqnorm(resid(edulis2))
qqline(resid(edulis2))
Anova(edulis2,type="III")

nephtys1<-lm(d13C~site,data=filter(data,type=="Nephtys spp." & season == "spring"))
plot(nephtys1)
Anova(nephtys1,type="III")
summary(nephtys1)

nephtys2<-lm(d15N~site,data=filter(data,type=="Nephtys spp." & season == "spring"))
plot(nephtys2)
Anova(nephtys2,type="III")
summary(nephtys2)

nephtys1<-lm(d13C~season,data=filter(data,type=="Nephtys spp." & site == "LAR"))
plot(nephtys1)
Anova(nephtys1,type="III")

nephtys2<-lm(d15N~season,data=filter(data,type=="Nephtys spp." & site == "LAR"))
plot(nephtys2)
Anova(nephtys2,type="III")
summary(nephtys2)

eximius1<-lm(d13C~site,data=filter(data,type=="Polycirrus eximius" & season == "spring"))
plot(eximius1)
Anova(eximius1, type="III")
summary(eximius1)

eximius2<-lm(d15N~site,data=filter(data,type=="Polycirrus eximius" & season == "spring"))
plot(eximius2)
Anova(eximius2, type="III")
summary(eximius2)

balanoides1<-lm(d13C~site,data=filter(data,type=="Semibalanus balanoides" & season == "spring"))
plot(balanoides1)
Anova(balanoides1, type="III")
summary(balanoides1)

balanoides2<-lm(d15N~site,data=filter(data,type=="Semibalanus balanoides" & season == "spring"))
plot(balanoides2)
Anova(balanoides2, type="III")

# season
virens3<-lm(d13C~season, data=filter(data,type=="Alitta virens" & site == "MHC"))
plot(virens3)
Anova(virens3, type="III")

virens4<-lm(d15N~season,data=filter(data,type=="Alitta virens" & site == "MHC"))
plot(virens4)
Anova(virens4,type="III")
summary(virens4)

limecola1<-lm(d13C~season, data=filter(data,type=="Limecola balthica" & site == "LAR"))
plot(limecola1)
Anova(limecola1, type="III")

limecola2<-lm(d15N~season, data=filter(data,type=="Limecola balthica" & site == "LAR"))
plot(limecola2)
Anova(limecola2,type="III")

metridium1<-lm(d13C~season, data=filter(data,type=="Metridium dianthus" & site=="MHC"))
plot(metridium1)
Anova(metridium1,type="III")
summary(metridium1)

metridium2<-lm(d15N~season, data=filter(data,type=="Metridium dianthus" & site=="MHC"))
plot(metridium2)
Anova(metridium2,type="III")

edulis3<-lm(d13C~season, data=filter(data, type=="Mytilus edulis" & site == "MHC"))
plot(edulis3)
Anova(edulis3,type="III")
summary(edulis3)

edulis4<-lm(d15N~season, data=filter(data, type=="Mytilus edulis" & site == "MHC"))
plot(edulis4)
Anova(edulis4,type="III")

eximius3<-lm(d13C~season, data=filter(data, type=="Polycirrus eximius" & site == "LAR"))
plot(eximius3)
Anova(eximius3, type="III")

eximius4<-lm(d15N~season, data=filter(data, type=="Polycirrus eximius" & site == "LAR"))
plot(eximius4)
Anova(eximius4, type="III")
summary(eximius4)

# Cluster analysis for full community

# R script #5


# remove outlier and clean data
isocluster<-data%>%
  filter(d13C>(-22))%>%
  group_by(type,name)%>%
  summarise(d13Cavg=mean(d13C),d15Navg=mean(d15N),
            d15Nse=sd(d15N)/sqrt(n()),d13Cse=sd(d13C)/sqrt(n()))%>%
  ungroup()%>%
  as.data.frame()


# load package
library(mclust)

# trial run

# format data - comine site and location combos (for now)

# S<-c(isocluster$season_site)
# table(S)
head(isocluster)
X<-isocluster[,3:4]
head(X)


clPairs(X,S)

BIC<-mclustBIC(X)
plot(BIC)
summary(BIC)

mod1<-Mclust(X,x=BIC)
summary(mod1,parameters = TRUE)

# table(S,mod1$classification)

plot(mod1,what="uncertainty")
plot(mod1,what="classification")

# --- by season and site---
iso.distinct<-data%>%
  filter(d13C>(-22))%>%
  select(season, site, d13C, d15N)%>%
  unite("season_site",1:2, sep="_")
S<-c(iso.distinct$season_site)
table(S)
X<-iso.distinct

output<-mod1$BIC
output
write.csv(output, "./output/full-cluster-output.csv",row.names = FALSE)

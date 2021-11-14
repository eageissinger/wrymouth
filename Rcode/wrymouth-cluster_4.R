# Cluster analysis for wrymouth

# R script #4

# required to run "data-format.R" script beforehand

# length data
fishTL<-gut%>%
  select(fish_number,length_cm,weight_g)%>%
  distinct()

wry<-data%>%
  filter(name=="C. maculatus")%>%
  select(fish_number,season,site,d13C,d15N)%>%
  left_join(fishTL)%>%
  select(-fish_number)%>%
  unite("season_site",1:2, sep="_")


# load package
library(mclust)

# trial run

# format data - combine site and location combos (for now)
S<-c(wry$season_site)
table(S)
head(wry)
X<-wry[,2:3]
head(X)

clPairs(X,S)

mod <- MclustDA(X, S, modelType = "EDDA")
summary(mod)
plot(mod, what = "scatterplot")
plot(mod, what = "classification")
mod
mod$models


moddr <- MclustDR(mod)
summary(moddr)
plot(moddr, what = "scatterplot")
plot(moddr, what = "boundaries", ngrid = 200)


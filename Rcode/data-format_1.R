# ---- Wrymouth data formatting ----

# R script #1

# read in raw data, format for analysis
# identify functional feeding groups for sampled species

# load packages

library(lubridate)
library(tidyverse)
library(ggrepel)

#load data
gut<-read.csv("./data/gut_contents.csv")
species<-read.csv("./data/species_list.csv")
isotope<-read.csv("./data/wry_isotope_full.csv")

# explore/check data
head(gut)
head(species)
head(isotope)

# create new species df to match isotope format
species%>%
  dplyr::select(scientific_name,common_name)%>%
  rename(type=scientific_name)%>%
  distinct(type,common_name)->species2

# combine isotope data with species data
data<-left_join(isotope,species2,by=c("common_name","type"))%>%
  filter(d15N != 10.99 & d13C != -15.15)%>% # take out outlier z. marina value (high N)
  filter(d15N !=11.27 & d13C !=-14.89)%>% # take out outlier z. marina value (high N)
  mutate(season=as.character(season))%>% # convert season to character class
  filter(season!="fall")%>% # remove fall data (not enough for analysis)
  filter(d13C!=-34.25)%>% # remove outlier value
  mutate(date=ymd(paste(year,month,day, sep="-")))


# Diet analysis based on functional groups
head(data)

data%>%distinct(name)

# clean gut content data
#clean data
gut2<-gut%>%
  mutate(contents=as.character(contents))%>%
  mutate(contents=replace(contents,contents=="E. trilineata", "Eteone trilineata"))%>%
  filter(contents!="empty")%>%
  mutate(contents=replace(contents, contents=="stickleback", 'Gasterosteidae spp.'))

# ---- functional groups -----

fun.group<-data.frame(name=c("Nephtys spp.","P. affinis","P. eximius","B. neotena",
                             "E. trilineata","A. virens","C. torquata","Glycera spp.",
                             "C. maenas","I. baltica","G. oceanicus","S. balanoides",
                             "C. volutator","H. americanus","C. bigelowi","C. septemspinosa",
                             "C. maculatus","P. americanus","M. dianthus","S. kowalevskii",
                             "M. edulis","M. arenaria","T. testudinalis","L. balthica",
                             "C. fornicata","L. littorea", "E. triseriata","C. lacteus",
                             "A. clathratum","A. nodosum", "C. crispus","D. incrassata","F. vesiculosus",
                             "P. latifolia","P. plantaginea","S. latissima", "U. intestinalis",
                             "U. lactuca", "Z. marina"),
                      f.group=c("Mobile predator","Mobile deposit feeder","Tube-dwelling deposit feeder","Mobile predator",
                                "Mobile predator","Mobile predator", "Tube-dwelling deposit feeder","Mobile predator",
                                "Mobile predator","Mobile grazer","Mobile grazer","Sedentary filter feeder",
                                "Mobile deposit feeder","Mobile predator","Mobile deposit feeder","Mobile omnivore",
                                "Mobile predator","Mobile predator","Sedentary predator","Sedentary filter/deposit feeder",
                                "Sedentary filter feeder","Sedentary filter feeder","Mobile grazer","Sedentary deposit feeder",
                                "Sedentary filter feeder","Mobile grazer","Mobile predator","Mobile predator",
                                "Algae","Algae", "Algae","Algae","Algae",
                                "Algae", "Algae", "Algae","Algae",
                                "Algae", "Plant"))
fundata<-left_join(data,fun.group)


# graphic visualization
fundata%>%
  group_by(f.group,name)%>%
  summarise(d15Navg=mean(d15N),d13Cavg=mean(d13C))%>%
  ggplot()+
  geom_point(aes(x=d13Cavg,y=d15Navg,colour=f.group),size=3)+
  geom_text_repel(aes(x=d13Cavg,y=d15Navg,label=name),size=3.5)



# ---- Wrymouth - exploratory analysis ----

# load packages

library(lubridate)
library(lme4)
library(car)
library(vegan)
library(tidyverse)
library(ggrepel)
library(ggpubr)

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


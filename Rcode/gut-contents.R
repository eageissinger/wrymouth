# ---- gut contents ----
# gut content analysis for wrymouth
# required to run "data-format.R" script beforehand

str(gut)

#clean data
gut2<-gut%>%
  mutate(contents=as.character(contents))%>%
  mutate(contents=replace(contents,contents=="E. trilineata", "Eteone trilineata"))%>%
  filter(contents!="empty")%>%
  mutate(contents=replace(contents, contents=="stickleback", 'Gasterosteidae spp.'))

ggplot(gut2)+geom_bar(aes(x=contents))+
  theme(axis.text.x = element_text(angle=45,hjust=1))


# gut fullness analysis


# fullness based on gut weight
gut3<-gut2%>%
  mutate(fullness=WFG-WEG)

# no difference in fullness
gut.ind<-gut3%>%
  dplyr::select(-contents,-notes)%>%
  distinct()
gut.m1<-lm(fullness~site+factor(month)+sex,data=gut.ind)
plot(gut.m1)
hist(resid(gut.m1))
Anova(gut.m1,type='III')
summary(gut.m1)

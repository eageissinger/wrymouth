# Figures
# see code for each analysis prior to running code for figures
library(tidyverse)
library(ggpubr)


# ---- Figure 3 ----
# Trophic position of wrymouth
# see cluster-trophic-position.R

Fig3<-wryTP%>%
  mutate(season=replace(season,season=="spring","Spring"))%>%
  mutate(season=replace(season,season=="summer","Summer"))%>%
  ggplot()+
  geom_boxplot(aes(x=season,y=TP,fill=site))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_fill_manual(values=c("grey45","grey70"))+
  theme(axis.text=element_text(size=12))+
  theme(axis.title = element_text(size=13))+
  guides(fill=guide_legend(title="Location"))+
  theme(legend.position = "top")+
  ylab("Trophic Position")+
  xlab("Season")+
  theme(legend.title = element_text(size=13))+
  theme(legend.text=element_text(size=12))+
  theme(axis.text=element_text(colour="black"))

 ggsave(file="./output/Fig3.jpg",plot=Fig3,width=4, height=4,units="in",dpi=300)  


# ---- Figure 5 -----
# Functional feeding group figure

isonames<-fundata%>%
  distinct(name,type)
abrev<-c('Neph','P. aff','P. exi','B. neo','E. tri',
         'A. vir','C. tor','Gly','C. mae','I. bal','G. oce',
         'S. bal','C. vol','H. amer','C. big','C. sep',
         'U. lac','U. int','Wry','P. amer','M. dia',
         'S. sac','M. edu','M. are','T. test','L. balt','C. for','L. lit',
         'E. tris','C. lac','A. clath','F. ves','S. lat',
         'P. lat','P. plan','A. nod','D. inc','C. crisp','Z. mar')

isoabrev<-cbind(isonames,abrev)

fun.sum<-fundata%>%
  group_by(name,type,f.group)%>%
  summarise(d15Navg=mean(d15N),d15Nse=sd(d15N)/sqrt(n()),
            d13Cavg=mean(d13C),d13Cse=sd(d13C)/sqrt(n()))%>%
  left_join(isoabrev)%>%
  filter(name!="C. maenas")%>%
  filter(name!="Glycera spp.")%>%
  filter(name!= "H. americanus")%>%
  filter(name!="P. latifolia")

clrs<-c("Mobile predator" = "mediumorchid4","Sedentary predator" = "mediumpurple2",
        "Mobile omnivore" = "palevioletred2", "Mobile grazer" = "darkolivegreen",
        "Sedentary filter feeder" = "dodgerblue", "Sedentary filter/deposit feeder" = "orange",
        "Sedentary deposit feeder" = "gold4", "Tube-dwelling deposit feeder" = "yellow3",
        "Mobile deposit feeder" = "peru", "Algae" = "brown", "Plant" = "darkgreen")  
# group1<-fun.sum%>%
#   filter(d13Cavg>=(-14.1))
# group2<-fun.sum%>%
#   filter(d13Cavg<(-14.1))

Fig5<-ggplot()+
  geom_vline(xintercept=-14.25,linetype='longdash',colour='gray30',size=1.25)+
  geom_hline(yintercept = 4.150289,linetype="dashed",colour='gray')+
  geom_hline(yintercept=7.550493, linetype='dashed',colour='gray')+
  geom_hline(yintercept=10.9507,linetype='dashed',colour='gray')+
  geom_errorbar(data=fun.sum,aes(x=d13Cavg, y=d15Navg,ymin=d15Navg-d15Nse,ymax=d15Navg+d15Nse),width=0,colour='darkgrey')+
  geom_errorbarh(data=fun.sum,aes(x=d13Cavg, y=d15Navg,xmin=d13Cavg-d13Cse,xmax=d13Cavg+d13Cse),height=0,colour="darkgrey")+
  geom_point(data=fun.sum,aes(x=d13Cavg,y=d15Navg,colour=as.factor(f.group)),size=2)+
  #geom_errorbar(data=MHCwry1,aes(ymin=d15Navg-d15Nse,ymax=d15Navg+d15Nse),width=0)+
  #geom_errorbarh(data=MHCwry1,aes(xmin=d13Cavg-d13Cse,xmax=d13Cavg+d13Cse),height=0)+
  geom_point(data=filter(fun.sum, name == "C. maculatus"),shape=22,size=2,aes(x=d13Cavg,y=d15Navg),fill='black')+
  theme_classic()+
  #geom_text_repel(data=filter(iso2,site=="LAR" & season == "summer"),aes(label=abrev),size=3.5)+
  geom_text_repel(data=fun.sum,aes(x=d13Cavg,y=d15Navg,label=abrev),size=5)+
  labs(x=expression(paste(delta^{13},"C (‰)")),
       y=expression(paste(delta^{15},"N (‰)")),
       colour="Feeding Group")+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.text=element_text(size=11))+
  theme(axis.title=element_text(size=12))+
  #xlim(-20.4,-7)+
  ylim(4,14)+
  theme(title = element_text(size=11))+
  scale_colour_manual(values=clrs)+
  theme(plot.title = element_text(hjust=0.5))+
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        legend.position = "right",
        legend.key.size = unit(1.0, "cm"))+
  annotate("text",x=(-17.3),y=13.5,label="Cluster 1",
           size=8)+
annotate("text",x=(-11),y=13.5,label="Cluster 2",
         size=8)

ggsave(file="./output/Fig5.jpg",plot=Fig5,width=6, height=7,units="in",dpi = 300)

# ---- Figure 6 ----
# Gut Contents

Fig6a<-ggplot()+
  geom_bar(data=filter(gut2,month==5),mapping=aes(x=forcats::fct_infreq(contents),fill=site),colour='black')+
  theme(axis.title.y = element_text(margin=margin(r=15)))+
  theme(axis.text = element_text(size=8))+
  theme(axis.title=element_text(size=11))+
  labs(x="Contents",
       y="Occurence",
       fill="Location")+
  scale_fill_grey()+
  theme_classic()+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  theme(plot.margin=unit(c(0.5,0,0.25,.5),"cm"))+
  ggtitle("Spring")+
  theme(plot.title=element_text(hjust=0.5))+
  theme(axis.text=element_text(colour='black'))
Fig6b<-ggplot()+
  geom_bar(data=filter(gut2,month==8),mapping=aes(x=forcats::fct_infreq(contents),fill=site),colour='black')+
  theme(axis.title.y = element_text(margin=margin(r=15)))+
  theme(axis.text = element_text(size=8))+
  theme(axis.title=element_text(size=11))+
  labs(x="Contents",
       y="Occurence",
       fill="Location")+
  scale_fill_grey()+
  theme_classic()+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  theme(plot.margin=unit(c(0.5,0,0.25,1.25),"cm"))+
  ggtitle("Summer")+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.text = element_text(colour="black"))
Fig6<-ggarrange(Fig2a,Fig2b,ncol=2,nrow=1,labels=c("a","b"),legend = 'top',common.legend = TRUE)

ggsave(file="./output/Fig6.jpg",plot=Fig6,width=6, height=4,units="in",dpi = 300)

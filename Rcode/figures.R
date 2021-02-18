# Figures
# see code for each analysis prior to running code for figures

# ---- Figure 2 ----
# Gut Contents
# see gut-contents.R

# gut contents

Fig2a<-ggplot()+
  geom_bar(data=filter(gut,month==5),mapping=aes(x=forcats::fct_infreq(contents),fill=site),colour='black')+
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
Fig2b<-ggplot()+
  geom_bar(data=filter(gut,month==8),mapping=aes(x=forcats::fct_infreq(contents),fill=site),colour='black')+
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
Fig2<-ggarrange(Fig3a,Fig3b,ncol=2,nrow=1,labels=c("a","b"),legend = 'top',common.legend = TRUE)

ggsave(file="./output/Fig2.jpg",plot=Fig2,width=6, height=4,units="in",dpi = 300)

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


# --- Figure 4 ----
# isotope figures with clusters and TP  position
# see cluster-trophic-position.R

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# MCH summer TP = -1.598 + 0.4902x
# MHC spring TP = -2.348 + 0.4902x
# LAR summer TP = -1.701 + 0.4902x
# LAR spring TP = -1.951 + 0.4902x
isonames<-isoSUM%>%
  distinct(name,type)
abrev<-c('A.cla','A.vir','A.nod','M.neo','C.mae',
         'C.big','C.lac','C.cri','C.tor','C.vol',
         'C.sep','C.for','C.mac','D.inc','E.tril',
         'E.tris','F.ves','G.oce','Gly','H.amer',
         'I.bal','L.bal','L.lit','M.dia','M.are',
         'M.edu','Neph','P.aff','P.exi','P.amer', 'P.lat',
         'P.plan','S.lat','S.kow','S.bal',
         'T.test','U.int','U.lac', 'Z.mar')
isoabrev<-cbind(isonames,abrev)
iso2<-left_join(iso,isoabrev)



# MHC spring TP = -2.348 + 0.4902x
# (TP + 2.348)/0.4902 = x
(1 + 2.348)/0.4902 # 6.829865
(2 + 2.348)/0.4902 # 8.869849
(3 + 2.348)/0.4902 # 10.90983
(4 + 2.348)/0.4902 # 12.94982

# TP = -0.6088 + 0.2941x
(1+0.6088)/0.2941
(2+0.6088)/0.2941
(3+0.6088)/0.2941
(4+0.6088)/0.2941

p.MHC1<-ggplot(data=filter(iso2,site=="MHC" & season=="spring"),aes(x=d13Cavg,y=d15Navg))+
  geom_hline(yintercept = 5.450248,linetype="dashed",colour='gray')+
  geom_hline(yintercept=8.870452, linetype='dashed',colour='gray')+
  geom_hline(yintercept=12.27066,linetype='dashed',colour='gray')+
  geom_hline(yintercept=15.67086,linetype='dashed',colour='gray')+
  geom_errorbar(aes(ymin=d15Navg-d15Nse,ymax=d15Navg+d15Nse),width=0)+
  geom_errorbarh(aes(xmin=d13Cavg-d13Cse,xmax=d13Cavg+d13Cse),height=0)+
  geom_point(aes(x=d13Cavg,y=d15Navg,fill=as.factor(clusterCut)),shape=21,size=2)+
  #geom_errorbar(data=MHCwry1,aes(ymin=d15Navg-d15Nse,ymax=d15Navg+d15Nse),width=0)+
  #geom_errorbarh(data=MHCwry1,aes(xmin=d13Cavg-d13Cse,xmax=d13Cavg+d13Cse),height=0)+
  geom_point(data=filter(iso2,site=="MHC" & season == "spring" & name=="C. maculatus"),shape=22,size=2,aes(fill=as.factor(clusterCut)))+
  theme_classic()+
  geom_text_repel(data=filter(iso2,site=="MHC" & season == "spring"),aes(label=abrev),size=3.5)+
  #geom_text_repel(data=MHCwry1,aes(label=abrev),size=4)+
  labs(x=expression(paste(delta^{13},"C (‰)")),
       y=expression(paste(delta^{15},"N (‰)")))+
  theme(axis.text=element_text(size=12))+
  xlim(-22.4,-7)+
  ylim(3,14)+
  theme(title = element_text(size=12))+
  scale_fill_manual(values=cbPalette)+
  labs(fill="Cluster")+
  ggtitle("MHC in Spring")+
  theme(plot.title = element_text(hjust=0.5))
p.MHC1

# MCH summer TP = -1.598 + 0.4902x
# (TP + 1.598)/0.4902 = x
(1+ 1.598)/0.4902 # 5.299878
(2+ 1.598)/0.4902 # 7.339861
(3+ 1.598)/0.4902 # 9.379845
(4+ 1.598)/0.4902 # 11.41983

# TP = -0.1588 + 0.2941x
(1+0.1588)/0.2941
(2+0.1588)/0.2941
(3+0.1588)/0.2941
(4+0.1588)/0.2941

p.MHC2<-ggplot(data=filter(iso2,site=="MHC" & season == "summer"),aes(x=d13Cavg,y=d15Navg))+
  geom_hline(yintercept=3.940156,linetype="dashed",colour='gray')+
  geom_hline(yintercept=7.34036, linetype='dashed',colour='gray')+
  geom_hline(yintercept=10.74056,linetype='dashed',colour='gray')+
  geom_errorbar(aes(ymin=d15Navg-d15Nse,ymax=d15Navg+d15Nse),width=0)+
  geom_errorbarh(aes(xmin=d13Cavg-d13Cse,xmax=d13Cavg+d13Cse),height=0)+
  geom_point(aes(x=d13Cavg,y=d15Navg,fill=as.factor(clusterCut)),shape=21,size=2)+
  #geom_errorbar(data=MHCwry1,aes(ymin=d15Navg-d15Nse,ymax=d15Navg+d15Nse),width=0)+
  #geom_errorbarh(data=MHCwry1,aes(xmin=d13Cavg-d13Cse,xmax=d13Cavg+d13Cse),height=0)+
  geom_point(data=filter(iso2, site == "MHC" & season == "summer" & name == "C. maculatus"),shape=22,size=2,aes(fill=as.factor(clusterCut)))+
  theme_classic()+
  geom_text_repel(data=filter(iso2, site== "MHC" & season == "summer"),aes(label=abrev),size=3.5)+
  #geom_text_repel(data=MHCwry1,aes(label=abrev),size=4)+
  labs(x=expression(paste(delta^{13},"C (‰)")),
       y=expression(paste(delta^{15},"N (‰)")))+
  theme(axis.title.y=element_text(size=14,colour='white'))+
  theme(axis.text=element_text(size=12))+
  xlim(-22.4,-7)+
  ylim(3,14)+
  theme(title = element_text(size=12))+
  scale_fill_manual(values=cbPalette)+
  labs(fill="Cluster")+
  ggtitle("MHC in Summer")+
  theme(plot.title = element_text(hjust=0.5))
p.MHC2

# LAR spring TP = -1.951 + 0.4902x
# (TP + 1.951)/0.4902 = x
(1+ 1.951)/0.4902 # 6.019992
(2+ 1.951)/0.4902 # 8.059976
(3+ 1.951)/0.4902 # 10.09996
(4+ 1.951)/0.4902 # 12.13994

# TP = -0.3706 + 0.2941x
(1+0.3706)/0.2941
(2+0.3706)/0.2941
(3+0.3706)/0.2941
(4+0.3706)/0.2941

p.LAR1<-ggplot(data=filter(iso2, site=="LAR" & season == "spring"),aes(x=d13Cavg,y=d15Navg))+
  geom_hline(yintercept = 4.66032,linetype="dashed",colour='gray')+
  geom_hline(yintercept=8.060524, linetype='dashed',colour='gray')+
  geom_hline(yintercept=11.46073,linetype='dashed',colour='gray')+
  geom_errorbar(aes(ymin=d15Navg-d15Nse,ymax=d15Navg+d15Nse),width=0)+
  geom_errorbarh(aes(xmin=d13Cavg-d13Cse,xmax=d13Cavg+d13Cse),height=0)+
  geom_point(aes(x=d13Cavg,y=d15Navg,fill=as.factor(clusterCut)),shape=21,size=2)+
  #geom_errorbar(data=MHCwry1,aes(ymin=d15Navg-d15Nse,ymax=d15Navg+d15Nse),width=0)+
  #geom_errorbarh(data=MHCwry1,aes(xmin=d13Cavg-d13Cse,xmax=d13Cavg+d13Cse),height=0)+
  geom_point(data=filter(iso2, site == "LAR" & season == "spring" & name == "C. maculatus"),shape=22,size=2,aes(fill=as.factor(clusterCut)))+
  theme_classic()+
  geom_text_repel(data=filter(iso2, site=="LAR" & season == "spring"),aes(label=abrev),size=3.5)+
  #geom_text_repel(data=MHCwry1,aes(label=abrev),size=4)+
  labs(x=expression(paste(delta^{13},"C (‰)")),
       y=expression(paste(delta^{15},"N (‰)")))+
  #theme(axis.title=element_text(size=14,colour='white'))+
  theme(axis.text=element_text(size=12))+
  theme(axis.title.x=element_text(size=14,colour='white'))+
  xlim(-22.4,-7)+
  ylim(3,14)+
  theme(title = element_text(size=12))+
  scale_fill_manual(values=cbPalette)+
  labs(fill="Cluster")+
  ggtitle("LAR in Spring")+
  theme(plot.title = element_text(hjust=0.5))
p.LAR1
# TP = -1.932 + 0.4878x

# TP = -.2206 +0.2941x
(1+0.2206)/0.2941
(2+0.2206)/0.2941
(3+0.2206)/0.2941
(4+0.2206)/0.2941

p.LAR2<-ggplot(data=filter(iso2,site=="LAR" & season == "summer"),aes(x=d13Cavg,y=d15Navg))+
  geom_hline(yintercept = 4.150289,linetype="dashed",colour='gray')+
  geom_hline(yintercept=7.550493, linetype='dashed',colour='gray')+
  geom_hline(yintercept=10.9507,linetype='dashed',colour='gray')+
  geom_hline(yintercept=14.3509,linetype='dashed',colour='gray')+
  geom_errorbar(aes(ymin=d15Navg-d15Nse,ymax=d15Navg+d15Nse),width=0)+
  geom_errorbarh(aes(xmin=d13Cavg-d13Cse,xmax=d13Cavg+d13Cse),height=0)+
  geom_point(aes(x=d13Cavg,y=d15Navg,fill=as.factor(clusterCut)),shape=21,size=2)+
  #geom_errorbar(data=MHCwry1,aes(ymin=d15Navg-d15Nse,ymax=d15Navg+d15Nse),width=0)+
  #geom_errorbarh(data=MHCwry1,aes(xmin=d13Cavg-d13Cse,xmax=d13Cavg+d13Cse),height=0)+
  geom_point(data=filter(iso2,site=="LAR" & season == "summer" & name == "C. maculatus"),shape=22,size=2,aes(fill=as.factor(clusterCut)))+
  theme_classic()+
  geom_text_repel(data=filter(iso2,site=="LAR" & season == "summer"),aes(label=abrev),size=3.5)+
  #geom_text_repel(data=MHCwry1,aes(label=abrev),size=4)+
  labs(x=expression(paste(delta^{13},"C (‰)")),
       y=expression(paste(delta^{15},"N (‰)")))+
  theme(axis.title.y=element_text(size=14,colour='white'))+
  theme(axis.text=element_text(size=12))+
  theme(axis.title=element_text(size=14,colour='white'))+
  xlim(-22.4,-7)+
  ylim(3,14)+
  theme(title = element_text(size=12))+
  scale_fill_manual(values=cbPalette)+
  labs(fill="Cluster")+
  ggtitle("LAR in Summer")+
  theme(plot.title = element_text(hjust=0.5))
p.LAR2
# TP = -1.683 + 0.4878x
# (TP + 1.683)/0.4878 = x


Fig4<-ggarrange(p.LAR1,p.LAR2,p.MHC1,p.MHC2, labels=c("a","b","c","d"),ncol=2,nrow=2,
                common.legend=TRUE,legend = "right")
ggsave(file="./output/Fig4.jpg",plot=Fig4,width=6.5, height=6.5,units="in",dpi=300)


# ---- Figure 5 ----
# mixutre distributions
# diet proportion results
# see siar-analysis.R

larsp<-results%>%
  filter(group=='1')%>%
  filter(parameter!="SD1")%>%
  filter(parameter!="SD2")%>%
  ggplot()+geom_col(aes(x=parameter,y=mean))+
  geom_errorbar(aes(x=parameter,ymin=Low95hdr,ymax=High95hdr),width=0,size=1)+
  theme_classic()+
  ylab("Average proportion")+xlab("Cluster")+
  theme(axis.title = element_text(size=12))+
  theme(axis.text = element_text(size=11))+
  theme(axis.title.x=element_blank())+
  ylim(0,1)+
  ggtitle("LAR in Spring")+
  theme(plot.title=element_text(hjust=0.5))

mhcsp<-results%>%
  filter(group=='2')%>%
  filter(parameter!="SD1")%>%
  filter(parameter!="SD2")%>%
  ggplot()+geom_col(aes(x=parameter,y=mean))+
  geom_errorbar(aes(x=parameter,ymin=Low95hdr,ymax=High95hdr),width=0,size=1)+
  theme_classic()+
  ylab("Average proportion")+xlab("Cluster")+
  theme(axis.title = element_text(size=12))+
  theme(axis.text = element_text(size=11))+
  ylim(0,1)+
  ggtitle("MHC in Spring")+
  theme(plot.title=element_text(hjust=0.5))

larsum<-results%>%
  filter(group=='3')%>%
  filter(parameter!="SD1")%>%
  filter(parameter!="SD2")%>%
  ggplot()+geom_col(aes(x=parameter,y=mean))+
  geom_errorbar(aes(x=parameter,ymin=Low95hdr,ymax=High95hdr),width=0,size=1)+
  theme_classic()+
  ylab("Average proportion")+xlab("Cluster")+
  theme(axis.title = element_text(size=12))+
  theme(axis.text = element_text(size=11))+
  theme(axis.title.x=element_blank())+
  ylim(0,1)+
  #scale_y_continuous(breaks = c(0.0,0.2,0.4,0.6,0.8,1.0),expand = c(0,0))+
  theme(axis.title.y=element_text(size=12,colour='white'))+
  ggtitle("LAR in Summer")+
  theme(plot.title=element_text(hjust=0.5))

mhcsum<-results%>%
  filter(group=='4')%>%
  filter(parameter!="SD1")%>%
  filter(parameter!="SD2")%>%
  ggplot()+geom_col(aes(x=parameter,y=mean))+
  geom_errorbar(aes(x=parameter,ymin=Low95hdr,ymax=High95hdr),width=0,size=1)+
  theme_classic()+
  ylab("Average proportion")+xlab("Cluster")+
  theme(axis.title = element_text(size=12))+
  theme(axis.text = element_text(size=11))+
  ylim(0,1)+
  # scale_y_continuous(breaks = c(0.0,0.2,0.4,0.6,0.8,1.0),expand = c(0,0))+
  theme(axis.title.y=element_text(size=12,colour='white'))+
  ggtitle("MHC in Summer")+
  theme(plot.title=element_text(hjust=0.5))

ggarrange(larsp,
          larsum,
          mhcsp,
          mhcsum,
          ncol=2,nrow=2,labels=c("a","b","c","d"))

c<-mhcsp+theme(axis.title.y =element_text(margin=margin(r=10)),
               axis.title.x=element_text(margin=margin(t=10)))
d<-mhcsum+theme(axis.title.x=element_text(margin=margin(t=10)))
a<-larsp+theme(axis.title.y=element_text(margin=margin(r=10)))
b<-larsum
Fig5<-ggarrange(a,b,c,d,ncol=2,nrow=2,labels=c('a','b','c','d'))

ggsave(file="Fig5.jpg",plot=Fig5,width=6, height=6,units="in",dpi = 300)
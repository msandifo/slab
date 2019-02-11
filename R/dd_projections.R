#reprojected Dan

source('/Volumes/data/Dropbox/msandifo/documents/manuscripts/januka/rscripts/build_data.R')


main.event <- gg.loc[gg.loc$mag==5.6,]
sec.event <-  dd.reloc[dd.reloc$mag<5 &dd.reloc$mag>4.2 ,]


section=218-90
dd.loc <- pro_events(dd.loc, sec.event, strike=section)
dd.reloc <- pro_events(dd.reloc, sec.event, strike=section)
dd.merge.reloc<-merge(dd.loc , dd.reloc, by=c("id", "datetime")) %>% add_seqs(pre="seq ")


p.perp<-ggplot(dd.merge.reloc, aes(dist.y, -depth.y, col=label, size=mag.x))+geom_point(shape=2)+
  geom_segment(aes(xend=dist.x, yend= -depth.x ),  size=.1, alpha=.75)+
  labs(subtitle= paste("b) section strike =", section), y= "depth", x="distance")+
  scale_radius(range=c(.4, 5))+
  scale_color_manual(values=c("blue2", "red2"))+
  geom_abline(slope= tan(pi*sec.event.np[2,2]/180)*sin(pi*(sec.event.np[2,1]-section)/180 ), intercept= -sec.event$depth , linetype=2, size=.3, colour="red2")+
  geom_abline(slope= tan(pi*sec.event.np[1,2]/180)*sin(pi*(sec.event.np[1,1]-section)/180 ), intercept= -sec.event$depth , linetype=4, size=.3, colour="red2")+
  xlim(c(-7,7))+
  stat_ellipse(data=dd.merge.reloc %>%   subset(label=="seq 3" & depth.y<10 & mag.y>0.), type = "t", level=.9)+
  stat_ellipse(data=dd.merge.reloc %>%   subset(label=="seq 2" & depth.y<10 & mag.y>0.), type = "t", level=.9, col="blue2")




section=218-180
dd.loc <-
  pro_events(dd.loc, sec.event, strike=section)

dd.reloc <-
  pro_events(dd.reloc, sec.event, strike=section)
dd.merge.reloc<-merge(dd.loc , dd.reloc, by=c("id", "datetime")) %>% add_seqs(pre="seq ")
p.parallel<-ggplot(dd.merge.reloc, aes(dist.y, -depth.y, col=label, size=mag.x))+geom_point(shape=2)+
  geom_segment(aes(xend=dist.x, yend= -depth.x ), size=.1, alpha=.75)+
  labs(subtitle= paste("c) section strike =", section+180), y= "depth", x="distance")+
  scale_radius(range=c(.4, 5))+
  scale_color_manual(values=c("blue2", "red2"))+
                       geom_abline(slope= tan(pi*sec.event.np[2,2]/180)*sin(pi*(sec.event.np[2,1]-section)/180 ), intercept= -sec.event$depth , linetype=2, size=.3, colour="red2")+
                       geom_abline(slope= tan(pi*sec.event.np[1,2]/180)*sin(pi*(sec.event.np[1,1]-section)/180 ), intercept= -sec.event$depth , linetype=4, size=.3, colour="red2")+
  xlim(c(-7,7))+
  stat_ellipse(data=dd.merge.reloc %>%   subset(label=="seq 3" & depth.y<10 & mag.y>0.), type = "t", level=.9)+
stat_ellipse(data=dd.merge.reloc %>%   subset(label=="seq 2" & depth.y<10 & mag.y>0.), type = "t", level=.9, col="blue2")



p.dd.1 <-ggplot(data=dd.reloc  %>% add_seqs(pre="seq "), aes(datetime, mag, col=label))+
  geom_point(aes(size=mag), shape=2)+
  labs(x=NULL, y="local magnitude")+
  scale_radius(range=c(.4, 5))+
  scale_color_manual(values=c("blue2", "red2"))+
  labs(subtitle= paste("a)"))



p.dd.sections <-cowplot::plot_grid(p.dd.1+ theme(legend.position = "None"),
                                    p.perp+ theme(legend.position = "None"),
                                p.parallel+ theme(legend.position = "None") , align="v" , ncol=1, rel_heights=c(.35,.35,.35))
p.dd.sections

ggsave( "figures/dd_sections.png", plot= p.dd.sections, width=8, height=9)
ggsave( "figures/dd_sections.pdf", plot= p.dd.sections, width=8, height=9)
# pdf("figures/dd_sections1.pdf")
# p.dd.sections
# dev.off()

erf_adj<- function(x= seq(0,350,1),
                   offset= 150,
                   amp= 5.5,
                   adj= -57,
                   spread= 45,
                   full=T
)
{
  y=(pracma::erf((x-offset)/spread)*amp)+adj
  if(full) data.frame(x= x, y=y, depth.adj= -round(y-adj+amp,1) ) else -round(y-adj+amp,1)
}

####

p2=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5 )
p2r=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5, right=0 )
p2l=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5, left=0 )
win <- owin_poly(p2)
# p2 <- pwin( mex$cmt.ppp, win=win)
# p2n <- pwin( mex$cmt.ppp, win=win, n=T)
p2b<-split_owin(bird.plates.psp[p2], p2 , rev=T, inds=c(2:1))
p2a<-split_owin(bird.plates.psp[p2], p2  )

#plot(p2b)
pll<- get_line(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7,  width= 3.2, length=5, psp=T,n=2)
tresh=4.


# a <- psp(runif(20),runif(20),runif(20),runif(20), window=owin())
# Z <- distmap(pll,  p2)
# plot(Z)


project_ppp( ehb1.ppp, p2, plates=pll) -> ehb.pll
# bird.plates.psp
#
#
# plot(p2)
# plot(p2a, add=T, col="red")
# plot(p2b, add=T, col="green")
# points(ehb.pll)

 max(ehb.pll[p2]$marks$distance)
 ehb.pll$marks
 ehb1.ppp$marks$ldis<- ehb.pll$marks$distance
 ehb1.ppp$marks$sector="eastern"
 ehb1.ppp$marks$sector[ ehb1.ppp$marks$ldis>150]="western"

 lg <- 7/3.2# 5 kms variationa cross the domian
 lo <- 1.15

ehb1.ppp$marks$ldis <- ehb.pll$marks$distance
ehb1.ppp$marks$adj.depth <- ehb1.ppp$marks$depth  # + lg*(ehb1.ppp$marks$ldis )/100 + lo

amp=3.5
adj= -54.5
spread=60
offset1=130
ehb1.ppp$marks$adj.depth1 <-  ehb1.ppp$marks$depth-
  erf_adj(x=ehb1.ppp$marks$ldis,full=F,  amp= amp, adj=adj, spread=spread, offset=offset1)

inds <-which(ehb1.ppp$marks$distance>150)
ehb1.ppp$marks$adj.depth[inds] <- ehb1.ppp$marks$depth[inds]  + lg*(ehb1.ppp$marks$ldis[inds] )/100 + lo

ehb1.ppp  %>% subset(distance>170 &  depth> -75  )  -> pll.sub



#da
pll.sub <- ehb1.ppp[p2] %>% subset(distance>150)
pll.sub$marks %>% group_by(sector) %>% dplyr::summarise(ldis=mean(ldis), depth=mean(depth)) -> pll.sum
cor.df<-erf_adj(x= seq(0,350,1),    amp= amp, adj=adj, spread=spread, offset=offset1,   full=T)
#pll.sub$marks$adj.depth1 <-  pll.sub$marks$depth-erf_adj(x=pll.sub$marks$ldis,full=F,  amp= 5.)


(cd.dip<- ggplot(pll.sub$marks  %>% subset(distance>150  ))+
  geom_line(data=mod1 , aes(x,depth ),  size=.45, linetype=1, col="blue3" )+
  geom_line(data=mid1 , aes(x,depth ),  size=.45, linetype=3, col="blue3" )+
  geom_path(data=c600 , aes(x , depth  ),  size=.45, linetype=5, col="orange") +
    geom_path(data=c650 , aes(x , depth  ),  size=.45, linetype=5, col="orange3") +
    # geom_smooth(data=pps  %>% subset(distance>150 &  (depth> -75  )),
    #           method="lm", aes(distance, depth ,     colour=sector), formula= y~1,se =F, size=.5)+
  geom_hline(yintercept = -c(58.5,71), linetype=2, size=.3)+
  geom_segment(aes(x= distance, xend= distance,y= -depth,yend= -adj.depth1),size=.25, arrow=arrow(  type="closed",length=unit(0.2,"cm")))+
  #geom_point(aes(distance, -adj.depth1, size=m,col=sector), shape=25, fill="white")+
  geom_point(aes(distance, -depth , size=m , shape=sector, col=sector), fill="white",show.legend = F)+
  theme(legend.position=c(.1,.3), legend.background = element_rect(linetype = 1, size = 0.25, colour = 1), legend.direction = "vertical" ,
          legend.box = 'horizontal')+
  scale_radius(range=c(.5,8))+
  scale_shape_manual(values=c(21,22)) +
  geom_text(data=pll.sum, aes( x=c(200,200), y=-depth, label = round(-depth,0), col=sector  ), vjust=-2.3)+
  #geom_text(data=pll.sum, aes( x=c(200,200), y=-depth, label = round(-ldis,0), col=sector  ), vjust=0)+
  xlim(c(155,340))+
  ylim(c(-87,-49))+
   #290 -60.76315
  #307 -76.26315
  geom_line(data= data.frame(x=c(290,307),y=c(-56, -71)), #arrow=arrow(length=unit(0.30,"cm")),
            aes(x=x, y=y), size=1.3, col="lightblue3", linetype=1) +
  annotate("text", x=c(160,350), y=c(-86,-86), label=c("South", "North"))+
    annotate("text", x=c(238,262), y=c(-50,-50), label=c("600°C", "650°C"), colour=c("orange", "orange2"))+

    labs(x="distance to trench")+
    geom_line(data= data.frame(x=c(290,307),y=c(-56, -71)-4.7), #arrow=arrow(length=unit(0.30,"cm")),
          aes(x=x, y=y), size=.95, col="lightblue3", linetype=2)+
    scale_shape_manual(values=c(1,0))+
    scale_colour_manual( values=c("red3","lightblue4"))
)


#ggsave("~/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/figures/corrected.depths.pdf", width=9, height=5)


#plot(cor.df$x,cor.df$y)

(cd.str <-ggplot(pll.sub$marks  %>% subset(distance>150  ))+
      geom_smooth(data=pll.sub$marks %>% subset(distance>150 &  depth > -73  ),   aes(ldis, -depth , colour=sector ),
                  method="lm",formula= y~1,span=1.2 ,se =F, size=.5)+
  geom_hline(yintercept = -c(58.5,71), linetype=2, size=.3)+
  geom_segment(aes(x= ldis, xend= ldis,y= -depth,yend= -adj.depth1),size=.25, arrow=arrow(  type="closed",length=unit(0.2,"cm")))+
#  geom_point(aes(ldis, -adj.depth1, size=m,col=sector), shape=25, fill="white")+
  geom_point(aes(ldis, -depth , size=m , shape=sector, col=sector), fill="white",show.legend = F)+
  theme(legend.position="None")+
  # theme(legend.position=c(.1,.2), legend.background = element_rect(linetype = 1, size = 0.25, colour = 1), legend.direction = "vertical" ,
  #       legend.box = 'horizontal')+
  scale_radius(range=c(.5,8))+
  scale_shape_manual(values=c(21,22)) +
  geom_polygon(data= data.frame(x=c(170,230, 230, 170),y=c(-56,-56, -73, -73)), #arrow=arrow(length=unit(0.30,"cm")),
            aes(x=x, y=y), size=1.3, fill="lightblue3", linetype=1, alpha=.3)+
  geom_line(data=cor.df, aes(x,y))+
  annotate("text", 0, cor.df$y[1], label="inferred slab top form", hjust=0,vjust=-.6)+
  annotate("segment", x=150, xend=150, y=min(cor.df$y) , yend= max(cor.df$y), lty=3, lwd=.3)+
  annotate("segment", x=150, xend=150, y=min(cor.df$y) , yend= max(cor.df$y), arrow=arrow(ends="both", type="closed",length=unit(0.3,"cm")),lty=5, lwd=0)+
annotate("text", 150, tail(cor.df$y,1), label=paste(round(max(cor.df$y)-min(cor.df$y),1), "km"), hjust=-.2)+
  annotate("text", x=c(1,350), y=c(-86,-86), label=c("East", "West"))+
  labs(x="distance to eastern bound")+
    scale_shape_manual(values=c(1,0))+
    scale_colour_manual( values=c("red3","lightblue4"))
)


cd.comb <-gridExtra::grid.arrange(cd.dip,cd.str, ncol=1)
ggsave("~/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/figures/cdepths_combo.pdf", cd.comb, width=11, height=9)

# +
#   geom_text(data=pll.sum, aes( x=c(200,200), y=-depth, label = round(-depth,0), col=sector  ), vjust=-2.3)+
#   geom_text(data=pll.sum, aes( x=c(200,200), y=-depth, label = round(-ldis,0), col=sector  ), vjust=0)+
#   xlim(c(155,340))+
#   ylim(c(-85,-49))+
#   geom_line(data= data.frame(x=c(170,170),y=c(-56, -73)), #arrow=arrow(length=unit(0.30,"cm")),
#             aes(x=x, y=y), size=1.3, col="lightblue4", linetype=1)
#   #geom_line(data= data.frame(x=c(170,170),y=c(-56, -73)-3.2), #arrow=arrow(length=unit(0.30,"cm")),
           # aes(x=x, y=y), size=.5, col="lightblue4", linetype=2)



#ehb1.ppp$marks$adj.depth <- ehb1.ppp$marks$depth  +  1.*(ehb1.ppp$marks$ldis )/100
p2=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5    )
p2l=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5 , left=0   )
names(ehb1.ppp[p2]$marks)
pps <-ehb1.ppp[p2]$marks[,c( "distance","adj.depth","depth", "m", "adj.depth1" )] %>% mutate(depth= -depth , adj.depth= -adj.depth,  adj.depth1= -adj.depth1 )
#pps <-ehb1.ppp[p2]$marks[,c( "distance","adj.depth","depth", "m" )] %>% mutate(depth= -depth , adj.depth= -adj.depth )
pe1<- -56- 4# lg*p.dis/100 -lo
#pe1 <-  pps$depth-erf_adj(x=p.dis,full=F,  amp= 5.)
pps <- rbind(pps,c(290,pe1, -56,7.1, pe1 ))
pps <- rbind(pps,c(290,pe1,-56,7.1,pe1 ))

#need to laod t.ras.c
source('~/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/read_ras.R')
pps$temp <-raster::extract(t.ras.c, pps[, c("distance", "depth")] )
pps$adj.temp <-raster::extract(t.ras.c, pps[ ,c("distance", "adj.depth1")] )
pps$stress <-raster::extract(s.ras.c,pps[,c("distance", "depth")]+c(0,-0)  )
pps$adj.stress <-raster::extract(s.ras.c,pps[, c("distance", "adj.depth1")] +c(0,0) )
ppsl <-ehb1.ppp[p2l]$marks[,c( "distance","adj.depth","depth", "m", "adj.depth1")] %>% mutate(depth= -depth )
ppsl <- rbind(ppsl,c(290,-56,7.1 , -59))
ppsl <- rbind(ppsl,c(290,-58,7.1 , -61))
ppsr <-ehb1.ppp[p2r]$marks[,c( "distance","adj.depth","depth", "m","adj.depth1")] %>% mutate(depth= -depth )

pps$sector="eastern"

pps$sector[(Hmisc::find.matches(pps[, c(1, 3:4)],ppsl[,c(1,3,4)]))[[1]] > 0]<-"western"


cmt.pps <-cmt1.ppp[p2]$marks[,c( "distance","depth", "m", "regime")] %>% mutate(depth= -depth)
cmt.pps$temp <- raster::extract(t.ras.c,cmt.pps[, 1:2] )
cmt.pps$stress <-raster::extract(s.ras.c,cmt.pps[,1:2]  )

pps$sector[(length(pps$sector)-1):length(pps$sector)] <- "western"
pps.p <- tail(pps,1)
pps <- head(pps,-1)

grav.col= BlRe.colours(  27, 60,F)

t.min.t <-apply(t.ras.c %>% crop(extent(-100, 400, -100, -48)) %>% as.matrix(),2,min)
t.min.dis <- seq(-100,400, 500/(length(t.min.t)-1))
length(t.min.t)
length(t.min.dis)
t.min <- data.frame(temp=t.min.t, distance=t.min.dis)



ggplot(pps, aes(distance, adj.temp))+
  # geom_smooth(data=pps %>% subset(distance>150 & adj.temp<600),  method ="lm", #aes(linetype=sector ),
  #             se=T, col="black" , fill="grey80", size=.35, linetype=1)+
 # geom_point(aes(y=temp), size=1.5, shape=23, col="blue")+
  geom_segment(data= pps %>% subset(distance>150),aes(xend=distance, yend=temp),
              size=.4, col="black"  )+


  geom_path(data=t.min %>% subset(distance<340), aes(y=temp), col="black",size=1, linetype=5)+
  geom_path(data=t.min %>% subset(distance<340), aes(y=temp+70), col="black",size=.3, linetype=3)+
  geom_path(data=t.min %>% subset(distance<340), aes(y=temp+35), col="black",size=.3, linetype=5)+
  # geom_point(aes( size=m,  shape=sector))+
  geom_point(data= pps %>% subset(distance>150), aes( size=m, fill=-adj.stress, shape=sector), show.legend = F)+
  geom_point(data= pps %>% subset(distance<150), aes(y= temp, size=m,fill= -stress, shape=sector), show.legend = T)+
  scale_fill_gradient2(low="red" ,high="blue")+
  geom_hline(yintercept=c(600 ), linetype=c(2 ), size=c(.5 )) +
 # geom_vline(xintercept=c(322.5), linetype=2, size=.2 ) +
 # ylim(c(0,900))+
    ylim(c(70,920))+
    xlim(c(-60,355))+
  scale_radius(range=c(.5, 7))+
  scale_shape_manual(values=c(21,22))+
 # labs( subtitle= "ISC-EHB")+
  annotate("text", x=c(-50 )+20, y=c(600  )-15, label=str_c(c(600 ), "°C" ), size=5)+
  annotate("text", x=140, y=100, label="above\nmegathrust", size=5)+
  annotate("text", x=-10, y=420, label="below\nmegathrust", size=5, col="blue3")+
 # annotate("text", x=280, y=450, label="MFS", size=7, col="red3")+
  annotate("text", x=295, y=680, label="Puebla", size=4 )+
  annotate("text", x=328, y=720, label="August\n1973", size=4 )+
  annotate("text", x=341, y=610, label="Smin", size=4 , col="black", hjust=0, angle=30)+
  annotate("text", x=341, y=642, label="+35", size=4 , col="black", hjust=0, angle=30)+
  annotate("text", x=341, y=685, label="+70", size=4 , col="black", hjust=0, angle=30)+
 # annotate("text", x=338, y=535, label="Kim et al.\nmodel", size=4. , col="black", hjust=0.5)+
  theme(legend.position = c(.85,.2),    legend.box = 'horizontal')+
   #geom_point(data=pps.p, shape=23, size=4, fill="yellow")+
   annotate("point", 322.5, 591,   shape=23, size=6, fill="yellow")+
  geom_segment(data=data.frame(x=322.2, xend=322.2, y=650, yend=598), aes(x=x,xend=xend, y=y, yend=yend), arrow=arrow(length=unit(0.4,"cm")), size=1)+
  labs(y="temperature, adjusted depths")

ggsave("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/figures/figS4.pdf", width=12, height=6.5)


#extract  remperatures for peubla rupture


peubla.loc <- c(-98.399,18.584) %>% as.ppp(p2 )
marks(peubla.loc) <-   data.frame(long= -98.399, lat=18.584, depth= 56, m=7.1 )
peubla.loc<- project_ppp(peubla.loc, p2, plates=pll)
 p.dis<- peubla.loc$marks$distance

cds1<-cbind(c(290,307),y=c(-56, -71.5)- 4)#+lg*p.dis/100 -lo

# cds1 <- rbind(c(295,-59.), c(318,-73))
# cds1 <- rbind(c(290,-58), c(313,-73.5))
plines <- spLines(cds1)
t.pueb<- (raster::extract(t.ras.c, plines, method="bilinear"))[[1]]
t.pueb.dis <-seq(290,307, 17/(length(t.pueb)-1))
t.pueb.df <- data.frame(adj.temp=t.pueb, distance=t.pueb.dis)


tplot <-ggplot(pps, aes(distance, adj.temp))+
#  geom_smooth(data=pps %>% subset(distance>150 & adj.temp<600),  method ="lm", #aes(linetype=sector ),
 #             se=T, col="black" , fill="grey70", size=0, linetype=5, fullrange=T, level = 0.995)+
  # geom_point(aes(y=temp), size=1.5, shape=23, col="blue")+
  geom_hline(yintercept=c(600 ), linetype=c(2 ), size=c(.25 )) +
 # geom_path(data=t.pueb.df[c(1,3,6, 9,12, 15, 17,18,19,20,21,22,23,24),],   size=1, linetype=1, arrow=arrow(length=unit(0.4,"cm")), colour="red3")+
#  geom_path(data=t.pueb.df ,   size=1, linetype=1, arrow=arrow(length=unit(0.4,"cm")), colour="red3")+
#  geom_segment(data= pps %>% subset(distance>150),aes(xend=distance, yend=temp),
#               size=.4, col="black"  )+

  geom_path(data=t.min %>% subset(distance<340), aes(y=temp), col="black",size=1, linetype=5)+
  geom_path(data=t.min %>% subset(distance<340), aes(y=temp+70), col="grey20",size=.3, linetype=4)+
  geom_path(data=t.min %>% subset(distance<340), aes(y=temp+35), col="grey20",size=.3, linetype=2)+
  # geom_point(aes( size=m,  shape=sector))+
  geom_point(data= pps %>% subset(distance>150), aes( size=m,fill= -adj.stress, shape=sector), show.legend = T)+
 # geom_point(data= pps %>% subset(distance<150), aes(y=temp, size=m,fill=adj.stress, shape=sector), show.legend = F)+
  scale_fill_gradient2(low="red" ,high="blue")+
   # geom_vline(xintercept=c(322.5), linetype=2, size=.2 ) +
  # ylim(c(0,900))+
  ylim(c(310,700))+
  xlim(c(165,345))+
  scale_radius(range=c(2, 10))+
  scale_shape_manual(values=c(21,22))+
  # labs( subtitle= "ISC-EHB")+
  annotate("text", x=c(-50 )+20, y=c(600  )-15, label=str_c(c(600 ), "°C" ), size=5)+
  annotate("text", x=140, y=100, label="above\nmegathrust", size=6)+
  annotate("text", x=-10, y=420, label="below\nmegathrust", size=6, col="blue3")+
  # annotate("text", x=280, y=450, label="MFS", size=7, col="blue3")+
  annotate("text", x=290, y=610, label="Puebla", size=8. )+
  annotate("text", x=310, y=673, label="August\n1973", size=8. )+
  annotate("text", x=341, y=610, label="Slab\nmin T", size=6. , col="black", hjust=0, angle=25)+
  annotate("text", x=341, y=648, label="+35°", size=6. , col="black", hjust=0, angle=25)+
  annotate("text", x=341, y=685, label="+70°", size=6. , col="black", hjust=0, angle=25)+
 # annotate("text", x=338, y=565, label="Kim model", size=4.5 , col="black", hjust=0.5,fontface="italic")+
  theme(legend.position = c(.82,.15),    legend.box = 'horizontal')+
  #geom_point(data=pps.p, shape=23, size=4, fill="yellow")+
  annotate("point", 322.5, 591,   shape=23, size=10, fill="yellow")+
  geom_segment(data=data.frame(x=322.3, xend=322.3, y=650, yend=598), aes(x=x,xend=xend, y=y, yend=yend), arrow=arrow(length=unit(0.4,"cm")), size=1)+
  geom_path(data=t.pueb.df[c(1,4,6, 9,12, 14, 16 ),],   size=1, linetype=1, arrow=arrow(length=unit(0.4,"cm")), colour="red3")+

  labs(y="temperature, adjusted depths")

ggsave("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/figures/fig4.pdf", plot=tplot,width=10, height=6)

tplot + theme(text = element_text(size=24))
# apply(t.ras.c %>% crop(extent(0, 400, 50, 100)) %>% as.matrix(),2,min)
tail(pps)


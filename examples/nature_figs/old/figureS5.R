
p2=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5  )
p2r=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5, right=0 )
p2l=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5, left=0 )
 win <- owin_poly(p2)
# p2 <- pwin( mex$cmt.ppp, win=win)
# p2n <- pwin( mex$cmt.ppp, win=win, n=T)
p2a<-split_owin(bird.plates.psp[p2], p2)
pll<- get_line(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7,  width= 3.2, length=5, psp=T,n=2)



project_ppp( ehb1.ppp[p2], p2, plates=pll) -> ehb.pll
ehb.marks <- ehb1.ppp[p2]$marks
ehb.marks$ldis <- ehb.pll$marks$distance
ehb.marks$sector="eastern"
ehb.marks$sector[ ehb.marks$ldis>150]="western"



tresh=4.




 amp=3.5
 adj= -54.5
 spread=60
 offset1=130



 ehb.marks$adj.depth <-   ehb.marks$depth-
   erf_adj(x=ehb.marks$ldis,full=F,  amp= amp, adj=adj, spread=spread, offset=offset1)
 ehb.marks$depth=  -ehb.marks$depth
 ehb.marks$adj.depth=  -ehb.marks$adj.depth
 ehb.marks$temp <-raster::extract( t.ras.c, ehb.marks[, c("distance", "depth")]  )
 ehb.marks$adj.temp <-raster::extract(t.ras.c,  ehb.marks[ ,c("distance", "adj.depth")] )



(p.ehb <-ggplot( ehb.marks, aes(distance, adj.depth, size=m  ))+
    geom_point(   aes(shape=sector, colour=sector))+
    scale_shape_manual(values=c(1,0))+
    scale_colour_manual( values=c("red3","lightblue4"))+
    theme(legend.position = c(.85,.8), legend.box = 'horizontal') +xlim(c(-70,350))+
    labs(subtitle=paste0("n=", length(ehb1.ppp[p2]$marks$lat))))

####
out.dir <-"/Volumes/data/data/regional/mexico/quakes/ssn/rdata"

load( paste0(out.dir, "/ssn.Rdata" ))

project_ppp1(ssn.ppp[p2], p2) -> ssn1.ppp
ssn1.ppp[p2a]$marks$distance <- -abs(  ssn1.ppp[p2a]$marks$distance)
# ssn.sub <- isc1.ppp[p2]$marks
# isc.sub$period="1960-2013"
# isc.sub$period[lubridate::year(isc.sub$date)>2013] ="2014+"

offset=513
# offset200=580
# offset100=560

mod <-"/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/mod1/"
source(paste0(mod,"offset.r"))
mod1 <-read.csv(paste0(mod,"slabTop.csv"), col.names=c("x", "depth")) %>%
  subset(x>= -offset-100 )%>%
  mutate(x= x+offset, depth = - depth )#((depth-50)*1 +50))

modc <-read.csv(paste0(mod,"slabTopCorrected.csv"), col.names=c("x", "depth")) %>%
  subset(x>= -offset-100 )%>%
  mutate(x= x+offset, depth = - depth )#((depth-50)*1 +50))

mid1 <-read.csv(paste0(mod,"midPlane.csv"), col.names=c("x", "depth")) %>%
  subset(x>= -offset -100) %>%
  mutate(x= x+offset, depth = -depth)

midc <-read.csv(paste0(mod,"midPlaneCorrected.csv"), col.names=c("x", "depth")) %>%
  subset(x>= -offset -100) %>%
  mutate(x= x+offset, depth = -depth)
# mid100 <-read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/midPlane100.csv",

# mid100 <-read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/midPlane100.csv",
#                 col.names=c("x", "depth")) %>%
#   subset(x>= -offset100-100)%>%
#   mutate(x= x+offset100, depth = -depth)
# mid200 <-read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/midPlane200.csv",
#                  col.names=c("x", "depth")) %>%
#   subset(x>= -offset200-100)%>%
#   mutate(x= x+offset200, depth = -depth)
c600 <-read.csv(paste0(mod,"600C_contour.csv"),
                col.names=c("x", "depth")) %>%
  mutate(x= x) %>%
  #subset(x>= -offset -100)%>%
  mutate(x= x+offset, depth = -depth)

c650 <-read.csv(paste0(mod,"650C_contour.csv"),
                col.names=c("x", "depth")) %>%
  mutate(x= x) %>%
  #subset(x>= -offset -100)%>%
  mutate(x= x+offset, depth = -depth)
c700 <-read.csv(paste0(mod,"700C_contour.csv"),
                col.names=c("x", "depth")) %>%
  mutate(x= x) %>%
  #subset(x>= -offset -100)%>%
  mutate(x= x+offset, depth = -depth)



modc$xp <- c(0,sqrt(diff(modc$x)^2 +diff(modc$depth)^2))%>% cumsum() + modc$x[1]
mod1$xp <- c(0,sqrt(diff(mod1$x)^2 +diff(mod1$depth)^2))%>% cumsum() + mod1$x[1]

ie <-c( which.min(abs(modc$xp-c(337))), which.min(abs(modc$xp-c(344))), which.min(abs(modc$xp-c(370))), which.min(abs(modc$xp-c(340))), which.min(abs(modc$xp-c(352))) )
is <- c(which.min(abs(mod1$xp-c(337))),which.min(abs(mod1$xp-c(344))),  which.min(abs(mod1$xp-c(370))) ,which.min(abs(mod1$xp-c(340))), which.min(abs(mod1$xp-c(352))) )

ehb.paths <- data.frame(x=mod1$x[is], y=mod1$depth[is], xend=modc$x[ie], yend=modc$depth[ie] , m=1)

ehb.paths

y.o<- 9.5; x.o <-3.1

y.1<- 11; x.1 <- 2.
ehb.marks[, c("depth", "distance")]

ehb.marks$temp <-raster::extract(t.ras.c, ehb.marks[, c( "distance", "depth")]%>% mutate(depth= -depth )) %>% signif(3)
ehb.marks$temp

adj.temp <- data.frame(x=mod1$x[is[1]]-x.o, depth=mod1$depth[is[1]]-y.o)
adj.temp$temp <-raster::extract(t.ras.c, adj.temp[1,]) %>% signif(3)

(map.kim.1 <- p.ehb+
  xlim(205,380)+ylim(c(-120,-25))  +
  geom_line(data=mod1 , aes(x  ,depth  ),  size=.95, linetype=1, col="blue3")+
  geom_line(data=modc %>% subset(x >300) , aes(x  ,depth  ),  size=.95, linetype=1, col="Red3")+
  geom_line(data=mid1 , aes(x ,depth  ),  size=.65, linetype=5, col="blue3")+
  geom_line(data=midc %>% subset(x >290), aes(x*mod.fac ,depth*mod.fac  ),  size=.65, linetype=2, col="red2")+
  labs(x="distance", y="depth")+
  scale_radius(range=c(2,10))+
   geom_path(data=c600 , aes(x , depth  ),  size=.35, linetype=5, col="orange") +#+
  geom_path(data=c650 , aes(x , depth  ),  size=.35, linetype=5, col="orange3") +#+
  geom_path(data=c700 , aes(x , depth  ),  size=.35, linetype=5, col="brown")+
   geom_text(data=ehb.marks %>% subset(adj.temp>590), aes(label=paste0(round(adj.temp,0),"°"), colour=sector), size=4., hjust=1.25, vjust=1. , show.legend=F )+
  geom_label(data=adj.temp, aes(x,  depth,label=paste0(temp,"°")), size=4., hjust=1.25, vjust=.5 , label.size=0)+
  geom_segment(data=ehb.paths[2,],aes(x=x-x.1, y=y-y.1, xend=xend-x.1, yend=yend-y.1) , arrow=arrow(length = unit(0.45,"cm")), col="orange3",size=1.3, linetype=1)+
  annotate("text", x=ehb.paths[2,1]-x.1+2, y=ehb.paths[2,2]-y.1+1.5, label= paste0(round( ehb.paths[2,2]-y.1), "km"), size=4.5, col="orange3" )+
  annotate("text", x=ehb.paths[2,3]-x.1+0,y= ehb.paths[2,4]-y.1-2.5, label= paste0(round( ehb.paths[2,4]-y.1), "km"), size=4.5, col="orange3" )+
  annotate("text", x=360,y= -65, label="our\nmodel", size=6, col="blue2" )+
  geom_point(data=ehb.paths[1,],aes(x=x-x.o, y=y-y.o ) , size=4, shape=21)+
    geom_segment(data=ehb.paths[3:5,],aes(x=x, y=y, xend=xend, yend=yend) , arrow=arrow(length = unit(0.45,"cm")), size=.6, linetype=1)+
    geom_segment(data=ehb.paths[1,],aes(x=x-x.o, y=y-y.o, xend=xend-x.o, yend=yend-y.o) , arrow=arrow(length = unit(0.45,"cm")), col="red2",size=.6, linetype=1)+
    annotate("text", x=358,y= -105, label="adjusted\nmodel", size=6, col="red3")+
    annotate("text", x=250,y= -55, label="slab top", size=6, col="blue2")+
    annotate("text", x=c(235,260,300),y= c(-30,-30,-30), label=c("600°", "650°", "700°"), size=4, col=c("orange2","orange3","orange4"))+
   labs(subtitle=NULL)+
  theme(legend.position= c(.9,.80))
)

ggsave(paste0(fig.dir,"/figS5.pdf"), plot=map.kim.1, width=13, height=7)



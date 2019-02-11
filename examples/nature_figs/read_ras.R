
temp <-read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/mod1/temp.csv", header=F, sep=",")  %>% as.matrix()
stress <- read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/mod1/stress.csv", header=F, sep=",") %>% as.matrix()
x <- read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/mod1/xcoords.csv", header=F, sep=",") %>% as.matrix()
y <- read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/mod1/ycoords.csv", header=F, sep=",") %>% as.matrix()

#off1<-  2500*min(x)/(max(x) -min(x))
t.ras <- raster(temp)
extent(t.ras)
xlim<-2500

source(paste0(mod,"offset.r"))
mod <-"/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/mod1/"
offset=510.0365
extent(t.ras)<- c(-2500,2500, -1250,0) +c(offset,offset,0,0) #c(c(-xlim,xlim)+offset, -(1-min(y))*2900, 0)
extent(t.ras)
t.ras.c <-crop(t.ras, extent(-100,550,-250,0))
temp.col= BlRe.colours( 15, 20,F)
#plot(t.ras.c, col=temp.col)
s.ras <- raster(stress)
sylim<- -2
extent(s.ras)<- c(-2500,2500, -1250,0) +c(offset,offset,0,0) #c(c(-xlim,xlim)+offset, -(1-min(y))*2900+sylim, sylim)
extent(s.ras)
s.ras.c <-crop(s.ras, extent(-100, 550,-250,0))

mod <-"/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/mod1/"
#source(paste0(mod,"offset.r"))
mod1 <-read.csv(paste0(mod,"slabTop2.csv"), col.names=c("x", "depth")) %>%
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


do.plots <-0
if (do.plots){
stress.col= BlRe.colours(  37, 37,F)


pdf(file = "/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/figures/fscaling.pdf", width=11.5, height=9)
par(mfrow=c(2,1), mar=c(0.5, 0.5, 2, 2),  oma = c(4, 4, 0.2, 0.2))
s.ras.c %>% crop(extent(-100,430,-150,0)) %>% fortify()
plot(s.ras.c %>% crop(extent(-100,430,-150,0)), col=stress.col)
lines(mod1$x,mod1$depth, lwd=1.5, col="black")
contour(t.ras.c, col="black", add=T, levels=c(600, 700, 1250))
#contour(s.ras.c, col="black", add=T, levels=c(0))
t.ras.c %>% crop(extent(-100,430,-150,0)) %>% fortify()
points(pps$distance, pps$adj.depth1, cex= (pps$m-4)/3,   pch=0)
plot(t.ras.c %>% crop(extent(-100,430,-150,0)), col=stress.col)
lines(mod1$x,mod1$depth, lwd=1.5, col="white")
contour(t.ras.c, col="black", add=T, levels=c(600, 700, 1250))
points(pps$distance, pps$adj.depth1, cex= (pps$m-4)/3,   pch=0)
#contour(s.ras.c, col="black", add=T, levels=c(0))


dev.off()


pdf(file = "/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/figures/fscaling2.pdf", width=10, height=8)
par(mfrow=c(1,1))
s.ras.c %>% crop(extent(-100,430,-150,0)) %>% fortify()
plot(s.ras.c %>% crop(extent(300,480,-170,-40)), col=stress.col)
contour(t.ras.c, col="black", add=T, levels=c(600, 700, 1250))
#contour(s.ras.c, col="black", add=T, levels=c(0))
lines(mod1$x,mod1$depth, lwd=3, col="grey60")
t.ras.c %>% crop(extent(-100,430,-150,0)) %>% fortify()

points(pps$distance, pps$adj.depth1, cex= pps$m/3,   pch=2)
# plot(t.ras.c %>% crop(extent(200,430,-150,0)), col=stress.col)
# contour(t.ras.c, col="black", add=T, levels=c(600, 700, 1250))
# #contour(s.ras.c, col="black", add=T, levels=c(0))
# lines(mod1$x,mod1$depth, lwd=3, col="grey60")

dev.off()



s.c <- s.ras.c %>% crop(extent(-100,430,-150,0)) #  %>% as.raster()
# %>% #as.data.frame.array()%>% fortify()
#  as( "SpatialPixelsDataFrame") %>% as.data.frame( )
# colnames(s.c) <- c("value", "x", "y")
library(RStoolbox)
ggR(s.c , geom_raster = TRUE)+
  geom_line(data=mod1, aes(x,depth))+  scale_fill_gradientn(  colours =stress.col)


plot(s.c)
grav.stretch<-  c(.05,  .95)
grav.col= BlRe.colours(  27, 60,F)

topo.col <- c(stretch_pal(pal="terrain.colors",stretch=c(.15,.25, .5, .7))) #stretch=c(.1, .5, .75, .9)))

dir5.stretch<-  c(.05,  .95)

fac=1.

p2=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5    )
p2l=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5 , left=0   )
# p2r=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5 , right=0   )
pps <-ehb1.ppp[p2]$marks[,c( "distance","depth", "m" )] %>% mutate(depth= -depth*fac )
pps <- rbind(pps,c(295,-56,7.1 ))
pps <- rbind(pps,c(295,-59,7.1 ))
ppspps$temp <-raster::extract(t.ras.c, pps[, 1:2] )
pps$stress <-raster::extract(s.ras.c,pps[,1:2]  )

pps$sector="eastern"
pps$sector="eastern"
pps$sector[(Hmisc::find.matches(pps[, 1:3],ppsl[,1:3]))[[1]] > 0]<-"western"


cmt.pps <-cmt1.ppp[p2]$marks[,c( "distance","depth", "m", "regime")] %>% mutate(depth= -depth*fac)
 cmt.pps$temp <-raster::extract(t.ras.c,cmt.pps[, 1:2] )
cmt.pps$stress <-raster::extract(s.ras.c,cmt.pps[,1:2]  )

  pps.p <- tail(pps,1)
  pps <- head(pps,-1)

#ggplot(pps, aes(stress, temp))+geom_point()
ggplot(pps, aes(distance, temp))+
  geom_smooth(data=pps %>% subset(distance>150),  method ="lm", #aes(linetype=sector ),
              se=T, col="black" , fill="grey90")+

         geom_point(aes( size=m,fill=stress, shape=sector))+
         scale_fill_gradient2(low="blue",mid="white",high="red")+
         geom_hline(yintercept=c(600, 650), linetype=c(2,5), size=c(.5,.2)) +
  geom_vline(xintercept=c(322.5), linetype=2, size=.2, col="red") +
  ylim(c(0,900))+
         scale_radius(range=c(.5, 11))+
  scale_shape_manual(values=c(21,22))+
         labs( subtitle= "ISC-EHB")+
   annotate("text", x=c(-50,-50)+20, y=c(600,650)-15, label=str_c(c(600,650), "°C" ), size=5)+
  annotate("text", x=140, y=100, label="above\nmegathrust", size=7)+
  annotate("text", x=-10, y=420, label="below\nmegathrust", size=7, col="blue3")+
  annotate("text", x=280, y=450, label="MFS", size=7, col="red3")+
  annotate("text", x=295, y=665, label="Puebla", size=5 )+
  theme(legend.position = c(.85,.12),    legend.box = 'horizontal')+
  geom_point(data=pps.p, shape=23, size=4, fill="yellow")+
  labs(y="temperature")


ggsave("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/figS4.pdf", width=12, height=8.5)


  pps %>% subset(temp>610& distance>150  )
  pps %>% subset(temp>650& distance<150  & distance >0  & m>=5.5) %>% count()
  pps %>% subset(temp>650& distance<150  & distance >0  ) %>% count()
  pps %>% subset(temp>600& distance<150  & distance >0  & m>=5.5) %>% count()
  pps %>% subset(temp>600& distance<150  & distance >0  ) %>% count()
  pps %>% subset( distance<150  & distance >0  ) %>% count()
  pps.p
ggplot(cmt.pps, aes(distance,depth))+
  geom_point( aes( size=m, fill=stress, shape=regime))+
  scale_fill_gradient2(low="blue",mid="white",high="red")+
  #geom_hline(yintercept=c(600,700), linetype=2, size=.2) +
  #ylim(c(0,900))+
  scale_radius(range=c(.5, 11))+ labs( subtitle= "MFS easternern sector")+
  scale_shape_manual(values=c(21,22,23,24))+
  geom_line(data=mid1 , aes(x ,depth  ),  size=.35, linetype=3, col="blue3")+
 # labs(  y=" " )+
  geom_line(data=mod1 , aes(x ,depth  ),  size=.35, linetype=1, col="blue3")+
  geom_path(data=c600 , aes(x , depth  ),  size=.25, linetype=5, col="orange") +#+
  geom_path(data=c650 , aes(x , depth  ),  size=.1, linetype=5, col="orange3") +#+
  geom_path(data=c700 , aes(x , depth  ),  size=.25, linetype=5, col="brown") +
  xlim(-100,400)+
  ylim(-100,3)#+

ggsave("T_distance_stress_easterncmt.pdf", width=10, height=8)



p2=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5 , left=0 )
pps <-ehb1.ppp[p2]$marks[,c( "distance","depth", "m")] %>% mutate(depth= -depth*fac)
pps <- rbind(pps,c(295,-56,7.1) )
pps$temp <-extract(t.ras.c,pps[, 1:2] )
pps$stress <-extract(s.ras.c,pps[,1:2]  )


ggplot(pps, aes(stress, temp))+geom_point()
ggplot(pps, aes(distance, temp,   size=m,fill=stress))+geom_point(shape=21)+scale_fill_gradient2(low="blue",mid="white",high="red")+
  geom_hline(yintercept=c(600,700), linetype=2, size=.2) +ylim(c(0,900))+
  scale_radius(range=c(.5, 11))+ labs( subtitle= "MFS easternern sector")
ggsave("T_distance_stress_western.pdf", width=10, height=8)

ggplot(pps, aes(stress, temp))+geom_point()
ggplot(pps, aes(distance, temp,   size=m,fill=stress))+geom_point(shape=21)+scale_fill_gradient2(low="blue",mid="white",high="red")+
  geom_hline(yintercept=c(600,700), linetype=2, size=.2) +ylim(c(0,900))+
  scale_radius(range=c(.5, 11))+ labs( subtitle= "MFS easternern sector")
ggsave("T_distance_stress_eastern.pdf", width=10, height=8)

p2=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5   )
pps <-ehb1.ppp[p2]$marks[,c( "distance","depth", "m")] %>% mutate(depth= -depth*fac)
pps <- rbind(pps,c(295,-56,7.1) )
pps$temp <-extract(t.ras.c,pps[, 1:2] )
pps$stress <-extract(s.ras.c,pps[,1:2]  )


ggplot(pps, aes(stress, temp))+geom_point()
ggplot(pps %>% subset(m>=5.5), aes(distance, temp,   size=m,fill=stress))+
  geom_point(shape=21)+scale_fill_gradient2(low="blue",mid="white",high="red")+
  geom_point(data=pps %>% subset(m<5.5), shape=4, size=.6, col="black", stroke=.23)+
  geom_hline(yintercept=c(600,700), linetype=2, size=.2) +ylim(c(0,900))+
  scale_radius(range=c(2, 11))+ labs( subtitle= "MFS, m>= 5.5")
ggsave("T_distance_stress.pdf", width=10, height=8)

}
# p2l=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5 , left=0   )
# p2r=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5 , right=0   )
# ehb.l <-ehb1.ppp[p2l]$marks[,c( "distance","depth", "m")] %>% mutate(depth= -depth*fac)
# cmt.l <-cmt1.ppp[p2l]$marks[,c( "distance","depth", "m", "regime")] %>% mutate(depth= -depth*fac)
# ehb.r <-ehb1.ppp[p2rl]$marks[,c( "distance","depth", "m")] %>% mutate(depth= -depth*fac)
# cmt.r <-cmt1.ppp[p2r]$marks[,c( "distance","depth", "m", "regime")] %>% mutate(depth= -depth*fac)
# ehb.l <- rbind(ehb.l,c(295,-56,7.1) )


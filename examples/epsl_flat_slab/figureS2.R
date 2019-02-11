plt.m <- function(fname, p2=p2, p2a=p2a, xlim=c(-70,350), tresh=4) {
  get_this_isc(fname,csv=T) %>% isc_ppp() -> isc.ppp
  project_ppp1(isc.ppp[p2], p2) -> isc1.ppp
  isc1.ppp[p2a]$marks$distance <- -abs(  isc1.ppp[p2a]$marks$distance)
  isc.sub <- isc1.ppp[p2]$marks
  isc.sub$period="1960-2013"
  isc.sub$period[lubridate::year(isc.sub$date)>2013] ="2014+"
  isc.sub$stroke=.5
  isc.sub$stroke[lubridate::year(isc.sub$date)>2013] =1

  ggplot(isc.sub %>% subset(is.na(depfix) & m>= thresh), aes(distance, -depth, size=m))+

    geom_point(data=isc.sub %>% subset(is.na(depfix) & m< thresh),  size=.2,shape=0, col="grey70" )+
    geom_point(shape=0, col="red" , aes( size=m))+ scale_radius(range=c(.1, 8))+

    geom_point(aes( shape=period, col=period , stroke=stroke))+
    scale_shape_manual(values=c(0,1))+
      scale_color_manual(values=c("red","black"))+
    theme(legend.position = c(.85,.8), legend.box = 'horizontal') +xlim(xlim)
}

sum.m <- function(ppp){

 list(n= length(ppp$marks$lat),
      p= length(ppp$marks$depfix[is.na(ppp$marks$depfix)]))
}

#length(ml.isc1.ppp$marks$depfix[is.na(ml.isc1.ppp$marks$depfix)])

get.downloads <- F

p2=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5  )

if (get.downloads==T) {
  get_isc_srn_rect(year=1960, min.mag=2.5, mag.type = "ML") ->ml.fn
  get_isc_srn_rect(year=1960, min.mag=2.5, mag.type = "MS") ->ms.fn
  get_isc_srn_rect(year=1960, min.mag=2.5, mag.type = "MW") ->mw.fn
 save(ml.fn,ms.fn,mw.fn, file="/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/data/fnames.Rdata")
}

load("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/data/fnames.Rdata")
get_this_isc(ml.fn,csv=T) %>% isc_ppp() -> ml.isc.ppp
project_ppp1(ml.isc.ppp[p2], p2) -> ml.isc1.ppp
ml.isc1.ppp[p2a]$marks$distance <- -abs(  ml.isc1.ppp[p2a]$marks$distance)
ml.s<-sum.m(ml.isc1.ppp[p2])


get_this_isc(ms.fn,csv=T) %>% isc_ppp() -> ms.isc.ppp
project_ppp1(ms.isc.ppp[p2], p2) -> ms.isc1.ppp
ms.isc1.ppp[p2a]$marks$distance <- -abs(  ms.isc1.ppp[p2a]$marks$distance)
ms.s<-sum.m(ms.isc1.ppp[p2])

get_this_isc(mw.fn,csv=T) %>% isc_ppp() -> mw.isc.ppp
project_ppp1(mw.isc.ppp[p2], p2) -> mw.isc1.ppp
mw.isc1.ppp[p2a]$marks$distance <- -abs(  mw.isc1.ppp[p2a]$marks$distance)
mw.s<-sum.m(mw.isc1.ppp[p2])





thresh=4.

(p.mw <- plt.m(mw.fn, p2, p2a)+labs(subtitle=paste0("ISC Mw, n=",mw.s[2], " (",mw.s[1],")")))

(p.ml <- plt.m(ml.fn, p2, p2a)+labs(subtitle=paste0("ISC Ml,n=",ml.s[2], " (",ml.s[1],")")))
(p.ms <- plt.m(ms.fn, p2, p2a)+labs(subtitle=paste0("ISC Ms, n=",ms.s[2], " (",ms.s[1],")")))

(p.ehb <-ggplot(ehb1.ppp[p2]$marks, aes(distance, -depth, size=m  ))+
  geom_point(col="red" , shape=0)+
  theme(legend.position = c(.85,.8), legend.box = 'horizontal') +xlim(c(-70,350))+
  labs(subtitle=paste0("ISC EHB, n=", length(ehb1.ppp[p2]$marks$lat))))

(p.cmt <-ggplot(cmt1.ppp[p2]$marks %>% subset(lubridate::year(date)<=2013) , aes(distance, -depth, size=m  ))+
  geom_point(col="red" , shape=0)+
    geom_point(data=cmt1.ppp[p2]$marks %>% subset(lubridate::year(date)>2013) ,col="black" , shape=1, stroke=1)+
  theme(legend.position = c(.85,.8), legend.box = 'horizontal') +xlim(c(-70,350))+
    labs(subtitle=paste0("CMT, n=", length(cmt1.ppp[p2]$marks$lat))))


# mod.fac=1
# (fig.sup.isc <-cowplot::plot_grid(
#     p.ml+ylim(c(-100,0)) +theme(legend.position = "None") +   geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=5, col="blue3"),
#     p.ms+ylim(c(-100,0)) +theme(legend.position = "None") +  geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=5, col="blue3"),
#     p.mw+ylim(c(-100,0))+theme(legend.position = c(.15,.25))+  geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=5, col="blue3"),
#     p.ehb+ylim(c(-100,0))  + geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=5, col="blue3"),
#     p.cmt+ylim(c(-100,0))  + geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=5, col="blue3"),
#     ncol=2,
#     labels = c("Ml", "Ms", "Mw", "EHB", "CMT") ))

#ggsave(paste0(fig.dir,"/figSx.pdf"),   plot=fig.sup.isc ,width=15, height=12)



####
out.dir <-"/Volumes/data/data/regional/mexico/quakes/ssn/rdata"

load( paste0(out.dir, "/ssn.Rdata" ))

project_ppp1(ssn.ppp[p2], p2) -> ssn1.ppp
ssn1.ppp[p2a]$marks$distance <- -abs(  ssn1.ppp[p2a]$marks$distance)
# ssn.sub <- isc1.ppp[p2]$marks
# isc.sub$period="1960-2013"
# isc.sub$period[lubridate::year(isc.sub$date)>2013] ="2014+"

#offset=513
# offset200=580
# offset100=560

mod <-"/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/mod1/"
source(paste0(mod,"offset.r"))
mod1 <-read.csv(paste0(mod,"slabTop.csv"), col.names=c("x", "depth")) %>%
  subset(x>= -offset-100 )%>%
  mutate(x= x+offset, depth = - depth )#((depth-50)*1 +50))
mid1 <-read.csv(paste0(mod,"midPlane.csv"),
                col.names=c("x", "depth")) %>%
  subset(x>= -offset -100) %>%
  mutate(x= x+offset, depth = -depth)
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


(p.ssn<-ggplot(data=ssn1.ppp$marks %>% subset(m>= thresh & date < lubridate::ymd("2014-01-01")), aes(distance, -depth))+
  geom_point(data=ssn1.ppp$marks %>% subset(m< thresh),  size=.2,shape=0, col="grey70" )+
    geom_point(shape=0, col="red" , aes( size=m))+
  scale_radius(range=c(.1, 8))+
  geom_point(shape=1, col="black", stroke=1,data=ssn1.ppp$marks %>% subset(m>= thresh & date >= lubridate::ymd("2014-01-01")), aes( size=m))+
  scale_radius(range=c(1, 8))+
  theme(legend.position = c(.075,.25), legend.box = 'horizontal')+
  # geom_line(data=mid1 , aes(x ,depth  ),  size=.35, linetype=3, col="blue3")+
  # geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=5, col="blue3")+
  # geom_path(data=c650 , aes(x , depth  ),  size=.35, linetype=5, col="orange") +#+
  # geom_path(data=c700 , aes(x , depth  ),  size=.35, linetype=5, col="orange3") +#+
ylim(c(-130,5))+xlim(-100,400)+
  labs(subtitle=paste0("SSN, n=",length(ssn1.ppp$marks$m) ))
)

fig.sup.isc <-cowplot::plot_grid(
  # p.ml+xlim(-100,400)+ylim(c(-100,3)) +theme(legend.position = c(.9,.7))+ guides(size="none") +labs(x=NULL)+
  #   geom_line(data=mid1 , aes(x ,depth  ),  size=.35, linetype=3, col="blue3")+
  #   geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=1, col="blue3")+
  #   geom_path(data=c600 , aes(x , depth  ),  size=.25, linetype=5, col="orange") +#+
  #   geom_path(data=c650 , aes(x , depth  ),  size=.1, linetype=5, col="orange3") +#+
  #   geom_path(data=c700 , aes(x , depth  ),  size=.25, linetype=5, col="brown"), #+
  # p.ms+xlim(-100,400)+ylim(c(-100,3)) +theme(legend.position = "None") +labs(x=NULL, y=" ")+
  #   geom_line(data=mid1 , aes(x ,depth  ),  size=.35, linetype=3, col="blue3")+
  #   geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=1, col="blue3")+
  #   geom_path(data=c600 , aes(x , depth  ),  size=.25, linetype=5, col="orange") +#+
  #   geom_path(data=c650 , aes(x , depth  ),  size=.1, linetype=5, col="orange3") +#+
  #   geom_path(data=c700 , aes(x , depth  ),  size=.25, linetype=5, col="brown"), #+
  # p.mw+xlim(-100,400)+ylim(c(-100,3))+theme(legend.position = "None")+
  #   geom_line(data=mid1 , aes(x ,depth  ),  size=.35, linetype=3, col="blue3")+labs(x=NULL)+
  #   geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=1, col="blue3")+
  #   geom_path(data=c600 , aes(x , depth  ),  size=.25, linetype=5, col="orange") +#+
  #   geom_path(data=c650 , aes(x , depth  ),  size=.1, linetype=5, col="orange3") +#+
  #   geom_path(data=c700 , aes(x , depth  ),  size=.25, linetype=5, col="brown"), #+
  p.join+
    theme( legend.background=element_blank()),
  p.ehb+xlim(-100,400)+ylim(c(-100,3))  + geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=5, col="blue3")+
    geom_line(data=mid1 , aes(x ,depth  ),  size=.35, linetype=3, col="blue3")+labs(x=NULL, y=" ")+
    geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=1, col="blue3")+
    geom_path(data=c600 , aes(x , depth  ),  size=.25, linetype=5, col="orange") +#+
    geom_path(data=c650 , aes(x , depth  ),  size=.1, linetype=5, col="orange3") +#+
    geom_path(data=c700 , aes(x , depth  ),  size=.25, linetype=5, col="brown"), #+
  p.ml+xlim(-100,400)+ylim(c(-100,3)) +theme(legend.position = "None") +labs(x=NULL, y=" ")+
    geom_line(data=mid1 , aes(x ,depth  ),  size=.35, linetype=3, col="blue3")+
    geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=1, col="blue3")+
    geom_path(data=c600 , aes(x , depth  ),  size=.25, linetype=5, col="orange") +#+
    geom_path(data=c650 , aes(x , depth  ),  size=.1, linetype=5, col="orange3") +#+
    geom_path(data=c700 , aes(x , depth  ),  size=.25, linetype=5, col="brown"), #+

  # p.cmt+xlim(-100,400)+ylim(c(-100,3))  + geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=5, col="blue3")+
  #   geom_line(data=mid1 , aes(x ,depth  ),  size=.35, linetype=3, col="blue3")+
  #   geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=1, col="blue3")+
  #   geom_path(data=c600 , aes(x , depth  ),  size=.25, linetype=5, col="orange") +#+
  #   geom_path(data=c650 , aes(x , depth  ),  size=.1, linetype=5, col="orange3") +#+
  #   geom_path(data=c700 , aes(x , depth  ),  size=.25, linetype=5, col="brown"), #+
  p.ssn+xlim(-100,400)+ylim(c(-100,3))  + geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=5, col="blue3")+
    geom_line(data=mid1 , aes(x ,depth  ),  size=.35, linetype=3, col="blue3")+labs(  y=" " )+
    geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=1, col="blue3")+
    geom_path(data=c600 , aes(x , depth  ),  size=.25, linetype=5, col="orange") +#+
    geom_path(data=c650 , aes(x , depth  ),  size=.1, linetype=5, col="orange3") +#+
    geom_path(data=c700 , aes(x , depth  ),  size=.25, linetype=5, col="brown"), #+
  ncol=1,
  labels = c("a) ", #ISC\n Ml",
             #"ISC\n Ms",
             #"ISC\n Mw",
             "b) ",
             "c) ",
            # "CMT",
             "d) ") )

ggsave(paste0(fig.dir,"/figS2.pdf"),   plot=fig.sup.isc ,width=10, height=19)


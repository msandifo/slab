

min.dis=140
p2l=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5, right=0 )
p2=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5, right=0 )
p2=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5, left=0 )
p2=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5 )

mfs.ehb <-(ehb1.ppp[p2] %>% subset(distance > min.dis))$marks


mfs.cmt <-(cmt1.ppp[p2] %>% subset(distance > min.dis & !duplicated(eventname)))$marks
mfs.cmt$thour <- mfs.cmt$date %>% trunc("min" ) %>% as.character()
mfs.ehb$thour <- mfs.ehb$time %>% trunc("min")  %>% as.character()
#library(data.table)
m.mfs <-  merge( mfs.cmt , mfs.ehb ,by="thour")

( mfs.depths.1 <-ggplot( m.mfs, aes(y= depth.x - depth.y, x= depth.y, size=m.y)) + geom_point(shape=1)+
    geom_point(aes(x= m.mfs$depth.x, size=m.x), col="red", shape=0)+
    geom_segment( aes( yend=m.mfs$depth.x - m.mfs$depth.y, xend= m.mfs$depth.x ), size=.2)+
    coord_flip()+
    geom_vline(xintercept= c(max(m.mfs$depth.y), min(m.mfs$depth.y)), linetype=5, size=.2)+
    geom_vline(xintercept= c(max(m.mfs$depth.x), min(m.mfs$depth.x)), linetype=3, size=.2, col="red")+
   # annotate("text", x=c(1,1),
   #          y = c(max(m.mfs$depth.y), min(m.mfs$depth.y)),
   #          label= round(c(max(m.mfs$depth.y), min(m.mfs$depth.y)),0)  )+
   #  annotate("text", x=1,
   #           y =  (max(m.mfs$depth.y) ),
   #           label= round((max(m.mfs$depth.y) ),0)  )+
    labs(y="GCMT(depth) - ISC_EHB(depth) - kms", x= "depth - kms", size="m")+
    xlim(-c(-100,-35))+
    theme(legend.position = c(.1, .25) )+
    scale_radius(range=c(.5,8))
)



(mfs.depths <-ggplot(mfs.ehb)+
    geom_density(aes(depth))+
    geom_point( aes(depth, y = (m-4)/(66),  size=distance), shape=1)+
    geom_density(data= mfs.cmt, aes(depth), col="red", linetype=5)+
    geom_point(data= mfs.cmt,  aes(depth, y = (m-4)/(66),  size=distance), shape=0, col="red")+

    scale_y_continuous(sec.axis = sec_axis(~.*(66) +4, name="Mw"), expand = c(0,0) )+
    geom_vline( xintercept= c(min(mfs.ehb$depth), max(mfs.ehb$depth)), linetype=5, size=.3)+
    geom_vline( xintercept= c(min(mfs.cmt$depth), max(mfs.cmt$depth)), linetype=3, size=.2, col="red")+
    xlim(-c(-100,-35))+
    coord_flip()+
    labs(x="depth - kms", y="kernel density")+
    theme(legend.position = c(.9,.25))+scale_radius(range=c(.5,8))
)

merge.mfs.depths <- gridExtra::grid.arrange(mfs.depths, mfs.depths.1 , ncol=1 )
ggsave(paste0(fig.dir,"/figS1.pdf"),   plot=merge.mfs.depths,width=8, height=9)

#
# length(mfs.ehb$depth)
# summary( (mfs.ehb$depth))
# length(mfs.ehb$depth)
# summary( (mfs.ehb$depth))
#
# mfs.cmt  %>% subset(m>6)
#
#
# (mfs.depths_m <-ggplot(mfs.ehb, aes(distance, m,size=depth))+
#     geom_point(   shape=1)+
#     #geom_smooth(method="glm", size=0, show.legend = F)+
#     scale_radius()+
#     geom_point( data=mfs.cmt, shape=1, col="red")
# )
#
#
# isc.sub <- isc1.ppp[p2]$marks
# isc.sub$period="1960-2013"
# isc.sub$period[lubridate::year(isc.sub$date)>2013] ="2014+"
#
# ggplot(isc.sub, aes(distance, -depth, size=m, shape=period, col=period))+
#   geom_point( )+scale_shape_manual(values=c(0,1))+scale_color_manual(values=c("red","black"))+
#   theme(legend.position = c(.85,.8), legend.box = 'horizontal')
#
# ehb1.ppp[p2]$marks %>% subset(distance> 140) %>% dplyr::select(depth) -> ehb1.p2.depths
# c(min( ehb1.p2.depths), max( ehb1.p2.depths))
# c(min())
#

plot_kernel <- function(df, cuts= c(4, 6.4,10), xlim=c(0,350), position=NA , alpha=.3, legend.position=c(.9, .75), show.leg=T, points=F, mscale=2, mlimit=4){
  df$m <-pmax(df$ms, df$mb )
  df <- df %>% subset(m >0)
 df$Mw <-  cut(df$m, cuts)

 p1 <- ggplot()+
   hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"),axis_text_size=14, axis_title_size=18)+
   theme(panel.grid.minor=element_blank() )+
   geom_density(data=df, aes(boundary.distance ), fill="white", linetype=2, size=.3, show.legend = F)+
   theme(legend.position = legend.position ) +
   labs(x="distance - kms", y="kernel density estimate")+
  # geom_vline( xintercept=c(100,300), size=.2, linetype=2)+
   scale_y_continuous(expand = c(0,0))+
   scale_x_continuous(expand = c(0,0), limits=xlim)

 if (points) p1<-p1+geom_point(data=df, aes(x= boundary.distance, y = (m-mlimit)/(1000/mscale), size=depth ), show.legend=T, shape=1, stroke=.2)+
   scale_radius(range=c(1,10)) + scale_y_continuous(sec.axis = sec_axis(~.*(1000/mscale) +mlimit, name="Mw"))

 if (is.na(position)) p1+ geom_density(data=df %>% subset(!is.na(Mw)), aes(boundary.distance , fill=Mw),alpha=alpha ) else
   p1+ geom_density(data=df %>% subset(!is.na(Mw)), aes(boundary.distance ,fill=Mw), position = position, alpha=alpha, show.legend = F)
}

(p.kernel.cmt<-plot_kernel(cmt.ppp$marks %>% subset(profile !="p1"), cuts= c(4,  6.4,10), points=T, leg="bottom", mscale=3.5 )+
  scale_x_continuous(lim=c(0,350), expand=c(0,0))+
  facet_wrap(~profile  ))
ggsave(paste0(fig.dir,"mexican.quake_kernel.pdf"), plot=p.kernel.cmt,width=12, height=8)


plot_kernel(ehb.ppp$marks %>% subset(profile =="p1"  & m != 0), cuts= c(4, 6.,10), points=T, leg="bottom", mscale=3.5, mlimit=3 )+
  annotate("text", 10, .0065, label="wedge", size=6)+
  annotate("text", 135, .0135, label="megathrust", size=6)+
  annotate("text", 325, .0035, label="flat slab", size=6)#+ ylim(c(0,0.016))

(p.kernel.cmt<-plot_kernel(cmt.ppp$marks %>% subset(profile =="p2" ), cuts= c( 5.4,10), points=T,
                           leg=c(.7,.95), mscale=3.5, xlim=c(-10,400 ))+
#  scale_x_continuous(lim=c(0,350), expand=c(0,0))+
 # annotate("text", 40, .0135, label="wedge", size=4.5)+
  annotate("text", 90, .0145, label="megathrust", size=4.5)+
  annotate("text", 300, .0145, label="flat slab", size=4.5)+ guides(fill="none")+theme(legend.direction = "horizontal")+
    geom_hline(yintercept= (5.4-4)/ (   (1000/3.5)+4), linetype=1, size=.2)
)


ggsave(paste0(fig.dir,"mexican.quake_kernel_cmt_p2.pdf"), plot=p.kernel.cmt,width=12, height=8)


# plates= mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123") ]
# coasts.df <- mex$coast%>% fortify()
#
#
# dplyr::select(mex$vo.ppp$marks, starts_with("lon"), starts_with("lat") )
# proj.laea <- "+proj=laea +lat_0=0 +lon_0=-97"
#
#
# #note the use of 'bearing' gives contrains on the selced field being in NNE sector (<45) in thsi case
# kernel.ppp <-  project_ppp(  cmt.ppp%>% subset(long< -90),plates)  %>% subset(((bearing > 0 & bearing < 90) | bearing >340) & pro.long< -94 &   pro.long> -100)
# #kernel.ppp <-  project_ppp( mex$ehb.ppp %>% subset(long< -90),mex$plates.ppp) %>% subset(bearing > 0 & bearing < 90)
# kernel.ppp <- cmt.ppp
# kernel.ppp <- ehb.ppp
# kernel.ppp$marks <- cmt.ppp$marks %>% subset(profile!="p0")
# kernel.ppp$marks$m <-pmax(kernel.ppp$marks$ms, kernel.ppp$marks$mb)
#
# ggplot(kernel.ppp$marks %>%subset(m>=4), aes(boundary.distance, m))+geom_point()+ xlim(c(0,400))
#
# kernel.ppp$marks$Mw =kernel.ppp$marks$m %>% cut( c(4.,   6.4,10))
#
# ggplot(kernel.ppp$marks %>%subset(profile !="p0"), aes(boundary.distance , fill=Mw))+
#   geom_density(position = "fill") +
#   xlim(c(0,350))+facet_wrap(~profile )
#
# ggplot(kernel.ppp$marks %>%subset(profile !="p0"), aes(boundary.distance , fill=Mw))+geom_density(position = "stack") + xlim(c(0,350))+
#   hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"),axis_text_size=14, axis_title_size=18)+
#   theme(panel.grid.minor=element_blank(),panel.grid.major=element_line(size=.1, linetype=1) )+
#   theme(legend.position = c(.9, .75)) +facet_wrap(~profile )
#
# (p.kernel.cmt<-ggplot(kernel.ppp$marks %>%subset(m>=0. & profile !="p0"), aes(boundary.distance , fill=Mw))+
#    geom_density(data=kernel.ppp$marks  %>%subset(m>=0. & profile !="p0"), aes(boundary.distance ), fill="white", linetype=2, size=.3)+
#   geom_density( alpha=.35, size=.35) +
#   xlim(c(0,350))+
#   # annotate("text", 20, .0065, label="wedge", size=6)+
#   # annotate("text", 130, .02, label="megathrust", size=6)+
#   # annotate("text", 320, .003, label="flat slab", size=6)+
#     labs(x="distance - kms", y="kenel density estimate")+
#   geom_vline( xintercept=c(88.5,290), size=.2, linetype=2)+
#   hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"),axis_text_size=14, axis_title_size=18)+
#   theme(panel.grid.minor=element_blank(),panel.grid.major=element_line(size=.1, linetype=1) )+
#    theme(legend.position = c(.9, .95)) +facet_wrap(~profile , scales="free_y")  )
#
# ggsave(paste0(fig.dir,"mexican.quake_kernel.pdf"), plot=p.kernel.cmt,width=12, height=8)
#
# (p.kernel.cmt<-ggplot(kernel.ppp$marks %>%subset(m>=0. & profile =="p2"), aes(boundary.distance , fill=Mw))+
#     geom_density(data=kernel.ppp$marks  %>%subset(m>=0. & profile =="p2"), aes(boundary.distance ), fill="white", linetype=2, size=.3)+
#     geom_density( alpha=.35, size=.35) +
#     xlim(c(0,350))+
#      annotate("text", 20, .0065, label="wedge", size=6)+
#     annotate("text", 135, .015, label="megathrust", size=6)+
#       annotate("text", 325, .005, label="flat slab", size=6)+
#     labs(x="distance - kms", y="kernel density estimate")+
#     geom_vline( xintercept=c(100,300), size=.2, linetype=2)+
#     hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"),axis_text_size=14, axis_title_size=18)+
#     theme(panel.grid.minor=element_blank(),panel.grid.major=element_line(size=.1, linetype=1) )+
#     theme(legend.position = c(.9, .75)) +facet_wrap(~profile ))
#
# ggsave(paste0(fig.dir,"mexican.quake_kernel_p2.pdf"), plot=p.kernel.cmt,width=12, height=8)
#
#
# kernel.ppp <-  project_ppp( mex$cmt.ppp%>% subset(long< -90),plates)  %>% subset(bearing > 0 & bearing < 45 & pro.long> -93 )
# #kernel.ppp <-  project_ppp( mex$ehb.ppp %>% subset(long< -90),mex$plates.ppp) %>% subset(bearing > 0 & bearing < 90)
#
# kernel.ppp$marks$m <-pmax(kernel.ppp$marks$ms, kernel.ppp$marks$mb)
# ggplot(kernel.ppp$marks %>%subset(m>=6), aes(boundary.distance, m))+geom_point()+ xlim(c(0,400))
#
# kernel.ppp$marks$Mw ="4-5"
# #kernel.ppp$marks$mplus[kernel.ppp$marks$m>4] ="4"
# kernel.ppp$marks$Mw[kernel.ppp$marks$m>=5] ="5.-6.5"
# #kernel.ppp$marks$mplus[kernel.ppp$marks$m>=6] ="6-7"
# kernel.ppp$marks$Mw[kernel.ppp$marks$m>=6.5] ="6.5+"
#
# ggplot(kernel.ppp$marks %>%subset(m>=4), aes(boundary.distance , fill=mplus))+geom_density(position = "fill") + xlim(c(0,350))
# ggplot(kernel.ppp$marks %>%subset(m>=4), aes(boundary.distance , fill=mplus))+geom_density(position = "stack") + xlim(c(0,400))
#
# (p.kernel.cmt<-ggplot(kernel.ppp$marks %>%subset(m>=5.), aes(boundary.distance , fill=Mw))+
#     geom_density(data=kernel.ppp$marks , aes(boundary.distance ), fill="white", linetype=2, size=.3)+
#     geom_density( alpha=.35, size=.35) +
#     xlim(c(0,350))+
#     annotate("text", 20, .0065, label="wedge", size=6)+
#     annotate("text", 130, .02, label="megathrust", size=6)+
#     annotate("text", 320, .003, label="flat slab", size=6)+
#     labs(x="distance - kms", y="kenel density estimate")+
#     geom_vline( xintercept=c(88.5,290), size=.2, linetype=2)+
#     hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"),axis_text_size=14, axis_title_size=18)+
#     theme(panel.grid.minor=element_blank(),panel.grid.major=element_line(size=.1, linetype=1) )+
#     theme(legend.position = c(.9, .75)) )
#
#
#
# ggplot(kernel.ppp$marks %>%subset(m>=6), aes(boundary.distance ))+
# geom_density(data=kernel.ppp$marks %>%subset(m>=6.6), col="red")+ xlim(c(0,400))+
#   geom_density(data=kernel.ppp$marks %>%subset(m>=7), col="blue")+ xlim(c(0,400))+
#   geom_density(data=kernel.ppp$marks %>%subset(m>=5.5), col="orange")+  xlim(c(0,350))+
#   labs(x="distance - kms", y="kenel density estimate")
#
#
#
# ggplot(kernel.ppp$marks %>%subset(m>=6), aes(boundary.distance ),fill=NA)+
#   geom_histogram(data=kernel.ppp$marks %>%subset(m>=6.5), fill=NA, col="red")+ xlim(c(0,400))+
#   geom_histogram(data=kernel.ppp$marks %>%subset(m>=7), fill=NA,col="blue")+ xlim(c(0,400))+
#   geom_histogram(data=kernel.ppp$marks %>%subset(m>=5.5), fill=NA,col="orange")+ xlim(c(0,400))
#
#

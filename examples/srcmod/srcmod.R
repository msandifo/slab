source('~/Dropbox/msandifo/documents/programming/r/packages/slab/examples/srcmod/funcs.R')

#update
update <- F

if (update) { update_downloads();  update_downloads(type="mat") }

##-----
SRCMODs.names<- get_SRCMOD_names()
map_df(SRCMODs.names , read.fsp) -> srcmod.df
names(srcmod.df) <- str_to_lower(names(srcmod.df))
names(srcmod.df)[str_detect(names(srcmod.df), "sumatra")]
srcmod.df %>% subset(lubridate::year(date)==2012 & (str_detect(event, "Sumatra")|str_detect(event, "sumatra")))
srcmod.df %>% subset(lubridate::year(date)>2008 & (str_detect(event, "Vanu")|str_detect(event, "vanu")))

(p.srcmod<- ggplot(srcmod.df %>% subset(date>lubridate::ymd("2010-01-01")), aes(mw, wid)) +
    geom_point()+
    scale_y_log10(lim=c(3,500))+
  stat_smooth(method="lm", level=.99999999, size=0)+
   geom_hline(yintercept = c(25,35, 50),  linetype=c(5,1,5))+
   geom_vline(xintercept = c(6.6, 7.1, 7.6)+.2, linetype=c(5,1,5))+
  scale_x_continuous(lim=c(5.5,8.6)))



width=seq(10:200)
strasser<-data.frame(width=width,
                     Mw = 3.407 +2.511 *log10(width))

strasser.se<-data.frame(width=width,
                        Mw =  3.407 +.317+ (2.511+.217)*log10(width))
strasser.se1<-data.frame(width=width,
                         Mw =  3.407 -.317+ (2.511-.217) *log10(width))

(p.srcmod.1 <- p.srcmod + geom_line(data=strasser, aes(Mw, width), col="red")+
  geom_line(data=strasser.se, aes(Mw, width), col="red", linetype=5)+
  geom_line(data=strasser.se1, aes(Mw, width), col="red", linetype=5))






bird.plates.fn <- "/Volumes/data/data/global/polygons/tectonicplates-master/PB2002_boundaries.shp"
bird.plates = readShapeLines(bird.plates.fn)
bird.plates.psp =readShapeLines(bird.plates.fn)  %>% as.psp()
plot(bird.plates.psp)

srcmod.ppp <- ppp(srcmod.df$lon, srcmod.df$lat, window= owin(xrange=c(-180,180), yrange=c(-90,90)), marks= srcmod.df)

points(srcmod.ppp)

ppp2spdf(srcmod.ppp, extent(slab))

y<- ppp2spdf (srcmod.ppp,extent(slab))



n_add_slab <- function(my.points, slab, d=0 ){

  my.points$slab.depth <-  my.points %>% raster::extract(slab$depth, .)
  my.points$slab.dip <-  my.points %>% raster::extract(slab$dip, .)
  my.points$slab.strike <-  my.points %>% raster::extract(slab$strike, .)
  my.points$depth.anom <-  my.points$dep + my.points$slab.depth
  my.points$type="other"
  my.points$type[my.points$depth.anom> d] ="subduction interplate"
  my.points$type[my.points$depth.anom<= d] ="subduction interface"

  return(my.points)

}

load("/Volumes/data/data/global/slabs/slab1/slabs.Rdata")

ppp2spdf (srcmod.ppp,extent(slab)) %>%
  n_add_slab( slab , d= 5 ) %>% as.data.frame() -> x10
x10$slab.dip
x10$wid

chiapas<-"s2017CHIAPA01OKUW"
x10[x10$id==chiapas,"type"] <-"subduction interplate"
x10[x10$id==chiapas,"rake"] <- -99
x10$id[x10$rake> 360]
x10$rake[x10$rake> 360]
ggplot(x10 %>% subset(type=="subduction interplate"& mw>5.5 &date >lubridate::ymd("2010-01-01")),aes(mw, wid))+
  geom_smooth(method="lm", size=.5, linetype=1, fullrange=T, col="black", se=F)+facet_wrap(~type)+
  geom_smooth(data=x10 %>% subset(type=="interface" & mw>5.5),method="lm", size=.5, linetype=5, fullrange=T, col="red", se=F)+

  geom_point(shape=0, size=4)+
  geom_point(data=x10 %>% subset(type=="interface" & mw>5.5),shape=1, col="red", size=2)+
  scale_y_log10(lim=c(9,500))+
  geom_hline(yintercept=35)+
  labs(x="Mw", y="width")

x10 %>% subset(type=="subduction interplate"& mw>5.5 &date >lubridate::ymd("2010-01-01"))
#source('~/Dropbox/msandifo/documents/programming/r/2017/AEMO/vic.power/hrbrtheme.R')
x10$fm <-"S"
x10$fm[x10$rake  >= 45 & x10$rake< 135] <-"T"
x10$fm[(x10$rake  <= -45 & x10$rake>= -135) | (x10$rake>900 & x10$mw<9)] <-"N"
x10$rake
x10$fm
x11<-x10 %>% subset(  mw>5.0 &  (((rake  > 30 & rake <150) |(rake  < -30 & rake > -150)) ))

x11<-x10 %>% subset(  mw>5.0 & abs(strk%%180 - rake%%180)  >30 & abs(strk%%180 - rake%%180) <150)
x11<-x10 %>% subset(  mw>5.0 &  ((rake  > 30 & rake <150) |(rake  < -30 & rake > -150)) & mw < 9 & len >wid & wid<150)

ggplot( x10 %>% subset(  mw>5.0 &mw<9.1 &fm !="S" & type != "other"),
       aes(mw , wid,   col=type, fill=type, shape=fm , linetype=fm))+
  geom_smooth(data=  x10 %>% subset(  mw>5.0 &mw<9.1 & fm !="SS" & type!="other"),
              method="lm", size=.35,  aes(group=type),  fullrange=T,   se=F, level=.9, show.legend=F, linetype=1, col="black", fill="white", shape=1)+

   geom_smooth(method="lm", size=.35,   fullrange=T,   se=F, level=.9, show.legend=T)+
  facet_wrap(~type)+
  geom_point( size=2.5,show.legend=T ) +
  scale_y_log10(lim=c(9,300))+
  geom_hline(yintercept=c(35, 60), linetype=5, size=.2)+
  scale_size(range=c(2,8))+
  scale_shape_manual(values=c(0,1,2))+
  labs(x="Mw", y="width")+
  theme(legend.position="none")+
  annotate("text", x=c(9 ), y=c(35 ), label=c("35 kms" ), vjust= -.5, size=2)+
  annotate("text", x=c(9 ), y=c(60 ), label=c("60 kms" ), vjust= -.5, size=2)+
  annotate("text", x=c(8.5), y=c(68 ), label=c("", "CHAIPAS"), vjust= -.5, col="blue3", size=3)+
  geom_vline(xintercept= c( 6.7, 7.2), linetype=5, size=.2)+
  theme_bw()+guides(colour=F,fill=F)+
  theme(legend.position = c(.05,.8))

ggsave(paste0(fig.dir,"/srcmod_scaling.pdf"), width=10, height=5)

lm(formula = mw ~  log10(wid), data = x10 %>% subset(type=="subduction interplate" & fm !="SS")) -> l1
 lm(formula = mw ~  log10(wid), data = x10 %>% subset(type=="subduction interface" & fm !="SS"))  -> l2
 lm(formula = mw ~  log10(wid), data = x10 %>% subset(type=="other"&fm !="SS")) -> l3

 stargazer::stargazer(l2,l1,  title="Results", align=TRUE)

 lm(formula = mw ~  log10(wid)+type, data = x10 )



 ####
 ###
 #
 #
 # slp.url="http://equake-rc.info/media/srcmod/_slp_files/"
 # url <-"http://equake-rc.info/SRCMOD/searchmodels/allevents/"
 #
 # XML::getHTMLLinks(url) -> links
 # #getHTMLExternalFiles(url)
 #
 # SRCMODs <-paste0( "http://equake-rc.info/", links %>% subset(str_detect(links, "searchmodels"))) %>% tail(-1)
 #
 # XML::getHTMLLinks(SRCMODs[1])
 # str_split(SRCMODs, "/")[[]][8]
 #  SRCMODs.names<-(str_split(SRCMODs, "/") %>% unlist())[seq(8,2800,9)]
 #
 # SRCMODs.fsp.names <-paste0("http://equake-rc.info/media/srcmod/_fsp_files/", SRCMODs.names, ".fsp")
 # SRCMODs.fsp.dests <-paste0("/Volumes/data/data/global/quakes/SRCMOD/fsp/", SRCMODs.names, ".fsp")
 # #download.file(SRCMODs.fsp.names, destfile=SRCMODs.fsp.dests )
 # for (i in 1:length(SRCMODs.fsp.names)) download.file(SRCMODs.fsp.names[i], destfile=SRCMODs.fsp.dests[i] )
 #
 # SRCMODs.mat.names <-paste0("http://equake-rc.info/media/srcmod/_mat_files/", SRCMODs.names, ".mat")
 # SRCMODs.mat.dests <-paste0("/Volumes/data/data/global/quakes/SRCMOD/mat/", SRCMODs.names, ".mat")
 # for (i in 1:length(SRCMODs.mat.names)) download.file(SRCMODs.mat.names[i], destfile=SRCMODs.mat.dests[i] )
 #
 #
 # library(R.matlab)
 # readMat(SRCMODs.mat.dests[1]) -> m1
 # m1$s2017CHIAPA01OKUW
 #
 # m1$s2017CHIAPA01OKUW %>% str()
 # rowname(m1)
 # bb(SRCMODs.fsp.dests[1])
 # read_table(SRCMODs.fsp.dests[1], skip = 5, n_max=4, col_names = F)



wd <- "/Users/msandifo/Dropbox/msandifo/documents/manuscripts/Moe Paper/hypoDDdata/examples/"

hyDD.loc <- paste0(wd, "hypoDD.loc")
hyDD.reloc <- paste0(wd, "hypoDD.reloc")

hyDD.loc.names = str_to_lower(c("ID","LAT","LON","DEPTH","X","Y","Z","EX","EY","EZ","YR","MO","DY","HR","MI","SC","MAG","CID"))
hyDD.reloc.names= str_to_lower(c("ID","LAT","LON","DEPTH","X","Y","Z","EX","EY","EZ","YR","MO","DY","HR","MI","SC","MAG","NCCP","NCCS","NCTP","NCTS","RCC","RCT","CID"))

loc <- read.table(hyDD.loc , col.names=hyDD.loc.names) %>% mutate(datetime= add_datetime(.))
reloc <-read.table(hyDD.reloc , col.names=hyDD.reloc.names)%>% mutate(datetime= add_datetime(.))

merge.loc <-merge(loc, reloc, by =c("id", "datetime"))

ggplot(merge.loc , aes( lon.x, lat.x)) +
  geom_point( col="red2", shape=17)+
  geom_point(  aes( lon.y, lat.y), col="blue2", shape=18)+
geom_segment(  aes(  xend=lon.y, yend=lat.y), size=.1)+
  geom_point( data=reloc[reloc$mag> 4,], aes( lon, lat), col="black", shape=19, size=3)+
geom_point( data=loc[loc$mag> 4,], aes( lon, lat), col="red2", shape=19, size=3)

hypoDD.reloc: Relocated hypocenter output
One event per line (written in fixed, but may be read in free format):
  ID, LAT, LON, DEPTH, X, Y, Z, EX, EY, EZ, YR, MO, DY, HR, MI, SC, MAG, NCCP, NCCS, NCTP,
NCTS, RCC, RCT, CID

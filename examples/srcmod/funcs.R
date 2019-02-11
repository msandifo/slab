message("funcs for reading and downloading SRCMOD Finite-Soruce database Rupture http://equake-rc.info/SRCMOD/")

read.fsp <- function(SRCMODs.name){
  SRCMODs.fsp.dest <-paste0("/Volumes/data/data/global/quakes/SRCMOD/fsp/", SRCMODs.name, ".fsp")
  all.lines <- readLines(SRCMODs.fsp.dest, n=9)
  all.lines %>% # readLines(SRCMODs.fsp.dest, n=9) %>%
    tail(-5) %>% str_replace_all("\t\t", ", ")  %>%
    str_replace_all("\t", ",")  %>%
    str_replace_all( "km/s",  "") %>%
    str_replace_all( "km",  "") %>%
    str_replace_all( "s",  "") %>%
    str_replace_all( "Nm",  "") %>%
    str_sub(start=10,end= -1) %>%
    str_c(collapse=",") %>%
    paste0("data.frame(id='", SRCMODs.name,"',",.,')') ->
    parsed.line

  all.lines[3] %>% str_split("\t\t", n=3, simplify=T)  %>% #str_replace("[", "") %>% str_replace("]") %>%
    str_squish()-> events
#  print(events %>% str())


  srcmod <- eval(parse(text= parsed.line))
  srcmod$event <- events[1] %>% str_sub(10,-1)
  srcmod$date <- lubridate::mdy(events[2] )
  srcmod$ref <- events[3]
  #   print(parsed.line)


  return(srcmod)
}


read.slp <- function(SRCMODs.name="s2017CHIAPA01OKUW"){
  SRCMODs.fsp.dest <-paste0("/Volumes/data/data/global/quakes/SRCMOD/slp/", SRCMODs.name, ".slp")
 all.lines <- readLines(SRCMODs.fsp.dest, n=11)
  all.lines %>% tail(-5) %>% str_replace_all("\t\t", ", ")  %>%
    str_replace_all("\t", ",")  %>%
    str_replace_all( "km/s",  "") %>%
    str_replace_all( "km",  "") %>%
    str_replace_all( "s",  "") %>%
    str_replace_all( "Nm",  "") %>%
    str_replace_all( "Hz",  "") %>%
    str_sub(start=9,end= -1) %>%
    str_c(collapse=",") %>%
    paste0("data.frame(id='", SRCMODs.name,"',",.,')') ->
    parsed.line
  #   print(parsed.line)

  all.lines[3] %>% str_split("\t\t", n=3, simplify=T)  %>% #str_replace("[", "") %>% str_replace("]") %>%
    str_squish()-> events
  # print(events %>% str())


 srcmod <- eval(parse(text= parsed.line))
 srcmod$event <- events[1] %>% str_sub(12,-1)
 srcmod$date <- lubridate::mdy(events[2] )
 srcmod$ref <- events[3]

 return(srcmod)
}

plot.slp <-function(SRCMODs.name="s2017CHIAPA01OKUW"){
  info <- read.slp(SRCMODs.name)
  lin <- get_event(SRCMODs.name, typ="slp")
  n.skip<- lin %>%  readLines() %>% str_detect("TOTAL SLIP") %>% which()
  print(n.skip)
  SRCMODs.fsp.dest <-paste0("/Volumes/data/data/global/quakes/SRCMOD/slp/", SRCMODs.name, ".slp")

  slip<-read.table(SRCMODs.fsp.dest,   skip=n.skip, nrows=info$Nz, header=F) %>% as.matrix( cols=info$Nx, byrow=T)
  ras<- raster(slip, xmn=-info$HypX, xmx=(info$Nx*info$inDx)-info$HypX, ymn=-(info$Nz*info$inDz+info$Htop), ymx= -info$Htop )
          #     ext=c(0, (info$Nx*info$inDx), -(info$Nz*info$inDz+info$Htop),-info$Htop))
 # if (info$RAKE != -999) {rake<-read.table(SRCMODs.fsp.dest,   skip=n.skip.1+info$Nz, nrows=info$Nz, header=F) %>% as.matrix( cols=info$Nx, byrow=T)
 # ras <- addLayer(ras, rake)}

  ras#stack(slip,rake)
}
bb<- function(file) system(paste("bbedit ", file))

get_event<-function(event="s2017CHIAPA01OKUW", dir="/Volumes/data/data/global/quakes/SRCMOD/", type="fsp"){
  all.files <-list.files(paste0(dir, type))
  paste0(dir,type,"/",all.files[str_detect(all.files , event)])
}

update_downloads <- function(url="http://equake-rc.info/SRCMOD/searchmodels/allevents/",
                             folder="/Volumes/data/data/global/quakes/SRCMOD/",
                             type="fsp"){

  `%ni%` = Negate(`%in%`)
  if (type=="slp") exclusions <-c("s2015ILLAPE01OKUW") else exclusions <- "111111111111111111"
  message("retrieving links...")
  XML::getHTMLLinks(url) -> links
  #print(links)
  SRCMODs <-paste0( "http://equake-rc.info/", links %>% subset(str_detect(links, "searchmodels"))) %>% tail(-1)
  SRCMODs.names<-(str_split(SRCMODs, "/") %>% unlist())[seq(8,2800,9)]
 # print(SRCMODs.names)
  SRCMODs.fsp.names <-paste0("http://equake-rc.info/media/srcmod/_", type,"_files/", SRCMODs.names, ".", type)
  SRCMODs.fsp.dests <-paste0("/Volumes/data/data/global/quakes/SRCMOD/",type,"/", SRCMODs.names, ".", type)
  SRCMODs.fsp.dests.downloads<- SRCMODs.fsp.dests[!file.exists(SRCMODs.fsp.dests )]

  for (i in 1:length(SRCMODs.fsp.names)){
    #   print(SRCMODs.fsp.names[i])
    if (file.exists(SRCMODs.fsp.dests[i])) message(SRCMODs.fsp.names[i], " already downloaded ....") else if (SRCMODs.names[i] %ni% exclusions)
    download.file(SRCMODs.fsp.names[i], destfile=SRCMODs.fsp.dests[i] )
}
}


get_SRCMOD_names<- function(folder="/Volumes/data/data/global/quakes/SRCMOD/",
                               type="fsp"){

       SRCMODs <-list.files(paste0(folder, type)) %>% str_remove(paste0(".", type))

       SRCMODs
}


poly_dims<-function(p){

  bdry <-data.frame(long= p$bdry[[1]]$x, lat= p$bdry[[1]]$y)
  lx <- length(bdry$long)
 distances <- geosphere::distGeo( c( bdry$long[1],  bdry$lat[1]),  c(bdry$long[2], bdry$lat[2]))
 for (i in 2:(lx-1))  distances <- c(distances,  geosphere::distGeo( c( bdry$long[i],  bdry$lat[i]),  c(bdry$long[i+1], bdry$lat[i+1])) )

  distances <- c(distances,  geosphere::distGeo( c( bdry$long[lx],  bdry$lat[lx]),  c(bdry$long[1], bdry$lat[1])))

  return(list(bdry=bdry, distances=distances/1000))
}

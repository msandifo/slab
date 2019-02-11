source('~/Dropbox/msandifo/documents/manuscripts/januka/rscripts/quake_functions.R')
setwd("/Users/msandifo/Dropbox/msandifo/documents/manuscripts/januka/rscripts")

# note two datasets contains dan's double difference solurtions
#data.dir <-"/Users/msandifo/Dropbox/msandifo/documents/manuscripts/januka/rscripts/data/ds_double_diff/20130421/"
data.dir <-"/Users/msandifo/Dropbox/msandifo/documents/manuscripts/januka/rscripts/data/ds_double_diff/20140714/"
files<-list.files(data.dir) %T>% glimpse()
dd.loc  <-read_df("hypoDD.loc", data.dir )
dd.reloc  <-read_df("hypoDD.reloc", data.dir )
dd.bind <-dd_bind(dd.loc,dd.reloc )

times = c(lubridate::ymd("2012/07/20"), lubridate::ymd("2012/07/08"))
dd.merge<-merge(dd.loc , dd.reloc, by=c("id", "datetime")) %>% add_seqs(pre="seq ")

#dd.merge[dd.merge$mag.x>4,]


gg.loc<-read.table("data/gg_locations/locations.txt", header = T)  %>%
  dplyr::rename_all(tolower) %>%
  dplyr::mutate(datetime= lubridate::ymd_hm(paste(date, stringr::str_pad(time,4, pad="0")))) %>%
  rename(mag=ml, depth=z, lon=long) %T>%
  glimpse()


main.event.np =  matrix(c(83,17,46,218, 78, 102), nrow=2, byrow=2)

sec.event.np = matrix(c(129, 20,34.5,252,79,107), nrow=2, byrow=2)

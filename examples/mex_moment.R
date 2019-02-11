get_moments <- function(df, int=10,  xlim=c(0,350), position=NA , alpha=.3, legend.position=c(.9, .75), points=F, mscale=2, mlimit=4){

  cuts = seq(xlim[1], xlim[2], int)
  df$cuts <-  cut(df$boundary.distance, cuts)

  df <- df %>% subset(exponent >16 & exponent < 30) %>% mutate(m = scalarmoment * 10^exponent)


  df.grouped <-  df %>% group_by(cuts, profile) %>% dplyr::summarise(m=sum(m), boundary.distance=mean(boundary.distance),  depth= mean(depth))
  df.grouped







plot_moments <- function(df,    alpha=.15, legend.position="None", points=F ){


ggplot(df , aes(boundary.distance, m, col=profile, fill=profile))+
  geom_point()+
    geom_smooth(alpha=alpha) +
    scale_y_log10()+
  facet_wrap(~profile  )+
  scale_x_continuous(lim=c(0,350))+

  hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"),axis_text_size=14, axis_title_size=18)+
  theme(panel.grid.minor=element_blank() )+
  theme(legend.position = legend.position ) +
  labs(x="distance - kms", y="moment")

}

m1 <-get_moments(cmt.ppp$marks, int=25)

p.moment<-plot_moments(m1%>% subset(profile != "p0")) +labs(title="25 km bins")
ggsave(paste0(fig.dir,"mexican.moment.pdf"), plot=p.moment,width=12, height=8)

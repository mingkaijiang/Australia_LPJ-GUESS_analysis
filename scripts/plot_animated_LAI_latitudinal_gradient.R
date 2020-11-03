plot_animated_LAI_latitudinal_gradient <- function(myDF) {
    
    ### Plot latitudinal pattern over time
    inDF <- myDF
    inDF[inDF == 0] <- NA
    
    ### convert to latitudinal gradient DF
    latDF <- summaryBy(TeNE+TeBS+IBS+TeBE+TrBE+TrIBE+TrBR+C3G+C4G+Total~Lat+Year, 
                       FUN=c(mean, sd),
                       keep.names=T, na.rm=T, data=inDF)
    
    
    ### plot C4G
    p9 <- ggplot(latDF, aes(x=Lat, y=TeNE.mean, col=Year)) + 
        #geom_ribbon(aes(x=Lat, ymin=TeNE.mean-TeNE.sd,
        #                ymax=TeNE.mean+TeNE.sd))+
        geom_point(show.legend = FALSE, alpha = 0.7) +
        coord_flip()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.box = 'none',
              legend.box.just = 'vertical',
              legend.position = "none")+
        #scale_color_viridis_d() +
        ggtitle("C4G")+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    
    
}
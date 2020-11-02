plot_temporal_PFT_shift <- function(myDF) {
    
    
    ### plot all LAI over 115 years
    for (i in 1901:2015) {
        
        ### subset
        tDF <- subset(myDF, Year == i)
        
        ### plotting
        p1 <- ggplot() + 
            geom_tile(data=tDF, aes(y=Lat, x=Lon, fill=Total)) +
            coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
            borders("world", col="grey", lwd=0.2) +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_text(size=14),
                  axis.title.x=element_text(size=16),
                  axis.text.y=element_text(size=14),
                  axis.title.y=element_text(size=16),
                  legend.text=element_text(size=10),
                  legend.title=element_text(size=12),
                  panel.grid.major=element_blank(),
                  legend.box = 'vertical',
                  legend.box.just = 'left',
                  legend.position = "bottom",
                  legend.background = element_rect(fill="white",
                                                   size=0.5, linetype="solid", 
                                                   colour ="white"),
                  plot.title = element_text(size=14, face="bold.italic", 
                                            hjust = 0.5))+
            scale_fill_continuous(type = "viridis")+
            ggtitle(i)
        
        
        pdf(paste0("output/lai_", i, ".pdf"), width=8, height=5)
        plot(p1)
        dev.off()
    }
    
}
plot_static_LAI_by_year_map_merge_all_PFT <- function(myDF) {
    
    ### Investigate distributions over space
    spDF <- myDF
    
    ### convert 0 to NA
    spDF[spDF == 0] <- NA
    
    ### convert all LAI < 0.1 to 0.1
    tDF <- spDF[,4:16]
    tDF[tDF <= 0.1] <- 0.1
    spDF[, 4:16] <- tDF
    
    ## set lim of color in log scale
    min.lim <- log(0.1)
    max.lim <- log(max(spDF$Total, na.rm=T))
    
    ### map
    for (i in c(1901, 1951, 2000, 2010, 2015)) {
        
        tDF <- subset(spDF, Year == i)
        
        ### plotting
        
        ### TeNE
        p1 <- ggplot() + 
            geom_tile(data=tDF, aes(y=Lat, x=Lon, fill=log(TeNE))) +
            coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
            borders("world", col="grey", lwd=0.2) +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=10),
                  legend.title=element_text(size=12),
                  panel.grid.major=element_blank(),
                  legend.box = 'none',
                  legend.box.just = 'vertical',
                  legend.position = "none",
                  legend.background = element_rect(fill="white",
                                                   size=0.5, linetype="solid", 
                                                   colour ="white"),
                  plot.title = element_text(size=14, face="bold.italic", 
                                            hjust = 0.5))+
            scale_fill_continuous(name="LAI",
                                  na.value = 'white',
                                  #trans = "log",
                                  type = "viridis",
                                  limits = c(min.lim,max.lim),
                                  breaks = c(log(0.1), log(0.5), log(2), 
                                             log(5), log(15)),
                                  labels = c(0.1, 0.5, 2, 5, 15))+
            ggtitle("TeNE")
        
        ### TeBS
        p2 <- ggplot() + 
            geom_tile(data=tDF, aes(y=Lat, x=Lon, fill=log(TeBS))) +
            coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
            borders("world", col="grey", lwd=0.2) +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=10),
                  legend.title=element_text(size=12),
                  panel.grid.major=element_blank(),
                  legend.box = 'none',
                  legend.box.just = 'vertical',
                  legend.position = "none",
                  legend.background = element_rect(fill="white",
                                                   size=0.5, linetype="solid", 
                                                   colour ="white"),
                  plot.title = element_text(size=14, face="bold.italic", 
                                            hjust = 0.5))+
            scale_fill_continuous(name="LAI",
                                  na.value = 'white',
                                  type = "viridis",
                                  limits = c(min.lim,max.lim),
                                  breaks = c(log(0.1), log(0.5), log(2), 
                                             log(5), log(15)),
                                  labels = c(0.1, 0.5, 2, 5, 15))+
            ggtitle("TeBS")
        
        
        ### IBS
        p3 <- ggplot() + 
            geom_tile(data=tDF, aes(y=Lat, x=Lon, fill=log(IBS))) +
            coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
            borders("world", col="grey", lwd=0.2) +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=10),
                  legend.title=element_text(size=12),
                  panel.grid.major=element_blank(),
                  legend.box = 'none',
                  legend.box.just = 'vertical',
                  legend.position = "none",
                  legend.background = element_rect(fill="white",
                                                   size=0.5, linetype="solid", 
                                                   colour ="white"),
                  plot.title = element_text(size=14, face="bold.italic", 
                                            hjust = 0.5))+
            scale_fill_continuous(name="LAI",
                                  na.value = 'white',
                                  type = "viridis",
                                  limits = c(min.lim,max.lim),
                                  breaks = c(log(0.1), log(0.5), log(2), 
                                             log(5), log(15)),
                                  labels = c(0.1, 0.5, 2, 5, 15))+
            ggtitle("IBS")
        
        
        
        ### TeBE
        p4 <- ggplot() + 
            geom_tile(data=tDF, aes(y=Lat, x=Lon, fill=log(TeBE))) +
            coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
            borders("world", col="grey", lwd=0.2) +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=10),
                  legend.title=element_text(size=12),
                  panel.grid.major=element_blank(),
                  legend.box = 'none',
                  legend.box.just = 'vertical',
                  legend.position = "none",
                  legend.background = element_rect(fill="white",
                                                   size=0.5, linetype="solid", 
                                                   colour ="white"),
                  plot.title = element_text(size=14, face="bold.italic", 
                                            hjust = 0.5))+
            scale_fill_continuous(name="LAI",
                                  na.value = 'white',
                                  type = "viridis",
                                  limits = c(min.lim,max.lim),
                                  breaks = c(log(0.1), log(0.5), log(2), 
                                             log(5), log(15)),
                                  labels = c(0.1, 0.5, 2, 5, 15))+
            ggtitle("TeBE")
        
        
        ### TrBE
        p5 <- ggplot() + 
            geom_tile(data=tDF, aes(y=Lat, x=Lon, fill=log(TrBE))) +
            coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
            borders("world", col="grey", lwd=0.2) +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=10),
                  legend.title=element_text(size=12),
                  panel.grid.major=element_blank(),
                  legend.box = 'none',
                  legend.box.just = 'vertical',
                  legend.position = "none",
                  legend.background = element_rect(fill="white",
                                                   size=0.5, linetype="solid", 
                                                   colour ="white"),
                  plot.title = element_text(size=14, face="bold.italic", 
                                            hjust = 0.5))+
            scale_fill_continuous(name="LAI",
                                  na.value = 'white',
                                  type = "viridis",
                                  limits = c(min.lim,max.lim),
                                  breaks = c(log(0.1), log(0.5), log(2), 
                                             log(5), log(15)),
                                  labels = c(0.1, 0.5, 2, 5, 15))+
            ggtitle("TrBE")
        
        ### TrIBE
        p6 <- ggplot() + 
            geom_tile(data=tDF, aes(y=Lat, x=Lon, fill=log(TrIBE))) +
            coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
            borders("world", col="grey", lwd=0.2) +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=10),
                  legend.title=element_text(size=12),
                  panel.grid.major=element_blank(),
                  legend.box = 'none',
                  legend.box.just = 'vertical',
                  legend.position = "none",
                  legend.background = element_rect(fill="white",
                                                   size=0.5, linetype="solid", 
                                                   colour ="white"),
                  plot.title = element_text(size=14, face="bold.italic", 
                                            hjust = 0.5))+
            scale_fill_continuous(name="LAI",
                                  na.value = 'white',
                                  type = "viridis",
                                  limits = c(min.lim,max.lim),
                                  breaks = c(log(0.1), log(0.5), log(2), 
                                             log(5), log(15)),
                                  labels = c(0.1, 0.5, 2, 5, 15))+
            ggtitle("TrIBE")
        
        ### TrBR
        p7 <- ggplot() + 
            geom_tile(data=tDF, aes(y=Lat, x=Lon, fill=log(TrBR))) +
            coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
            borders("world", col="grey", lwd=0.2) +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=10),
                  legend.title=element_text(size=12),
                  panel.grid.major=element_blank(),
                  legend.box = 'none',
                  legend.box.just = 'vertical',
                  legend.position = "none",
                  legend.background = element_rect(fill="white",
                                                   size=0.5, linetype="solid", 
                                                   colour ="white"),
                  plot.title = element_text(size=14, face="bold.italic", 
                                            hjust = 0.5))+
            scale_fill_continuous(name="LAI",
                                  na.value = 'white',
                                  type = "viridis",
                                  limits = c(min.lim,max.lim),
                                  breaks = c(log(0.1), log(0.5), log(2), 
                                             log(5), log(15)),
                                  labels = c(0.1, 0.5, 2, 5, 15))+
            ggtitle("TrBR")
        
        ### C3G
        p8 <- ggplot() + 
            geom_tile(data=tDF, aes(y=Lat, x=Lon, fill=log(C3G))) +
            coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
            borders("world", col="grey", lwd=0.2) +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=10),
                  legend.title=element_text(size=12),
                  panel.grid.major=element_blank(),
                  legend.box = 'none',
                  legend.box.just = 'vertical',
                  legend.position = "none",
                  legend.background = element_rect(fill="white",
                                                   size=0.5, linetype="solid", 
                                                   colour ="white"),
                  plot.title = element_text(size=14, face="bold.italic", 
                                            hjust = 0.5))+
            scale_fill_continuous(name="LAI",
                                  na.value = 'white',
                                  type = "viridis",
                                  limits = c(min.lim,max.lim),
                                  breaks = c(log(0.1), log(0.5), log(2), 
                                             log(5), log(15)),
                                  labels = c(0.1, 0.5, 2, 5, 15))+
            ggtitle("C3G")
        
        ### C4G
        p9 <- ggplot() + 
            geom_tile(data=tDF, aes(y=Lat, x=Lon, fill=log(C4G))) +
            coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
            borders("world", col="grey", lwd=0.2) +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=10),
                  legend.title=element_text(size=12),
                  panel.grid.major=element_blank(),
                  legend.box = 'none',
                  legend.box.just = 'vertical',
                  legend.position = "none",
                  legend.background = element_rect(fill="white",
                                                   size=0.5, linetype="solid", 
                                                   colour ="white"),
                  plot.title = element_text(size=14, face="bold.italic", 
                                            hjust = 0.5))+
            scale_fill_continuous(name="LAI",
                                  na.value = 'white',
                                  type = "viridis",
                                  limits = c(min.lim,max.lim),
                                  breaks = c(log(0.1), log(0.5), log(2), 
                                             log(5), log(15)),
                                  labels = c(0.1, 0.5, 2, 5, 15))+
            ggtitle("C4G")
        
        ### Total
        p10 <- ggplot() + 
            geom_tile(data=tDF, aes(y=Lat, x=Lon, fill=log(Total))) +
            coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
            borders("world", col="grey", lwd=0.2) +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=10),
                  legend.title=element_text(size=12),
                  panel.grid.major=element_blank(),
                  legend.box = 'none',
                  legend.box.just = 'vertical',
                  legend.position = "none",
                  legend.background = element_rect(fill="white",
                                                   size=0.5, linetype="solid", 
                                                   colour ="white"),
                  plot.title = element_text(size=14, face="bold.italic", 
                                            hjust = 0.5))+
            scale_fill_continuous(name="LAI",
                                  na.value = 'white',
                                  type = "viridis",
                                  limits = c(min.lim,max.lim),
                                  breaks = c(log(0.1), log(0.5), log(2), 
                                             log(5), log(15)),
                                  labels = c(0.1, 0.5, 2, 5, 15))+
            ggtitle("Total")
        
        
        ### combine legend
        combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                                 legend.box = 'vertical',
                                                 legend.box.just = 'left'))
        
        
        ### combine plot
        combined_plot <- plot_grid(p1, p2, p3, p4, p5, 
                                   p6, p7, p8, p9, p10,
                                   ncol=5, align="vh", axis = "l")
        
        ### pdf
        #grDevices::pdf(paste0("output/LAI_", i, ".pdf"), width=16, height=4)
        out_plot <- plot_grid(combined_plot, combined_legend, 
                              ncol=1, rel_heights=c(1, 1))
        #grDevices::dev.off() 
        
        save_plot(paste0("output/basic/LAI_", i, ".pdf"),
                  out_plot, base_width=4)
        
        
    }
    
}
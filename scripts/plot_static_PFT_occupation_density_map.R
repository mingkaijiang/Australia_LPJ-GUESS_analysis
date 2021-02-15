plot_static_PFT_occupation_density_map <- function(myDF) {
    
    ### count number of years for each PFT to occupy a cell 
    sDF <- myDF[,4:16]
    sDF <- ifelse(sDF > 0, 1, 0)
    oDF <- myDF
    oDF[,4:16] <- sDF
    
    
    ### plot TeNE
    p1 <- ggplot() + 
        borders("world", col="grey", lwd=0.2, fill = "white") +
        geom_tile(data=oDF[oDF$Year == 1901,], 
                  aes(y=Lat, x=Lon, fill=as.character(TeNE)),
                  alpha=0.1) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_manual(name="Occupation",
                          na.value = 'white',
                          breaks = c(0, 1),
                          labels = c("absence", "presence"),
                          values = c("white", "blue"))+
        ggtitle("TeNE")
    
    for (i in c(1902:2015)) {
        p1 <- p1 + geom_tile(data=oDF[oDF$Year == i,], 
                             aes(y=Lat, x=Lon, fill=as.character(TeNE)),
                             alpha=0.1)
        
    }
    
    ## pdf
    pdf(paste0("output/basic/PFT_TeNE_occupation_density_map.pdf"))
    plot(p1)
    dev.off()
    
    
    ### plot TeBS
    p2 <- ggplot() + 
        borders("world", col="grey", lwd=0.2, fill = "white") +
        geom_tile(data=oDF[oDF$Year == 1901,], 
                  aes(y=Lat, x=Lon, fill=as.character(TeBS)),
                  alpha=0.1) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_manual(name="Occupation",
                          na.value = 'white',
                          breaks = c(0, 1),
                          labels = c("absence", "presence"),
                          values = c("white", "blue"))+
        ggtitle("TeBS")
    
    for (i in c(1902:2015)) {
        p2 <- p2 + geom_tile(data=oDF[oDF$Year == i,], 
                             aes(y=Lat, x=Lon, fill=as.character(TeBS)),
                             alpha=0.1)
        
    }
    
    ## pdf
    pdf(paste0("output/basic/PFT_TeBS_occupation_density_map.pdf"))
    plot(p2)
    dev.off()
    
    ### plot IBS
    p3 <- ggplot() + 
        borders("world", col="grey", lwd=0.2, fill = "white") +
        geom_tile(data=oDF[oDF$Year == 1901,], 
                  aes(y=Lat, x=Lon, fill=as.character(IBS)),
                  alpha=0.1) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_manual(name="Occupation",
                          na.value = 'white',
                          breaks = c(0, 1),
                          labels = c("absence", "presence"),
                          values = c("white", "blue"))+
        ggtitle("IBS")
    
    for (i in c(1902:2015)) {
        p3 <- p3 + geom_tile(data=oDF[oDF$Year == i,], 
                             aes(y=Lat, x=Lon, fill=as.character(IBS)),
                             alpha=0.1)
        
    }
    
    ## pdf
    pdf(paste0("output/basic/PFT_IBS_occupation_density_map.pdf"))
    plot(p3)
    dev.off()
    
    ### plot TeBE
    p4 <- ggplot() + 
        borders("world", col="grey", lwd=0.2, fill = "white") +
        geom_tile(data=oDF[oDF$Year == 1901,], 
                  aes(y=Lat, x=Lon, fill=as.character(TeBE)),
                  alpha=0.1) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_manual(name="Occupation",
                          na.value = 'white',
                          breaks = c(0, 1),
                          labels = c("absence", "presence"),
                          values = c("white", "blue"))+
        ggtitle("TeBE")
    
    for (i in c(1902:2015)) {
        p4 <- p4 + geom_tile(data=oDF[oDF$Year == i,], 
                             aes(y=Lat, x=Lon, fill=as.character(TeBE)),
                             alpha=0.1)
        
    }
    
    ## pdf
    pdf(paste0("output/basic/PFT_TeBE_occupation_density_map.pdf"))
    plot(p4)
    dev.off()
    
    ### plot TrBE
    p5 <- ggplot() + 
        borders("world", col="grey", lwd=0.2, fill = "white") +
        geom_tile(data=oDF[oDF$Year == 1901,], 
                  aes(y=Lat, x=Lon, fill=as.character(TrBE)),
                  alpha=0.1) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_manual(name="Occupation",
                          na.value = 'white',
                          breaks = c(0, 1),
                          labels = c("absence", "presence"),
                          values = c("white", "blue"))+
        ggtitle("TrBE")
    
    for (i in c(1902:2015)) {
        p5 <- p5 + geom_tile(data=oDF[oDF$Year == i,], 
                             aes(y=Lat, x=Lon, fill=as.character(TrBE)),
                             alpha=0.1)
        
    }
    
    ## pdf
    pdf(paste0("output/basic/PFT_TrBE_occupation_density_map.pdf"))
    plot(p5)
    dev.off()
    
    ### plot TrIBE
    p6 <- ggplot() + 
        borders("world", col="grey", lwd=0.2, fill = "white") +
        geom_tile(data=oDF[oDF$Year == 1901,], 
                  aes(y=Lat, x=Lon, fill=as.character(TrIBE)),
                  alpha=0.1) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_manual(name="Occupation",
                          na.value = 'white',
                          breaks = c(0, 1),
                          labels = c("absence", "presence"),
                          values = c("white", "blue"))+
        ggtitle("TrIBE")
    
    for (i in c(1902:2015)) {
        p6 <- p6 + geom_tile(data=oDF[oDF$Year == i,], 
                             aes(y=Lat, x=Lon, fill=as.character(TrIBE)),
                             alpha=0.1)
        
    }
    
    ## pdf
    pdf(paste0("output/basic/PFT_TrIBE_occupation_density_map.pdf"))
    plot(p6)
    dev.off()
    
    ### plot TrBR
    p7 <- ggplot() + 
        borders("world", col="grey", lwd=0.2, fill = "white") +
        geom_tile(data=oDF[oDF$Year == 1901,], 
                  aes(y=Lat, x=Lon, fill=as.character(TrBR)),
                  alpha=0.1) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_manual(name="Occupation",
                          na.value = 'white',
                          breaks = c(0, 1),
                          labels = c("absence", "presence"),
                          values = c("white", "blue"))+
        ggtitle("TrBR")
    
    for (i in c(1902:2015)) {
        p7 <- p7 + geom_tile(data=oDF[oDF$Year == i,], 
                             aes(y=Lat, x=Lon, fill=as.character(TrBR)),
                             alpha=0.1)
        
    }
    
    ## pdf
    pdf(paste0("output/basic/PFT_TrBR_occupation_density_map.pdf"))
    plot(p7)
    dev.off()
    
    ### plot C3G
    p8 <- ggplot() + 
        borders("world", col="grey", lwd=0.2, fill = "white") +
        geom_tile(data=oDF[oDF$Year == 1901,], 
                  aes(y=Lat, x=Lon, fill=as.character(C3G)),
                  alpha=0.1) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_manual(name="Occupation",
                          na.value = 'white',
                          breaks = c(0, 1),
                          labels = c("absence", "presence"),
                          values = c("white", "blue"))+
        ggtitle("C3G")
    
    for (i in c(1902:2015)) {
        p8 <- p8 + geom_tile(data=oDF[oDF$Year == i,], 
                             aes(y=Lat, x=Lon, fill=as.character(C3G)),
                             alpha=0.1)
        
    }
    
    ## pdf
    pdf(paste0("output/basic/PFT_C3G_occupation_density_map.pdf"))
    plot(p8)
    dev.off()
    
    ### plot C4G
    p9 <- ggplot() + 
        borders("world", col="grey", lwd=0.2, fill = "white") +
        geom_tile(data=oDF[oDF$Year == 1901,], 
                  aes(y=Lat, x=Lon, fill=as.character(C4G)),
                  alpha=0.1) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_manual(name="Occupation",
                          na.value = 'white',
                          breaks = c(0, 1),
                          labels = c("absence", "presence"),
                          values = c("white", "blue"))+
        ggtitle("C4G")
    
    for (i in c(1902:2015)) {
        p9 <- p9 + geom_tile(data=oDF[oDF$Year == i,], 
                             aes(y=Lat, x=Lon, fill=as.character(C4G)),
                             alpha=0.1)
        
    }
    
    ## pdf
    pdf(paste0("output/basic/PFT_C4G_occupation_density_map.pdf"))
    plot(p9)
    dev.off()
    
    
    
}
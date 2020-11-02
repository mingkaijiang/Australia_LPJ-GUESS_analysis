plot_temporal_PFT_shift <- function(myDF) {
    
    ### PFTs in LPJ-GUESS are:
    ### BNE:  Boreal needle-leaved evergreen trees
    ### BINE: Boreal shade-intolerant needle-leaved evergreen tree
    ### BNS:  Boreal needle-leaved summergreen tree
    ### TeNE: Temperate needle-leaved evergreen tree
    ### TeBS: Temperate broadleaved summergreen tree
    ### IBS:  Temperate shade-intolerant broadleaved summergreen tree
    ### TeBE: Temperate broadleaved evergreen tree
    ### TrBE: Tropical broadleaved evergreen tree
    ### TrIBE:Tropical shade-intolerant broadleaved evergreen tree
    ### TrBR: Tropical broadleaved raingreen tree
    ### C3G:  C3 grasses
    ### C4G:  C4 grasses
    ### Definition in Smith et al. (2014). Biogeosciences, 11, 2027–2054.
    
    
    ### Total LAI change over time
    outDF <- data.frame(c(1901:2015), 
                        NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, 
                        NA, NA, NA)
    colnames(outDF) <- c("Year", "BNE", "BINE", "BNS", "TeNE",
                         "TeBS", "IBS", "TeBE", "TrBE", "TrIBE",
                         "TrBR", "C3G", "C4G", "Total")
    
    for (i in c(1901:2015)) {
        tDF <- subset(myDF, Year == i)
        
        tDF$BNE <- ifelse(tDF$BNE > 0, 1, 0)
        tDF$BINE <- ifelse(tDF$BINE > 0, 1, 0)
        tDF$BNS <- ifelse(tDF$BNS > 0, 1, 0)
        tDF$TeNE <- ifelse(tDF$TeNE > 0, 1, 0)
        tDF$TeBS <- ifelse(tDF$TeBS > 0, 1, 0)
        tDF$IBS <- ifelse(tDF$IBS > 0, 1, 0)
        tDF$TeBE <- ifelse(tDF$TeBE > 0, 1, 0)
        tDF$TrBE <- ifelse(tDF$TrBE > 0, 1, 0)
        tDF$TrIBE <- ifelse(tDF$TrIBE > 0, 1, 0)
        tDF$TrBR <- ifelse(tDF$TrBR > 0, 1, 0)
        tDF$C3G <- ifelse(tDF$C3G > 0, 1, 0)
        tDF$C4G <- ifelse(tDF$C4G > 0, 1, 0)
        tDF$Total <- ifelse(tDF$Total > 0, 1, 0)
        
        tmp <- colSums(tDF[,4:16])
        
        outDF[outDF$Year == i, 2:14] <- tmp
    }
    
    ### sum of all grids
    outDF$All <- rowSums(outDF[,2:12])
    
    ### calculate %
    pctDF <- outDF[,1:13]
    pctDF$BNE <- pctDF$BNE/outDF$All
    pctDF$BINE <- pctDF$BINE/outDF$All
    pctDF$BNS <- pctDF$BNS/outDF$All
    pctDF$TeNE <- pctDF$TeNE/outDF$All
    pctDF$TeBS <- pctDF$TeBS/outDF$All
    pctDF$IBS <- pctDF$IBS/outDF$All
    pctDF$TeBE <- pctDF$TeBE/outDF$All
    pctDF$TrBE <- pctDF$TrBE/outDF$All
    pctDF$TrIBE <- pctDF$TrIBE/outDF$All
    pctDF$TrBR <- pctDF$TrBR/outDF$All
    pctDF$C3G <- pctDF$C3G/outDF$All
    pctDF$C4G <- pctDF$C4G/outDF$All
    
    outDF <- melt(pctDF, id.vars=c("Year"),
                  measure.vars=c("BNE", "BINE", "BNS", "TeNE",
                                 "TeBS", "IBS", "TeBE", "TrBE", 
                                 "TrIBE", "TrBR", "C3G", "C4G"),
                  variable.name="PFT",
                  value.name="pct")
    
    ### plot PFT by year
    col.lab <- brewer.pal(12, name="Paired")
    
    p1 <- ggplot(outDF) +
        geom_bar(aes(x = Year, y = pct, fill = PFT), stat="identity", width = 0.7)+
        coord_flip()+
        scale_fill_manual(values = col.lab)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylim(c(0,1.15))
    
    ### save pdf
    pdf("output/PFT_by_year.pdf", width = 6, height = 8)
    plot(p1)
    dev.off()
    
    
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
    for (i in c(1901:2015)) {
        
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
        combined_plot <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
                                   ncol=10, align="vh", axis = "l")
        
        ### pdf
        pdf(paste0("output/LAI_", i, ".pdf"), width=16, height=4)
        plot_grid(combined_plot, combined_legend, 
                  ncol=1, rel_heights=c(1, 1))
        dev.off() 
        
        
    }
    
    
    
    
}

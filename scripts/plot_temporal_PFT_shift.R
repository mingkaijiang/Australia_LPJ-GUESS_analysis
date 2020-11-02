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
        
        save_plot(paste0("output/LAI_", i, ".pdf"),
                  out_plot, base_width=4)
        
        
    }
    
    
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
    pdf(paste0("output/PFT_TeNE_occupation_density_map.pdf"))
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
    pdf(paste0("output/PFT_TeBS_occupation_density_map.pdf"))
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
    pdf(paste0("output/PFT_IBS_occupation_density_map.pdf"))
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
    pdf(paste0("output/PFT_TeBE_occupation_density_map.pdf"))
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
    pdf(paste0("output/PFT_TrBE_occupation_density_map.pdf"))
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
    pdf(paste0("output/PFT_TrIBE_occupation_density_map.pdf"))
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
    pdf(paste0("output/PFT_TrBR_occupation_density_map.pdf"))
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
    pdf(paste0("output/PFT_C3G_occupation_density_map.pdf"))
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
    pdf(paste0("output/PFT_C4G_occupation_density_map.pdf"))
    plot(p9)
    dev.off()
    
    
    
    
    #
}

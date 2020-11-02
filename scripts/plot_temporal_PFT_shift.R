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
    
    
    pdf("output/PFT_by_year.pdf")
    plot(p1)
    dev.off()
    
    ### Investigate C3 and C4 grasses
    
    
    
    
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
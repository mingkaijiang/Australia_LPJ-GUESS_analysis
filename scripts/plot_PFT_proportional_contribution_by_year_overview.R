plot_PFT_proportional_contribution_by_year_overview <- function(myDF) {
    
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
    outDF$All <- rowSums(outDF[,2:13])
    
    ### calculate %
    pctDF <- outDF[,1:13]
    pctDF$BNE <- outDF$BNE/outDF$All
    pctDF$BINE <- outDF$BINE/outDF$All
    pctDF$BNS <- outDF$BNS/outDF$All
    pctDF$TeNE <- outDF$TeNE/outDF$All
    pctDF$TeBS <- outDF$TeBS/outDF$All
    pctDF$IBS <- outDF$IBS/outDF$All
    pctDF$TeBE <- outDF$TeBE/outDF$All
    pctDF$TrBE <- outDF$TrBE/outDF$All
    pctDF$TrIBE <- outDF$TrIBE/outDF$All
    pctDF$TrBR <- outDF$TrBR/outDF$All
    pctDF$C3G <- outDF$C3G/outDF$All
    pctDF$C4G <- outDF$C4G/outDF$All
    

    outDF2 <- reshape2::melt(pctDF, id.vars=c("Year"),
                             measure.vars=c("BNE", "BINE", "BNS", "TeNE",
                                            "TeBS", "IBS", "TeBE", "TrBE", 
                                            "TrIBE", "TrBR", "C3G", "C4G"),
                             variable.name="PFT",
                             value.name="pct")
    
    
    ### plot PFT by year
    col.lab <- brewer.pal(12, name="Paired")
    
    p1 <- ggplot(outDF2) +
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
        ylim(c(0,1.0))
    
    ### save pdf
    pdf("output/basic/PFT_by_year.pdf", width = 6, height = 8)
    plot(p1)
    dev.off()
    
    
    
 
}

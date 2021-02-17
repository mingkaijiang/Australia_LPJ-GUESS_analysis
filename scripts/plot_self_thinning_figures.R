plot_self_thinning_figures <- function(myDF, patcharea) {
    
    ### patcharea in unit of m2
    
    ### only look at trees
    subDF <- myDF[myDF$PFT%in%c(0,1,2,3,4,5,6,7,8,9),]
 
    ### calculate sum of wood
    subDF$StemC <- subDF$SapC + subDF$HeartC
    
    ### calculate proxy for diameter - PD
    subDF$PD <- 1.0 / subDF$Idens
    
    ### sum biomass and density within each patch
    sumDF <- summaryBy(StemC+Idens+PD~ID+Lon+Lat+Year+Patch+Page+PFT, 
                       FUN=sum, data=subDF, 
                       keep.names=T, na.rm=T)
    
    ### color labels
    col.list <- brewer.pal(12, name="Paired")
    
    ### prepare arrow plot - check 10-yr difference
    subDF1 <- subset(subDF, Year == 2005)
    subDF2 <- subset(subDF, Year == 2015)
    mgDF <- merge(subDF1, subDF2, by = c("Lon", "Lat", "Patch", "PFT", "Indiv"), all=T)
    
    ### 
    p1 <- ggplot() +
        geom_point(subDF, mapping = aes(PD, StemC, col=Iage))+
        geom_segment(mgDF, mapping = aes(x = PD.x, y = StemC.x, xend = PD.y, yend = StemC.y, col=Iage.y),
                     arrow = arrow(length = unit(0.3, "cm")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        xlab("Proxy for tree diameter (1 / density)")+
        ylab("Stem C")
    
    plot(p1)
    
    
    ### save pdf
    pdf("output/self-thinning/diameter_vs_stemC_all_trees.pdf", width = 6, height = 4)
    plot(p1)
    dev.off()
    
    
    
    
}

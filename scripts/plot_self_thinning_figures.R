plot_self_thinning_figures <- function(myDF, patcharea) {
    
    ### patcharea in unit of m2
    
    ### only look at trees
    subDF <- myDF[myDF$PFT%in%c(0,1,2,3,4,5,6,7,8,9),]
 
    ### calculate sum of wood
    subDF$StemC <- subDF$SapC + subDF$HeartC
    
    ### sum biomass and density within each patch
    sumDF <- summaryBy(StemC+Idens~ID+Lon+Lat+Year+Patch+Page+PFT, 
                       FUN=sum, data=subDF, 
                       keep.names=T, na.rm=T)
    
    summary(sumDF$Idens)
    
    ### color labels
    col.list <- brewer.pal(12, name="Paired")
    
    ### plot each PFT
    plotDF1 <- sumDF[sumDF$PFT == 6, ]
    
    ### 
    p1 <- ggplot(sumDF) +
        geom_point(aes(Idens, StemC))+
        #scale_color_manual(name="PFT", 
        #                   values=col.list,
        #                   labels=c("0"="BNE","1"="BINE","2"="BNS", 
        #                            "3"="TeNE","4"="TeBS","5"="IBS",
        #                            "6"="TeBE","7"="TrBE","8"="TrIBE", 
        #                            "9"="TrBR"))+
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
        xlab("Patch stem density (indiv/patch)")+
        ylab("Stem C")
    
    plot(p1)
    
    
    ### read in density dataset
    densDF <- read.table("input/run1/dens.out", header=T)
    
    ### save pdf
    pdf("output/self-thinning/trees_age_comparison.pdf", width = 6, height = 4)
    plot(p1)
    dev.off()
    
    
    
    
}
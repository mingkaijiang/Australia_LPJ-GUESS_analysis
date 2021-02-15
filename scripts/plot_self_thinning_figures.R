plot_self_thinning_figures <- function(myDF) {
    
    ### check on TeBE
    subDF <- myDF[myDF$PFT=="9",]
    
    ### check patch age and individual age relationship
    p1 <- ggplot(subDF) +
        geom_point(aes(Page, Iage, fill=Year, col=Year))+
        scale_fill_continuous(type = "viridis") +
        scale_color_continuous(type = "viridis") +
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
        xlab("Patch Age")+
        ylab("Individual Age")
    
    ### save pdf
    pdf("output/self-thinning/TeBE_age_comparison.pdf", width = 6, height = 4)
    plot(p1)
    dev.off()
    
    
    ### check on all trees
    subDF <- myDF[myDF$PFT%in%c(0,1,2,3,4,5,6,7,8,9),]
    
    ### check patch age and individual age relationship
    p1 <- ggplot(subDF) +
        geom_point(aes(Page, Iage, fill=Year, col=Year))+
        scale_fill_continuous(type = "viridis") +
        scale_color_continuous(type = "viridis") +
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
        xlab("Patch Age")+
        ylab("Individual Age")
    
    ### save pdf
    pdf("output/self-thinning/trees_age_comparison.pdf", width = 6, height = 4)
    plot(p1)
    dev.off()
    
    
    ### calculate sum of wood
    subDF$StemC <- subDF$SapC + subDF$HeartC
    
    ### color labels
    col.list <- brewer.pal(12, name="Paired")
    
    ### 
    p1 <- ggplot(subDF) +
        geom_point(aes(Iage, StemC, col=as.factor(PFT)))+
        scale_color_manual(name="PFT", 
                           values=col.list,
                           labels=c("0"="BNE","1"="BINE","2"="BNS", 
                                    "3"="TeNE","4"="TeBS","5"="IBS",
                                    "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                    "9"="TrBR"))+
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
        xlab("Individual Age")+
        ylab("Stem C")
    
    ### save pdf
    pdf("output/self-thinning/trees_age_comparison.pdf", width = 6, height = 4)
    plot(p1)
    dev.off()
    
}
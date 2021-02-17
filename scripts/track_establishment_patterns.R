track_establishment_patterns <- function(myDF) {
    
    ### track establishment of each individual
    
    myDF$StemC <- myDF$SapC + myDF$HeartC

    ### check on TeBE
    subDF <- myDF[myDF$PFT=="9",]
    
    ### check patch age and individual age relationship
    p1 <- ggplot(subDF) +
        geom_point(aes(Iage, LAI_indiv), col=alpha("brown", alpha=0.3))+
        geom_smooth(aes(Iage, LAI_indiv),col="black",
                    span = 0.3)+
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
        ylab("Individual LAI")
    
    
    p2 <- ggplot(subDF) +
        geom_point(aes(Iage, FPC), col=alpha("blue", alpha=0.3))+
        geom_smooth(aes(Iage, FPC),col="black",
                    span = 0.3)+
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
        ylab("FPC")+
        ylim(0,1)
    
    
    p3 <- ggplot(subDF) +
        geom_point(aes(Iage, StemC), col=alpha("orange", alpha=0.3))+
        geom_smooth(aes(Iage, StemC),col="black",
                    span = 0.3)+
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
        ylab("StemC")
    
    p4 <- ggplot(subDF) +
        geom_point(aes(Iht, Icrarea), col=alpha("orange", alpha=0.3))+
        #geom_smooth(aes(Iht, Icrarea),col="black",
        #            span = 0.3)+
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
        xlab("Individual Height")+
        ylab("Individual crown area")
    
    p5 <- ggplot(subDF) +
        geom_point(aes(Idens, LAI_indiv), col=alpha("orange", alpha=0.3))+
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
        xlab("Individual density  (indiv/m2)")+
        ylab("Individual LAI")
    
    ### plot
    combined_plot <- plot_grid(p1, p2, p3, p4, p5, 
                               ncol=3, align="vh", axis = "l")
    
    save_plot(paste0("output/establishment/basic_TeBE_summary.pdf"),
              combined_plot, base_width=10, base_height = 8)
    
    
    ### check on all
    subDF <- myDF[myDF$PFT%in%c(0,1,2,3,4,5,6,7,8,9)&myDF$Iage==0,]
    
    subDF$PFTC <- as.character(subDF$PFT)
    
    sumDF <- summaryBy(LAI_indiv+FPC+StemC~PFT, data=subDF, FUN=c(mean,se),
                       keep.names=T)
    
    ### plot bar chart of establishment year
    p1 <- ggplot(subDF, aes(PFT, LAI_indiv, fill=PFTC)) +
        #geom_point()+
        geom_boxplot(notch=T)+
        #geom_jitter(width = 0.2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        xlab("PFT")+
        ylab("Individual LAI at establishment")+
        scale_x_discrete(limits=c("0"="BNE","1"="BINE","2"="BNS", 
                                  "3"="TeNE","4"="TeBS","5"="IBS",
                                  "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                  "9"="TrBR"))
    
    
    p2 <- ggplot(subDF, aes(PFT, FPC, fill=PFTC)) +
        #geom_point()+
        geom_boxplot(notch=T)+
        #geom_jitter(width = 0.2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        xlab("PFT")+
        ylab("Individual FPC at establishment")+
        scale_x_discrete(limits=c("0"="BNE","1"="BINE","2"="BNS", 
                                  "3"="TeNE","4"="TeBS","5"="IBS",
                                  "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                  "9"="TrBR"))+
        ylim(c(0,1))
    
    
    p3 <- ggplot(subDF, aes(PFT, StemC, fill=PFTC)) +
        #geom_point()+
        geom_boxplot(notch=T)+
        #geom_jitter(width = 0.2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        xlab("PFT")+
        ylab("Individual Stem C at establishment")+
        scale_x_discrete(limits=c("0"="BNE","1"="BINE","2"="BNS", 
                                  "3"="TeNE","4"="TeBS","5"="IBS",
                                  "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                  "9"="TrBR"))+
        ylim(c(0,1))
    
    
    p4 <- ggplot(subDF, aes(PFT, Iht, fill=PFTC)) +
        #geom_point()+
        geom_boxplot(notch=T)+
        #geom_jitter(width = 0.2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        xlab("PFT")+
        ylab("Individual height")+
        scale_x_discrete(limits=c("0"="BNE","1"="BINE","2"="BNS", 
                                  "3"="TeNE","4"="TeBS","5"="IBS",
                                  "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                  "9"="TrBR"))
    
    
    p5 <- ggplot(subDF, aes(PFT, Icrarea, fill=PFTC)) +
        #geom_point()+
        geom_boxplot(notch=T)+
        #geom_jitter(width = 0.2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        xlab("PFT")+
        ylab("Individual crown area")+
        scale_x_discrete(limits=c("0"="BNE","1"="BINE","2"="BNS", 
                                  "3"="TeNE","4"="TeBS","5"="IBS",
                                  "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                  "9"="TrBR"))
    
    ### plot
    combined_plot <- plot_grid(p1, p2, p3, p4, p5,
                               ncol=2, align="vh", axis = "l")
    
    save_plot(paste0("output/establishment/establishment_summary.pdf"),
              combined_plot, base_width=10, base_height = 8)
    
    
    ### comment:
    ### it seems that establishment is high in year 0,
    ### reflected via high LAI and FPC.
    ### So there isn't necessarily a gap in the patch for future growth;
    ### Next, need to check stem density, FPC and LAI relationship within each patch. 
    
    
    
}
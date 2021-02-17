track_mortality_events <- function(myDF, delete_mortality_file) {
    
    ### track mortality patch/individual
    ### if the patch/individual no longer present in the next year,
    ### then it is dead in the current year
    ### Generate a table summarizing the mortality events
    ### and corresponds to climate extremes
    
    if (delete_mortality_file == T & file.exists("output/mortality/individual_mortality_table.csv")) {
        file.remove("output/mortality/individual_mortality_table.csv")
    }
    
    ### sum all stem C
    myDF$StemC <- myDF$SapC + myDF$HeartC
    
    ## get final year in the dataset
    final.year <- max(myDF$Year)
    
    ## get first year in the dataset
    first.year <- min(myDF$Year)
    
    if (!file.exists("output/mortality/individual_mortality_table.csv")) {
        
        ### We now have a dataset in the year before individual plant disappears;
        ### this should be considered as mortality dataset.
        ### We can look at the year information first to see if there is any pattern.
        prepare_mortality_dataset(myDF) 
        print("preparing mortality dataset ... ... ")
    }
    
    ### read input
    outDF <- read.csv("output/mortality/individual_mortality_table.csv")
    
    ### prepare plot settings
    ### color labels
    col.list <- brewer.pal(12, name="Paired")
    
    ### remove grasses
    plotDF <- outDF[outDF$PFT%in%c(0,1,2,3,4,5,6,7,8,9),]
    
    ### add character of PFT
    plotDF$PFTC <- as.character(plotDF$PFT)
    
    ### plotting - year prior to mortality
    p1 <- ggplot(plotDF, aes(x=Year, color=as.factor(PFT))) +
        geom_density()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        scale_color_manual(name="PFT", 
                           values=col.list,
                           labels=c("0"="BNE","1"="BINE","2"="BNS", 
                                    "3"="TeNE","4"="TeBS","5"="IBS",
                                    "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                    "9"="TrBR","10"="C3G","11"="C4G"))+
        xlab("Year prior to mortality")+
        scale_x_continuous(breaks = seq(first.year,final.year, 1))
    
    ### plotting - FPC vs. NPP relationship
    p2 <- ggplot(plotDF, aes(x=FPC, y = NPP, color=as.factor(PFT))) +
        geom_point()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        scale_color_manual(name="PFT", 
                           values=col.list,
                           labels=c("0"="BNE","1"="BINE","2"="BNS", 
                                    "3"="TeNE","4"="TeBS","5"="IBS",
                                    "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                    "9"="TrBR","10"="C3G","11"="C4G"))
    
    ### plotting - FPC vs. individual LAI relationship
    p3 <- ggplot(plotDF, aes(x=FPC, y = LAI_indiv, color=as.factor(PFT))) +
        geom_point()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        scale_color_manual(name="PFT", 
                           values=col.list,
                           labels=c("0"="BNE","1"="BINE","2"="BNS", 
                                    "3"="TeNE","4"="TeBS","5"="IBS",
                                    "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                    "9"="TrBR","10"="C3G","11"="C4G"))+
        ylab("Individual LAI") +
        ylim(c(0,20))
    
    
    ### plotting - Page vs. Iage
    p4 <- ggplot(plotDF, aes(x=Page, y = Iage, color=as.factor(PFT))) +
        geom_point()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        scale_color_manual(name="PFT", 
                           values=col.list,
                           labels=c("0"="BNE","1"="BINE","2"="BNS", 
                                    "3"="TeNE","4"="TeBS","5"="IBS",
                                    "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                    "9"="TrBR","10"="C3G","11"="C4G"))
    
    
    ### plotting - bar chart of FPC and Iage
    p5 <- ggplot(plotDF, aes(PFT, FPC, fill=PFTC)) +
        geom_boxplot(notch=T)+
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
        ylab("FPC")+
        scale_x_discrete(limits=c("0"="BNE","1"="BINE","2"="BNS", 
                                  "3"="TeNE","4"="TeBS","5"="IBS",
                                  "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                  "9"="TrBR"))
    
    
    p6 <- ggplot(plotDF, aes(PFT, Iage, fill=PFTC)) +
        geom_boxplot(notch=T)+
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
        ylab("Individual age")+
        scale_x_discrete(limits=c("0"="BNE","1"="BINE","2"="BNS", 
                                  "3"="TeNE","4"="TeBS","5"="IBS",
                                  "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                  "9"="TrBR"))
    
    
    p7 <- ggplot(plotDF, aes(PFT, Iht, fill=PFTC)) +
        geom_boxplot(notch=T)+
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
    
    
    p8 <- ggplot(plotDF, aes(x=Iht, y = StemC, color=as.factor(PFT))) +
        geom_point()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        scale_color_manual(name="PFT", 
                           values=col.list,
                           labels=c("0"="BNE","1"="BINE","2"="BNS", 
                                    "3"="TeNE","4"="TeBS","5"="IBS",
                                    "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                    "9"="TrBR","10"="C3G","11"="C4G"))+
        xlab("Individual height") +
        ylab("Individual StemC")
        
    plot(p8)
    
    ### plot
    combined_plot <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8,
                               ncol=2, align="vh", axis = "l")
    
    save_plot(paste0("output/mortality/mortality_summary.pdf"),
              combined_plot, base_width=10, base_height = 16)
    
    
    
}

track_mortality_events <- function(myDF) {
    
    ### track mortality patch/individual
    ### if the patch/individual no longer present in the next year,
    ### then it is dead in the current year
    ### Generate a table summarizing the mortality events
    ### and corresponds to climate extremes
    
    ### sum all stem C
    myDF$StemC <- myDF$SapC + myDF$HeartC
    
    ## get final year in the dataset
    final.year <- max(myDF$Year)
    
    ## get first year in the dataset
    first.year <- min(myDF$Year)
    
    ## get length of the years
    d1 <- length(c(first.year:final.year))
    
    ### assign ID to each grid
    lon <- unique(myDF$Lon)
    lat <- unique(myDF$Lat)
    
    lonlatDF <- data.frame("Lon" = rep(lon, each=length(lat)), 
                           "Lat" = rep(lat, length(lon)),
                           "ID" = c(1:(length(lon) * length(lat))))
    
    myDF <- merge(myDF, lonlatDF, by=c("Lon", "Lat"), all.x=T)
    
    ### get grid ID
    gridID <- unique(myDF$ID)
    patchID <- c(0:99)
    
    ### prepare outDF to store the output
    outDF <- c()
    
    ### loop through grid
    for (i in gridID) {
        subDF1 <- subset(myDF, ID == i)
        
        ## loop through patch
        for (j in patchID) {
            subDF2 <- subset(subDF1, Patch == j)
            
            pftID <- unique(subDF2$PFT)
            
            ## loop through PFT
            for (k in pftID) {
                subDF3 <- subset(subDF2, PFT == k)
                
                indivID <- unique(subDF3$Indiv)
                
                for (l in indivID) {
                    subDF4 <- subset(subDF3, Indiv == l)
                    
                    ## get maximum year
                    mx.year <- max(subDF4$Year)
                    
                    ## get dimension of the dataset
                    d2 <- dim(subDF4)[1]
                    
                    ### extract year of mortality
                    if (mx.year < final.year & d2 < d1) {
                        ## extract the year before the individual disappear
                        outDF2 <- subDF4[d2,]
                        
                        ### row merge
                        outDF <- rbind(outDF, outDF2)
                    } # end if
                } # end l
            } # end k
        } # end j
    } # end i
    
    
    ### We now have a dataset in the year before individual plant disappears;
    ### this should be considered as mortality dataset.
    ### We can look at the year information first to see if there is any pattern.
    
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
                                    "9"="TrBR","10"="C3G","11"="C4G"))
    
    
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
        
    
    ### plot
    combined_plot <- plot_grid(p1, p2, p3, p4, p5, p6, 
                               ncol=2, align="vh", axis = "l")
    
    save_plot(paste0("output/mortality/mortality_summary.pdf"),
              combined_plot, base_width=10, base_height = 12)
    
    
    
}
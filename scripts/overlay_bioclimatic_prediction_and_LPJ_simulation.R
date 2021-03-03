overlay_bioclimatic_prediction_and_LPJ_simulation <- function() {
    
    ### read bioclimatic prediction
    bioclimDF <- readRDS("output/climate/bioclimatic_predicted_vegetation_distribution.rds")
    
    ### read in LPJ file, without fire to rule out effect of fire on vegetation distribution
    myDF2 <- read.table(paste0("input/withoutfire/run1/dens.out"), header=T)
    for (i in 2:20) {
        tmpDF <- read.table(paste0("input/withoutfire/run", i, "/dens.out"), header=T)
        myDF2 <- rbind(myDF2, tmpDF)
    }
    
    ### prepare plot settings
    lonlim <- range(myDF2$Lon)
    latlim <- range(myDF2$Lat)

    
    ### merge
    plotDF1 <- merge(myDF2, bioclimDF, by=c("Lon", "Lat", "Year"), 
                     keep.x=T)
    
    ### rename
    colnames(plotDF1) <- c("Lon", "Lat", "Year", "BNEnofire", "BINEnofire",
                           "BNSnofire", "TeNEnofire", "TeBSnofire", "IBSnofire",
                           "TeBEnofire", "TrBEnofire", "TrIBEnofire", "TrBRnofire",
                           "C3Gnofire", "C4Gnofire", "Total", 
                           "tcmin_est", "tcmax_est", "twmin_est", "gdd5min_est",
                           "BNEbio", "BINEbio", "BNSbio", "TeNEbio", "TeBSbio",
                           "IBSbio", "TeBEbio", "C3Gbio", "C4Gbio")
    
    plotDF1$Total <- NULL
    
    
    ### convert to 0 and 1s
    plotDF1$BNEnofire <- ifelse(plotDF1$BNEnofire > 0, 1, 0)
    plotDF1$BINEnofire <- ifelse(plotDF1$BINEnofire > 0, 1, 0)
    plotDF1$BNSnofire <- ifelse(plotDF1$BNSnofire > 0, 1, 0)
    plotDF1$TeNEnofire <- ifelse(plotDF1$TeNEnofire > 0, 1, 0)
    plotDF1$TeBSnofire <- ifelse(plotDF1$TeBSnofire > 0, 1, 0)
    plotDF1$IBSnofire <- ifelse(plotDF1$IBSnofire > 0, 1, 0)
    plotDF1$TeBEnofire <- ifelse(plotDF1$TeBEnofire > 0, 1, 0)
    plotDF1$TrBEnofire <- ifelse(plotDF1$TrBEnofire > 0, 1, 0)
    plotDF1$TrIBEnofire <- ifelse(plotDF1$TrIBEnofire > 0, 1, 0)
    plotDF1$C3Gnofire <- ifelse(plotDF1$C3Gnofire > 0, 1, 0)
    plotDF1$C4Gnofire <- ifelse(plotDF1$C4Gnofire > 0, 1, 0)
    
    plotDF1$BNEbio <- ifelse(plotDF1$BNEbio > 0, 1, 0)
    plotDF1$BINEbio <- ifelse(plotDF1$BINEbio > 0, 1, 0)
    plotDF1$BNSbio <- ifelse(plotDF1$BNSbio > 0, 1, 0)
    plotDF1$TeNEbio <- ifelse(plotDF1$TeNEbio > 0, 1, 0)
    plotDF1$TeBSbio <- ifelse(plotDF1$TeBSbio > 0, 1, 0)
    plotDF1$IBSbio <- ifelse(plotDF1$IBSbio > 0, 1, 0)
    plotDF1$TeBEbio <- ifelse(plotDF1$TeBEbio > 0, 1, 0)
    plotDF1$C3Gbio <- ifelse(plotDF1$C3Gbio > 0, 1, 0)
    plotDF1$C4Gbio <- ifelse(plotDF1$C4Gbio > 0, 1, 0)
    
    plotDF1$BNEVegDymEffect <- with(plotDF1, (BNEbio - BNEnofire))
    plotDF1$BINEVegDymEffect <- with(plotDF1, (BINEbio - BINEnofire))
    plotDF1$BNSVegDymEffect <- with(plotDF1, (BNSbio - BNSnofire))
    plotDF1$TeNEVegDymEffect <- with(plotDF1, (TeNEbio - TeNEnofire))
    plotDF1$TeBSVegDymEffect <- with(plotDF1, (TeBSbio - TeBSnofire))
    plotDF1$IBSVegDymEffect <- with(plotDF1, (IBSbio - IBSnofire))
    plotDF1$TeBEVegDymEffect <- with(plotDF1, (TeBEbio - TeBEnofire))
    plotDF1$C3GVegDymEffect <- with(plotDF1, (C3Gbio - C3Gnofire))
    plotDF1$C4GVegDymEffect <- with(plotDF1, (C4Gbio - C4Gnofire))
    
    ## ignore bioclimatic none presence
    plotDF1$BNEVegDymEffect <- ifelse(plotDF1$BNEbio == 0 & plotDF1$BNEnofire == 0, "2", plotDF1$BNEVegDymEffect)
    plotDF1$BINEVegDymEffect <- ifelse(plotDF1$BINEbio == 0 & plotDF1$BINEnofire == 0, "2", plotDF1$BINEVegDymEffect)
    plotDF1$BNSVegDymEffect <- ifelse(plotDF1$BNSbio == 0 & plotDF1$BNSnofire == 0, "2", plotDF1$BNSVegDymEffect)
    plotDF1$TeNEVegDymEffect <- ifelse(plotDF1$TeNEbio == 0 & plotDF1$TeNEnofire == 0, "2", plotDF1$TeNEVegDymEffect)
    plotDF1$TeBSVegDymEffect <- ifelse(plotDF1$TeBSbio == 0 & plotDF1$TeBSnofire == 0, "2", plotDF1$TeBSVegDymEffect)
    plotDF1$IBSVegDymEffect <- ifelse(plotDF1$IBSbio == 0 & plotDF1$IBSnofire == 0, "2", plotDF1$IBSVegDymEffect)
    plotDF1$TeBEVegDymEffect <- ifelse(plotDF1$TeBEbio == 0 & plotDF1$TeBEnofire == 0, "2", plotDF1$TeBEVegDymEffect)
    plotDF1$C3GVegDymEffect <- ifelse(plotDF1$C3Gbio == 0 & plotDF1$C3Gnofire == 0, "2", plotDF1$C3GVegDymEffect)
    plotDF1$C4GVegDymEffect <- ifelse(plotDF1$C4Gbio == 0 & plotDF1$C4Gnofire == 0, "2", plotDF1$C4GVegDymEffect)
    
    
    ### as character
    plotDF1$BNEVegDymEffect <- as.character(plotDF1$BNEVegDymEffect)
    plotDF1$BINEVegDymEffect <- as.character(plotDF1$BINEVegDymEffect)
    plotDF1$BNSVegDymEffect <- as.character(plotDF1$BNSVegDymEffect)
    plotDF1$TeNEVegDymEffect <- as.character(plotDF1$TeNEVegDymEffect)
    plotDF1$TeBSVegDymEffect <- as.character(plotDF1$TeBSVegDymEffect)
    plotDF1$IBSVegDymEffect <- as.character(plotDF1$IBSVegDymEffect)
    plotDF1$TeBEVegDymEffect <- as.character(plotDF1$TeBEVegDymEffect)
    plotDF1$C3GVegDymEffect <- as.character(plotDF1$C3GVegDymEffect)
    plotDF1$C4GVegDymEffect <- as.character(plotDF1$C4GVegDymEffect)
    
    
        
    ### plot TeNE
    p1 <- ggplot() + 
        geom_tile(data=plotDF1, aes(y=Lat, x=Lon, fill=TeNEVegDymEffect)) +
        coord_quickmap(xlim=lonlim, ylim=latlim)+
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
              legend.position = "right",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="white"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        scale_fill_manual(name="TeNE Vegetation Dynamic Effect",
                          breaks = c("-1", "0", "1", "2"),
                          labels = c("Outside bioclimatic limit", 
                                     "Within bioclimatic limit, present without fire", 
                                     "Within bioclimatic limit, but disappeared due to other constraints",
                                     "Bioclimatic non-existence"),
                          values = c("green", "yellow", "red", "grey"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_vegetation_effect_on_TeNE_distribution.gif"), animation=last_animation(), path="output/climate/")
    
    
    
    ### plot TeBS
    p1 <- ggplot() + 
        geom_tile(data=plotDF1, aes(y=Lat, x=Lon, fill=TeBSVegDymEffect)) +
        coord_quickmap(xlim=lonlim, ylim=latlim)+
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
              legend.position = "right",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="white"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        scale_fill_manual(name="TeBS Vegetation Dynamic Effect",
                          breaks = c("-1", "0", "1", "2"),
                          labels = c("Outside bioclimatic limit", 
                                     "Within bioclimatic limit, present without fire", 
                                     "Within bioclimatic limit, but disappeared due to other constraints",
                                     "Bioclimatic non-existence"),
                          values = c("green", "yellow", "red", "grey"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_vegetation_effect_on_TeBS_distribution.gif"), animation=last_animation(), path="output/climate/")
    
    
    
    ### plot IBS
    p1 <- ggplot() + 
        geom_tile(data=plotDF1, aes(y=Lat, x=Lon, fill=IBSVegDymEffect)) +
        coord_quickmap(xlim=lonlim, ylim=latlim)+
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
              legend.position = "right",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="white"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        scale_fill_manual(name="IBS Vegetation Dynamic Effect",
                          breaks = c("-1", "0", "1", "2"),
                          labels = c("Outside bioclimatic limit", 
                                     "Within bioclimatic limit, present without fire", 
                                     "Within bioclimatic limit, but disappeared due to other constraints",
                                     "Bioclimatic non-existence"),
                          values = c("green", "yellow", "red", "grey"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_vegetation_effect_on_IBS_distribution.gif"), animation=last_animation(), path="output/climate/")
    
    
    ### plot TeBE
    p1 <- ggplot() + 
        geom_tile(data=plotDF1, aes(y=Lat, x=Lon, fill=TeBEVegDymEffect)) +
        coord_quickmap(xlim=lonlim, ylim=latlim)+
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
              legend.position = "right",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="white"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        scale_fill_manual(name="TeBE Vegetation Dynamic Effect",
                          breaks = c("-1", "0", "1", "2"),
                          labels = c("Outside bioclimatic limit", 
                                     "Within bioclimatic limit, present without fire", 
                                     "Within bioclimatic limit, but disappeared due to other constraints",
                                     "Bioclimatic non-existence"),
                          values = c("green", "yellow", "red", "grey"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_vegetation_effect_on_TeBE_distribution.gif"), animation=last_animation(), path="output/climate/")
    
    
    ### plot C3G
    p1 <- ggplot() + 
        geom_tile(data=plotDF1, aes(y=Lat, x=Lon, fill=C3GVegDymEffect)) +
        coord_quickmap(xlim=lonlim, ylim=latlim)+
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
              legend.position = "right",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="white"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        scale_fill_manual(name="C3G Vegetation Dynamic Effect",
                          breaks = c("-1", "0", "1", "2"),
                          labels = c("Outside bioclimatic limit", 
                                     "Within bioclimatic limit, present without fire", 
                                     "Within bioclimatic limit, but disappeared due to other constraints",
                                     "Bioclimatic non-existence"),
                          values = c("green", "yellow", "red", "grey"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_vegetation_effect_on_C3G_distribution.gif"), animation=last_animation(), path="output/climate/")
    
    
    ### plot C4G
    p1 <- ggplot() + 
        geom_tile(data=plotDF1, aes(y=Lat, x=Lon, fill=C4GVegDymEffect)) +
        coord_quickmap(xlim=lonlim, ylim=latlim)+
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
              legend.position = "right",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="white"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        scale_fill_manual(name="C4G Vegetation Dynamic Effect",
                          breaks = c("-1", "0", "1", "2"),
                          labels = c("Outside bioclimatic limit", 
                                     "Within bioclimatic limit, present without fire", 
                                     "Within bioclimatic limit, but disappeared due to other constraints",
                                     "Bioclimatic non-existence"),
                          values = c("green", "yellow", "red", "grey"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_vegetation_effect_on_C4G_distribution.gif"), animation=last_animation(), path="output/climate/")
    
    
    ### end.
}
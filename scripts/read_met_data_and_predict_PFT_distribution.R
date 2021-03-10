read_met_data_and_predict_PFT_distribution <- function() {
    
    ### read in met forcing data
    #sourceDir <- "/Users/mingkaijiang/Documents/Research/Projects/Australia_DGVM_MDI/Models/LPJ-Guess/forcing_data/cruncepv7cruP_2/"
    #sourceFile <- file(paste0(sourceDir, "cruncep_1901_2015.bin"), "rb")
    #metDF <- readBin(sourceFile, integer(), endian = "little")
    
    sourceDir <- "/Users/mingkaijiang/Documents/Research/Projects/Trendy/Trendy_Australia_Analysis/output/climate/"
    tmpDF <- readRDS(paste0(sourceDir, "cru_ts4.03.1901.2018.tmp.dat.rds"))
    
    ### create lat, lon and time list
    aus.lon.min <- 110.25
    aus.lon.max <- 155.25
    aus.lat.min <- -45.25
    aus.lat.max <- -10.25
    
    lon.list <- seq(aus.lon.min, aus.lon.max, 0.5)
    lat.list <- seq(aus.lat.max, aus.lat.min, -0.5)
    
    ### calculate coldest month T, warmest month T of each year, and GDD
    if (!file.exists("output/climate/coldest_month_T.rds")) {
        
        print("preparing bioclimate dataset ... ... ")
        
        ### create the dataset
        tminDF <- extract_coldest_month_T(tmpDF)
        tmaxDF <- extract_warmest_month_T(tmpDF)
        gddDF <- extract_annual_gdd(tmpDF, sourceDir)
        
    } else {
        print("reading in bioclimatic dataset ... ...")
        
        tminDF <- readRDS("output/climate/coldest_month_T.rds")
        tmaxDF <- readRDS("output/climate/warmest_month_T.rds")
        gddDF <- readRDS("output/climate/annual_gdd.rds")
        
    }
    
    ### 20-yr running mean calculation
    bioclimDF <- calculate_20_yr_running_mean(tminDF, tmaxDF, gddDF, lon.list, lat.list)
     
    
    ### set bioclimatic parmaeters for each PFT
    paramDF <- define_PFT_bioclimatic_parameters()
    
    
    ### Generate spatial distribution of each PFT based on bioclimatic limits
    plotDF <- bioclimDF
    
    ### BNE
    plotDF$BNE <- ifelse(plotDF$tcmin_est < paramDF$tcmin_est[paramDF$PFT=="BNE"] | 
                             plotDF$tcmax_est > paramDF$tcmax_est[paramDF$PFT=="BNE"] |
                             plotDF$twmin_est < paramDF$twmin_est[paramDF$PFT=="BNE"] | 
                             plotDF$gdd5min_est < paramDF$gdd5min_est[paramDF$PFT=="BNE"], 0, 1)
    
    ### BINE
    plotDF$BINE <- ifelse(plotDF$tcmin_est < paramDF$tcmin_est[paramDF$PFT=="BINE"] | 
                             plotDF$tcmax_est > paramDF$tcmax_est[paramDF$PFT=="BINE"] |
                             plotDF$twmin_est < paramDF$twmin_est[paramDF$PFT=="BINE"]| 
                             plotDF$gdd5min_est < paramDF$gdd5min_est[paramDF$PFT=="BINE"], 0, 1)
    
    ### BNS
    plotDF$BNS <- ifelse(plotDF$tcmin_est < paramDF$tcmin_est[paramDF$PFT=="BNS"] | 
                             plotDF$tcmax_est > paramDF$tcmax_est[paramDF$PFT=="BNS"] |
                             plotDF$twmin_est < paramDF$twmin_est[paramDF$PFT=="BNS"]| 
                             plotDF$gdd5min_est < paramDF$gdd5min_est[paramDF$PFT=="BNS"], 0, 1)
    
    ### TeNE
    plotDF$TeNE <- ifelse(plotDF$tcmin_est < paramDF$tcmin_est[paramDF$PFT=="TeNE"] | 
                             plotDF$tcmax_est > paramDF$tcmax_est[paramDF$PFT=="TeNE"] |
                             plotDF$twmin_est < paramDF$twmin_est[paramDF$PFT=="TeNE"]| 
                             plotDF$gdd5min_est < paramDF$gdd5min_est[paramDF$PFT=="TeNE"], 0, 1)
    
    ### TeBS
    plotDF$TeBS <- ifelse(plotDF$tcmin_est < paramDF$tcmin_est[paramDF$PFT=="TeBS"] | 
                             plotDF$tcmax_est > paramDF$tcmax_est[paramDF$PFT=="TeBS"] |
                             plotDF$twmin_est < paramDF$twmin_est[paramDF$PFT=="TeBS"]| 
                             plotDF$gdd5min_est < paramDF$gdd5min_est[paramDF$PFT=="TeBS"], 0, 1)
    
    ### IBS
    plotDF$IBS <- ifelse(plotDF$tcmin_est < paramDF$tcmin_est[paramDF$PFT=="IBS"] | 
                             plotDF$tcmax_est > paramDF$tcmax_est[paramDF$PFT=="IBS"] |
                             plotDF$twmin_est < paramDF$twmin_est[paramDF$PFT=="IBS"]| 
                             plotDF$gdd5min_est < paramDF$gdd5min_est[paramDF$PFT=="IBS"], 0, 1)
    
    ### TeBE
    plotDF$TeBE <- ifelse(plotDF$tcmin_est < paramDF$tcmin_est[paramDF$PFT=="TeBE"] | 
                             plotDF$tcmax_est > paramDF$tcmax_est[paramDF$PFT=="TeBE"] |
                             plotDF$twmin_est < paramDF$twmin_est[paramDF$PFT=="TeBE"] | 
                             plotDF$gdd5min_est < paramDF$gdd5min_est[paramDF$PFT=="TeBE"], 
                          0, 1)
    
    
    ### C3G
    plotDF$C3G <- ifelse(plotDF$tcmin_est < paramDF$tcmin_est[paramDF$PFT=="C3G"] | 
                             plotDF$tcmax_est > paramDF$tcmax_est[paramDF$PFT=="C3G"] |
                             plotDF$twmin_est < paramDF$twmin_est[paramDF$PFT=="C3G"]| 
                             plotDF$gdd5min_est < paramDF$gdd5min_est[paramDF$PFT=="C3G"], 0, 1)
    
    ### C4G
    plotDF$C4G <- ifelse(plotDF$tcmin_est < paramDF$tcmin_est[paramDF$PFT=="C4G"] | 
                             plotDF$tcmax_est > paramDF$tcmax_est[paramDF$PFT=="C4G"] |
                             plotDF$twmin_est < paramDF$twmin_est[paramDF$PFT=="C4G"]| 
                             plotDF$gdd5min_est < paramDF$gdd5min_est[paramDF$PFT=="C4G"], 0, 1)
    
    
    ### save the output
    saveRDS(plotDF, "output/climate/bioclimatic_predicted_vegetation_distribution.rds")
    
    
    ### plotting - bioclimatic limits
    xlimlon <- range(lon.list)
    ylimlat <- range(lat.list)
    
    tcmin.min <- min(plotDF$tcmin_est)
    tcmin.max <- max(plotDF$tcmin_est)
    
    tcmax.min <- min(plotDF$tcmax_est)
    tcmax.max <- max(plotDF$tcmax_est)
    
    twmin.min <- min(plotDF$twmin_est)
    twmin.max <- max(plotDF$twmin_est)
    
    gdd5min.min <- min(plotDF$gdd5min_est)
    gdd5min.max <- max(plotDF$gdd5min_est)
    
    
    
    ### tcmin
    p1 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=Lat, x=Lon, fill=tcmin_est)) +
        coord_quickmap(xlim=xlimlon, ylim=ylimlat)+
        borders("world", col="grey", lwd=0.2) +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_continuous(name="Min coldest month T",
                              na.value = 'white',
                              type = "viridis",
                              limits = c(tcmin.min,tcmin.max))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    gganimate::animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_tcmin.gif", animation=last_animation(), path="output/climate/")
    
    
    
    ### tcmax
    p1 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=Lat, x=Lon, fill=tcmax_est)) +
        coord_quickmap(xlim=xlimlon, ylim=ylimlat)+
        borders("world", col="grey", lwd=0.2) +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_continuous(name="Max coldest month T",
                              na.value = 'white',
                              type = "viridis",
                              limits = c(tcmax.min,tcmax.max))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    gganimate::animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_tcmax.gif", animation=last_animation(), path="output/climate/")
    
    
    ### twmin
    p1 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=Lat, x=Lon, fill=twmin_est)) +
        coord_quickmap(xlim=xlimlon, ylim=ylimlat)+
        borders("world", col="grey", lwd=0.2) +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_continuous(name="Min warmest month T",
                              na.value = 'white',
                              type = "viridis",
                              limits = c(twmin.min,twmin.max))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    gganimate::animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_twmin.gif", animation=last_animation(), path="output/climate/")
    
    
    ### gdd5min
    p1 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=Lat, x=Lon, fill=gdd5min_est)) +
        coord_quickmap(xlim=xlimlon, ylim=ylimlat)+
        borders("world", col="grey", lwd=0.2) +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_continuous(name="growing degree day",
                              na.value = 'white',
                              type = "viridis",
                              limits = c(gdd5min.min,gdd5min.max))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    gganimate::animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_gdd5min.gif", animation=last_animation(), path="output/climate/")
    
    
    ############# plot PFT distribution ##############
    ### TeNE
    p1 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=Lat, x=Lon, fill=as.character(TeNE))) +
        coord_quickmap(xlim=xlimlon, ylim=ylimlat)+
        borders("world", col="grey", lwd=0.2) +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_manual(name="TeNE presence/absence",
                          values = c("0", "1"),
                          labels = c("absence", "presence"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)

        
    ## save animation
    gganimate::animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_TeNE.gif", animation=last_animation(), path="output/climate/")
    
    
    
    ### TeBS
    p1 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=Lat, x=Lon, fill=as.character(TeBS))) +
        coord_quickmap(xlim=xlimlon, ylim=ylimlat)+
        borders("world", col="grey", lwd=0.2) +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_manual(name="TeBS presence/absence",
                          values = c("0", "1"),
                          labels = c("absence", "presence"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    gganimate::animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_TeBS.gif", animation=last_animation(), path="output/climate/")
    
    
    ### IBS
    p1 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=Lat, x=Lon, fill=as.character(IBS))) +
        coord_quickmap(xlim=xlimlon, ylim=ylimlat)+
        borders("world", col="grey", lwd=0.2) +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_manual(name="IBS presence/absence",
                          values = c("0", "1"),
                          labels = c("absence", "presence"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    gganimate::animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_IBS.gif", animation=last_animation(), path="output/climate/")
    
    
    
    ### TeBE
    p1 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=Lat, x=Lon, fill=as.character(TeBE))) +
        coord_quickmap(xlim=xlimlon, ylim=ylimlat)+
        borders("world", col="grey", lwd=0.2) +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_manual(name="TeBE presence/absence",
                          values = c("0", "1"),
                          labels = c("absence", "presence"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    gganimate::animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_TeBE.gif", animation=last_animation(), path="output/climate/")
    
    
    
    
    ### C3G
    p1 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=Lat, x=Lon, fill=as.character(C3G))) +
        coord_quickmap(xlim=xlimlon, ylim=ylimlat)+
        borders("world", col="grey", lwd=0.2) +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_manual(name="C3G presence/absence",
                          values = c("0", "1"),
                          labels = c("absence", "presence"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    gganimate::animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_C3G.gif", animation=last_animation(), path="output/climate/")
    
    
    
    ### C4G
    p1 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=Lat, x=Lon, fill=as.character(C4G))) +
        coord_quickmap(xlim=xlimlon, ylim=ylimlat)+
        borders("world", col="grey", lwd=0.2) +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_manual(name="C4G presence/absence",
                          values = c("0", "1"),
                          labels = c("absence", "presence"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    gganimate::animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_C4G.gif", animation=last_animation(), path="output/climate/")
    
    
    ### end.    
}

plot_animated_LAI_map <- function(myDF) {
    
    ### conver 0 to NAs
    inDF <- myDF
    inDF[inDF == 0] <- NA
    
    ## set lim of color in log scale
    min.lim <- log(0.1)
    max.lim <- log(max(inDF$Total, na.rm=T))
    
    ### Plot animated maps
    ### TeNE
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=log(TeNE))) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_continuous(name="LAI TeNE",
                              na.value = 'white',
                              #trans = "log",
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(log(0.1), log(0.5), log(2), 
                                         log(5), log(15)),
                              labels = c(0.1, 0.5, 2, 5, 15))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_TeNE.gif", animation=last_animation(), path="output/")
    
    
    ### TeBS
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=log(TeBS))) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_continuous(name="LAI TeBS",
                              na.value = 'white',
                              #trans = "log",
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(log(0.1), log(0.5), log(2), 
                                         log(5), log(15)),
                              labels = c(0.1, 0.5, 2, 5, 15))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_TeBS.gif", animation=last_animation(), path="output/")
    
    
    ### IBS
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=log(IBS))) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_continuous(name="LAI IBS",
                              na.value = 'white',
                              #trans = "log",
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(log(0.1), log(0.5), log(2), 
                                         log(5), log(15)),
                              labels = c(0.1, 0.5, 2, 5, 15))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_IBS.gif", animation=last_animation(), path="output/")
    
    
    
    ### TeBE
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=log(TeBE))) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_continuous(name="LAI TeBE",
                              na.value = 'white',
                              #trans = "log",
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(log(0.1), log(0.5), log(2), 
                                         log(5), log(15)),
                              labels = c(0.1, 0.5, 2, 5, 15))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_TeBE.gif", animation=last_animation(), path="output/")
    
    
    ### TrBE
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=log(TrBE))) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_continuous(name="LAI TrBE",
                              na.value = 'white',
                              #trans = "log",
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(log(0.1), log(0.5), log(2), 
                                         log(5), log(15)),
                              labels = c(0.1, 0.5, 2, 5, 15))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_TrBE.gif", animation=last_animation(), path="output/")
    
    
    ### TrIBE
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=log(TrIBE))) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_continuous(name="LAI TrIBE",
                              na.value = 'white',
                              #trans = "log",
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(log(0.1), log(0.5), log(2), 
                                         log(5), log(15)),
                              labels = c(0.1, 0.5, 2, 5, 15))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_TrIBE.gif", animation=last_animation(), path="output/")
    
    ### TrBR
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=log(TrBR))) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_continuous(name="LAI TrBR",
                              na.value = 'white',
                              #trans = "log",
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(log(0.1), log(0.5), log(2), 
                                         log(5), log(15)),
                              labels = c(0.1, 0.5, 2, 5, 15))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_TrBR.gif", animation=last_animation(), path="output/")
    
    ### C3G
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=log(C3G))) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_continuous(name="LAI C3G",
                              na.value = 'white',
                              #trans = "log",
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(log(0.1), log(0.5), log(2), 
                                         log(5), log(15)),
                              labels = c(0.1, 0.5, 2, 5, 15))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_C3G.gif", animation=last_animation(), path="output/")
    
    
    ### C4G
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=log(C4G))) +
        coord_quickmap(xlim=range(tDF$Lon), ylim=range(tDF$Lat))+
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
        scale_fill_continuous(name="LAI C4G",
                              na.value = 'white',
                              #trans = "log",
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(log(0.1), log(0.5), log(2), 
                                         log(5), log(15)),
                              labels = c(0.1, 0.5, 2, 5, 15))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_C4G.gif", animation=last_animation(), path="output/")
    #
    
}
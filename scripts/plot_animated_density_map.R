plot_animated_density_map <- function(myDF) {
    
    ### conver 0 to NAs
    inDF <- myDF
    inDF[inDF == 0] <- NA
    
    ## set lim of color in log scale
    min.lim <- min(inDF$Total, na.rm=T)
    max.lim <- max(inDF$Total, na.rm=T)
    
    ### Plot animated maps
    ### TeNE
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=TeNE)) +
        coord_quickmap(xlim=range(myDF$Lon), ylim=range(myDF$Lat))+
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
        scale_fill_continuous(name="density TeNE (indiv/m2)",
                              na.value = 'white',
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9),
                              labels = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_density_TeNE.gif", animation=last_animation(), path="output/basic/")
    
    
    ### TeBS
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=TeBS)) +
        coord_quickmap(xlim=range(inDF$Lon), ylim=range(inDF$Lat))+
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
        scale_fill_continuous(name="density TeBS (indiv/m2)",
                              na.value = 'white',
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9),
                              labels = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_density_TeBS.gif", animation=last_animation(), path="output/basic/")
    
    
    ### IBS
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=IBS)) +
        coord_quickmap(xlim=range(inDF$Lon), ylim=range(inDF$Lat))+
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
        scale_fill_continuous(name="density IBS (indiv/m2)",
                              na.value = 'white',
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9),
                              labels = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_density_IBS.gif", animation=last_animation(), path="output/basic/")
    
    
    
    ### TeBE
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=TeBE)) +
        coord_quickmap(xlim=range(inDF$Lon), ylim=range(inDF$Lat))+
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
        scale_fill_continuous(name="density TeBE (indiv/m2)",
                              na.value = 'white',
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9),
                              labels = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_density_TeBE.gif", animation=last_animation(), path="output/basic/")
    
    
    ### TrBE
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=TrBE)) +
        coord_quickmap(xlim=range(inDF$Lon), ylim=range(inDF$Lat))+
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
        scale_fill_continuous(name="density TrBE (indiv/m2)",
                              na.value = 'white',
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9),
                              labels = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_density_TrBE.gif", animation=last_animation(), path="output/basic/")
    
    
    ### TrIBE
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=TrIBE)) +
        coord_quickmap(xlim=range(inDF$Lon), ylim=range(inDF$Lat))+
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
        scale_fill_continuous(name="density TrIBE (indiv/m2)",
                              na.value = 'white',
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9),
                              labels = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_density_TrIBE.gif", animation=last_animation(), path="output/basic/")
    
    ### TrBR
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=TrBR)) +
        coord_quickmap(xlim=range(inDF$Lon), ylim=range(inDF$Lat))+
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
        scale_fill_continuous(name="density TrBR (indiv/m2)",
                              na.value = 'white',
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9),
                              labels = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_density_TrBR.gif", animation=last_animation(), path="output/basic/")
    
   
    ### Total
    p1 <- ggplot() + 
        geom_tile(data=inDF, aes(y=Lat, x=Lon, fill=Total)) +
        coord_quickmap(xlim=range(inDF$Lon), ylim=range(inDF$Lat))+
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
        scale_fill_continuous(name="density Total (indiv/m2)",
                              na.value = 'white',
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9),
                              labels = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1,
                                         1.3, 1.5, 1.7, 1.9))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_map_density_C4G.gif", animation=last_animation(), path="output/basic/")
    #
    
}
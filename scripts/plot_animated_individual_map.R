plot_animated_individual_map <- function(myDF) {
    
    
    ### how does each individual evolve within each grid over time
    test <- subset(myDF, Lon == 150.25 && Lat == -34.25)
    
    ### averaging all individual for each year
    sumDF <- summaryBy(Iage+FPC~Year+PFT, FUN=c(mean, sd), data=test, keep.names=T, na.rm=T)
    
    ### plot
    p1 <- ggplot(test) +
        geom_point(aes(Iage, FPC, col=Year, fill=Year))+
        scale_fill_continuous(type = "viridis")+
        scale_color_continuous(type = "viridis")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=6),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylim(c(0,1))
    
    plot(p1)
    
    ### to start with, just look at PDF of patch age within each year for all grids
    for (i in 2005:2015) {
        test <- subset(myDF, Year == 2005)
        
        ### plot Stand NPP vs. Patch age
        p1 <- ggplot(test) +
            geom_hex(mapping=aes(x=Page, y=LAI), bins = 50) +
            scale_fill_continuous(type = "viridis") +
            #geom_point(faceDF, mapping=aes(x=MAT, y=MAP, 
            #                               color=factor(Biome),
            #                               shape=factor(Biome), 
            #                               size = factor(Biome)),
            #           inherit.aes = FALSE)+
            #xlab(expression("MAT (" * degree * "C)")) + 
            #ylab("MAP (mm)") +
            #scale_color_manual(name="", 
            #                   values=col.list2,
            #                   labels=forests2) +
            #scale_shape_manual(values=c(17, rep(15, 7)),
            #                   labels=forests2) +
            #scale_size_manual(values=c(rep(4, 8)),
            #                  labels=forests2)+
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.title.x = element_text(size=12), 
                  axis.text.x = element_text(size=12),
                  axis.text.y=element_text(size=12),
                  axis.title.y=element_text(size=12),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=12),
                  panel.grid.major=element_blank(),
                  legend.position="bottom",
                  legend.text.align=0)#+
            #guides(size = "none",
            #       shape = "none",
            #       #fill = viridis,
            #       color = guide_legend(ncol=5, override.aes = 
            #                                list(size = 4, shape=c(17, rep(15, 7)), 
            #                                     colour=col.list2)))
        
        plot(p1)
            
    }

    
    
    
    
    
    
    
    
    
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
        scale_fill_continuous(name="density TeNE",
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
    anim_save("animated_map_density_TeNE.gif", animation=last_animation(), path="output/")
    
    
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
        scale_fill_continuous(name="density TeBS",
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
    anim_save("animated_map_density_TeBS.gif", animation=last_animation(), path="output/")
    
    
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
        scale_fill_continuous(name="density IBS",
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
    anim_save("animated_map_density_IBS.gif", animation=last_animation(), path="output/")
    
    
    
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
        scale_fill_continuous(name="density TeBE",
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
    anim_save("animated_map_density_TeBE.gif", animation=last_animation(), path="output/")
    
    
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
        scale_fill_continuous(name="density TrBE",
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
    anim_save("animated_map_density_TrBE.gif", animation=last_animation(), path="output/")
    
    
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
        scale_fill_continuous(name="density TrIBE",
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
    anim_save("animated_map_density_TrIBE.gif", animation=last_animation(), path="output/")
    
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
        scale_fill_continuous(name="density TrBR",
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
    anim_save("animated_map_density_TrBR.gif", animation=last_animation(), path="output/")
    
   
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
        scale_fill_continuous(name="density Total",
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
    anim_save("animated_map_density_C4G.gif", animation=last_animation(), path="output/")
    #
    
}
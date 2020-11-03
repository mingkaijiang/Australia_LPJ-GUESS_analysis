plot_animated_LAI_latitudinal_gradient <- function(myDF) {
    
    ### Plot latitudinal pattern over time
    inDF <- myDF
    inDF[inDF == 0] <- NA
    
    ### convert to latitudinal gradient DF
    latDF <- summaryBy(TeNE+TeBS+IBS+TeBE+TrBE+TrIBE+TrBR+C3G+C4G+Total~Lat+Year, 
                       FUN=c(mean, sd),
                       keep.names=T, na.rm=T, data=inDF)
    
    
    ### plot TeNE
    p1 <- ggplot(latDF, aes(x=Lat, y=TeNE.mean)) + 
        stat_smooth(formula = y ~ x, method = "loess", se = FALSE, 
                    size = 0.5, color = "black", linetype="dotted") +
        geom_point(show.legend = FALSE, alpha = 0.7) +
        coord_flip()+
        theme_linedraw() +
        ylab("LAI mean TeNE")+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.box = 'none',
              legend.box.just = 'vertical',
              legend.position = "none")+
        ggtitle("TeNE")+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_latitudinal_TeNE.gif", animation=last_animation(), path="output/")
    
    
    ### plot TeBS
    p1 <- ggplot(latDF, aes(x=Lat, y=TeBS.mean)) + 
        stat_smooth(formula = y ~ x, method = "loess", se = FALSE, 
                    size = 0.5, color = "black", linetype="dotted") +
        geom_point(show.legend = FALSE, alpha = 0.7) +
        coord_flip()+
        theme_linedraw() +
        ylab("LAI mean TeBS")+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.box = 'none',
              legend.box.just = 'vertical',
              legend.position = "none")+
        ggtitle("TeBS")+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_latitudinal_TeBS.gif", animation=last_animation(), path="output/")
    
    
    ### plot IBS
    p1 <- ggplot(latDF, aes(x=Lat, y=IBS.mean)) + 
        stat_smooth(formula = y ~ x, method = "loess", se = FALSE, 
                    size = 0.5, color = "black", linetype="dotted") +
        geom_point(show.legend = FALSE, alpha = 0.7) +
        coord_flip()+
        theme_linedraw() +
        ylab("LAI mean IBS")+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.box = 'none',
              legend.box.just = 'vertical',
              legend.position = "none")+
        ggtitle("IBS")+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_latitudinal_IBS.gif", animation=last_animation(), path="output/")
    
    
    ### plot TeBE
    p1 <- ggplot(latDF, aes(x=Lat, y=TeBE.mean)) + 
        stat_smooth(formula = y ~ x, method = "loess", se = FALSE, 
                    size = 0.5, color = "black", linetype="dotted") +
        geom_point(show.legend = FALSE, alpha = 0.7) +
        coord_flip()+
        theme_linedraw() +
        ylab("LAI mean TeBE")+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.box = 'none',
              legend.box.just = 'vertical',
              legend.position = "none")+
        ggtitle("TeBE")+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_latitudinal_TeBE.gif", animation=last_animation(), path="output/")
    
    
    
    ### plot TrBE
    p1 <- ggplot(latDF, aes(x=Lat, y=TrBE.mean)) + 
        stat_smooth(formula = y ~ x, method = "loess", se = FALSE, 
                    size = 0.5, color = "black", linetype="dotted") +
        geom_point(show.legend = FALSE, alpha = 0.7) +
        coord_flip()+
        theme_linedraw() +
        ylab("LAI mean TrBE")+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.box = 'none',
              legend.box.just = 'vertical',
              legend.position = "none")+
        ggtitle("TrBE")+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_latitudinal_TrBE.gif", animation=last_animation(), path="output/")
    
    
    
    ### plot TrIBE
    p1 <- ggplot(latDF, aes(x=Lat, y=TrIBE.mean)) + 
        stat_smooth(formula = y ~ x, method = "loess", se = FALSE, 
                    size = 0.5, color = "black", linetype="dotted") +
        geom_point(show.legend = FALSE, alpha = 0.7) +
        coord_flip()+
        theme_linedraw() +
        ylab("LAI mean TrIBE")+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.box = 'none',
              legend.box.just = 'vertical',
              legend.position = "none")+
        ggtitle("TrIBE")+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_latitudinal_TrIBE.gif", animation=last_animation(), path="output/")
    
    
    ### plot TrBR
    p1 <- ggplot(latDF, aes(x=Lat, y=TrBR.mean)) + 
        stat_smooth(formula = y ~ x, method = "loess", se = FALSE, 
                    size = 0.5, color = "black", linetype="dotted") +
        geom_point(show.legend = FALSE, alpha = 0.7) +
        coord_flip()+
        theme_linedraw() +
        ylab("LAI mean TrBR")+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.box = 'none',
              legend.box.just = 'vertical',
              legend.position = "none")+
        ggtitle("TrBR")+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_latitudinal_TrBR.gif", animation=last_animation(), path="output/")
    
    
    ### plot C3G
    p1 <- ggplot(latDF, aes(x=Lat, y=C3G.mean)) + 
        stat_smooth(formula = y ~ x, method = "loess", se = FALSE, 
                    size = 0.5, color = "black", linetype="dotted") +
        geom_point(show.legend = FALSE, alpha = 0.7) +
        coord_flip()+
        theme_linedraw() +
        ylab("LAI mean C3G")+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.box = 'none',
              legend.box.just = 'vertical',
              legend.position = "none")+
        ggtitle("C3G")+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_latitudinal_C3G.gif", animation=last_animation(), path="output/")
    
    
    
    ### plot C4G
    p1 <- ggplot(latDF, aes(x=Lat, y=C4G.mean)) + 
        stat_smooth(formula = y ~ x, method = "loess", se = FALSE, 
                    size = 0.5, color = "black", linetype="dotted") +
        geom_point(show.legend = FALSE, alpha = 0.7) +
        coord_flip()+
        theme_linedraw() +
        ylab("LAI mean C4G")+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.box = 'none',
              legend.box.just = 'vertical',
              legend.position = "none")+
        ggtitle("C4G")+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_latitudinal_C4G.gif", animation=last_animation(), path="output/")
    
}
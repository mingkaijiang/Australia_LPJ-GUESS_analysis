plot_self_thinning_figures <- function(myDF, patcharea) {
    
    ### patcharea in unit of m2
    
    ### only look at trees
    subDF <- myDF[myDF$PFT%in%c(0,1,2,3,4,5,6,7,8,9),]
 
    ### calculate sum of wood
    subDF$StemC <- subDF$SapC + subDF$HeartC
    
    ### calculate proxy for diameter - PD
    subDF$PD <- 1.0 / subDF$Idens
    
    ### color labels
    col.list <- brewer.pal(12, name="Paired")
    
    ### prepare arrow plot - check 10-yr difference
    subDF1 <- subset(subDF, Year == 2005)
    subDF2 <- subset(subDF, Year == 2015)
    mgDF <- merge(subDF1, subDF2, by = c("Lon", "Lat", "Patch", "PFT", "Indiv"), all=T)
    
    ### 
    p1 <- ggplot() +
        #geom_point(subDF, mapping = aes(PD, StemC, col=Iage))+
        geom_segment(mgDF, mapping = aes(x = PD.x, y = StemC.x, xend = PD.y, yend = StemC.y, col=Iage.y),
                     arrow = arrow(length = unit(0.3, "cm")))+
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
        xlab("Proxy for tree diameter (1 / density)")+
        ylab("Stem C")
    

    ### calculate patch sums
    ### sum biomass and density within each patch
    sumDF <- summaryBy(StemC+Idens+PD~Lon+Lat+Year+Patch+Page+PFT, 
                       FUN=sum, data=subDF, 
                       keep.names=T, na.rm=T)
    
    subDF1 <- subset(sumDF, Year == 2005)
    subDF2 <- subset(sumDF, Year == 2015)
    mgDF2 <- merge(subDF1, subDF2, by = c("Lon", "Lat", "Patch", "PFT"), all=T)
    
    
    p2 <- ggplot() +
        #geom_point(sumDF, mapping = aes(PD, StemC, col=Page))+
        geom_segment(mgDF2, mapping = aes(x = PD.x, y = StemC.x, xend = PD.y, yend = StemC.y, col=Page.y),
                     arrow = arrow(length = unit(0.3, "cm")))+
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
        xlab("Proxy for tree diameter (1 / density) within each pacth")+
        ylab("Stem C")
    
    
    ### save pdf
    pdf("output/self-thinning/diameter_vs_stemC_individual.pdf", width = 6, height = 4)
    plot(p1)
    dev.off()
    
    ### save pdf
    pdf("output/self-thinning/diameter_vs_stemC_patch.pdf", width = 6, height = 4)
    plot(p2)
    dev.off()
    
    
    
    ### animated plot
    ## individual
    p3 <- ggplot() +
        geom_point(subDF, mapping = aes(PD, StemC, col=Iage))+
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
        xlab("Proxy for tree diameter (1 / density)")+
        ylab("Stem C")+
        transition_time(Page)+
        labs(title = "Individual age: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p3, fps = 10, width = 750, height = 450, renderer = gifski_renderer())
    anim_save("animated_individual_thinning_with_age.gif", animation=last_animation(), path="output/self-thinning/")
    
    ## patch
    p4 <- ggplot() +
        geom_point(sumDF, mapping = aes(PD, StemC, col=Page))+
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
        xlab("Proxy for tree diameter (1 / density) within each pacth")+
        ylab("Stem C")+
        transition_time(Page)+
        labs(title = "Patch age: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p4, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_patch_thinning_with_age.gif", animation=last_animation(), path="output/self-thinning/")
    
    
    
    ### look at FPC of each patch - forest gap analysis
    
    ## summing all FPC within each patch
    sumDF2 <- summaryBy(FPC~Lon+Lat+Year+Patch+Page, FUN=sum, data=myDF, keep.names=T)
    
    
    p1 <- ggplot() +
        geom_hex(sumDF2, mapping = aes(Page, FPC), bins=30)+
        geom_smooth(sumDF2, mapping=aes(Page, FPC),col="red",
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
        xlab("Patch age")+
        ylab("FPC of each patch (including trees only)")
    
    
    p2 <- ggplot() +
        geom_hex(sumDF2[sumDF2$Page < 20,], mapping = aes(Page, FPC), bins=10)+
        geom_smooth(sumDF2[sumDF2$Page < 20,], mapping=aes(Page, FPC),col="red",
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
        xlab("Patch age (< 20 only)")+
        ylab("FPC of each patch (including trees only)")
    

    ### plot
    combined_plot <- plot_grid(p1, p2,
                               ncol=2, align="vh", axis = "l")
    
    save_plot(paste0("output/self-thinning/forest_gap_analysis.pdf"),
              combined_plot, base_width=10)
    
}

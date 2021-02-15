plot_animated_individual_map <- function(myDF) {
    
    ### color labels
    col.list <- brewer.pal(12, name="Paired")
    
    ### Basic stuff - patch age, separated by PFT and year
    p1 <- ggplot(myDF,aes(x=Page, color=as.factor(PFT))) +
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
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)+
        scale_color_manual(name="PFT", 
                           values=col.list,
                           labels=c("0"="BNE","1"="BINE","2"="BNS", 
                                    "3"="TeNE","4"="TeBS","5"="IBS",
                                    "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                    "9"="TrBR","10"="C3G","11"="C4G"))+
        xlab("Patch age")
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_density_patch_age.gif", animation=last_animation(), path="output/dgvm/")
    
    
    #### Plot individual age, separated by PFT and year
    p1 <- ggplot(myDF,aes(x=Iage, color=as.factor(PFT))) +
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
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)+
        scale_color_manual(name="PFT", 
                           values=col.list,
                           labels=c("0"="BNE","1"="BINE","2"="BNS", 
                                    "3"="TeNE","4"="TeBS","5"="IBS",
                                    "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                    "9"="TrBR","10"="C3G","11"="C4G"))+
        xlab("Individual age")
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save("animated_density_individual_age.gif", animation=last_animation(), path="output/dgvm")
    
    
    ### Look at a selected list of PFTs
    ### "3"="TeNE","4"="TeBS","5"="IBS",
    ### "6"="TeBE","7"="TrBE","8"="TrIBE", 
    ### "9"="TrBR"
    tDF <- data.frame(c(3,4,5,6,7,8,9),
                      c("TeNE", "TeBS", "IBS",
                        "TeBE", "TrBE", "TrIBE",
                        "TrBR"))
    colnames(tDF) <- c("ID", "PFT")
    
    d <- dim(tDF)[1]
    
    ### loop
    for (i in 1:d) {
        ### subset
        test <- subset(myDF, PFT == tDF$ID[i])
        
        ### min and max 
        min.Iage <- min(test$Iage)
        max.Iage <- max(test$Iage)
        
        min.Page <- min(test$Page)
        max.Page <- max(test$Page)
        
        ### plot
        p1 <- ggplot(test,aes(x=Iage)) +
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
            transition_time(Year)+
            labs(title = "Year: {frame_time}")+
            shadow_wake(wake_length = 0.1, alpha = FALSE)+
            xlab(paste0(tDF$PFT[i], " Individual Age"))+
            xlim(c(min.Iage, max.Iage))
        
        ## save animation
        animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
        anim_save(paste0("animated_density_individual_age_", tDF$PFT[i], ".gif"), 
                  animation=last_animation(), path="output/dgvm")
        
        ### plot
        p2 <- ggplot(test,aes(x=Page)) +
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
            transition_time(Year)+
            labs(title = "Year: {frame_time}")+
            shadow_wake(wake_length = 0.1, alpha = FALSE)+
            xlab(paste0(tDF$PFT[i], " Patch Age"))+
            xlim(c(min.Page, max.Page))
        
        ## save animation
        animate(p2, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
        anim_save(paste0("animated_density_patch_age_", tDF$PFT[i], ".gif"), 
                  animation=last_animation(), path="output/dgvm")
    }
    
    
    
    ### how does each individual evolve within each grid over time
    test <- subset(myDF, Lon == 150.25 && Lat == -34.25)
    
    ### plot
    p1 <- ggplot(test) +
        geom_point(aes(Iage, FPC, color=as.factor(PFT)))+
        scale_color_manual(name="PFT", 
                           values=col.list,
                           labels=c("0"="BNE","1"="BINE","2"="BNS", 
                                    "3"="TeNE","4"="TeBS","5"="IBS",
                                    "6"="TeBE","7"="TrBE","8"="TrIBE", 
                                    "9"="TrBR","10"="C3G","11"="C4G"))+
        #geom_smooth(aes(Iage, FPC, color=as.factor(PFT)),
        #            span = 0.3, se=F)+
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
        ylim(c(0,1))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)+
        xlab("Individual Age")
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_individual_age_vs_fpc_single_grid.gif"), 
              animation=last_animation(), path="output/dgvm")
    
    
   
}


    
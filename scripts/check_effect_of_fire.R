check_effect_of_fire_basic <- function(fire.model) {
    
    #### read input LAI
    ### fire
    myDF1 <- read.table(paste0("input/withfire/run1/lai.out"), header=T)
    for (i in 2:20) {
        tmpDF <- read.table(paste0("input/withfire/run", i, "/lai.out"), header=T)
        myDF1 <- rbind(myDF1, tmpDF)
    }

    ### no fire
    myDF2 <- read.table(paste0("input/withoutfire/run1/lai.out"), header=T)
    for (i in 2:20) {
        tmpDF <- read.table(paste0("input/withoutfire/run", i, "/lai.out"), header=T)
        myDF2 <- rbind(myDF2, tmpDF)
    }
    
    
    
    ### Total LAI change over time
    outDF1 <- data.frame(c(1901:2015), 
                        NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, 
                        NA, NA, NA)
    colnames(outDF1) <- c("Year", "BNE", "BINE", "BNS", "TeNE",
                         "TeBS", "IBS", "TeBE", "TrBE", "TrIBE",
                         "TrBR", "C3G", "C4G", "Total")
    
    outDF2 <- outDF1
    
    for (i in c(1901:2015)) {
        tDF1 <- subset(myDF1, Year == i)
        tDF2 <- subset(myDF2, Year == i)
        
        tDF1$BNE <- ifelse(tDF1$BNE > 0, 1, 0)
        tDF1$BINE <- ifelse(tDF1$BINE > 0, 1, 0)
        tDF1$BNS <- ifelse(tDF1$BNS > 0, 1, 0)
        tDF1$TeNE <- ifelse(tDF1$TeNE > 0, 1, 0)
        tDF1$TeBS <- ifelse(tDF1$TeBS > 0, 1, 0)
        tDF1$IBS <- ifelse(tDF1$IBS > 0, 1, 0)
        tDF1$TeBE <- ifelse(tDF1$TeBE > 0, 1, 0)
        tDF1$TrBE <- ifelse(tDF1$TrBE > 0, 1, 0)
        tDF1$TrIBE <- ifelse(tDF1$TrIBE > 0, 1, 0)
        tDF1$TrBR <- ifelse(tDF1$TrBR > 0, 1, 0)
        tDF1$C3G <- ifelse(tDF1$C3G > 0, 1, 0)
        tDF1$C4G <- ifelse(tDF1$C4G > 0, 1, 0)
        tDF1$Total <- ifelse(tDF1$Total > 0, 1, 0)
        
        tDF2$BNE <- ifelse(tDF2$BNE > 0, 1, 0)
        tDF2$BINE <- ifelse(tDF2$BINE > 0, 1, 0)
        tDF2$BNS <- ifelse(tDF2$BNS > 0, 1, 0)
        tDF2$TeNE <- ifelse(tDF2$TeNE > 0, 1, 0)
        tDF2$TeBS <- ifelse(tDF2$TeBS > 0, 1, 0)
        tDF2$IBS <- ifelse(tDF2$IBS > 0, 1, 0)
        tDF2$TeBE <- ifelse(tDF2$TeBE > 0, 1, 0)
        tDF2$TrBE <- ifelse(tDF2$TrBE > 0, 1, 0)
        tDF2$TrIBE <- ifelse(tDF2$TrIBE > 0, 1, 0)
        tDF2$TrBR <- ifelse(tDF2$TrBR > 0, 1, 0)
        tDF2$C3G <- ifelse(tDF2$C3G > 0, 1, 0)
        tDF2$C4G <- ifelse(tDF2$C4G > 0, 1, 0)
        tDF2$Total <- ifelse(tDF2$Total > 0, 1, 0)
        
        tmp1 <- colSums(tDF1[,4:16])
        tmp2 <- colSums(tDF2[,4:16])
        
        outDF1[outDF1$Year == i, 2:14] <- tmp1
        outDF2[outDF2$Year == i, 2:14] <- tmp2
        
    }
    
    ### sum of all grids
    outDF1$All <- rowSums(outDF1[,2:13])
    outDF2$All <- rowSums(outDF2[,2:13])
    
    ### calculate %
    pctDF1 <- outDF1[,1:13]
    pctDF2 <- outDF2[,1:13]
    
    pctDF1$BNE <- outDF1$BNE/outDF1$All
    pctDF1$BINE <- outDF1$BINE/outDF1$All
    pctDF1$BNS <- outDF1$BNS/outDF1$All
    pctDF1$TeNE <- outDF1$TeNE/outDF1$All
    pctDF1$TeBS <- outDF1$TeBS/outDF1$All
    pctDF1$IBS <- outDF1$IBS/outDF1$All
    pctDF1$TeBE <- outDF1$TeBE/outDF1$All
    pctDF1$TrBE <- outDF1$TrBE/outDF1$All
    pctDF1$TrIBE <- outDF1$TrIBE/outDF1$All
    pctDF1$TrBR <- outDF1$TrBR/outDF1$All
    pctDF1$C3G <- outDF1$C3G/outDF1$All
    pctDF1$C4G <- outDF1$C4G/outDF1$All
    
    pctDF2$BNE <- outDF2$BNE/outDF2$All
    pctDF2$BINE <- outDF2$BINE/outDF2$All
    pctDF2$BNS <- outDF2$BNS/outDF2$All
    pctDF2$TeNE <- outDF2$TeNE/outDF2$All
    pctDF2$TeBS <- outDF2$TeBS/outDF2$All
    pctDF2$IBS <- outDF2$IBS/outDF2$All
    pctDF2$TeBE <- outDF2$TeBE/outDF2$All
    pctDF2$TrBE <- outDF2$TrBE/outDF2$All
    pctDF2$TrIBE <- outDF2$TrIBE/outDF2$All
    pctDF2$TrBR <- outDF2$TrBR/outDF2$All
    pctDF2$C3G <- outDF2$C3G/outDF2$All
    pctDF2$C4G <- outDF2$C4G/outDF2$All
    
    
    plotDF1 <- reshape2::melt(pctDF1, id.vars=c("Year"),
                             measure.vars=c("BNE", "BINE", "BNS", "TeNE",
                                            "TeBS", "IBS", "TeBE", "TrBE", 
                                            "TrIBE", "TrBR", "C3G", "C4G"),
                             variable.name="PFT",
                             value.name="pct")
    
    plotDF2 <- reshape2::melt(pctDF2, id.vars=c("Year"),
                              measure.vars=c("BNE", "BINE", "BNS", "TeNE",
                                             "TeBS", "IBS", "TeBE", "TrBE", 
                                             "TrIBE", "TrBR", "C3G", "C4G"),
                              variable.name="PFT",
                              value.name="pct")
    
    
    ### plot PFT by year
    col.lab <- brewer.pal(12, name="Paired")
    
    p1 <- ggplot(plotDF1) +
        geom_bar(aes(x = Year, y = pct, fill = PFT), stat="identity", width = 0.7)+
        coord_flip()+
        scale_fill_manual(values = col.lab)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylim(c(0,1.0))
    
    p2 <- ggplot(plotDF2) +
        geom_bar(aes(x = Year, y = pct, fill = PFT), stat="identity", width = 0.7)+
        coord_flip()+
        scale_fill_manual(values = col.lab)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylim(c(0,1.0))
    
    
    ### plot
    combined_plot <- plot_grid(p1, p2, 
                               ncol=2, align="vh", axis = "l")
    
    save_plot(paste0("output/fire/PFT_by_year_effect_of_fire.pdf"),
              combined_plot, base_width=10, base_height = 10)
    
    
    #### Plot static LAI in selected years
    subDF1 <- subset(myDF1, Year == 2015)
    subDF2 <- subset(myDF2, Year == 2015)
    
    ### set 0 to 0.1
    subDF1$Total[subDF1$Total <= 0.1] <- 0.1
    subDF2$Total[subDF2$Total <= 0.1] <- 0.1
    
    ### set boundary
    min.lim <- log(0.1)
    max.lim <- log(max(c(subDF1$Total, subDF2$Total), na.rm=T))
    
    ### plot
    p1 <- ggplot() + 
        geom_tile(data=subDF1, aes(y=Lat, x=Lon, fill=log(Total))) +
        coord_quickmap(xlim=range(subDF1$Lon), ylim=range(subDF1$Lat))+
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
              legend.box.just = 'vertical',
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="white"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        scale_fill_continuous(name="LAI",
                              na.value = 'white',
                              #trans = "log",
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(log(0.1), log(0.5), log(2), 
                                         log(5), log(15)),
                              labels = c(0.1, 0.5, 2, 5, 15))+
        ggtitle(paste0("With fire ", fire.model))
    
    
    
    p2 <- ggplot() + 
        geom_tile(data=subDF2, aes(y=Lat, x=Lon, fill=log(Total))) +
        coord_quickmap(xlim=range(subDF2$Lon), ylim=range(subDF2$Lat))+
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
              legend.box.just = 'vertical',
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="white"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        scale_fill_continuous(name="LAI",
                              na.value = 'white',
                              #trans = "log",
                              type = "viridis",
                              limits = c(min.lim,max.lim),
                              breaks = c(log(0.1), log(0.5), log(2), 
                                         log(5), log(15)),
                              labels = c(0.1, 0.5, 2, 5, 15))+
        ggtitle(paste0("Without fire"))
    
    
    ### plot
    ### combine legend
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                             legend.box = 'vertical',
                                             legend.box.just = 'left'))
    
    combined_plot <- plot_grid(p1, p2, 
                               ncol=2, align="vh", axis = "l")
    
    out_plot <- plot_grid(combined_plot, combined_legend, 
                          ncol=1, rel_heights=c(1, 0.1))
    
    save_plot(paste0("output/fire/LAI_in_year_", i, "_effect_of_fire.pdf"),
              out_plot, base_width=10, base_height = 10)
    
    
    ### density plot
    myDF1$Fire <- "With Fire"
    myDF2$Fire <- "Without Fire"
    
    mgDF <- rbind(myDF1, myDF2)
    
    ### plot
    p1 <- ggplot(mgDF, aes(x=Total, color=Fire)) +
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
        xlab("Total LAI")
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_fire_effect_on_LAI.gif"), animation=last_animation(), path="output/fire/")
    
    
    #### read input tree density
    ### fire
    myDF1 <- read.table(paste0("input/withfire/run1/dens.out"), header=T)
    for (i in 2:20) {
        tmpDF <- read.table(paste0("input/withfire/run", i, "/dens.out"), header=T)
        myDF1 <- rbind(myDF1, tmpDF)
    }
    
    ### no fire
    myDF2 <- read.table(paste0("input/withoutfire/run1/dens.out"), header=T)
    for (i in 2:20) {
        tmpDF <- read.table(paste0("input/withoutfire/run", i, "/dens.out"), header=T)
        myDF2 <- rbind(myDF2, tmpDF)
    }
    
    
    ### density plot
    myDF1$Fire <- "With Fire"
    myDF2$Fire <- "Without Fire"
    
    mgDF <- rbind(myDF1, myDF2)
    
    ### plot
    p1 <- ggplot(mgDF, aes(x=Total, color=Fire)) +
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
        xlab("Tree density")
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_fire_effect_on_tree_density.gif"), animation=last_animation(), path="output/fire/")
    
    
    ### check the effect of fire on PFT distribution
    ### ie. overlay with and without fire, to see which grids were wiped out by fire completely.
    presenceDF1 <- myDF1
    presenceDF2 <- myDF2
    
    ### prepare plot settings
    lonlim <- range(presenceDF1$Lon)
    latlim <- range(presenceDF1$Lat)
    
    ### TeNE
    plotDF1 <- cbind(presenceDF1[,1:3], presenceDF1$TeNE, presenceDF2$TeNE)
    colnames(plotDF1) <- c("Lon", "Lat", "Year", "With", "Without")
    plotDF1$FireEffect <- with(plotDF1, (Without - With))
    summary(plotDF1$FireEffect)
    plotDF1$FireEffect <- ifelse(plotDF1$FireEffect>0, 1, ifelse(plotDF1$FireEffect < 0, -1, 0))
    plotDF1$FireEffect <- as.character(plotDF1$FireEffect)

    p1 <- ggplot() + 
        geom_tile(data=plotDF1, aes(y=Lat, x=Lon, fill=FireEffect)) +
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
        scale_fill_manual(name="TeNE Fire Effect",
                              breaks = c("-1", "0", "1"),
                              labels = c("Fire introduced", "No effect", "Fire destroyed"),
                          values = c("green", "white", "red"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    

    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_fire_effect_on_TeNE_distribution.gif"), animation=last_animation(), path="output/fire/")
    
    
    ### TeBS
    plotDF1 <- cbind(presenceDF1[,1:3], presenceDF1$TeBS, presenceDF2$TeBS)
    colnames(plotDF1) <- c("Lon", "Lat", "Year", "With", "Without")
    plotDF1$FireEffect <- with(plotDF1, (Without - With))
    summary(plotDF1$FireEffect)
    plotDF1$FireEffect <- ifelse(plotDF1$FireEffect>0, 1, ifelse(plotDF1$FireEffect < 0, -1, 0))
    plotDF1$FireEffect <- as.character(plotDF1$FireEffect)
    
    p1 <- ggplot() + 
        geom_tile(data=plotDF1, aes(y=Lat, x=Lon, fill=FireEffect)) +
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
        scale_fill_manual(name="TeBS Fire Effect",
                          breaks = c("-1", "0", "1"),
                          labels = c("Fire introduced", "No effect", "Fire destroyed"),
                          values = c("green", "white", "red"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_fire_effect_on_TeBS_distribution.gif"), animation=last_animation(), path="output/fire/")
    
    
    
    ### IBS
    plotDF1 <- cbind(presenceDF1[,1:3], presenceDF1$IBS, presenceDF2$IBS)
    colnames(plotDF1) <- c("Lon", "Lat", "Year", "With", "Without")
    plotDF1$FireEffect <- with(plotDF1, (Without - With))
    summary(plotDF1$FireEffect)
    plotDF1$FireEffect <- ifelse(plotDF1$FireEffect>0, 1, ifelse(plotDF1$FireEffect < 0, -1, 0))
    plotDF1$FireEffect <- as.character(plotDF1$FireEffect)
    
    p1 <- ggplot() + 
        geom_tile(data=plotDF1, aes(y=Lat, x=Lon, fill=FireEffect)) +
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
        scale_fill_manual(name="IBS Fire Effect",
                          breaks = c("-1", "0", "1"),
                          labels = c("Fire introduced", "No effect", "Fire destroyed"),
                          values = c("green", "white", "red"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_fire_effect_on_IBS_distribution.gif"), animation=last_animation(), path="output/fire/")
    
    
    ### TeBE
    plotDF1 <- cbind(presenceDF1[,1:3], presenceDF1$TeBE, presenceDF2$TeBE)
    colnames(plotDF1) <- c("Lon", "Lat", "Year", "With", "Without")
    plotDF1$FireEffect <- with(plotDF1, (Without - With))
    summary(plotDF1$FireEffect)
    plotDF1$FireEffect <- ifelse(plotDF1$FireEffect>0, 1, ifelse(plotDF1$FireEffect < 0, -1, 0))
    plotDF1$FireEffect <- as.character(plotDF1$FireEffect)
    
    p1 <- ggplot() + 
        geom_tile(data=plotDF1, aes(y=Lat, x=Lon, fill=FireEffect)) +
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
        scale_fill_manual(name="TeBE Fire Effect",
                          breaks = c("-1", "0", "1"),
                          labels = c("Fire introduced", "No effect", "Fire destroyed"),
                          values = c("green", "white", "red"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_fire_effect_on_TeBE_distribution.gif"), animation=last_animation(), path="output/fire/")
    
    
    
    ### C3G
    plotDF1 <- cbind(presenceDF1[,1:3], presenceDF1$C3G, presenceDF2$C3G)
    colnames(plotDF1) <- c("Lon", "Lat", "Year", "With", "Without")
    plotDF1$FireEffect <- with(plotDF1, (Without - With))
    summary(plotDF1$FireEffect)
    plotDF1$FireEffect <- ifelse(plotDF1$FireEffect>0, 1, ifelse(plotDF1$FireEffect < 0, -1, 0))
    plotDF1$FireEffect <- as.character(plotDF1$FireEffect)
    
    p1 <- ggplot() + 
        geom_tile(data=plotDF1, aes(y=Lat, x=Lon, fill=FireEffect)) +
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
        scale_fill_manual(name="C3G Fire Effect",
                          breaks = c("-1", "0", "1"),
                          labels = c("Fire introduced", "No effect", "Fire destroyed"),
                          values = c("green", "white", "red"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_fire_effect_on_C3G_distribution.gif"), animation=last_animation(), path="output/fire/")
    
    
    
    ### C4G
    plotDF1 <- cbind(presenceDF1[,1:3], presenceDF1$C4G, presenceDF2$C4G)
    colnames(plotDF1) <- c("Lon", "Lat", "Year", "With", "Without")
    plotDF1$FireEffect <- with(plotDF1, (Without - With))
    summary(plotDF1$FireEffect)
    plotDF1$FireEffect <- ifelse(plotDF1$FireEffect>0, 1, ifelse(plotDF1$FireEffect < 0, -1, 0))
    plotDF1$FireEffect <- as.character(plotDF1$FireEffect)
    
    p1 <- ggplot() + 
        geom_tile(data=plotDF1, aes(y=Lat, x=Lon, fill=FireEffect)) +
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
        scale_fill_manual(name="C4G Fire Effect",
                          breaks = c("-1", "0", "1"),
                          labels = c("Fire introduced", "No effect", "Fire destroyed"),
                          values = c("green", "white", "red"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_fire_effect_on_C4G_distribution.gif"), animation=last_animation(), path="output/fire/")
    
    
    
    ### TrBE
    plotDF1 <- cbind(presenceDF1[,1:3], presenceDF1$TrBE, presenceDF2$TrBE)
    colnames(plotDF1) <- c("Lon", "Lat", "Year", "With", "Without")
    plotDF1$FireEffect <- with(plotDF1, (Without - With))
    summary(plotDF1$FireEffect)
    plotDF1$FireEffect <- ifelse(plotDF1$FireEffect>0, 1, ifelse(plotDF1$FireEffect < 0, -1, 0))
    plotDF1$FireEffect <- as.character(plotDF1$FireEffect)
    
    p1 <- ggplot() + 
        geom_tile(data=plotDF1, aes(y=Lat, x=Lon, fill=FireEffect)) +
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
        scale_fill_manual(name="TrBE Fire Effect",
                          breaks = c("-1", "0", "1"),
                          labels = c("Fire introduced", "No effect", "Fire destroyed"),
                          values = c("green", "white", "red"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_fire_effect_on_TrBE_distribution.gif"), animation=last_animation(), path="output/fire/")
    
    
    ### TrIBE
    plotDF1 <- cbind(presenceDF1[,1:3], presenceDF1$TrIBE, presenceDF2$TrIBE)
    colnames(plotDF1) <- c("Lon", "Lat", "Year", "With", "Without")
    plotDF1$FireEffect <- with(plotDF1, (Without - With))
    summary(plotDF1$FireEffect)
    plotDF1$FireEffect <- ifelse(plotDF1$FireEffect>0, 1, ifelse(plotDF1$FireEffect < 0, -1, 0))
    plotDF1$FireEffect <- as.character(plotDF1$FireEffect)
    
    p1 <- ggplot() + 
        geom_tile(data=plotDF1, aes(y=Lat, x=Lon, fill=FireEffect)) +
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
        scale_fill_manual(name="TrIBE Fire Effect",
                          breaks = c("-1", "0", "1"),
                          labels = c("Fire introduced", "No effect", "Fire destroyed"),
                          values = c("green", "white", "red"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_fire_effect_on_TrIBE_distribution.gif"), animation=last_animation(), path="output/fire/")
    
    
    ### TrBR
    plotDF1 <- cbind(presenceDF1[,1:3], presenceDF1$TrBR, presenceDF2$TrBR)
    colnames(plotDF1) <- c("Lon", "Lat", "Year", "With", "Without")
    plotDF1$FireEffect <- with(plotDF1, (Without - With))
    summary(plotDF1$FireEffect)
    plotDF1$FireEffect <- ifelse(plotDF1$FireEffect>0, 1, ifelse(plotDF1$FireEffect < 0, -1, 0))
    plotDF1$FireEffect <- as.character(plotDF1$FireEffect)
    
    p1 <- ggplot() + 
        geom_tile(data=plotDF1, aes(y=Lat, x=Lon, fill=FireEffect)) +
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
        scale_fill_manual(name="TrBR Fire Effect",
                          breaks = c("-1", "0", "1"),
                          labels = c("Fire introduced", "No effect", "Fire destroyed"),
                          values = c("green", "white", "red"))+
        transition_time(Year)+
        labs(title = "Year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_fire_effect_on_TrBR_distribution.gif"), animation=last_animation(), path="output/fire/")
    
    
    
    ### latitudinal gradient
    
    
    
    
    ## end
}
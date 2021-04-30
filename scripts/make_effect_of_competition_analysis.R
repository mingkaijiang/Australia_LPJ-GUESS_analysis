make_effect_of_competition_analysis <- function() {
    

    ### read the RDS for different PFT
    var.list <- c("lai")#, "fpc", "cpool")
    
    pft.list <- c("BNE", "BINE", "BNS", 
                  "TeNE", "IBS", "TeBE", 
                  "TeBS", 
                  "TrBE", "TrIBE", "TrBR", 
                  "C3G", "C4G")
    
    for (i in var.list) {
        ### read individual file and merge
        myDF <- readRDS(paste0("output/competition/TeBE_", i, ".out.rds"))
        myDF$Total <- NULL
        
        tmpDF <- readRDS(paste0("output/competition/IBS_", i, ".out.rds"))
        tmpDF$Total <- NULL
        myDF <- merge(myDF, tmpDF, by=c("Lon", "Lat", "Year"))
        
        tmpDF <- readRDS(paste0("output/competition/TeNE_", i, ".out.rds"))
        tmpDF$Total <- NULL
        myDF <- merge(myDF, tmpDF, by=c("Lon", "Lat", "Year"))
        
        tmpDF <- readRDS(paste0("output/competition/TeBS_", i, ".out.rds"))
        tmpDF$Total <- NULL
        myDF <- merge(myDF, tmpDF, by=c("Lon", "Lat", "Year"))
        
        tmpDF <- readRDS(paste0("output/competition/C3G_", i, ".out.rds"))
        tmpDF$Total <- NULL
        myDF <- merge(myDF, tmpDF, by=c("Lon", "Lat", "Year"))
        
        tmpDF <- readRDS(paste0("output/competition/C4G_", i, ".out.rds"))
        tmpDF$Total <- NULL
        myDF <- merge(myDF, tmpDF, by=c("Lon", "Lat", "Year"))
        
        tmpDF <- readRDS(paste0("output/competition/BNE_", i, ".out.rds"))
        tmpDF$Total <- NULL
        myDF <- merge(myDF, tmpDF, by=c("Lon", "Lat", "Year"))
        
        tmpDF <- readRDS(paste0("output/competition/BINE_", i, ".out.rds"))
        tmpDF$Total <- NULL
        myDF <- merge(myDF, tmpDF, by=c("Lon", "Lat", "Year"))
        
        #tmpDF <- readRDS(paste0("output/competition/BNS_", i, ".out.rds"))
        #tmpDF$Total <- NULL
        #myDF <- merge(myDF, tmpDF, by=c("Lon", "Lat", "Year"))
        myDF$BNS <- 0.0
        
        tmpDF <- readRDS(paste0("output/competition/TrBE_", i, ".out.rds"))
        tmpDF$Total <- NULL
        myDF <- merge(myDF, tmpDF, by=c("Lon", "Lat", "Year"))
        
        tmpDF <- readRDS(paste0("output/competition/TrIBE_", i, ".out.rds"))
        tmpDF$Total <- NULL
        myDF <- merge(myDF, tmpDF, by=c("Lon", "Lat", "Year"))
        
        tmpDF <- readRDS(paste0("output/competition/TrBR_", i, ".out.rds"))
        tmpDF$Total <- NULL
        myDF <- merge(myDF, tmpDF, by=c("Lon", "Lat", "Year"))
        
        myDF <- myDF[,c("Lon", "Lat", "Year", "BNE", "BINE", "BNS",
                        "TeNE", "TeBS", "IBS", "TeBE", 
                        "TrBE", "TrIBE", "TrBR", "C3G", "C4G")]
        
        myDF$Total <- rowSums(myDF[,c("BNE", "BINE", "BNS", 
                                      "TeNE", "TeBS", "IBS", "TeBE", 
                                      "TrBE", "TrIBE", "TrBR", "C3G", "C4G")])
        
        
        ### read in the data with competition, with fire
        fireDF <- read.table(paste0("input/withfire/run1/", i, ".out"), header=T)
        
        for (j in 2:20) {
            tmpDF <- read.table(paste0("input/withfire/run", j, "/", i, ".out"), header=T)
            fireDF <- rbind(fireDF, tmpDF)
        }
        
        
        ### read in the data with competition, without fire
        nofireDF <- read.table(paste0("input/withoutfire/run1/", i, ".out"), header=T)
        
        for (j in 2:20) {
            tmpDF <- read.table(paste0("input/withoutfire/run", j, "/", i, ".out"), header=T)
            nofireDF <- rbind(nofireDF, tmpDF)
        }
        
        
        ### add total to the pft.list
        pft.list2 <- c(pft.list, "Total")
        pft.list2 <- c("TeNE", "IBS", "TeBE", 
                       "TeBS", 
                       "TrBE", "TrIBE", "TrBR", 
                       "C3G", "C4G", "Total")
        
        ### get lon and lat list
        lon.list <- unique(nofireDF$Lon)
        lat.list <- unique(nofireDF$Lat)
        
        
        ### merge individual PFT
        for (j in pft.list2) {
            plotDF <- merge(myDF[,c("Lon", "Lat", "Year", paste0(j))], 
                            fireDF[,c("Lon", "Lat", "Year", paste0(j))],
                            by=c("Lon", "Lat", "Year"))
            colnames(plotDF) <- c("Lon", "Lat", "Year", "NoCompetition", "Competition")
            plotDF <- merge(plotDF, nofireDF[,c("Lon", "Lat", "Year", paste0(j))], 
                            by=c("Lon", "Lat", "Year"))
            colnames(plotDF) <- c("Lon", "Lat", "Year", "NoCompetition", "Competition",
                                  "NoFire")
            
            
            ### convert all 0s to NAs
            plotDF$NoCompetition <- ifelse(plotDF$NoCompetition == 0, NA, plotDF$NoCompetition)
            plotDF$Competition <- ifelse(plotDF$Competition == 0, NA, plotDF$Competition)
            plotDF$NoFire <- ifelse(plotDF$NoFire == 0, NA, plotDF$NoFire)
            
            plotDF$Competition_Diff <- with(plotDF, NoCompetition - Competition)
            plotDF$Fire_Diff <- with(plotDF, NoFire - Competition)
            
            ### average all years
            sumDF <- summaryBy(NoCompetition+Competition+NoFire+Competition_Diff+Fire_Diff~Lon+Lat, 
                               FUN=mean, data=plotDF, na.rm=T, keep.names=T)
            
            sumDF$ID <- c(1:max(dim(sumDF)[1]))
            idDF <- sumDF[,c("Lon", "Lat", "ID")]
            plotDF <- merge(plotDF, idDF, by=c("Lon", "Lat"))
            
            xlim.range <- range(sumDF$Lon)
            ylim.range <- range(sumDF$Lat)
            
            
            ### set scales
            plot.scale.range <- range(c(sumDF$NoCompetition, sumDF$Competition, sumDF$NoFire), na.rm=T)
            plot.scale.range[1] <- ifelse(plot.scale.range[1] > 0, 0.01, plot.scale.range[1])
            log.plot.scale.range <- log(plot.scale.range)
            log.plot.scale.range[1] <- log(0.01)
            log.plot.scale.breaks <- seq(log.plot.scale.range[1], log.plot.scale.range[2], length.out=5)
            plot.scale.breaks <- round(exp(log.plot.scale.breaks),2)
            plot.scale.range[2] <- ifelse(plot.scale.range[2] == 0.0, plot.scale.range[2], plot.scale.range[2] + 1.0)
            

            #### obtain gridded linear fit relationships
            #for (k in unique(idDF$ID)) {
            #    test <- subset(plotDF, ID==k)
            #    lm1 <- lm(NoCompetition~Year, data=test)
            #    lm2 <- lm(Competition~Year, data=test)
            #    lm3 <- lm(NoFire~Year, data=test)
            #    lm4 <- lm(Competition_Diff~Year, data=test)
            #    lm5 <- lm(Fire_Diff~Year, data=test)
            #    
            #    sumDF$NoCompetition_slope[sumDF$ID==k] <- coef(lm1)[2]
            #    sumDF$NoCompetition_p[sumDF$ID==k] <- summary(lm1)$coefficients[,4][2]
            #    
            #    sumDF$Competition_slope[sumDF$ID==k] <- coef(lm2)[2]
            #    sumDF$Competition_p[sumDF$ID==k] <- summary(lm2)$coefficients[,4][2]
            #    
            #    sumDF$NoFire_slope[sumDF$ID==k] <- coef(lm3)[2]
            #    sumDF$NoFire_p[sumDF$ID==k] <- summary(lm3)$coefficients[,4][2]
            #    
            #    sumDF$Competition_Diff_slope[sumDF$ID==k] <- coef(lm4)[2]
            #    sumDF$Competition_Diff_p[sumDF$ID==k] <- summary(lm4)$coefficients[,4][2]
            #    
            #    sumDF$Fire_Diff_slope[sumDF$ID==k] <- coef(lm5)[2]
            #    sumDF$Fire_Diff_p[sumDF$ID==k] <- summary(lm5)$coefficients[,4][2]
            #    
            #}
            #
            #
            ##### write
            #saveRDS(sumDF, file=paste0("output/competition/Effect_of_competition_", 
            #                           i, "_", j, "_all_yr.rds"))

            
            ### Plot 100-yr average results
            p1 <- ggplot() + 
                geom_tile(data=sumDF, aes(y=Lat, x=Lon, fill=NoCompetition)) +
                coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
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
                scale_fill_continuous(name=paste0("LAI ", j),
                                      na.value = 'white',
                                      trans="log",
                                      limits = plot.scale.range,
                                      breaks = plot.scale.breaks,
                                      labels = plot.scale.breaks,
                                      type = "viridis")+
                ggtitle("No competition")
            

            p2 <- ggplot() + 
                geom_tile(data=sumDF, aes(y=Lat, x=Lon, fill=Competition)) +
                coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
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
                scale_fill_continuous(name=paste0("LAI ", j),
                                      na.value = 'white',
                                      trans="log",
                                      limits = plot.scale.range,
                                      breaks = plot.scale.breaks,
                                      labels = plot.scale.breaks,
                                      type = "viridis")+
                ggtitle("With competition")
            
            
            p3 <- ggplot() + 
                geom_tile(data=sumDF, aes(y=Lat, x=Lon, fill=NoFire)) +
                coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
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
                scale_fill_continuous(name=paste0("LAI ", j),
                                      na.value = 'white',
                                      trans="log",
                                      limits = plot.scale.range,
                                      breaks = plot.scale.breaks,
                                      labels = plot.scale.breaks,
                                      type = "viridis")+
                ggtitle("No Fire")
            
            
            p4 <- ggplot() + 
                geom_tile(data=sumDF, aes(y=Lat, x=Lon, fill=Competition_Diff)) +
                coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
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
                scale_fill_continuous(name=paste0("LAI ", j),
                                      na.value = 'white',
                                      type = "viridis")+
                ggtitle("No Competition - Competition")
            
            
            p5 <- ggplot() + 
                geom_tile(data=sumDF, aes(y=Lat, x=Lon, fill=Fire_Diff)) +
                coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
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
                scale_fill_continuous(name=paste0("LAI ", j),
                                      na.value = 'white',
                                      type = "viridis")+
                ggtitle("No Fire - Fire")
            
            ## save plot
            combined_plot <- plot_grid(p1, p2, p3, p4, p5,
                                       ncol=3, align="vh", axis = "l")
            
            save_plot(paste0("output/competition/Effect_of_competition_", i, "_", j, ".pdf"),
                      combined_plot, base_width=10, base_height = 8)
            
            
            ###################### 
            ### plot temporal patterns
            
            
            #### make NAs
            #sumDF2 <- sumDF
            #sumDF2$NoCompetition_slope <- ifelse(sumDF2$NoCompetition_p <= 0.05, sumDF2$NoCompetition_slope, NA)
            #sumDF2$Competition_slope <- ifelse(sumDF2$Competition_p <= 0.05, sumDF2$Competition_slope, NA)
            #sumDF2$NoFire_slope <- ifelse(sumDF2$NoFire_p <= 0.05, sumDF2$NoFire_slope, NA)
            #sumDF2$Competition_Diff_slope <- ifelse(sumDF2$Competition_Diff_p <= 0.05, sumDF2$Competition_Diff_slope, NA)
            #sumDF2$Fire_Diff_slope <- ifelse(sumDF2$Fire_Diff_p <= 0.05, sumDF2$Fire_Diff_slope, NA)
            #
            #if (sum(sumDF2$NoCompetition_slope, na.rm=T) == 0.0) {
            #    next
            #} else {
            #    ### plot
            #    p1 <- ggplot() + 
            #        geom_tile(data=sumDF2, aes(y=Lat, x=Lon, fill=NoCompetition_slope)) +
            #        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
            #        borders("world", col="grey", lwd=0.2) +
            #        theme(panel.grid.minor=element_blank(),
            #              axis.text.x=element_blank(),
            #              axis.title.x=element_blank(),
            #              axis.text.y=element_blank(),
            #              axis.title.y=element_blank(),
            #              legend.text=element_text(size=10),
            #              legend.title=element_text(size=12),
            #              panel.grid.major=element_blank(),
            #              legend.position = "right")+
            #        scale_fill_continuous(name=paste0("LAI slope ", j),
            #                              na.value = 'white',
            #                              type = "viridis")+
            #        ggtitle("No Competition slope")
            #    
            #    
            #    p2 <- ggplot() + 
            #        geom_tile(data=sumDF2, aes(y=Lat, x=Lon, fill=Competition_slope)) +
            #        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
            #        borders("world", col="grey", lwd=0.2) +
            #        theme(panel.grid.minor=element_blank(),
            #              axis.text.x=element_blank(),
            #              axis.title.x=element_blank(),
            #              axis.text.y=element_blank(),
            #              axis.title.y=element_blank(),
            #              legend.text=element_text(size=10),
            #              legend.title=element_text(size=12),
            #              panel.grid.major=element_blank(),
            #              legend.position = "right")+
            #        scale_fill_continuous(name=paste0("LAI slope ", j),
            #                              na.value = 'white',
            #                              type = "viridis")+
            #        ggtitle("Competition slope")
            #    
            #    
            #    p3 <- ggplot() + 
            #        geom_tile(data=sumDF2, aes(y=Lat, x=Lon, fill=NoFire_slope)) +
            #        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
            #        borders("world", col="grey", lwd=0.2) +
            #        theme(panel.grid.minor=element_blank(),
            #              axis.text.x=element_blank(),
            #              axis.title.x=element_blank(),
            #              axis.text.y=element_blank(),
            #              axis.title.y=element_blank(),
            #              legend.text=element_text(size=10),
            #              legend.title=element_text(size=12),
            #              panel.grid.major=element_blank(),
            #              legend.position = "right")+
            #        scale_fill_continuous(name=paste0("LAI slope ", j),
            #                              na.value = 'white',
            #                              type = "viridis")+
            #        ggtitle("No Fire slope")
            #    
            #    
            #    p4 <- ggplot() + 
            #        geom_tile(data=sumDF2, aes(y=Lat, x=Lon, fill=Competition_Diff_slope)) +
            #        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
            #        borders("world", col="grey", lwd=0.2) +
            #        theme(panel.grid.minor=element_blank(),
            #              axis.text.x=element_blank(),
            #              axis.title.x=element_blank(),
            #              axis.text.y=element_blank(),
            #              axis.title.y=element_blank(),
            #              legend.text=element_text(size=10),
            #              legend.title=element_text(size=12),
            #              panel.grid.major=element_blank(),
            #              legend.position = "right")+
            #        scale_fill_continuous(name=paste0("LAI slope ", j),
            #                              na.value = 'white',
            #                              type = "viridis")+
            #        ggtitle("Competition Difference slope")
            #    
            #    
            #    p5 <- ggplot() + 
            #        geom_tile(data=sumDF2, aes(y=Lat, x=Lon, fill=Fire_Diff_slope)) +
            #        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
            #        borders("world", col="grey", lwd=0.2) +
            #        theme(panel.grid.minor=element_blank(),
            #              axis.text.x=element_blank(),
            #              axis.title.x=element_blank(),
            #              axis.text.y=element_blank(),
            #              axis.title.y=element_blank(),
            #              legend.text=element_text(size=10),
            #              legend.title=element_text(size=12),
            #              panel.grid.major=element_blank(),
            #              legend.position = "right")+
            #        scale_fill_continuous(name=paste0("LAI slope ", j),
            #                              na.value = 'white',
            #                              type = "viridis")+
            #        ggtitle("Fire Difference slope")
            #    
            #    
            #    ## save plot
            #    combined_plot <- plot_grid(p1, p2, p3, p4, p5,
            #                               ncol=3, align="vh", axis = "l")
            #    
            #    save_plot(paste0("output/competition/Effect_of_competition_temporal_slope_", i, "_", j, ".pdf"),
            #              combined_plot, base_width=10, base_height = 8)
            #}
            
            
            
            
        } ## pft.list, j
        
        
    } ## var.list, i
    
    ### end.
}

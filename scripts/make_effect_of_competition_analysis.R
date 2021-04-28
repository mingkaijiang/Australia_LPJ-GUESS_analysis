make_effect_of_competition_analysis <- function() {
    

    ### read the RDS for different PFT
    var.list <- c("lai", "fpc", "cpool")
    
    pft.list <- c("BNE", "BINE", #"BNS", 
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
        
        ### merge individual PFT
        for (j in pft.list) {
            plotDF <- merge(myDF[,c("Lon", "Lat", "Year", paste0(j))], 
                            fireDF[,c("Lon", "Lat", "Year", paste0(j))],
                            by=c("Lon", "Lat", "Year"))
            colnames(plotDF) <- c("Lon", "Lat", "Year", "NoCompetition", "Competition")
            plotDF <- merge(plotDF, nofireDF[,c("Lon", "Lat", "Year", paste0(j))], 
                            by=c("Lon", "Lat", "Year"))
            colnames(plotDF) <- c("Lon", "Lat", "Year", "NoCompetition", "Competition",
                                  "NoFire")
            
            plotDF$Competition_Diff <- with(plotDF, NoCompetition - Competition)
            plotDF$Fire_Diff <- with(plotDF, NoFire - Competition)
            
            sumDF <- summaryBy(NoCompetition+Competition+NoFire+Competition_Diff+Fire_Diff~Lon+Lat, 
                               FUN=mean, data=plotDF, na.rm=T, keep.names=T)
            
            xlim.range <- range(sumDF$Lon)
            ylim.range <- range(sumDF$Lat)
            
            ### set scales
            plot.scale.range <- round(range(c(sumDF$NoCompetition, sumDF$Competition, sumDF$NoFire)), 0)
            plot.scale.range[1] <- ifelse(plot.scale.range[1] > 0, 0, plot.scale.range[1])
            plot.scale.range[2] <- plot.scale.range[2] + 1.0
            #plot.scale.breaks <- round(c(plot.scale.range[1], 
            #                             plot.scale.range[2]/15, 
            #                             plot.scale.range[2]/15 * 2 + plot.scale.range[2]/15, 
            #                             plot.scale.range[2]/15 * 3 + plot.scale.range[2]/15,
            #                             plot.scale.range[2]), 1)

            ### Plot animated maps
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
                                      limits = plot.scale.range,
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
                                      limits = plot.scale.range,
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
                                      limits = plot.scale.range,
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
            
            save_plot(paste0("output/competition/Effect_of_competition_LAI_", j, ".pdf"),
                      combined_plot, base_width=10, base_height = 8)
            
        } ## pft.list, j
        
    } ## var.list, i
    
    ### end.
}
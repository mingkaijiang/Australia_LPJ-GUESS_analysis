check_CO2_effect <- function() {
    
    ### simulation list
    sim.list <- paste0("run", c(1:20))
    
    ## variable list
    var.list <- c("lai.out", "cpool.out")
    
    ########################################################################
    ### files under variable climate forcing
    lai.files <- paste0("input/withfire/run", c(1:20), "/lai.out")
    cpool.files <- paste0("input/withfire/run", c(1:20), "/cpool.out")

    ### read input files
    myDF1.lai = lai.files %>% 
        purrr::map_df(~read.table(.,header=T))
    
    myDF1.cpool = cpool.files %>% 
        purrr::map_df(~read.table(.,header=T))
    
    ########################################################################
    ### files under fixed CO2 variable T
    lai.files <- paste0("input/run", c(1:20), "/fixCO2varT/lai.out")
    cpool.files <- paste0("input/run", c(1:20), "/fixCO2varT/cpool.out")

    ### read input files
    myDF2.lai = lai.files %>% 
        purrr::map_df(~read.table(.,header=T))
    
    myDF2.cpool = cpool.files %>% 
        purrr::map_df(~read.table(.,header=T))
        
        
    ### merge dataset
    myDF.lai <- merge(myDF1.lai, myDF2.lai, by=c("Lon", "Lat", "Year"))
    myDF.cpool <- merge(myDF1.cpool, myDF2.cpool, by=c("Lon", "Lat", "Year"))
    
    ### calculate CO2 effect difference in absolute and % terms
    ## lai
    myDF.lai$TeNE.abs.diff <- with(myDF.lai, TeNE.x - TeNE.y)
    myDF.lai$TeNE.pct.diff <- with(myDF.lai, TeNE.x / TeNE.y)
    
    myDF.lai$TeBS.abs.diff <- with(myDF.lai, TeBS.x - TeBS.y)
    myDF.lai$TeBS.pct.diff <- with(myDF.lai, TeBS.x / TeBS.y)
    
    myDF.lai$IBS.abs.diff <- with(myDF.lai, IBS.x - IBS.y)
    myDF.lai$IBS.pct.diff <- with(myDF.lai, IBS.x / IBS.y)
    
    myDF.lai$TeBE.abs.diff <- with(myDF.lai, TeBE.x - TeBE.y)
    myDF.lai$TeBE.pct.diff <- with(myDF.lai, TeBE.x / TeBE.y)
    
    myDF.lai$TrBE.abs.diff <- with(myDF.lai, TrBE.x - TrBE.y)
    myDF.lai$TrBE.pct.diff <- with(myDF.lai, TrBE.x / TrBE.y)
    
    myDF.lai$TrIBE.abs.diff <- with(myDF.lai, TrIBE.x - TrIBE.y)
    myDF.lai$TrIBE.pct.diff <- with(myDF.lai, TrIBE.x / TrIBE.y)
    
    myDF.lai$TrBR.abs.diff <- with(myDF.lai, TrBR.x - TrBR.y)
    myDF.lai$TrBR.pct.diff <- with(myDF.lai, TrBR.x / TrBR.y)
    
    myDF.lai$C3G.abs.diff <- with(myDF.lai, C3G.x - C3G.y)
    myDF.lai$C3G.pct.diff <- with(myDF.lai, C3G.x / C3G.y)
    
    myDF.lai$C4G.abs.diff <- with(myDF.lai, C4G.x - C4G.y)
    myDF.lai$C4G.pct.diff <- with(myDF.lai, C4G.x / C4G.y)
    
    myDF.lai$Total.abs.diff <- with(myDF.lai, Total.x - Total.y)
    myDF.lai$Total.pct.diff <- with(myDF.lai, Total.x / Total.y)
    
    ### cpool
    myDF.cpool$VegC.abs.diff <- with(myDF.cpool, VegC.x - VegC.y)
    myDF.cpool$VegC.pct.diff <- with(myDF.cpool, VegC.x / VegC.y)
    
    myDF.cpool$LitterC.abs.diff <- with(myDF.cpool, LitterC.x - LitterC.y)
    myDF.cpool$LitterC.pct.diff <- with(myDF.cpool, LitterC.x / LitterC.y)
    
    myDF.cpool$SoilC.abs.diff <- with(myDF.cpool, SoilC.x - SoilC.y)
    myDF.cpool$SoilC.pct.diff <- with(myDF.cpool, SoilC.x / SoilC.y)
    
    myDF.cpool$Total.abs.diff <- with(myDF.cpool, Total.x - Total.y)
    myDF.cpool$Total.pct.diff <- with(myDF.cpool, Total.x / Total.y)
    
    
    ### clean by filtering out incomplete data
    myDF.cpool[myDF.cpool=="NAN"] <- NA
    myDF.lai[myDF.lai=="NAN"] <- NA
    myDF.cpool[sapply(myDF.cpool, is.infinite)] <- NA
    myDF.lai[sapply(myDF.lai, is.infinite)] <- NA
    
    
    ### check temporal pattern, ignore spatial variability
    sumDF1 <- summaryBy(VegC.pct.diff+LitterC.pct.diff+SoilC.pct.diff+Total.pct.diff~Year,
                         FUN=c(mean, sd), data=myDF.cpool, na.rm=T, keep.names=T)
    
    sumDF2 <- melt(setDT(sumDF1), id.vars = c("Year"),
                 measure.vars = 2:5)
    
    sumDF3 <- melt(setDT(sumDF1), id.vars = c("Year"),
                   measure.vars = 6:9)
    
    sumDF2$sd <- sumDF3$value
    sumDF2$variable <- gsub(".pct.diff.mean", "", sumDF2$variable)
    
    colnames(sumDF2) <- c("Year", "Cpool", "mean.val", "sd.val")
    
    ### Plot 100-yr average results
    p1 <- ggplot() + 
        geom_point(data=sumDF2, aes(x=Year, y=mean.val, fill=Cpool), pch = 21, size=3) +
        geom_errorbar(data=sumDF2, aes(x=Year, ymin=mean.val-sd.val, ymax=mean.val+sd.val, 
                                       col=Cpool)) +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        theme_linedraw() +
        ylab("CO2 effect (ele/amb) on C pools")+
        ylim(0.5, 2.0)
    
    
    ### check temporal pattern, ignore spatial variability
    sumDF1 <- summaryBy(TeNE.pct.diff+TeBS.pct.diff+IBS.pct.diff+TeBE.pct.diff+TrBE.pct.diff+TrIBE.pct.diff+TrBR.pct.diff+C3G.pct.diff+C4G.pct.diff~Year,
                        FUN=c(mean, sd), data=myDF.lai, na.rm=T, keep.names=T)
    
    sumDF2 <- melt(setDT(sumDF1), id.vars = c("Year"),
                   measure.vars = 2:10)
    
    sumDF3 <- melt(setDT(sumDF1), id.vars = c("Year"),
                   measure.vars = 11:19)
    
    sumDF2$sd <- sumDF3$value
    sumDF2$variable <- gsub(".pct.diff.mean", "", sumDF2$variable)
    
    colnames(sumDF2) <- c("Year", "PFT", "mean.val", "sd.val")
    
    ### Plot 100-yr average results
    p2 <- ggplot() + 
        geom_point(data=sumDF2, aes(x=Year, y=mean.val, fill=PFT), pch = 21, size=3) +
        #geom_errorbar(data=sumDF2, aes(x=Year, ymin=mean.val-sd.val, ymax=mean.val+sd.val, 
        #                               col=PFT)) +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        theme_linedraw() +
        ylab("CO2 effect (ele/amb) on LAI")+
        ylim(0.5, 5.0)
    
    
    pdf("output/climate_sensitivity/cpool_lai_by_year.pdf", width = 6, height = 8)
    plot(p1)
    plot(p2)
    dev.off()
    
    ### prepare grid ID
    gridDF <- subset(myDF.lai, Year == "1901")
    gridDF$GridID <- c(1:length(gridDF$Lon))
    gridDF <- gridDF[,c("Lon", "Lat", "GridID")]
    
    myDF.lai <- merge(myDF.lai, gridDF, by=c("Lon", "Lat"))
    
    lon.list <- unique(gridDF$Lon)
    lat.list <- unique(gridDF$Lat)
    xlim.range <- range(gridDF$Lon)
    ylim.range <- range(gridDF$Lat)
    
    ### perform linear regression at each grid location for different PFT
    ## Total 
    my.lm1 = function(myDF.lai) {
        tryCatch(coef(lm(Total.pct.diff ~ Year, data=myDF.lai)),error=function(e) NULL)
    }
    my.lm2 = function(myDF.lai) {
        tryCatch(summary(lm(Total.pct.diff ~ Year, data=myDF.lai))$coefficients[,4],error=function(e) NULL)
    }
    
    tmpDF = split(myDF.lai, f=myDF.lai$GridID)
    coefDF1 = sapply(tmpDF, FUN=my.lm1)
    coefDF2 = sapply(tmpDF, FUN=my.lm2)
    gridDF$Total_int <- coefDF1[1,]
    gridDF$Total_slp <- coefDF1[2,]
    gridDF$Total_pval <- coefDF2[2,]
    
    
    ## TeNE
    my.lm1 = function(myDF.lai) {
        tryCatch(coef(lm(TeNE.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[1],error=function(e) NULL)
    }
    my.lm2 = function(myDF.lai) {
        tryCatch(coef(lm(TeNE.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[2],error=function(e) NULL)
    }
    my.lm3 = function(myDF.lai) {
        tryCatch(summary(lm(TeNE.pct.diff ~ Year, data=myDF.lai))$coefficients[2,4],error=function(e) NULL)
    }
    
    tmpDF = split(myDF.lai, f=myDF.lai$GridID)
    coefDF1 = sapply(tmpDF, FUN=my.lm1)
    coefDF2 = sapply(tmpDF, FUN=my.lm2)
    coefDF3 = sapply(tmpDF, FUN=my.lm3)
    
    coefDF1[sapply(coefDF1, is.null)] <- NA
    coefDF2[sapply(coefDF2, is.null)] <- NA
    coefDF3[sapply(coefDF3, is.null)] <- NA
    
    gridDF$TeNE_int <- as.numeric(unlist(coefDF1))
    gridDF$TeNE_slp <- as.numeric(unlist(coefDF2))
    gridDF$TeNE_pval <- as.numeric(unlist(coefDF3))
    
    
    ## TeBS
    my.lm1 = function(myDF.lai) {
        tryCatch(coef(lm(TeBS.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[1],error=function(e) NULL)
    }
    my.lm2 = function(myDF.lai) {
        tryCatch(coef(lm(TeBS.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[2],error=function(e) NULL)
    }
    my.lm3 = function(myDF.lai) {
        tryCatch(summary(lm(TeBS.pct.diff ~ Year, data=myDF.lai))$coefficients[2,4],error=function(e) NULL)
    }
    
    tmpDF = split(myDF.lai, f=myDF.lai$GridID)
    coefDF1 = sapply(tmpDF, FUN=my.lm1)
    coefDF2 = sapply(tmpDF, FUN=my.lm2)
    coefDF3 = sapply(tmpDF, FUN=my.lm3)
    
    coefDF1[sapply(coefDF1, is.null)] <- NA
    coefDF2[sapply(coefDF2, is.null)] <- NA
    coefDF3[sapply(coefDF3, is.null)] <- NA
    
    gridDF$TeBS_int <- as.numeric(unlist(coefDF1))
    gridDF$TeBS_slp <- as.numeric(unlist(coefDF2))
    gridDF$TeBS_pval <- as.numeric(unlist(coefDF3))
    
    
    ## IBS
    my.lm1 = function(myDF.lai) {
        tryCatch(coef(lm(IBS.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[1],error=function(e) NULL)
    }
    my.lm2 = function(myDF.lai) {
        tryCatch(coef(lm(IBS.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[2],error=function(e) NULL)
    }
    my.lm3 = function(myDF.lai) {
        tryCatch(summary(lm(IBS.pct.diff ~ Year, data=myDF.lai))$coefficients[2,4],error=function(e) NULL)
    }
    
    tmpDF = split(myDF.lai, f=myDF.lai$GridID)
    coefDF1 = sapply(tmpDF, FUN=my.lm1)
    coefDF2 = sapply(tmpDF, FUN=my.lm2)
    coefDF3 = sapply(tmpDF, FUN=my.lm3)
    
    coefDF1[sapply(coefDF1, is.null)] <- NA
    coefDF2[sapply(coefDF2, is.null)] <- NA
    coefDF3[sapply(coefDF3, is.null)] <- NA
    
    gridDF$IBS_int <- as.numeric(unlist(coefDF1))
    gridDF$IBS_slp <- as.numeric(unlist(coefDF2))
    gridDF$IBS_pval <- as.numeric(unlist(coefDF3))
    
    
    ## TeBE 
    my.lm1 = function(myDF.lai) {
        tryCatch(coef(lm(TeBE.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[1],error=function(e) NULL)
    }
    my.lm2 = function(myDF.lai) {
        tryCatch(coef(lm(TeBE.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[2],error=function(e) NULL)
    }
    my.lm3 = function(myDF.lai) {
        tryCatch(summary(lm(TeBE.pct.diff ~ Year, data=myDF.lai))$coefficients[2,4],error=function(e) NULL)
    }
    
    tmpDF = split(myDF.lai, f=myDF.lai$GridID)
    coefDF1 = sapply(tmpDF, FUN=my.lm1)
    coefDF2 = sapply(tmpDF, FUN=my.lm2)
    coefDF3 = sapply(tmpDF, FUN=my.lm3)
    
    coefDF1[sapply(coefDF1, is.null)] <- NA
    coefDF2[sapply(coefDF2, is.null)] <- NA
    coefDF3[sapply(coefDF3, is.null)] <- NA
    
    gridDF$TeBE_int <- as.numeric(unlist(coefDF1))
    gridDF$TeBE_slp <- as.numeric(unlist(coefDF2))
    gridDF$TeBE_pval <- as.numeric(unlist(coefDF3))
    
    
    ## TrBE
    my.lm1 = function(myDF.lai) {
        tryCatch(coef(lm(TrBE.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[1],error=function(e) NULL)
    }
    my.lm2 = function(myDF.lai) {
        tryCatch(coef(lm(TrBE.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[2],error=function(e) NULL)
    }
    my.lm3 = function(myDF.lai) {
        tryCatch(summary(lm(TrBE.pct.diff ~ Year, data=myDF.lai))$coefficients[2,4],error=function(e) NULL)
    }
    
    tmpDF = split(myDF.lai, f=myDF.lai$GridID)
    coefDF1 = sapply(tmpDF, FUN=my.lm1)
    coefDF2 = sapply(tmpDF, FUN=my.lm2)
    coefDF3 = sapply(tmpDF, FUN=my.lm3)
    
    coefDF1[sapply(coefDF1, is.null)] <- NA
    coefDF2[sapply(coefDF2, is.null)] <- NA
    coefDF3[sapply(coefDF3, is.null)] <- NA
    
    gridDF$TrBE_int <- as.numeric(unlist(coefDF1))
    gridDF$TrBE_slp <- as.numeric(unlist(coefDF2))
    gridDF$TrBE_pval <- as.numeric(unlist(coefDF3))
    
    
    ## TrIBE
    my.lm1 = function(myDF.lai) {
        tryCatch(coef(lm(TrIBE.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[1],error=function(e) NULL)
    }
    my.lm2 = function(myDF.lai) {
        tryCatch(coef(lm(TrIBE.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[2],error=function(e) NULL)
    }
    my.lm3 = function(myDF.lai) {
        tryCatch(summary(lm(TrIBE.pct.diff ~ Year, data=myDF.lai))$coefficients[2,4],error=function(e) NULL)
    }
    
    tmpDF = split(myDF.lai, f=myDF.lai$GridID)
    coefDF1 = sapply(tmpDF, FUN=my.lm1)
    coefDF2 = sapply(tmpDF, FUN=my.lm2)
    coefDF3 = sapply(tmpDF, FUN=my.lm3)
    
    coefDF1[sapply(coefDF1, is.null)] <- NA
    coefDF2[sapply(coefDF2, is.null)] <- NA
    coefDF3[sapply(coefDF3, is.null)] <- NA
    
    gridDF$TrIBE_int <- as.numeric(unlist(coefDF1))
    gridDF$TrIBE_slp <- as.numeric(unlist(coefDF2))
    gridDF$TrIBE_pval <- as.numeric(unlist(coefDF3))
    
    ## TrBR
    my.lm1 = function(myDF.lai) {
        tryCatch(coef(lm(TrBR.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[1],error=function(e) NULL)
    }
    my.lm2 = function(myDF.lai) {
        tryCatch(coef(lm(TrBR.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[2],error=function(e) NULL)
    }
    my.lm3 = function(myDF.lai) {
        tryCatch(summary(lm(TrBR.pct.diff ~ Year, data=myDF.lai))$coefficients[2,4],error=function(e) NULL)
    }
    
    tmpDF = split(myDF.lai, f=myDF.lai$GridID)
    coefDF1 = sapply(tmpDF, FUN=my.lm1)
    coefDF2 = sapply(tmpDF, FUN=my.lm2)
    coefDF3 = sapply(tmpDF, FUN=my.lm3)
    
    coefDF1[sapply(coefDF1, is.null)] <- NA
    coefDF2[sapply(coefDF2, is.null)] <- NA
    coefDF3[sapply(coefDF3, is.null)] <- NA
    
    gridDF$TrBR_int <- as.numeric(unlist(coefDF1))
    gridDF$TrBR_slp <- as.numeric(unlist(coefDF2))
    gridDF$TrBR_pval <- as.numeric(unlist(coefDF3))
    
    
    ## C3G
    my.lm1 = function(myDF.lai) {
        tryCatch(coef(lm(C3G.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[1],error=function(e) NULL)
    }
    my.lm2 = function(myDF.lai) {
        tryCatch(coef(lm(C3G.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[2],error=function(e) NULL)
    }
    my.lm3 = function(myDF.lai) {
        tryCatch(summary(lm(C3G.pct.diff ~ Year, data=myDF.lai))$coefficients[2,4],error=function(e) NULL)
    }
    
    tmpDF = split(myDF.lai, f=myDF.lai$GridID)
    coefDF1 = sapply(tmpDF, FUN=my.lm1)
    coefDF2 = sapply(tmpDF, FUN=my.lm2)
    coefDF3 = sapply(tmpDF, FUN=my.lm3)
    
    coefDF1[sapply(coefDF1, is.null)] <- NA
    coefDF2[sapply(coefDF2, is.null)] <- NA
    coefDF3[sapply(coefDF3, is.null)] <- NA
    
    gridDF$C3G_int <- as.numeric(unlist(coefDF1))
    gridDF$C3G_slp <- as.numeric(unlist(coefDF2))
    gridDF$C3G_pval <- as.numeric(unlist(coefDF3))
    
    
    ## C4G
    my.lm1 = function(myDF.lai) {
        tryCatch(coef(lm(C4G.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[1],error=function(e) NULL)
    }
    my.lm2 = function(myDF.lai) {
        tryCatch(coef(lm(C4G.pct.diff ~ Year, data=myDF.lai, na.action = na.omit))[2],error=function(e) NULL)
    }
    my.lm3 = function(myDF.lai) {
        tryCatch(summary(lm(C4G.pct.diff ~ Year, data=myDF.lai))$coefficients[2,4],error=function(e) NULL)
    }
    
    tmpDF = split(myDF.lai, f=myDF.lai$GridID)
    coefDF1 = sapply(tmpDF, FUN=my.lm1)
    coefDF2 = sapply(tmpDF, FUN=my.lm2)
    coefDF3 = sapply(tmpDF, FUN=my.lm3)
    
    coefDF1[sapply(coefDF1, is.null)] <- NA
    coefDF2[sapply(coefDF2, is.null)] <- NA
    coefDF3[sapply(coefDF3, is.null)] <- NA
    
    gridDF$C4G_int <- as.numeric(unlist(coefDF1))
    gridDF$C4G_slp <- as.numeric(unlist(coefDF2))
    gridDF$C4G_pval <- as.numeric(unlist(coefDF3))
    
    
    ### plotting
    p1 <- ggplot(data=gridDF[gridDF$Total_pval<=0.05,]) + 
        geom_tile(aes(y=Lat, x=Lon, fill=Total_slp)) +
        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
        borders("world", col="grey", lwd=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_steps2(name=paste0("Total slope"),
                         low = "red",
                         mid = "white",
                         high = "blue",
                         midpoint = 0,
                         na.value = 'white')
    
    p2 <- ggplot(data=gridDF[gridDF$TeNE_pval<=0.05,]) + 
        geom_tile(aes(y=Lat, x=Lon, fill=TeNE_slp)) +
        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
        borders("world", col="grey", lwd=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_steps2(name=paste0("TeNE slope"),
                          low = "red",
                          mid = "white",
                          high = "blue",
                          midpoint = 0,
                          na.value = 'white')
    
    
    p3 <- ggplot(data=gridDF[gridDF$TeBS_pval<=0.05,]) + 
        geom_tile(aes(y=Lat, x=Lon, fill=TeBS_slp)) +
        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
        borders("world", col="grey", lwd=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_steps2(name=paste0("TeBS slope"),
                          low = "red",
                          mid = "white",
                          high = "blue",
                          midpoint = 0,
                          na.value = 'white')
    
    
    p4 <- ggplot(data=gridDF[gridDF$IBS_pval<=0.05,]) + 
        geom_tile(aes(y=Lat, x=Lon, fill=IBS_slp)) +
        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
        borders("world", col="grey", lwd=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_steps2(name=paste0("IBS slope"),
                          low = "red",
                          mid = "white",
                          high = "blue",
                          midpoint = 0,
                          na.value = 'white')
    
    
    p5 <- ggplot(data=gridDF[gridDF$TeBE_pval<=0.05,]) + 
        geom_tile(aes(y=Lat, x=Lon, fill=TeBE_slp)) +
        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
        borders("world", col="grey", lwd=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_steps2(name=paste0("TeBE slope"),
                          low = "red",
                          mid = "white",
                          high = "blue",
                          midpoint = 0,
                          na.value = 'white')
    
    
    p6 <- ggplot(data=gridDF[gridDF$TrBE_pval<=0.05,]) + 
        geom_tile(aes(y=Lat, x=Lon, fill=TrBE_slp)) +
        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
        borders("world", col="grey", lwd=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_steps2(name=paste0("TrBE slope"),
                          low = "red",
                          mid = "white",
                          high = "blue",
                          midpoint = 0,
                          na.value = 'white')
    
    
    p7 <- ggplot(data=gridDF[gridDF$TrIBE_pval<=0.05,]) + 
        geom_tile(aes(y=Lat, x=Lon, fill=TrIBE_slp)) +
        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
        borders("world", col="grey", lwd=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_steps2(name=paste0("TrIBE slope"),
                          low = "red",
                          mid = "white",
                          high = "blue",
                          midpoint = 0,
                          na.value = 'white')
    
    
    p8 <- ggplot(data=gridDF[gridDF$TrBR_pval<=0.05,]) + 
        geom_tile(aes(y=Lat, x=Lon, fill=TrBR_slp)) +
        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
        borders("world", col="grey", lwd=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_steps2(name=paste0("TrBR slope"),
                          low = "red",
                          mid = "white",
                          high = "blue",
                          midpoint = 0,
                          na.value = 'white')
    
    
    p9 <- ggplot(data=gridDF[gridDF$C3G_pval<=0.05,]) + 
        geom_tile(aes(y=Lat, x=Lon, fill=C3G_slp)) +
        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
        borders("world", col="grey", lwd=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_steps2(name=paste0("C3G slope"),
                          low = "red",
                          mid = "white",
                          high = "blue",
                          midpoint = 0,
                          na.value = 'white')
    
    p10 <- ggplot(data=gridDF[gridDF$C4G_pval<=0.05&!is.na(gridDF$C4G_slp),]) + 
        geom_tile(aes(y=Lat, x=Lon, fill=C4G_slp)) +
        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
        borders("world", col="grey", lwd=0.2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "right")+
        scale_fill_steps2(name=paste0("C4G slope"),
                          low = "red",
                          mid = "white",
                          high = "blue",
                          midpoint = 0,
                          na.value = 'white')
    
    combined_plot <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
                               ncol=4, align="vh", axis = "l")
    
    save_plot(paste0("output/climate_sensitivity/CO2_response_slope_by_PFT.pdf"),
              combined_plot, base_width=20, base_height = 20)
    
    
    
    
    ### PFT specific patterns
    ### read in individual output dataset
        
        
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
        
        

    
    
    
}
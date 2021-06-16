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
    
    
    
    ########################################################################
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
    
    
    ########################################################################
    
    ### simulation list
    sim.list <- paste0("run", c(1:20))
    
    ### files under variable climate forcing
    indiv.files1 <- paste0("input/withfire/run", c(1:20), "/indiv.out")
    indiv.files2 <- paste0("input/run", c(1:20), "/fixCO2varT/indiv.out")
    
    ### read input files
    ### the input datasets are for 2005 - 2015, for each individual within each patch within each grid and year.
    
    myDF1 = indiv.files1 %>% 
        purrr::map_df(~read.table(.,header=T))
    
    myDF2 = indiv.files2 %>% 
        purrr::map_df(~read.table(.,header=T))
    

    ### does eCO2 affect patch age?
    test1 <- subset(myDF1, Year == 2015)
    test2 <- subset(myDF2, Year == 2015)
    
    p1 <- ggplot()+
        geom_density(data=test1, aes(x=Page), col="black")+
        geom_density(data=test2, aes(x=Page), col="red")
    plot(p1)
    
    
    
    ### does eCO2 affect individual plant age? What is the PFT-specific pattern?
    gridDF <- test1[,c("Lon", "Lat")]
    gridDF <- gridDF[!duplicated(gridDF[c(1,2)]),]
    gridDF$SiteID <- c(1:length(gridDF$Lon))
    
    
    p2 <- ggplot()+
        geom_density(data=test1[test1$PFT==6,], aes(x=Iage), col="black")+
        geom_density(data=test2[test2$PFT==6,], aes(x=Iage), col="red")
    plot(p2)
    
    dens1 <- density(test1$Iage)
    loc1 <- which.max(dens1$y)
    iage1 <- dens1$x[loc1]
    
    dens2 <- density(test2$Iage)
    loc2 <- which.max(dens2$y)
    iage2 <- dens2$x[loc2]
    
    p2 <- ggplot()+
        geom_density(data=test1, aes(x=Iage), col="black")+
        geom_density(data=test2, aes(x=Iage), col="red")
    plot(p2)
    
    ### does eCO2 affect individual tree height, density? What is the PFT-specific pattern?
    
        

    
    
    
}
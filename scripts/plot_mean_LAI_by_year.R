plot_mean_LAI_by_year <- function(myDF) {
    
    ### Investigate distributions over space
    spDF <- myDF
    
    ### convert 0 to NA
    spDF[spDF == 0] <- NA
    
    ### convert all LAI < 0.1 to 0.1
    tDF <- spDF[,4:16]
    tDF[tDF <= 0.1] <- 0.1
    spDF[, 4:16] <- tDF
    
    ## set lim of color in log scale
    min.lim <- log(0.1)
    max.lim <- log(max(spDF$Total, na.rm=T))
    
    subDF <- spDF[spDF$Year%in%c(1901,2015),]
    subDF$Lon <- NULL
    subDF$Lat <- NULL
    
    sumDF <- summaryBy(.~Year, FUN=c(mean, sd), data=subDF,
                       na.rm=T, keep.names=T)
    
    write.csv(sumDF, "output/static/mean_LAI_by_year_PFT.csv", row.names=F)
    
    
    ### prepare an outDF
    outDF <- data.frame("Year"=rep(c(1905.5, 2010.5), each=10),
                        "PFT"=rep(c("TeNE", "TeBS", "IBS", "TeBE",
                                    "TrBE", "TrIBE", "TrBR", "C3G", "C4G",
                                    "Total"), 2),
                        "overall_LAI_mean" = NA, 
                        "overall_LAI_sd" = NA, 
                        "overall_LAI_n" = NA,
                        "expansion_LAI_mean" = NA,
                        "expansion_LAI_sd" = NA,
                        "expansion_LAI_n" = NA)
    
    
    ### look at expansion and change in LAI
    subDF1 <- spDF[spDF$Year%in%c(1901:1910),]
    subDF2 <- spDF[spDF$Year%in%c(2006:2015),]
    
    sumDF1 <- summaryBy(.~Lon+Lat, FUN=mean, na.rm=T, keep.names=T, data=subDF1)
    sumDF2 <- summaryBy(.~Lon+Lat, FUN=mean, na.rm=T, keep.names=T, data=subDF2)
    
    ### merge the two
    pDF <- merge(sumDF1, sumDF2, by=c("Lon", "Lat"), all=T)
    
    ### get presence absence information
    pDF$TeNE.x.p <- ifelse(is.nan(pDF$TeNE.x), "absence", "presence")
    pDF$TeNE.y.p <- ifelse(is.nan(pDF$TeNE.y), "absence", "presence")
    
    pDF$TeBS.x.p <- ifelse(is.nan(pDF$TeBS.x), "absence", "presence")
    pDF$TeBS.y.p <- ifelse(is.nan(pDF$TeBS.y), "absence", "presence")
    
    pDF$IBS.x.p <- ifelse(is.nan(pDF$IBS.x), "absence", "presence")
    pDF$IBS.y.p <- ifelse(is.nan(pDF$IBS.y), "absence", "presence")
    
    pDF$TeBE.x.p <- ifelse(is.nan(pDF$TeBE.x), "absence", "presence")
    pDF$TeBE.y.p <- ifelse(is.nan(pDF$TeBE.y), "absence", "presence")
    
    
    pDF$TrBE.x.p <- ifelse(is.nan(pDF$TrBE.x), "absence", "presence")
    pDF$TrBE.y.p <- ifelse(is.nan(pDF$TrBE.y), "absence", "presence")
    
    pDF$TrIBE.x.p <- ifelse(is.nan(pDF$TrIBE.x), "absence", "presence")
    pDF$TrIBE.y.p <- ifelse(is.nan(pDF$TrIBE.y), "absence", "presence")
    
    pDF$TrBR.x.p <- ifelse(is.nan(pDF$TrBR.x), "absence", "presence")
    pDF$TrBR.y.p <- ifelse(is.nan(pDF$TrBR.y), "absence", "presence")
    
    pDF$C3G.x.p <- ifelse(is.nan(pDF$C3G.x), "absence", "presence")
    pDF$C3G.y.p <- ifelse(is.nan(pDF$C3G.y), "absence", "presence")
    
    pDF$C4G.x.p <- ifelse(is.nan(pDF$C4G.x), "absence", "presence")
    pDF$C4G.y.p <- ifelse(is.nan(pDF$C4G.y), "absence", "presence")
    
    pDF$Total.x.p <- ifelse(is.nan(pDF$Total.x), "absence", "presence")
    pDF$Total.y.p <- ifelse(is.nan(pDF$Total.y), "absence", "presence")
    
    
    ### add presence, absence, shrinking and expansion information to each PFT
    pDF$TeNE.p <- ifelse(pDF$TeNE.x.p == pDF$TeNE.y.p & pDF$TeNE.x.p=="presence", 
                         "presence", 
                         ifelse(pDF$TeNE.x.p != pDF$TeNE.y.p & pDF$TeNE.x.p=="absence", 
                                "expansion", 
                                ifelse(pDF$TeNE.x.p != pDF$TeNE.y.p & pDF$TeNE.x.p=="absence",
                                       "shrinking", "absence")))
    
    pDF$TeBS.p <- ifelse(pDF$TeBS.x.p == pDF$TeBS.y.p & pDF$TeBS.x.p=="presence", 
                         "presence", 
                         ifelse(pDF$TeBS.x.p != pDF$TeBS.y.p & pDF$TeBS.x.p=="absence", 
                                "expansion", 
                                ifelse(pDF$TeBS.x.p != pDF$TeBS.y.p & pDF$TeBS.x.p=="absence",
                                       "shrinking", "absence")))
    
    pDF$IBS.p <- ifelse(pDF$IBS.x.p == pDF$IBS.y.p & pDF$IBS.x.p=="presence", 
                         "presence", 
                         ifelse(pDF$IBS.x.p != pDF$IBS.y.p & pDF$IBS.x.p=="absence", 
                                "expansion", 
                                ifelse(pDF$IBS.x.p != pDF$IBS.y.p & pDF$IBS.x.p=="absence",
                                       "shrinking", "absence")))
    
    pDF$TeBE.p <- ifelse(pDF$TeBE.x.p == pDF$TeBE.y.p & pDF$TeBE.x.p=="presence", 
                         "presence", 
                         ifelse(pDF$TeBE.x.p != pDF$TeBE.y.p & pDF$TeBE.x.p=="absence", 
                                "expansion", 
                                ifelse(pDF$TeBE.x.p != pDF$TeBE.y.p & pDF$TeBE.x.p=="absence",
                                       "shrinking", "absence")))
    
    pDF$TrBE.p <- ifelse(pDF$TrBE.x.p == pDF$TrBE.y.p & pDF$TrBE.x.p=="presence", 
                         "presence", 
                         ifelse(pDF$TrBE.x.p != pDF$TrBE.y.p & pDF$TrBE.x.p=="absence", 
                                "expansion", 
                                ifelse(pDF$TrBE.x.p != pDF$TrBE.y.p & pDF$TrBE.x.p=="absence",
                                       "shrinking", "absence")))
    
    pDF$TrIBE.p <- ifelse(pDF$TrIBE.x.p == pDF$TrIBE.y.p & pDF$TrIBE.x.p=="presence", 
                         "presence", 
                         ifelse(pDF$TrIBE.x.p != pDF$TrIBE.y.p & pDF$TrIBE.x.p=="absence", 
                                "expansion", 
                                ifelse(pDF$TrIBE.x.p != pDF$TrIBE.y.p & pDF$TrIBE.x.p=="absence",
                                       "shrinking", "absence")))
    
    pDF$TrBR.p <- ifelse(pDF$TrBR.x.p == pDF$TrBR.y.p & pDF$TrBR.x.p=="presence", 
                         "presence", 
                         ifelse(pDF$TrBR.x.p != pDF$TrBR.y.p & pDF$TrBR.x.p=="absence", 
                                "expansion", 
                                ifelse(pDF$TrBR.x.p != pDF$TrBR.y.p & pDF$TrBR.x.p=="absence",
                                       "shrinking", "absence")))
    
    pDF$C3G.p <- ifelse(pDF$C3G.x.p == pDF$C3G.y.p & pDF$C3G.x.p=="presence", 
                         "presence", 
                         ifelse(pDF$C3G.x.p != pDF$C3G.y.p & pDF$C3G.x.p=="absence", 
                                "expansion", 
                                ifelse(pDF$C3G.x.p != pDF$C3G.y.p & pDF$C3G.x.p=="absence",
                                       "shrinking", "absence")))
    
    pDF$C4G.p <- ifelse(pDF$C4G.x.p == pDF$C4G.y.p & pDF$C4G.x.p=="presence", 
                         "presence", 
                         ifelse(pDF$C4G.x.p != pDF$C4G.y.p & pDF$C4G.x.p=="absence", 
                                "expansion", 
                                ifelse(pDF$C4G.x.p != pDF$C4G.y.p & pDF$C4G.x.p=="absence",
                                       "shrinking", "absence")))
    
    pDF$Total.p <- ifelse(pDF$Total.x.p == pDF$Total.y.p & pDF$Total.x.p=="presence", 
                         "presence", 
                         ifelse(pDF$Total.x.p != pDF$Total.y.p & pDF$Total.x.p=="absence", 
                                "expansion", 
                                ifelse(pDF$Total.x.p != pDF$Total.y.p & pDF$Total.x.p=="absence",
                                       "shrinking", "absence")))
    
    
    ### calculate total means
    tmpDF1 <- pDF[,c("Lon", "Lat", "Year.x", "TeNE.x", "TeBS.x", "IBS.x",
                     "TeBE.x", "TrBE.x", "TrIBE.x",
                     "TrBR.x", "C3G.x", "C4G.x",
                     "Total.x")]
    tmpDF2 <- pDF[,c("Lon", "Lat", "Year.y", "TeNE.y", "TeBS.y", "IBS.y",
                     "TeBE.y", "TrBE.y", "TrIBE.y",
                     "TrBR.y", "C3G.y", "C4G.y",
                     "Total.y")]
    tmpDF3 <- pDF[,c("Lon", "Lat", "Year.x", "TeNE.x.p", "TeBS.x.p", "IBS.x.p",
                     "TeBE.x.p", "TrBE.x.p", "TrIBE.x.p",
                     "TrBR.x.p", "C3G.x.p", "C4G.x.p",
                     "Total.x.p")]
    tmpDF4 <- pDF[,c("Lon", "Lat", "Year.y", "TeNE.y.p", "TeBS.y.p", "IBS.y.p",
                     "TeBE.y.p", "TrBE.y.p", "TrIBE.y.p",
                     "TrBR.y.p", "C3G.y.p", "C4G.y.p",
                     "Total.y.p")]
    colnames(tmpDF1) <- colnames(tmpDF2) <- c("Lon", "Lat", "Year", "TeNE", "TeBS", "IBS",
                                              "TeBE", "TrBE", "TrIBE",
                                              "TrBR", "C3G", "C4G",
                                              "Total")
    colnames(tmpDF3) <- colnames(tmpDF4) <- c("Lon", "Lat", "Year", "TeNE", "TeBS", "IBS",
                                              "TeBE", "TrBE", "TrIBE",
                                              "TrBR", "C3G", "C4G",
                                              "Total")
        
    mgDF1 <- rbind(tmpDF1, tmpDF2)    
    mgDF2 <- rbind(tmpDF3, tmpDF4)    
    
    mgDF2[mgDF2=="presence"] <- 1
    mgDF2[mgDF2=="absence"] <- 0
    
    mgDF2 <- as.data.frame(sapply(mgDF2, as.numeric))
    
    totDF1 <- summaryBy(.~Year, data=mgDF1, na.rm=T, keep.names=T, FUN=c(mean,sd))
    totDF2 <- summaryBy(TeNE+TeBS+IBS+TeBE+TrBE+TrIBE+TrBR+C3G+C4G+Total~Year, 
                        data=mgDF2, na.rm=T, keep.names=T, FUN=sum)
    
    
    for (i in c(1905.5, 2010.5)) {
        ### TeNE
        outDF$overall_LAI_mean[outDF$Year==i&outDF$PFT=="TeNE"] <- totDF1$TeNE.mean[totDF1$Year==i]
        outDF$overall_LAI_sd[outDF$Year==i&outDF$PFT=="TeNE"] <- totDF1$TeNE.sd[totDF1$Year==i]
        outDF$overall_LAI_n[outDF$Year==i&outDF$PFT=="TeNE"] <- totDF2$TeNE[totDF1$Year==i]
        
        ### TeBS
        outDF$overall_LAI_mean[outDF$Year==i&outDF$PFT=="TeBS"] <- totDF1$TeBS.mean[totDF1$Year==i]
        outDF$overall_LAI_sd[outDF$Year==i&outDF$PFT=="TeBS"] <- totDF1$TeBS.sd[totDF1$Year==i]
        outDF$overall_LAI_n[outDF$Year==i&outDF$PFT=="TeBS"] <- totDF2$TeBS[totDF1$Year==i]
        
        ### IBS
        outDF$overall_LAI_mean[outDF$Year==i&outDF$PFT=="IBS"] <- totDF1$IBS.mean[totDF1$Year==i]
        outDF$overall_LAI_sd[outDF$Year==i&outDF$PFT=="IBS"] <- totDF1$IBS.sd[totDF1$Year==i]
        outDF$overall_LAI_n[outDF$Year==i&outDF$PFT=="IBS"] <- totDF2$IBS[totDF1$Year==i]
        
        ### TeBE
        outDF$overall_LAI_mean[outDF$Year==i&outDF$PFT=="TeBE"] <- totDF1$TeBE.mean[totDF1$Year==i]
        outDF$overall_LAI_sd[outDF$Year==i&outDF$PFT=="TeBE"] <- totDF1$TeBE.sd[totDF1$Year==i]
        outDF$overall_LAI_n[outDF$Year==i&outDF$PFT=="TeBE"] <- totDF2$TeBE[totDF1$Year==i]
        
        ### TrBE
        outDF$overall_LAI_mean[outDF$Year==i&outDF$PFT=="TrBE"] <- totDF1$TrBE.mean[totDF1$Year==i]
        outDF$overall_LAI_sd[outDF$Year==i&outDF$PFT=="TrBE"] <- totDF1$TrBE.sd[totDF1$Year==i]
        outDF$overall_LAI_n[outDF$Year==i&outDF$PFT=="TrBE"] <- totDF2$TrBE[totDF1$Year==i]
        
        ### TrIBE
        outDF$overall_LAI_mean[outDF$Year==i&outDF$PFT=="TrIBE"] <- totDF1$TrIBE.mean[totDF1$Year==i]
        outDF$overall_LAI_sd[outDF$Year==i&outDF$PFT=="TrIBE"] <- totDF1$TrIBE.sd[totDF1$Year==i]
        outDF$overall_LAI_n[outDF$Year==i&outDF$PFT=="TrIBE"] <- totDF2$TrIBE[totDF1$Year==i]
        
        ### TrBR
        outDF$overall_LAI_mean[outDF$Year==i&outDF$PFT=="TrBR"] <- totDF1$TrBR.mean[totDF1$Year==i]
        outDF$overall_LAI_sd[outDF$Year==i&outDF$PFT=="TrBR"] <- totDF1$TrBR.sd[totDF1$Year==i]
        outDF$overall_LAI_n[outDF$Year==i&outDF$PFT=="TrBR"] <- totDF2$TrBR[totDF1$Year==i]
        
        ### C3G
        outDF$overall_LAI_mean[outDF$Year==i&outDF$PFT=="C3G"] <- totDF1$C3G.mean[totDF1$Year==i]
        outDF$overall_LAI_sd[outDF$Year==i&outDF$PFT=="C3G"] <- totDF1$C3G.sd[totDF1$Year==i]
        outDF$overall_LAI_n[outDF$Year==i&outDF$PFT=="C3G"] <- totDF2$C3G[totDF1$Year==i]
        
        ### C4G
        outDF$overall_LAI_mean[outDF$Year==i&outDF$PFT=="C4G"] <- totDF1$C4G.mean[totDF1$Year==i]
        outDF$overall_LAI_sd[outDF$Year==i&outDF$PFT=="C4G"] <- totDF1$C4G.sd[totDF1$Year==i]
        outDF$overall_LAI_n[outDF$Year==i&outDF$PFT=="C4G"] <- totDF2$C4G[totDF1$Year==i]
        
        ### Total
        outDF$overall_LAI_mean[outDF$Year==i&outDF$PFT=="Total"] <- totDF1$Total.mean[totDF1$Year==i]
        outDF$overall_LAI_sd[outDF$Year==i&outDF$PFT=="Total"] <- totDF1$Total.sd[totDF1$Year==i]
        outDF$overall_LAI_n[outDF$Year==i&outDF$PFT=="Total"] <- totDF2$Total[totDF1$Year==i]
        
        
    }
    
    
    ### only presence for each species
    ##TeNE
    pDF1 <- subset(pDF, TeNE.p == "expansion")

    tmpDF1 <- pDF1[,c("Lon", "Lat", "Year.y", "TeNE.y")]
    tmpDF2 <- pDF1[,c("Lon", "Lat", "Year.y", "TeNE.p")]
    
    colnames(tmpDF1) <- c("Lon", "Lat", "Year", "LAI")
    colnames(tmpDF2) <- c("Lon", "Lat", "Year", "count")
    
    tmpDF2$count <- ifelse(tmpDF2$count == "expansion", 1, 0) 
    tmpDF2$count <- as.numeric(tmpDF2$count)
    
    totDF1 <- summaryBy(LAI~Year, data=tmpDF1, na.rm=T, keep.names=T, FUN=c(mean,sd))
    outDF$expansion_LAI_n[outDF$Year==2010.5&outDF$PFT=="TeNE"] <- sum(tmpDF2$count)
    outDF$expansion_LAI_mean[outDF$Year==2010.5&outDF$PFT=="TeNE"] <- totDF1$LAI.mean
    outDF$expansion_LAI_sd[outDF$Year==2010.5&outDF$PFT=="TeNE"] <- totDF1$LAI.sd
    
    
    ### TeBS
    pDF1 <- subset(pDF, TeBS.p == "expansion")
    
    tmpDF1 <- pDF1[,c("Lon", "Lat", "Year.y", "TeBS.y")]
    tmpDF2 <- pDF1[,c("Lon", "Lat", "Year.y", "TeBS.p")]
    
    colnames(tmpDF1) <- c("Lon", "Lat", "Year", "LAI")
    colnames(tmpDF2) <- c("Lon", "Lat", "Year", "count")
    
    tmpDF2$count <- ifelse(tmpDF2$count == "expansion", 1, 0) 
    tmpDF2$count <- as.numeric(tmpDF2$count)
    
    totDF1 <- summaryBy(LAI~Year, data=tmpDF1, na.rm=T, keep.names=T, FUN=c(mean,sd))
    outDF$expansion_LAI_n[outDF$Year==2010.5&outDF$PFT=="TeBS"] <- sum(tmpDF2$count)
    outDF$expansion_LAI_mean[outDF$Year==2010.5&outDF$PFT=="TeBS"] <- totDF1$LAI.mean
    outDF$expansion_LAI_sd[outDF$Year==2010.5&outDF$PFT=="TeBS"] <- totDF1$LAI.sd
    
    
    
    ### IBS
    pDF1 <- subset(pDF, IBS.p == "expansion")
    
    tmpDF1 <- pDF1[,c("Lon", "Lat", "Year.y", "IBS.y")]
    tmpDF2 <- pDF1[,c("Lon", "Lat", "Year.y", "IBS.p")]
    
    colnames(tmpDF1) <- c("Lon", "Lat", "Year", "LAI")
    colnames(tmpDF2) <- c("Lon", "Lat", "Year", "count")
    
    tmpDF2$count <- ifelse(tmpDF2$count == "expansion", 1, 0) 
    tmpDF2$count <- as.numeric(tmpDF2$count)
    
    totDF1 <- summaryBy(LAI~Year, data=tmpDF1, na.rm=T, keep.names=T, FUN=c(mean,sd))
    outDF$expansion_LAI_n[outDF$Year==2010.5&outDF$PFT=="IBS"] <- sum(tmpDF2$count)
    outDF$expansion_LAI_mean[outDF$Year==2010.5&outDF$PFT=="IBS"] <- totDF1$LAI.mean
    outDF$expansion_LAI_sd[outDF$Year==2010.5&outDF$PFT=="IBS"] <- totDF1$LAI.sd
    
    
    ### TeBE
    pDF1 <- subset(pDF, TeBE.p == "expansion")
    
    tmpDF1 <- pDF1[,c("Lon", "Lat", "Year.y", "TeBE.y")]
    tmpDF2 <- pDF1[,c("Lon", "Lat", "Year.y", "TeBE.p")]
    
    colnames(tmpDF1) <- c("Lon", "Lat", "Year", "LAI")
    colnames(tmpDF2) <- c("Lon", "Lat", "Year", "count")
    
    tmpDF2$count <- ifelse(tmpDF2$count == "expansion", 1, 0) 
    tmpDF2$count <- as.numeric(tmpDF2$count)
    
    totDF1 <- summaryBy(LAI~Year, data=tmpDF1, na.rm=T, keep.names=T, FUN=c(mean,sd))
    outDF$expansion_LAI_n[outDF$Year==2010.5&outDF$PFT=="TeBE"] <- sum(tmpDF2$count)
    outDF$expansion_LAI_mean[outDF$Year==2010.5&outDF$PFT=="TeBE"] <- totDF1$LAI.mean
    outDF$expansion_LAI_sd[outDF$Year==2010.5&outDF$PFT=="TeBE"] <- totDF1$LAI.sd
    
    
    ### TrBR
    pDF1 <- subset(pDF, TrBR.p == "expansion")
    
    tmpDF1 <- pDF1[,c("Lon", "Lat", "Year.y", "TrBR.y")]
    tmpDF2 <- pDF1[,c("Lon", "Lat", "Year.y", "TrBR.p")]
    
    colnames(tmpDF1) <- c("Lon", "Lat", "Year", "LAI")
    colnames(tmpDF2) <- c("Lon", "Lat", "Year", "count")
    
    tmpDF2$count <- ifelse(tmpDF2$count == "expansion", 1, 0) 
    tmpDF2$count <- as.numeric(tmpDF2$count)
    
    totDF1 <- summaryBy(LAI~Year, data=tmpDF1, na.rm=T, keep.names=T, FUN=c(mean,sd))
    outDF$expansion_LAI_n[outDF$Year==2010.5&outDF$PFT=="TrBR"] <- sum(tmpDF2$count)
    outDF$expansion_LAI_mean[outDF$Year==2010.5&outDF$PFT=="TrBR"] <- totDF1$LAI.mean
    outDF$expansion_LAI_sd[outDF$Year==2010.5&outDF$PFT=="TrBR"] <- totDF1$LAI.sd
    
    
    ### TrIBE
    pDF1 <- subset(pDF, TrIBE.p == "expansion")
    
    tmpDF1 <- pDF1[,c("Lon", "Lat", "Year.y", "TrIBE.y")]
    tmpDF2 <- pDF1[,c("Lon", "Lat", "Year.y", "TrIBE.p")]
    
    colnames(tmpDF1) <- c("Lon", "Lat", "Year", "LAI")
    colnames(tmpDF2) <- c("Lon", "Lat", "Year", "count")
    
    tmpDF2$count <- ifelse(tmpDF2$count == "expansion", 1, 0) 
    tmpDF2$count <- as.numeric(tmpDF2$count)
    
    totDF1 <- summaryBy(LAI~Year, data=tmpDF1, na.rm=T, keep.names=T, FUN=c(mean,sd))
    outDF$expansion_LAI_n[outDF$Year==2010.5&outDF$PFT=="TrIBE"] <- sum(tmpDF2$count)
    outDF$expansion_LAI_mean[outDF$Year==2010.5&outDF$PFT=="TrIBE"] <- totDF1$LAI.mean
    outDF$expansion_LAI_sd[outDF$Year==2010.5&outDF$PFT=="TrIBE"] <- totDF1$LAI.sd
    
    
    ### TrBE
    pDF1 <- subset(pDF, TrBE.p == "expansion")
    
    tmpDF1 <- pDF1[,c("Lon", "Lat", "Year.y", "TrBE.y")]
    tmpDF2 <- pDF1[,c("Lon", "Lat", "Year.y", "TrBE.p")]
    
    colnames(tmpDF1) <- c("Lon", "Lat", "Year", "LAI")
    colnames(tmpDF2) <- c("Lon", "Lat", "Year", "count")
    
    tmpDF2$count <- ifelse(tmpDF2$count == "expansion", 1, 0) 
    tmpDF2$count <- as.numeric(tmpDF2$count)
    
    totDF1 <- summaryBy(LAI~Year, data=tmpDF1, na.rm=T, keep.names=T, FUN=c(mean,sd))
    outDF$expansion_LAI_n[outDF$Year==2010.5&outDF$PFT=="TrBE"] <- sum(tmpDF2$count)
    outDF$expansion_LAI_mean[outDF$Year==2010.5&outDF$PFT=="TrBE"] <- totDF1$LAI.mean
    outDF$expansion_LAI_sd[outDF$Year==2010.5&outDF$PFT=="TrBE"] <- totDF1$LAI.sd
    
    
    ### C4G
    pDF1 <- subset(pDF, C4G.p == "expansion")
    
    tmpDF1 <- pDF1[,c("Lon", "Lat", "Year.y", "C4G.y")]
    tmpDF2 <- pDF1[,c("Lon", "Lat", "Year.y", "C4G.p")]
    
    colnames(tmpDF1) <- c("Lon", "Lat", "Year", "LAI")
    colnames(tmpDF2) <- c("Lon", "Lat", "Year", "count")
    
    tmpDF2$count <- ifelse(tmpDF2$count == "expansion", 1, 0) 
    tmpDF2$count <- as.numeric(tmpDF2$count)
    
    totDF1 <- summaryBy(LAI~Year, data=tmpDF1, na.rm=T, keep.names=T, FUN=c(mean,sd))
    outDF$expansion_LAI_n[outDF$Year==2010.5&outDF$PFT=="C4G"] <- sum(tmpDF2$count)
    outDF$expansion_LAI_mean[outDF$Year==2010.5&outDF$PFT=="C4G"] <- totDF1$LAI.mean
    outDF$expansion_LAI_sd[outDF$Year==2010.5&outDF$PFT=="C4G"] <- totDF1$LAI.sd
    
    
    ### C3G
    pDF1 <- subset(pDF, C3G.p == "expansion")
    
    tmpDF1 <- pDF1[,c("Lon", "Lat", "Year.y", "C3G.y")]
    tmpDF2 <- pDF1[,c("Lon", "Lat", "Year.y", "C3G.p")]
    
    colnames(tmpDF1) <- c("Lon", "Lat", "Year", "LAI")
    colnames(tmpDF2) <- c("Lon", "Lat", "Year", "count")
    
    tmpDF2$count <- ifelse(tmpDF2$count == "expansion", 1, 0) 
    tmpDF2$count <- as.numeric(tmpDF2$count)
    
    totDF1 <- summaryBy(LAI~Year, data=tmpDF1, na.rm=T, keep.names=T, FUN=c(mean,sd))
    outDF$expansion_LAI_n[outDF$Year==2010.5&outDF$PFT=="C3G"] <- sum(tmpDF2$count)
    outDF$expansion_LAI_mean[outDF$Year==2010.5&outDF$PFT=="C3G"] <- totDF1$LAI.mean
    outDF$expansion_LAI_sd[outDF$Year==2010.5&outDF$PFT=="C3G"] <- totDF1$LAI.sd
    
    
    write.csv(outDF, "output/static/LAI_by_year_decadal.csv", row.names=F)
    
}

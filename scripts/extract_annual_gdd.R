extract_annual_gdd <- function(inDF, sourceDir) {
    
    ### extract coldest month T for each year
    d <- dim(inDF)
    
    ### prepare monthDF
    monDF <- data.frame(c(1:(12*118)), rep(1:12, 118),
                        rep(1901:2018, each=12))
    colnames(monDF) <- c("ID", "Month", "Year")
    
    dayDF <- data.frame("ID" = c(1:(12*118*30)), 
                        "DOM" = rep(1:30, 118*12),
                        "Month" = rep(rep(1:12, each = 30), 118),
                        "Year" = rep(1901:2018, each=12*30))
    
    
    ### read in tmin and tmax for each month
    #tmnDF <- readRDS(paste0(sourceDir, "cru_ts4.03.1901.2018.tmn.dat.rds"))
    #tmxDF <- readRDS(paste0(sourceDir, "cru_ts4.03.1901.2018.tmx.dat.rds"))
    
    ### set seed
    set.seed(123)
    
    ### prepare an outputDF
    outDF <- array(NA, c(d[1], d[2], 118))
    
    ### calculate grid-specific annual rainfall
    for (i in 1:d[1]) {
        for (j in 1:d[2]) {
            ### select the grid
            sDF <- inDF[i,j,]
            
            ### calculate annual mean, based on monthly values for each grid
            sDF <- cbind(monDF, sDF)
            
            ### expand to daily values, based on monthly mean
            #daily.matrix <- ifelse(is.na(sDF$sDF[1]), NA, t(mapply(rnorm,30,sDF$sDF,sDF$sDF/3)))
            daily.matrix <- t(mapply(rnorm,30,sDF$sDF,sDF$sDF/3))
            
            dayDF$value <- as.vector(daily.matrix)
            
            ### perform threshold check
            dayDF$gdd <- ifelse(dayDF$value-5.0>0.0, dayDF$value-5.0, 0.0)
            #sDF$gdd <- ifelse(sDF$sDF-5.0>0.0, sDF$sDF-5.0, 0.0)
            
            ### sum all day in a month
            mDF <- summaryBy(gdd~Year+Month, FUN=sum, data=dayDF, keep.names=T)
            
            ### sum all month in a year
            annDF <- summaryBy(gdd~Year, FUN=sum, data=mDF, keep.names=T)
            #annDF <- summaryBy(gdd~Year, FUN=sum, data=sDF, keep.names=T)
            
            ### obtain 118 year record
            outDF[i,j,] <- annDF$gdd
        }
    }
    
    ### checking data
    #require(raster)
    #test <- outDF[,,1]
    #r <- raster(test)
    #plot(r)
    
    ### save the data
    saveRDS(outDF, "output/climate/annual_gdd.rds")

    
    ## return
    return(outDF)
    
}


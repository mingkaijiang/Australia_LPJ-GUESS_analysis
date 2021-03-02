calculate_20_yr_running_mean <- function (tminDF, tmaxDF, gddDF,
                                          lon.list, lat.list) {
    
    ### to compute tcmin_est, tcmax_est, twmin_est, and gdd5min_est
    
    ### get dimensions
    d <- dim(tminDF)
    
    ### prepare output DF
    tcmin_est <- tcmax_est <- twmin_est <- gdd5min_est <- array(NA, c(d[1], d[2], 118))
    
    ### need to create random data list to swap the first 19 years of data
    set.seed(123)
    rand.loc <- sample(1:118, 19)
    
    ### generate random year data frame for the first 19 years
    tminDF.sub <- tminDF[,,rand.loc]
    tmaxDF.sub <- tmaxDF[,,rand.loc]
    gddDF.sub <- gddDF[,,rand.loc]
    
    ### create a complete dataset
    tminDF.full <- abind(tminDF.sub, tminDF, along = 3)
    tmaxDF.full <- abind(tmaxDF.sub, tmaxDF, along = 3)
    gddDF.full <- abind(gddDF.sub, gddDF, along = 3)
    
    ### calculate running means of every 20-yrs of data
    ### calculate grid-specific annual rainfall
    for (i in 1:d[1]) {
        for (j in 1:d[2]) {
            ### select the grid
            sDF1 <- tminDF.full[i,j,]
            sDF2 <- tmaxDF.full[i,j,]
            sDF3 <- gddDF.full[i,j,]
            
            ### calculate annual mean, based on monthly values for each grid
            tcmin_est[i,j,] <- runmin(sDF1, 20, endrule = "trim", align = "left")
            tcmax_est[i,j,] <- runmax(sDF1, 20, endrule = "trim", align = "left")
            twmin_est[i,j,] <- runmin(sDF2, 20, endrule = "trim", align = "left")
            gdd5min_est[i,j,] <- runmin(sDF3, 20, endrule = "trim", align = "left")
            
        }
    }
    
    
    ### convert into long format
    outDF <- reshape2::melt(tcmin_est)
    outDF$Lon <- rep(lon.list, length(lat.list) * 118)
    outDF$Lat <- rep(rep(rev(lat.list), each=length(lon.list)), 118)
    outDF$Year <- rep(c(1901:2018), each = d[1] * d[2])
    outDF$Var1 <- NULL
    outDF$Var2 <- NULL
    outDF$Var3 <- NULL
    
    names(outDF)[names(outDF) == "value"] <- "tcmin_est"
    
    tmpDF <- reshape2::melt(tcmax_est)
    outDF$tcmax_est <- tmpDF$value
    
    tmpDF <- reshape2::melt(twmin_est)
    outDF$twmin_est <- tmpDF$value
    
    tmpDF <- reshape2::melt(gdd5min_est)
    outDF$gdd5min_est <- tmpDF$value
    
    
    ### reorganize
    out <- outDF[,c("Lon", "Lat", "Year", 
                    "tcmin_est", "tcmax_est",
                    "twmin_est", "gdd5min_est")]
    
    return(out)
    
}

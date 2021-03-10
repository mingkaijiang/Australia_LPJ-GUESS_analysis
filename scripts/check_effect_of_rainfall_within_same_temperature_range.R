check_effect_of_rainfall_within_same_temperature_range <- function() {
    
    ### calculate MAP based on CRU
    preDF <- calculate_MAP_based_on_CRU()
    
    ### create lat, lon and time list
    aus.lon.min <- 110.25
    aus.lon.max <- 155.25
    aus.lat.min <- -45.25
    aus.lat.max <- -10.25
    
    lon.list <- seq(aus.lon.min, aus.lon.max, 0.5)
    lat.list <- seq(aus.lat.max, aus.lat.min, -0.5)
    
    d <- dim(preDF)
    
    ### convert into long format
    preDF <- reshape2::melt(preDF)
    preDF$Lon <- rep(lon.list, length(lat.list) * 118)
    preDF$Lat <- rep(rep(rev(lat.list), each=length(lon.list)), 118)
    preDF$Year <- rep(c(1901:2018), each = d[1] * d[2])
    preDF$Var1 <- NULL
    preDF$Var2 <- NULL
    preDF$Var3 <- NULL
    
    ### read in simulation results
    ## LAI
    laiDF <- read.table(paste0("input/withfire/run1/lai.out"), header=T)
    for (i in 2:20) {
        tmpDF <- read.table(paste0("input/withfire/run", i, "/lai.out"), header=T)
        laiDF <- rbind(laiDF, tmpDF)
    }
    
    ## tree density
    densDF <- read.table(paste0("input/withfire/run1/dens.out"), header=T)
    for (i in 2:20) {
        tmpDF <- read.table(paste0("input/withfire/run", i, "/dens.out"), header=T)
        densDF <- rbind(densDF, tmpDF)
    }
    
    ## individual
    indDF <- read.table(paste0("input/withfire/run1/indiv.out"), header=T)
    for (i in 2:20) {
        tmpDF <- read.table(paste0("input/withfire/run", i, "/indiv.out"), header=T)
        indDF <- rbind(indDF, tmpDF)
    }
    
    ### merge
    #laiDF <- merge(laiDF, preDF, by=c("Lon", "Lat", "Year"), 
    #                 keep.x=T)
    
    #densDF <- merge(densDF, preDF, by=c("Lon", "Lat", "Year"), 
    #               keep.x=T)
    
    ### end.
    
}
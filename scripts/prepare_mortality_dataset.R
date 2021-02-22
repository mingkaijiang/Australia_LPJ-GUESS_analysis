prepare_mortality_dataset <- function(myDF) {
    
    ### sum all stem C
    myDF$StemC <- myDF$SapC + myDF$HeartC
    
    ## get final year in the dataset
    final.year <- max(myDF$Year)
    
    ## get first year in the dataset
    first.year <- min(myDF$Year)
    
    ## get length of the years
    d1 <- length(c(first.year:final.year))
    
    ### assign ID to each grid
    lon <- unique(myDF$Lon)
    lat <- unique(myDF$Lat)
    
    lonlatDF <- data.frame("Lon" = rep(lon, each=length(lat)), 
                           "Lat" = rep(lat, length(lon)),
                           "ID" = c(1:(length(lon) * length(lat))))
    
    myDF <- merge(myDF, lonlatDF, by=c("Lon", "Lat"), all.x=T)
    
    ### get grid ID
    gridID <- unique(myDF$ID)
    patchID <- c(0:99)
    
    ### prepare outDF to store the output
    myDF <- setDT(myDF)
    
    outDF <- myDF %>% 
        group_by(ID, Patch, PFT, Indiv) %>% 
        filter(Year==max(Year))
    
    
    ### remove year 2015
    outDF2 <- outDF[outDF$Year < 2015, ]
    
    ### save the dataset to save time
    write.csv(outDF2, "output/mortality/individual_mortality_table.csv", row.names=F)
    
}
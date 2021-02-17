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
    outDF <- c()
    
    ### loop through grid
    for (i in gridID) {
        subDF1 <- subset(myDF, ID == i)
        
        ## loop through patch
        for (j in patchID) {
            subDF2 <- subset(subDF1, Patch == j)
            
            pftID <- unique(subDF2$PFT)
            
            ## loop through PFT
            for (k in pftID) {
                subDF3 <- subset(subDF2, PFT == k)
                
                indivID <- unique(subDF3$Indiv)
                
                for (l in indivID) {
                    subDF4 <- subset(subDF3, Indiv == l)
                    
                    ## get maximum year
                    mx.year <- max(subDF4$Year)
                    
                    ## get dimension of the dataset
                    d2 <- dim(subDF4)[1]
                    
                    ### extract year of mortality
                    if (mx.year < final.year & d2 < d1) {
                        ## extract the year before the individual disappear
                        outDF2 <- subDF4[d2,]
                        
                        ### row merge
                        outDF <- rbind(outDF, outDF2)
                    } # end if
                } # end l
            } # end k
        } # end j
    } # end i
    
    
    ### save the dataset to save time
    write.csv(outDF, "output/mortality/individual_mortality_table.csv", row.names=F)
    
}
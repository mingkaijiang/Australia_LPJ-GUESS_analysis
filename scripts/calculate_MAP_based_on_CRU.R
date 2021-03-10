calculate_MAP_based_on_CRU <- function() {
    
    ### read cru dataset
    sourceDir <- "/Users/mingkaijiang/Documents/Research/Projects/Trendy/Trendy_Australia_Analysis/output/climate/"
    tmpDF <- readRDS(paste0(sourceDir, "cru_ts4.03.1901.2018.pre.dat.rds"))
    
    ### calculate annual MAP
    if (!file.exists("output/climate/cru_map_annual.rds")) {
        
        print("calculating annual MAP... ... ")
        
        ### calculation
        ## get dimension
        d <- dim(tmpDF)
        
        ## prepare monthDF
        monDF <- data.frame(c(1:(12*118)), rep(1:12, 118),
                            rep(1901:2018, each=12))
        colnames(monDF) <- c("ID", "Month", "Year")
        
        
        ### prepare an outputDF
        outDF <- array(NA, c(d[1], d[2], 118))
        
        ### calculate grid-specific annual rainfall
        for (i in 1:d[1]) {
            for (j in 1:d[2]) {
                ### select the grid
                sDF <- tmpDF[i,j,]
                
                ### calculate annual sum, based on monthly values for each grid
                sDF <- cbind(monDF, sDF)
                annDF <- summaryBy(sDF~Year, FUN=sum, data=sDF, na.rm=T, keep.names=T)
                
                ### obtain 118 year record
                outDF[i,j,] <- annDF$sDF
            }
        }
        
        ### save the data
        saveRDS(outDF, "output/climate/cru_map_annual.rds")
        
        ### check
        #require(raster)
        #test <- outDF[,,1]
        #r <- raster(test)
        #plot(r)
        
    } else {
        print("reading in precipitation dataset ... ...")
        outDF <- readRDS("output/climate/cru_map_annual.rds")
    }
    
    return(outDF)
    
}
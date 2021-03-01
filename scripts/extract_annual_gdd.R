extract_annual_gdd <- function(inDF) {
    
    ### extract coldest month T for each year
    d <- dim(inDF)
    
    ### prepare monthDF
    monDF <- data.frame(c(1:(12*118)), rep(1:12, 118),
                        rep(1901:2018, each=12))
    colnames(monDF) <- c("ID", "Month", "Year")
    
    
    ### prepare an outputDF
    outDF <- array(NA, c(d[1], d[2], 118))
    
    ### calculate grid-specific annual rainfall
    for (i in 1:d[1]) {
        for (j in 1:d[2]) {
            ### select the grid
            sDF <- inDF[i,j,]
            
            ### calculate annual mean, based on monthly values for each grid
            sDF <- cbind(monDF, sDF)
            annDF <- summaryBy(sDF~Year, FUN=max, data=sDF, na.rm=T, keep.names=T)
            
            ### obtain 118 year record
            outDF[i,j,] <- annDF$sDF
        }
    }
    
    ### save the data
    write.csv(outDF, "output/climate/warmest_month_T.csv", row.names=F)
    
    
    ## return
    
    return(outDF)
    
}


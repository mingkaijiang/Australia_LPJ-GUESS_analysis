prepare_fixed_T_input_data <- function(sourceDir) {
    
    clim.dir <- paste0(sourceDir, "/cruncepv7cruP_2/")
    
    infile.name <- "cruncep_1901_2015.bin"
    
    myDF <- readBin(paste0(clim.dir, infile.name), "raw", 10e6)
    
    test <- t(matrix(myDF, nrow = 12))
    
    
    
    
    
    
    val <- myDF$V2[myDF$V1=="1901"]
    
    myDF$V2 <- val
    
    out.name <- "co2_1901_2018_fixed.txt"
    
    write.table(myDF, paste0(co2.dir, out.name), 
                sep = "\t", col.names=F, row.names=F)
    
}
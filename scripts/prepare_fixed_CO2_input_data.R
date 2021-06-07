prepare_fixed_CO2_input_data <- function(sourceDir) {
    
    co2.dir <- paste0(sourceDir, "/co2/")
    
    infile.name <- "co2_1901_2018.txt"
    
    myDF <- read.delim(paste0(co2.dir, infile.name), header=F)
    
    val <- myDF$V2[myDF$V1=="1901"]
    
    myDF$V2 <- val
    
    out.name <- "co2_1901_2018_fixed.txt"
    
    write.table(myDF, paste0(co2.dir, out.name), 
                sep = "\t", col.names=F, row.names=F)
    
}
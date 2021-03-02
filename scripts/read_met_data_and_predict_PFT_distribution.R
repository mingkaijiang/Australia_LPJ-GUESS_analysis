read_met_data_and_predict_PFT_distribution <- function() {
    
    ### read in met forcing data
    #sourceDir <- "/Users/mingkaijiang/Documents/Research/Projects/Australia_DGVM_MDI/Models/LPJ-Guess/forcing_data/cruncepv7cruP_2/"
    #sourceFile <- file(paste0(sourceDir, "cruncep_1901_2015.bin"), "rb")
    #metDF <- readBin(sourceFile, integer(), endian = "little")
    
    sourceDir <- "/Users/mingkaijiang/Documents/Research/Projects/Trendy/Trendy_Australia_Analysis/output/climate/"
    tmnDF <- readRDS(paste0(sourceDir, "cru_ts4.03.1901.2018.tmn.dat.rds"))
    
    ### create lat, lon and time list
    aus.lon.min <- 110.25
    aus.lon.max <- 155.25
    aus.lat.min <- -45.25
    aus.lat.max <- -10.25
    
    lon.list <- seq(aus.lon.min, aus.lon.max, 0.5)
    lat.list <- seq(aus.lat.max, aus.lat.min, -0.5)
    
    ### calculate coldest month T, warmest month T of each year, and GDD
    if (!file.exists("output/climate/coldest_month_T.csv")) {
        
        print("preparing bioclimate dataset ... ... ")
        
        ### create the dataset
        tminDF <- extract_coldest_month_T(tmnDF)
        tmaxDF <- extract_warmest_month_T(tmnDF)
        gddDF <- extract_annual_gdd(tmnDF)
        
    } else {
        print("reading in bioclimatic dataset ... ...")
        
        tminDF <- readRDS("output/climate/coldest_month_T.rds")
        tmaxDF <- readRDS("output/climate/warmest_month_T.rds")
        gddDF <- readRDS("output/climate/annual_gdd.rds")
        
    }
    
    ### 20-yr running mean calculation
    bioclimDF <- calculate_20_yr_running_mean(tminDF, tmaxDF, gddDF, lon.list, lat.list)
    
    test <- subset(bioclimDF, Year == 1901)
    
    require(fields)
    with(test, quilt.plot(Lon, Lat, gdd5min_est))
     
    
    ### set bioclimatic parmaeters for each PFT
    paramDF <- define_PFT_bioclimatic_parameters()
    
}

define_PFT_bioclimatic_parameters <- function() {
    
    ### PFTs in LPJ-GUESS are:
    ### BNE:  Boreal needle-leaved evergreen trees
    ### BINE: Boreal shade-intolerant needle-leaved evergreen tree
    ### BNS:  Boreal needle-leaved summergreen tree
    ### TeNE: Temperate needle-leaved evergreen tree
    ### TeBS: Temperate broadleaved summergreen tree
    ### IBS:  Temperate shade-intolerant broadleaved summergreen tree
    ### TeBE: Temperate broadleaved evergreen tree
    ### TrBE: Tropical broadleaved evergreen tree
    ### TrIBE:Tropical shade-intolerant broadleaved evergreen tree
    ### TrBR: Tropical broadleaved raingreen tree
    ### C3G:  C3 grasses
    ### C4G:  C4 grasses
    ### Definition in Smith et al. (2014). Biogeosciences, 11, 2027–2054.
    
    myDF <- data.frame("PFT" = c("BNE", "BINE", "BNS", "TeNE",
                                 "TeBS", "IBS", "TeBE", "TrBE",
                                 "TrIBE", "TrBR", "C3G", "C4G"),
                       "tcmin_surv" = c(-31.0, -31.0, -1000.0, -2.0, 
                                        -14.0, -30.0, -1.0, NA, 
                                        NA, NA, -1000.0, 15.5),
                       "tcmin_est" = c(-30.0, -30.0, -1000.0, -2.0, 
                                       -13.0, -30.0, 0.0, NA, 
                                       NA, NA, -1000.0, 15.5),
                       "tcmax_est" = c(-1.0, -1.0, -2.0, 10.0, 
                                       6.0, 7.0, 18.8, NA, 
                                       NA, NA, 1000.0, 1000.0),
                       "twmin_est" = c(5.0, 5.0, -1000.0, 5.0, 
                                       5.0, -1000.0, 5.0, NA, 
                                       NA, NA, -1000.0, -1000.0),
                       "gdd5min_est" = c(500, 500, 350.0, 2000.0, 
                                         1000.0, 350.0, 2000.0, NA, 
                                         NA, NA, 0.0, 0.0))
    
    
    return(myDF)
}
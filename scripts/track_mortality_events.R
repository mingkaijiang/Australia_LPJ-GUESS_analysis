track_mortality_events <- function(myDF) {
    
    ### track mortality patch/individual
    ### if the patch/individual no longer present in the next year,
    ### then it is dead in the current year
    ### Generate a table summarizing the mortality events
    ### and corresponds to climate extremes
    
    ### sum all stem C
    myDF$StemC <- myDF$SapC + myDF$HeartC
    
    ### extract individual ID
    indiv.id <- unique(myDF$Indiv)
    
    test2 <- subset(myDF, Indiv==756)
    
    
}
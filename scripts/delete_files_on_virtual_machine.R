delete_files_on_virtual_machine <- function() {
    
    ### run list folder
    runlist <- c(1:20)
    
    ### scenarios
    pftlist <- c("BNE", "BINE", "BNS", "TeNE", "TeBS", "IBS", "TrBE",
                 "TrIBE", "TrBR", "TeBE", "C3G", "C4G")
    
    firelist <- c("withoutfire", "withfire")
    
    climlist <- c("fixCO2fixT", "fixCO2varT", "varCO2fixT")
    
    ### subfolders
    subfolders <- c(pftlist, firelist, climlist)
    
    
    ### command text
    path <- "rm -f /data/Q3416/Jiang_M/LPJ-GUESS_trunk_r8538_v2/build/run"
    end.text <- paste0("/*.out")
    
    
    ### go through each folder
    for (i in runlist) {
        for (j in subfolders) {
            
            print(paste0(paste0(path, i, "/", j, end.text)))
            
            system(paste0(path, i, "/", j, end.text))
        }
    }

    
    
}
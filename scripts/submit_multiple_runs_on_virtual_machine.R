submit_multiple_runs_on_virtual_machine <- function() {
    
    
    ### run list folder
    runlist <- c(1:20)
    
    ### command text
    command1 <- "cd /data/Q3416/Jiang_M/LPJ-GUESS_trunk_r8538_v2/build/run"
    command2 <- "./guess guess.ins > guess.log&"
        
    ### go through each folder to submit the job
    for (i in runlist) {

        system(paste0(command1, i))
        
        print(paste0(command1, i))
            
        system(command2)
        
    }
    
    
    ### after all simulation finished, copy and paste all the simulation to relevant storage folders
    ### and replace the path files
    
    
    ### scenarios
    #pftlist <- c("BNE", "BINE", "BNS", "TeNE", "TeBS", "IBS", "TrBE",
    #             "TrIBE", "TrBR", "TeBE", "C3G", "C4G")
    #
    #firelist <- c("withoutfire", "withfire")
    #
    #climlist <- c("fixCO2fixT", "fixCO2varT", "varCO2fixT")
    #
    #### subfolders
    #subfolders <- c(pftlist, firelist, climlist)
    
}
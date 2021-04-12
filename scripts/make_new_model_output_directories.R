make_new_model_output_directories <- function() {
    
    ### simulation list
    sim.list <- paste0("run", c(1:20))
    
    ### pft list
    pft.list <- c("BNE", "BINE", "BNS", "TeNE",
                  "TeBS", "IBS", "TeBE", "TrBE", 
                  "TrIBE", "TrBR", "C3G", "C4G")
    
    
    ## variable list
    var.list <- c("lai.out", "fpc.out", "cpool.out")
    
    ### check if file exists, if not, then copy over
    if (!file.exists("input/run1/TeBE/lai.out")) {
        
        ### message
        print("Copying files from HIE General 2...")
        
        ### create folder
        for (i in 1:length(sim.list)) {
            if(!dir.exists(paste0("input/", sim.list[i]))) {
                
                dir.create(paste0("input/", sim.list[i]), showWarnings = FALSE)
                
                for (j in 1:length(pft.list)) {
                    dir.create(paste0("input/", sim.list[i], "/", pft.list[j]), showWarnings = FALSE)
                } # j
            } # if
        } # i
        
        
        ### establish connection
        require(ssh)
        session <- ssh_connect("u30046137@43.240.97.5")
        
        ### command line
        for (i in sim.list) {
            for (j in pft.list) {
                for (k in var.list) {
                    command1 <- paste0("cp /data/Jiang_M/LPJ-GUESS_trunk_r8538/build/",i,"/",j,"/", k)
                    
                    command2 <- paste0(getwd(), "/input/", i, "/", j, "/")
                    scp_download(session, command1, to=command2)
                }
            }
        }
        
        
        ssh_disconnect(session)
        
        
    } else (
        print("files exist, no need to copy over from HIE General 2")
    )
    
    
}

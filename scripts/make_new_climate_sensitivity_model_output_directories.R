make_new_climate_sensitivity_model_output_directories <- function() {
    
    ### simulation list
    sim.list <- paste0("run", c(1:20))
    
    ### pft list
    clim.list <- c("fixCO2varT")#,"fixCO2fixT","varCO2fixT")
    
    
    ## variable list
    var.list <- c("lai.out", "fpc.out", "cpool.out")
    
    ### check if file exists, if not, then copy over
    if (!file.exists("input/run1/fixCO2varT/lai.out")) {
        
        ### message
        print("Copying files from HIE General 2...")
        
        ### create folder
        for (i in 1:length(sim.list)) {
            #if(!dir.exists(paste0("input/", sim.list[i]))) {
                
                #dir.create(paste0("input/", sim.list[i]), showWarnings = FALSE)
                
                for (j in 1:length(clim.list)) {
                    dir.create(paste0("input/", sim.list[i], "/", clim.list[j]), showWarnings = FALSE)
                } # j
            #} # if
        } # i
        
        
        ### establish connection
        session <- ssh_connect("u30046137@203.101.231.47")
        
        ### command line
        for (i in sim.list) {
            for (j in clim.list) {
                for (k in var.list) {
                    command1 <- paste0("cp /data/mounts/Q3416/Jiang_M/LPJ-GUESS_trunk_r8538/build/",i,"/",j,"/", k)
                    
                    command2 <- paste0(getwd(), "/input/", i, "/", j, "/")
                    scp_download(session, command1, to=command2)
                }
            }
        }
        
        
        ssh_disconnect(session)
        
        
    } else (
        print("files exist, no need to copy over from HIE General 2")
    )
    
    
    
    ### create output directory
    if(!dir.exists(paste0("output/climate_sensitivity"))) {
        dir.create(paste0("output/climate_sensitivity"), showWarnings = FALSE)
    }
    
    ### read in all files and prepare RDS output
    ## LAI
    for (k in var.list) {
        for (i in clim.list) {
            
            myDF <- read.table(paste0("input/run1/", i, "/", k), header=T)
            
            ### merge all 20 runs together
            for (j in 2:20) {
                myDF2 <- read.table(paste0("input/run", j, "/", i, "/", k), header=T)
                myDF <- rbind(myDF, myDF2)
            }
            
            saveRDS(myDF, paste0("output/climate_sensitivity/", i, "_", k, ".rds"))
            
        }
    }
    
    
    ### end
    
}

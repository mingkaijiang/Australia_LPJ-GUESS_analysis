merge_runs_into_scenarios <- function() {
    
    ### simulation list
    sim.list <- paste0("run", c(1:20))
    
    ### pft list
    pft.list <- c("BNE", "BINE", "BNS", "TeNE",
                  "TeBS", "IBS", "TeBE", "TrBE", 
                  "TrIBE", "TrBR", "C3G", "C4G")
    
    ## variable list
    var.list <- c("lai.out", "fpc.out", "cpool.out")
    
    for (i in pft.list) {
    lai.files <- paste0("input/run", c(1:20), "/", i, "/lai.out")
    cpool.files <- paste0("input/run", c(1:20), "/", i, "/cpool.out")
    fpc.files <- paste0("input/run", c(1:20), "/", i, "/fpc.out")
    
    ### read input files
    myDF.lai = lai.files %>% 
        purrr::map_df(~read.table(.,header=T))
    
    myDF.fpc = fpc.files %>% 
        purrr::map_df(~read.table(.,header=T))
    
    myDF.cpool = cpool.files %>% 
        purrr::map_df(~read.table(.,header=T))
    
    saveRDS(myDF.lai, file=paste0("output/competition/", i, "_lai.rds"))
    saveRDS(myDF.fpc, file=paste0("output/competition/", i, "_fpc.rds"))
    saveRDS(myDF.cpool, file=paste0("output/competition/", i, "_cpool.rds"))
    
    } # end of i
    
    
    
    
}
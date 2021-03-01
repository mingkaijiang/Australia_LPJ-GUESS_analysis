check_effect_of_fire_basic <- function(fire.model) {
    
    #### read input
    ### fire
    myDF1 <- read.table(paste0("input/withfire/run1/lai.out"), header=T)
    for (i in 2:20) {
        tmpDF <- read.table(paste0("input/withfire/run", i, "/lai.out"), header=T)
        myDF1 <- rbind(myDF1, tmpDF)
    }

    ### no fire
    myDF2 <- read.table(paste0("input/withoutfire/run1/lai.out"), header=T)
    for (i in 2:20) {
        tmpDF <- read.table(paste0("input/withoutfire/run", i, "/lai.out"), header=T)
        myDF2 <- rbind(myDF2, tmpDF)
    }
    
    
    
    ## end
}
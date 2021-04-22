#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

### static output 
if(!dir.exists("output/static")) {
    dir.create("output/static", showWarnings = FALSE)
}

### basic animation plot - default output
if(!dir.exists("output/basic")) {
    dir.create("output/basic", showWarnings = FALSE)
}

### individual-level animation/static plot 
if(!dir.exists("output/dgvm")) {
    dir.create("output/dgvm", showWarnings = FALSE)
}


### individual-level mortality plot 
if(!dir.exists("output/mortality")) {
    dir.create("output/mortality", showWarnings = FALSE)
}

### individual-level establishment plot 
if(!dir.exists("output/establishment")) {
    dir.create("output/establishment", showWarnings = FALSE)
}


### individual-level self-thinning plot 
if(!dir.exists("output/self-thinning")) {
    dir.create("output/self-thinning", showWarnings = FALSE)
}


### individual-level self-thinning plot 
if(!dir.exists("output/fire")) {
    dir.create("output/fire", showWarnings = FALSE)
}

### individual-level self-thinning plot 
if(!dir.exists("output/climate")) {
    dir.create("output/climate", showWarnings = FALSE)
}

### competition
#if(!dir.exists("output/competition")) {
#    dir.create("output/competition", showWarnings = FALSE)
#}

#run.list <- paste0("run", c(1:20))
#pft.list <- c("BNE", "BINE", "BNS", 
#              "TeBS", "TrBE", "TrIBE",
#              "TrBR", "TeBE", "C3G", "C4G",
#              "IBS", "TeNE")
#
#for (i in run.list) {
#    if(!dir.exists(paste0("output/competition/", i))) {
#        dir.create(paste0("output/competition/", i), showWarnings = FALSE)
#    }
#    
#    for (j in pft.list) {
#        if(!dir.exists(paste0("output/competition/", i, "/", j))) {
#            dir.create(paste0("output/competition/", i, "/", j), showWarnings = FALSE)
#        }
#    }
#    
#}

#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr, 
               doBy, 
               lubridate,
               ggplot2,
               viridis,
               sciplot,
               scales,
               data.table,
               cowplot,
               gridExtra,
               ggthemes,
               RColorBrewer,
               gganimate,
               gifski,
               abind,
               caTools,
               purrr)    


#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("scripts", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z1 in sourcefiles)source(z1)

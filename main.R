### Repository to analyze LPJ-GUESS simulation result for Australia
### M.Jiang


############################# Set up #################################
#### clear wk space
rm(list=ls(all=TRUE))

#### read prepare.R
source("prepare.R")


############################# Read input #################################
### get access to HIE General 2
### not fully developed yet
#scp(host="43.240.97.5", 
#    path="/data/Jiang_M/", 
#    keypasswd = NA, 
#    user = "u30046137", rsa = TRUE)

### use local LAI example to develop the plotting script first
myDF <- read.table("input/run1/lai.out", header=T)

### merge all 20 runs together
for (i in 2:20) {
    myDF2 <- read.table(paste0("input/run", i, "/lai.out"), header=T)
    myDF <- rbind(myDF, myDF2)
}


############################# plotting #################################
### plot PFT-based response variable change over time
plot_PFT_proportional_contribution_by_year_overview(myDF)

plot_static_LAI_by_year_map_merge_all_PFT(myDF)

plot_static_PFT_occupation_density_map(myDF)

plot_animated_LAI_map(myDF)

plot_animated_LAI_latitudinal_gradient(myDF)


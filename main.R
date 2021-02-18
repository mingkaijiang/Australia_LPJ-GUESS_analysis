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

### plot PFT-based response variable change over time
plot_PFT_proportional_contribution_by_year_overview(myDF)

plot_static_LAI_by_year_map_merge_all_PFT(myDF)

plot_static_PFT_occupation_density_map(myDF)

plot_animated_LAI_map(myDF)

plot_animated_LAI_latitudinal_gradient(myDF)



############################# Read input on tree density #################################
### tree density
myDF <- read.table("input/run1/dens.out", header=T)

### merge all 20 runs together
for (i in 2:20) {
    myDF2 <- read.table(paste0("input/run", i, "/dens.out"), header=T)
    myDF <- rbind(myDF, myDF2)
}

### plotting 
plot_animated_density_map(myDF)


############################# Read input on individual output #################################
### individual data 
### depending on number of patches and number of years in the output, files can be very large;
### so it might be more feasible to process each run separately, i.e. don't merge all runs together.
myDF <- read.table("input/run1/indiv.out", header=T)

### merge all 20 runs together
for (i in 2:20) {
    myDF2 <- read.table(paste0("input/run", i, "/indiv.out"), header=T)
    myDF <- rbind(myDF, myDF2)
}

### plotting 
plot_animated_individual_map(myDF)

track_establishment_patterns(myDF)

track_mortality_events(myDF, delete_mortality_file = T)

plot_self_thinning_figures(myDF, patcharea = 1000)


############################# To-do list #################################
### 1. How does individual mortality respond to known drought events?
###    1.1. Why smaller trees preferentially die in most of the time?
###    1.2. Why in other times, big trees die?
###    1.3. How do drought-mortality relationships of different PFT at different location/regions compare?
###    1.4. How do drought-mortality responses to known drought events compare with observation (e.g. satelite and field data)?
###    1.5. What is the plant C, N and water status during drought-mortality events?
###    1.6. Is there a drought-mortality pattern under climate change (i.e. CO2 effect)?

### 2. How does establishment respond to climate change pattern and known drought events?
###    2.1. How does establishment respond to long-term climate change forcing?
###    2.2. How do known drought events affect establishment rate?

### 3. What is the self-thinning rule in LPJ-GUESS, and how does it compare with CABLE-POP?
###    3.1. How does simulated density-stemC relationship compare against observation? 
###    3.2. How does LPJ-GUESS and CABLE-POP compare?

### Need to think about ways to show the data beyond animated plot, i.e. static figure for publication. 


#### End

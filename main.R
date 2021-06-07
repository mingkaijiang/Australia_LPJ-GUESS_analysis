### Repository to analyze LPJ-GUESS simulation result for Australia
### M.Jiang



############################# Set up #################################
#### clear wk space
rm(list=ls(all=TRUE))

#### read prepare.R
source("prepare.R")

############################# Prepare fixed CO2 and fixed temperature data #################################
prepare_fixed_CO2_input_data(sourceDir=paste0("/Users/mingkaijiang/Documents/Research/Projects/Australia_DGVM_MDI/Models/LPJ-Guess/forcing_data"))

prepare_fixed_T_input_data(sourceDir=paste0("/Users/mingkaijiang/Documents/Research/Projects/Australia_DGVM_MDI/Models/LPJ-Guess/forcing_data"))


############################# Read input #################################
### get access to HIE General 2
### and download data from HIE General 2 
### not fully developed yet
### (you can develop code to run R on HIE general 2, too.)
#scp(host="43.240.97.5", 
#    path="/data/Jiang_M/", 
#    keypasswd = NA, 
#    user = "u30046137", rsa = TRUE)


### check effect of fire 
check_effect_of_fire_basic(fire.model="blaze")


### setting fire directory
fire.option <- "withfire"

############################# LAI #################################

### use local LAI example to develop the plotting script first
myDF <- read.table(paste0("input/", fire.option, "/run1/lai.out"), header=T)

### merge all 20 runs together
for (i in 2:20) {
    myDF2 <- read.table(paste0("input/", fire.option, "/run", i, "/lai.out"), header=T)
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
myDF <- read.table(paste0("input/", fire.option, "/run1/dens.out"), header=T)

### merge all 20 runs together
for (i in 2:20) {
    myDF2 <- read.table(paste0("input/", fire.option, "/run", i, "/dens.out"), header=T)
    myDF <- rbind(myDF, myDF2)
}

### plotting 
plot_animated_density_map(myDF)



############################# check bioclimatic limit on PFT distribution #################################
### In LPJ-GUESS, each PFT has its own parameter set to define the bioclimatic limit. 
### Question: can we use pre-defined bioclimatic parameter to determine the PFT distribution? 
### To answer this question, I will need to analyze the climate data to "predict" PFT distribution, and 
### compare it against the model simulations.
### If the maps are comparable, that means PFT distribution is largely determined by bioclimatic limit.
### If the maps are not comparable, that means PFT distribution is determined by other factors (e.g. N competition) 
### in addition to the bioclimatic limits.

### Note: the CRU forcing data that I am reading in is based on TRENDY, 
### which is not exactly the same as used in LPJ-GUESS forcing.
read_met_data_and_predict_PFT_distribution()



#### Comments on the result:
#### It seems that bioclimatic limit is the major factor determining PFT distribution.
#### Plant distribution is less affected by rainfall variability, but
###  mostly by temperature-related variables. 
#### Background mortality is a consequence of tree longevity. 
#### There are also mortality associated with disturbance and fire, and self-thinning.  

#### Question to explore: How/Why do trees die in response to drought? 
#### Here I will need to investigate the rainfall effect on PFT distribution. 
#### In particular, for death year, what rainfall threshold do we see for different PFT?

### Step 1: Overlay T-based PFT distribution with real model simulation results.
###         The gap in coverage is due to vegetation dynamics, disturbance, fire, nutrient, and rainfall.
###         The effect of fire is estimated based on turning fire on and off in the simulation,
###         so the gap is due to vegetation dynamcis, disturbance and nutrient competition.
###         Note that there is also vegetation presence outside bioclimatic boundary in the simulation result.
###         This is because monthly data was downscaled to daily using weather generator in LPJ-GUESS,
###         hence there is regions where mis-matches are apparent.
overlay_bioclimatic_prediction_and_LPJ_simulation()


### Step 2: Disturbances - it affects vegetation composition within each grid,
###         and affects individual age and productivity.
###         Is disturbance a stochastic or deterministic event in the model?




### Step 3: Results at step 1 suggest that, at the grid level,
###         PFT presence/absence almost never responds to water.
###         Maybe at the individual/stand/patch level, 
###         trees can still die as a result of drought.
###         Hence, need to investigate effect of water on vegetation dynamics
###         (abundance, productivity, reproductive fitness) at the cohort level,
###         within the bioclimatic boundary.
###         That is, for the same temperature (MAT or seasonality),
###         how rainfall variability correlates with vegetation dynamics of each cohort.
###         Is it necessary to fully replicate the weather generator to investigate cohort level response?
check_effect_of_rainfall_within_same_temperature_range()




### Step 4: Nutrient competition.





############################# Read input on individual output #################################
### individual data 
### depending on number of patches and number of years in the output, files can be very large;
### so it might be more feasible to process each run separately, i.e. don't merge all runs together.
myDF <- read.table(paste0("input/", fire.option, "/run1/indiv.out"), header=T)

### merge all 20 runs together
for (i in 2:20) {
    myDF2 <- read.table(paste0("input/", fire.option, "/run", i, "/indiv.out"), header=T)
    myDF <- rbind(myDF, myDF2)
}

### plotting 
plot_animated_individual_map(myDF)

track_establishment_patterns(myDF)

track_mortality_events(myDF, delete_mortality_file = F)

plot_self_thinning_figures(myDF, patcharea = 1000)

plot_tree_age_and_density_relationship(myDF)



############### environmental gradient analysis at selected locations ###################
#### Possibly not needed, because we have provided analyses on bioclimatic limit and hence
#### knows exactly what bioclimatic factors affect PFT distribution (all T related).


#### Let's check water effect on mortality first, i.e. rainfall gradient and 
#### understand why establishment doesn't happen for rainfall range. 




############### data-model validation at selected locations ###################
### Based on data availability, we need to choose a selected list of locations
### to do model parameterization and data-model intercomparison.
### The purpose of this is to see if we can simply parameterize the model
### to match the vegetation dynamics we see in the data.
### If that's the case, then we are basically proving the 
### vegetation dynamic processes in the model are adequate.
### We will start with LPJ-GUESS and progressively move to other models.





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




############### effect of competition on LAI ###################

### make new simulation output directories locally
### and copy over the files from HIE General2
make_new_model_output_directories()


### Merge data for each simulation scenarios
merge_runs_into_scenarios()

### Make comparison plots
make_effect_of_competition_analysis()

#### End

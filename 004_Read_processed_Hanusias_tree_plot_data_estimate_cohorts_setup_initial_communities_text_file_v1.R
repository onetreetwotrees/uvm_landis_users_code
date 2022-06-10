library(sp)
library(tidyr)
library(dplyr)

# Code developed by Jane Foster at the University of Vermont
# This code reads in pre-processed inventory data with tree and plot measurements.
# This example version assumes you have already run code to process FIA data
# for counties surrounding the White Mountains in NH and ME. County shapefile
# Can be created using code 001_*.R & 003_*.R

# This version assumes you have tree-level dbh and stand level age data.

# Update directories to write shapefiles or output data tables to
data_dir <- "data_processing"
landis_dir <- "landis_runs"

# Set next var to "yes" if you want to write output .csvs to local directory
write_local_output <- "yes"

# Update: Character string prefix for study-area for reading and writing files.
study_area_prefix <- 'VT-NH-MA'

# Set diameter at breast height (DBH) limit in cm between overstory and sapling/ small tree data.
dbhOverstoryMin <- 5 * 2.54 # cm (5 inches in FIA db) convert to cm 1 inch/2.54 cm

# optional (set = 1 if you want to apply)
# use this to ignore species at a plot that are below a
# minimum threshold based on relative abundance (Relative Basal Area, RBA)
apply_threshold <- 0
pct_thresh <- 0.1 # RBA

# Set the year for which age observations will be set to, adjust for different inventory years.
simStartYear <- 2020 # Set this for intended start year of Landis simulation

# Jane adding this to try to align with Erin's Matlab code, but methods differ
# cohort_option = 1 will generate 1 age-cohort per plot - Not implemented here yet.
# cohort_option = 2 will generate 1-2 age-cohorts per plot, from large tree data (DIA > 5 inches in FIA)
# cohort_option = 3 will generate 1-3 age-cohorts per plot, adding in small tree data for youngest (DIA <= 5 inches in FIA)
# cohort_option = 4 same as option 3, but species only added from small tree data IF they
# do not occur in next oldest cohort. Effort to simplify and lower initial # of cohorts.
# The range of potential cohort ages are drawn from OLS prediction intervals around a model of
# STDAGE ~ basalAreaM2Ha_sum (plot BA). (unif random sample of 3 vals within 95% prediction CI)
# if there is an observation of STDAGE for plot, we assume this represents the oldest cohort,
# and random sample is drawn between the lower CI and the age observation.
# If the age observation falls below the lower CI, we draw cohort ages from arbitrary
# number years for ages younger than the observed age (15 years).
# This results in 3 potential cohort ages per plot. 
# Species are distributed into oldest cohort if they occur >= median DBH of large trees.
# Species are distributed into 2nd oldest cohort if they are large trees with DBH < median of large trees.
# Species are distributed into 3rd oldest cohort if they are small trees (DBH < 5 inches for FIA)
# cohort_method = 5 will sort cohorts on plot-level abundance spRbai only

cohort_option <- 5

# Add a cohort_method to test different ways to sort tree data into cohorts
# cohort_method = 1 will sort tree data into cohort based on species observations >= | < median dbh_cm per plot
# cohort_method = 2 Maybe Erin's approach? Cohorts determined by mean & sd dbh_cm by species and plot?
# cohort_method = 3 will sort tree data into cohorts based on species obs based on both
# >= | < both median dbh_cm and ht_m per plot.
# cohort_method = 5 will sort cohorts on plot-level abundance spRbai

# Read in pre-processed forest inventory data tables with tree-level and plot-level measures
tree <- read.csv(paste(data_dir, "//", study_area_prefix, "_fia_tree.csv", sep = ""))
plots <- read.csv(paste(data_dir, "//", study_area_prefix, "_fia_plot_totals_with_age.csv", sep = ""))
spbio <- read.csv(paste(data_dir, "//", study_area_prefix, "_fia_plots_sp_bio.csv", sep = ""))
spba <- read.csv(paste(data_dir, "//", study_area_prefix, "_fia_plots_sp_ba.csv", sep = ""))
spp_code_key <- read.csv(paste(data_dir, "//", study_area_prefix, "_fia_spp_code_key.csv", sep = ""))
spp_long <- read.csv(paste("data_processing//Landis_species_longevity_with_duplicates.csv", sep=""))

# Join SPCD with SPECIES_SYMBOL to facilitate join with NEON taxonID or FSVEG SPECIES.
spp_grp <- read.csv(paste(data_dir, "//", "FIA_spp_grp.csv", sep=""))
spp_code_key <- inner_join(spp_code_key, spp_grp %>% dplyr::select(SPCD, SPECIES_SYMBOL))

# Read in pre-processed forest inventory data tables with tree-level and plot-level measures
dir_hanusia <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\uvm_landis_users_code\\data\\Hanusia_plot_data_init_communities\\"
sprbaHanusia <- read.csv(paste(dir_hanusia, "lstopospout_20220519.csv", sep=""))
# Hanusia's data still needs inventory year, get this from FSVeg plot table
plotsFsveg <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\data_processed\\GMNF_fsveg_plots_with_LL.csv")
treeFsveg <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\data_processed\\GMNF_fsveg_tree_data_with_LL.csv")

# Read in a pre-processed Landis species input table. Only include species selected for simulation.
# In this case, using dynamic inputs for biomass succession to limit the species list for testing.
speciesTxt <- read.csv(paste(dir_hanusia, "FINAL_species_list_for_LANDIS_18Jan2022.csv",sep=""))
speciesTxt <- speciesTxt %>% rename(SPCD = FIA.Code) %>% inner_join(spp_code_key)
# Write out this merged table for future use. Only do this 1x or if updating...
#write.csv(speciesTxt, paste(dir_hanusia, "species_list_code_key_hanusia.csv",sep=""), row.names=F)
species <- speciesTxt$sppcode
print(species)

# Inspect data tables
print(head(tree))
print(head(plots))

print(head(sprbaHanusia))

# Two pltIDs in plots have multiple elev_m observations, trim here to first one.
plots <- plots %>% group_by(pltID) %>% slice(1) %>% ungroup()

# Hanusia's data still needs inventory year, get this from FSVeg plot table
sprbaHanusia <- plotsFsveg %>% dplyr::select(STANDPLOT_CN, INV_YEAR) %>% 
  rename(plotindex = STANDPLOT_CN) %>% right_join(sprbaHanusia) %>% 
  left_join(plots %>% rename(plotindex = pltID) %>% dplyr::select(plotindex, INVYR)) %>% 
  mutate(YEAR = case_when(is.na(INVYR) ~ INV_YEAR, TRUE ~ INVYR)) %>% 
  mutate(YEAR = case_when(is.na(YEAR) ~ as.integer(2020), TRUE ~ YEAR)) %>% arrange(rowID)

# Create list of potential simulation species from summarized plot biomass by species table
# TO DO: Include option to read in Landis-II species text file to pull species list from.
# sppTab <- read.table(paste(data_dir, "//Species.txt", sep=""), skip = 5, header=F)
sppSim <- names(spbio)[(names(spbio) %in% spp_code_key$sppcode)]
# sppSim <- sppSim[which(sppSim %in% sppTab$V1)]
# spp_long <- data.frame(sppcode = sppTab[,1], longevity = sppTab[,2])
# Reduce list of species to those for which species longevity parameter is available.
sppSim <- sppSim[which(sppSim %in% spp_long$sppcode)]
# Further reduce list to simulated species specified in dynamic inputs or species Text file
sppSim <- sppSim[which(sppSim %in% species)]
print(sppSim)

# Trim tree data to most recent inventory year, using previously trimmed fia plots table
# May want to trim to penultimate inventory year, then run the model for 5 years to compare results...
tree <- tree %>% semi_join(plots, by=c("INVYR"="INVYR", "pltID"="pltID"))
treeFsveg <- treeFsveg %>% inner_join(plotsFsveg %>% dplyr::select("STANDPLOT_CN", "INV_YEAR"), # Align col. names with FIA tree variables
              by=c("STANDPLOT_CN"="STANDPLOT_CN")) %>% rename(pltID = STANDPLOT_CN,
              INVYR = INV_YEAR, DIA = DIAMETER, SPECIES_SYMBOL = SPECIES, 
              ht_m = htm, dbh_cm = dbhcm) %>% 
  inner_join(spp_code_key %>% dplyr::select(SPCD, SPECIES_SYMBOL, sppcode))

# Append FSVEG tree table to FIA tree table with bind_rows
tree <- tree %>% bind_rows(treeFsveg %>% dplyr::select(any_of(names(tree))))

# Plot-level data summaries for OLS model that predicts stand age from plot basal area.
# This can be used to predict ages for plots with no age, and estimate other cohort ages.
lplotage.lm <- lm(STDAGE ~ 0 + log(basalAreaM2PerHa_sum), 
                  data=plots[which(plots$basalAreaM2PerHa_sum > 0),])
summary(lplotage.lm)

# Visualize relationship with scatter plot
newx <- data.frame(basalAreaM2PerHa_sum=seq(from=0.5,to=400,by=1))
with(plots, plot(basalAreaM2PerHa_sum,STDAGE))
for (i in 1:3) {
  lines(unlist(newx),predict(lplotage.lm,newdata=newx, interval = "prediction")[,i], col=2)
}
mtext(paste("Prediction interval STDAGE ~ plot BA", study_area_prefix))

## Define a function to round ages off to nearest 2, 5 or 10 year interval
mround <- function(x,base){
  base*round(x/base)
} 

# Hardcode the max longevity by spsim list ...Need this if model
# predicts species' cohort age beyond max longevity set in the model.
# For demo, slice compiled species longevity data to youngest version, with slice function
spp_long <- spp_long %>% group_by(sppcode) %>% dplyr::arrange(sppcode, longevity) %>% 
  slice(1) %>% dplyr::filter(sppcode %in% sppSim)

# Create a rowID variable, to act as mapCode for initial communities text file
#spba$rowID <- 1:nrow(spba)
# This has already been assigned in forest GMM community class map, join here
spba <- sprbaHanusia %>% rename(pltID = plotindex, basalAreaM2PerHa_sum = baM2ha) %>% 
  dplyr::select(pltID, rowID, YEAR, basalAreaM2PerHa_sum, any_of(sppSim))

# Potential forest ages that can be derived from the Landsat disturbance record (Hansen or NAFD)
# Hardcode values based on disturbance maps 2020 used for Hanusia's landscape (can see around line line 160 KNN code, youngAges)
youngAges <- c(1:34) 
ages3 <- c()

# Create new dataframe to assign new potential youngAge communities. 
# Or, for existing plots, the maximum assigned age predicted for the plot.
# In this version, each young community of a given class will have x number of plots, 
# That correspond to a single species of the given age for all species > 0.01% rba in the class.
# The result should mean young stands are "sprinkled" with the species that occur in the mapped class.
newplotclass <- data.frame("rowID"=rep(NA,nclass*length(youngAges)*length(sppSim) + nrow(spba)),
                           "sppcode"=NA,"nspp"=NA,"class"=NA,"class2"=NA,"age"=NA)

## Set up data frame that will hold ragged array for initial communities text file.
incomm <- data.frame(matrix(NA,nrow=dim(spba)[1],ncol=20))
incomm[1,1] <- "LandisData"
incomm[1,2] <- "Initial Communities"
incomm[3,1] <- "MapCode 0"
#incomm[which(is.na(incomm)),] <- ""

# Alternative, create external text file and write lines directly from code.
# File names are appended with today's date, but running on the same day will overwrite.
fileNameOutText <- paste(landis_dir, "\\Initial_communities_", study_area_prefix, "_", Sys.Date(), ".txt", sep="")
# Create new output text file
file.create(fileNameOutText)

fileConnection <- file(fileNameOutText)
# Set up the appropriate text header info for Landis-II Initial Communities file.
writeLines(c(paste("LandisData ", sprintf('"%s"', c("Initial Communities"))),
           "", 
           c("MapCode 0"), 
           ""), fileConnection)

close(fileConnection)

startn <- 5
ages <- c()
oryrs <- c()
rowId_not_used <- c()

set.seed <- 4626

## Create a log file for initial communities file parameters if one doesn't exist,
## Otherwise append existing file
LogFileNameOut <- paste("Log_initial_communities_", study_area_prefix, ".txt", sep="")
logLineOut <- paste(fileNameOutText, paste("cohort_option=", cohort_option, sep=""),
                paste("simStartYear=",simStartYear, sep=""),
                paste("dbhOverstoryMin=",dbhOverstoryMin, sep=""),
                paste("apply_threshold=",apply_threshold, sep=""),
                paste("pct_thresh=", pct_thresh, sep=""), sep=",")

if (length(grep(LogFileNameOut, dir(landis_dir))) > 0) {
  write.table(logLineOut, paste(landis_dir, "\\",LogFileNameOut,sep=""), append=T)
} else {
  # Create new output text file
  file.create(paste(landis_dir, "\\",LogFileNameOut, sep=""))
  
  fileConnection <- file(LogFileNameOut)
  # Set up the appropriate text header info for Landis-II Initial Communities file.
  writeLines(logLineOut, fileConnection)
  close(fileConnection)
}


## Loop through FIA inventory plots. Pull out dbh and species from live overstory
## Tree data. Predict possible cohort ages by drawing from a uniform random sample
## between the min and max values of a "prediction" interval around the OLS point estimate
## For stand age from the lplotage.lm model, for each live tree. This model
## predicts plot stand age as a function of plot basal area (bam2ha).

for (i in 1:nrow(spba)) {
  ploti <- spba$pltID[i]
  rowIDi <- spba$rowID[i]
  baploti <- spba %>% dplyr::filter(pltID == ploti) %>% dplyr::select(basalAreaM2PerHa_sum) %>% unname()
  # Compute relative basal area of present species from spba data table. Already done Hanusia's table.
  # Make sure to trim to simulated species "sppSim"
  spRbai <- spba %>%
         dplyr::filter(pltID == ploti) %>%
         dplyr::select(any_of(sppSim)) %>% dplyr::select_if(~max(.) > 0) %>% 
         data.frame()
  invyri <- spba$YEAR[i]

  if (grep("STDAGE", names(plots)) > 0){
      ageObsi <- plots$STDAGE[which(plots$pltID == ploti)]
  } 
  
  # Check that an age observation was extracted, otherwise remove
  if (length(ageObsi) == 0) {
    rm(ageObsi)
  }

  # Check if inventory plot ID exists in tree data table and apply cohort_option
  if (ploti %in% tree$pltID) {
    #TO DO: Revise later to bring in small trees.
    if (cohort_option == 2) {
    # Extract DBH for trees > 12 cm DBH (overstory)  = dbhOverstoryMin set above.
    plotiData <- tree %>% dplyr::filter(pltID == ploti & dbh_cm >= dbhOverstoryMin) %>%
      # Trim plot species to those species to be simulated
      dplyr::filter(sppcode %in% sppSim)
    } else if (cohort_option == 3 | 
               cohort_option == 4 | cohort_option == 5) {
    # Retain tree observations for small and large trees based on DBH
      plotiData <- tree %>% dplyr::filter(pltID == ploti) %>%
        # Trim plot species to those species to be simulated
        dplyr::filter(sppcode %in% sppSim)
    }
    
     # Advance loop if no overstory trees are present.
    if (nrow(plotiData) == 0 & cohort_option < 5) {
      rowId_not_used <- c(rowId_not_used,rowIDi)
      next()
    }
  
    # Advance loop if no simulated species are present
    if (length(spRbai) == 0) {
      rowId_not_used <- c(rowId_not_used,rowIDi)
      next()
    }
  } else if (cohort_option == 5 & ncol(spRbai) > 0) {
    # Create empty data frame for now
    plotiData = tree[0,]
    } else {next()} # Skip plot if no trees are present
  
  # Predict mean plot age from OLS model created above, Stand age ~ plot basal area.
  # Prediction interval at a point produces fitted line value and upper/lower CI.
  plotage_pt <- predict(newdata=data.frame(basalAreaM2PerHa_sum = baploti),
                        lplotage.lm, interval = "prediction")
  
  # Visualize dbh-ht distribution by species
  if ("ht_m" %in% names(plotiData) & nrow(plotiData) > 0) {
    # check that there are non-NA values for height before trying to plot
    if (any(!(is.na(plotiData$ht_m)))) {
      with(plotiData, plot(dbh_cm, ht_m, cex=0.5))
      with(plotiData, text(dbh_cm, ht_m, labels=sppcode, col=SPCD))
      abline(v = median(plotiData$dbh_cm), lty=2)
      abline(h = median(plotiData$ht_m), lty=2)
      abline(v = dbhOverstoryMin, col=2)
      try(mtext(paste("row", rowIDi, "plot", ploti)), silent=T)
      legend("bottomright", c("medians", "dbhOverstoryMin"), lty=c(2,1), col=c(1,2), bty="n")
    }
  }
  
  # Adjust STDAGE variable (set to inventory year) to desired simulation year set above..
    # Increase age to a simulation year later than inventory year
    # Reduce age to a simulation year earlier than inventory year
    ageAdj <- simStartYear - invyri

  # Apply age adjustment to prediction interval and observed age if exists.
  plotage_pt <- round(plotage_pt + ageAdj)
  if (exists("ageObsi")) {
    ageObsi <- ageObsi + ageAdj
    if (ageObsi < 1) {
      ageObsi <- 1
    }
  }
  
  # Capture any predicted ages < 0, must be at least 1 years old in Landis for spin-up
  if (any(plotage_pt < 1)) {
    plotage_pt[which(plotage_pt < 1)] <- 1
  }
  
  # Determine species longevity parameter for dominant species on the plot
  domSppi <- names(spRbai[which.max(spRbai)])
  domSppLongi <- spp_long$longevity[which(spp_long$sppcode == domSppi)]
  
  # Check observed stand age against species longevity parameter for dominant species
  if (exists('ageObsi')) {
    if (domSppLongi < ageObsi) {
      # Change to observed age < species longevity of dominant species if needed
      ageObsi <- round(runif(1, min = domSppLongi - 20, max = domSppLongi - 1))
    }
  }
  
  if (nrow(plotiData) > 0 | (nrow(plotiData) == 0  & cohort_option == 5)) {
    # Draw 3 random values between upper and lower CI, uniform distribution.
    # If an observed age exists, assume it represents the oldest cohort & adjust CI
    if (exists("ageObsi")) {
      if (ageObsi >= plotage_pt[1,2]) {
        cohort_ages <- sort(runif(3,min=plotage_pt[1,2],max=ageObsi))
      }
      if (ageObsi < plotage_pt[1,2]) {
        # If observed age is younger than 95% CI, draw from ages {(ageObsi - 15):ageObsi}
        cohort_ages <- sort(runif(3,min=max(1,ageObsi - 15),max=ageObsi))
      }
      # Last, set oldest cohort age to observed age
      cohort_ages[3] <- ageObsi

    } else {
      # Check predicted stand age against species longevity parameter for dominant species
      if (plotage_pt[1,3] > domSppLongi) {
        plotage_pt[1,3] <- round(runif(1, min = domSppLongi - 1/2 * domSppLongi, max = domSppLongi - 1))
        # Now check that lower CI age is smaller than new upper CI age. Redraw if needed
        if (plotage_pt[1,2] > plotage_pt[1,3]){
          plotage_pt[1,2] <- round(runif(1, min = plotage_pt[1,3] - 1/2 * plotage_pt[1,3], 
                                         max = plotage_pt[1,3] - 1))
        }
      }
      cohort_ages <- sort(runif(3,min=plotage_pt[1,2],max=plotage_pt[1,3]))
    }    
    
    # Round to nearest x years
    cohort_ages <- round(cohort_ages)
    
    # Ensure there are no negative ages or 0 (resulting from ageAdj)
    # Cohorts must be at least 1 year old for Landis spinup
    cohort_ages[cohort_ages < 1] <- max(1, abs(cohort_ages[which(cohort_ages < 1)])) 
    
    # For each tree or species, assign it to cohort number. 
    if (cohort_option == 2 | cohort_option == 3 | cohort_option == 4) {
      
    # Calculate Median DBH of Large trees
    dbhMedianLarge <- plotiData %>%  dplyr::filter(dbh_cm >= dbhOverstoryMin) %>%
        summarize_at(vars(dbh_cm), list(median)) %>% unlist() %>% unname()
      
    # Large trees with DBH > median, 3
    # Large trees with DBH < median, 2. If using small trees, assign those to cohort 1.
    sp_cohorts <- plotiData %>% mutate(dbhCohort = case_when(dbh_cm < dbhMedianLarge &
                  dbh_cm >= dbhOverstoryMin ~ 2,
                  dbh_cm >= dbhMedianLarge &
                  dbh_cm >= dbhOverstoryMin ~ 3,
                  dbh_cm < dbhOverstoryMin ~ 1)) %>%
                  dplyr::select(sppcode, dbhCohort) %>% 
      group_by(sppcode, dbhCohort) %>% count()
    sp_cohorts_wide <- sp_cohorts %>%   pivot_wider(
      names_from = dbhCohort,
      #names_glue = "{.value}_{dbhCohort}",
      values_from = n
    )
    }
    
    if (cohort_option == 4) {
      # Trim species from youngest age class if already occuring in 2nd oldest
      sp_cohorts_wide$'1'[which(sp_cohorts_wide$'1' > 0 & sp_cohorts_wide$'2' > 0)] <- NA
      sp_cohorts <- sp_cohorts_wide %>% pivot_longer(cols = c('1','2','3'), names_to = "dbhCohort", 
                                                     values_to = "n", values_drop_na=T)
    }
    
    if (cohort_option == 5) {
      # Assign cohorts based on relative dominance at plot, with most dominant being oldest.
      sp_cohorts <- data.frame(sppcode = names(spRbai[order(unlist(spRbai), decreasing = T)]),
                               dbhCohort = unname(unlist(sort(unlist(spRbai), decreasing=T))))
      # For top 3 species, assign 3 ages, 1 new cohort age drawn between existing draws.
      cohort_ages <- c(cohort_ages[1:2], round(runif(1, min = cohort_ages[2], max = cohort_ages[3])),
                     cohort_ages[3])
      for (j in 1: nrow(sp_cohorts)) {
        if (j < 4) {
        sp_cohorts$dbhCohort[j] <- cohort_ages[length(cohort_ages) - (j-1)]
        }
        if (j >= 4) {
          # Species ranked lower than 3rd are all assigned to youngest cohort
          sp_cohorts$dbhCohort[j] <- cohort_ages[1]
        }
      }
    } else {
      print(paste("Sorry, cohort_option ", cohort_option, "has not been implemented yet."))
      break()
      }
    
  # Plot histogram of plot DBH in cm for large trees
  if (nrow(plotiData) > 0) {
    try(with(plotiData, hist(dbh_cm, nclass=30 ,xlab="DBH (cm) ")),silent=T)
    try(abline(h=0))
    try(legend("topright",1:length(unique(sp_cohorts$dbhCohort)),
               unique(sp_cohorts$dbhCohort), bty="n"), silent=T)
    try(mtext(paste(rowIDi, ploti)), silent=T)
    try(with(plotiData, text(dbh_cm, jitter(rep(2, nrow(plotiData)), 0, 1), 
          labels=sppcode, las=2, srt = 90, col=rgb(1,0,0,0.5))), silent=T)
    try(abline(v = dbhOverstoryMin, col=2), silent=T)
  }
    
  
  # Add text for Initial Communities file entry for this plot: Mapcode value
  linei <- paste("MapCode ", paste(rowIDi))
  # write function to add the file
  #write(linei, file = "fileTxt", append = TRUE, sep = "\n")
  write.table(linei,file=fileNameOutText,append=TRUE,row.names=F,col.names=F,
              sep = "\t",quote=F)
  
  # Add text to table stored in Memory
  incomm[startn,1] <- paste("MapCode ")
  incomm[startn,2] <- paste(rowIDi)
  
  maxplotage <- max(cohort_ages, na.rm=T)
  
  # Loop through unique species in plot i
  for (k in 1:length(spRbai)) {
    spk <- names(spRbai)[k]
    if (cohort_option < 5) {
    agesIndex <- sp_cohorts %>% data.frame() %>% dplyr::filter(sppcode == spk) %>% 
      dplyr::select(dbhCohort) %>% unname() %>% unlist()
    plot_ages <- cohort_ages[agesIndex]
    } else {
      plot_ages <- sp_cohorts %>% data.frame() %>% dplyr::filter(sppcode == spk) %>% 
        dplyr::select(dbhCohort) %>% unname() %>% unlist()
    }
    # Check if estimated cohort age exceeds species longevity and adjust if needed
    # Set to 5 years younger than max longevity.
    plot_ages[which(plot_ages > spp_long$longevity[which(spp_long$sppcode==spk)])] <- spp_long$longevity[which(spp_long$sppcode==spk)] - 5
    plot_ages <- sort(unique(plot_ages))
    
    # Make an adjustment for slower growing conifer species. Landis-II initializes by 
    # planting and growing species with competition. To get initial communities where
    # Slower growing conifers are dominant, they need an age advantage, otherwise
    # They will always be suppressed if cohorts of faster growing species are present.
    
    if (spp_code_key$conifer[which(spp_code_key$sppcode == spk)] & maxplotage %in% plot_ages) {
      ## As slower growing species, give conifers a boost in spin up
      plot_ages[which(plot_ages == maxplotage)] <- maxplotage + 5 
    }
    
    # Update maxplotage if it changes due to line above
    if (max(plot_ages, na.rm=T) > maxplotage) {
      maxplotage == max(plot_ages, na.rm=T)
    }
    
    incomm[(startn + k),1] <- paste(spk)
    incomm[startn + k,2:(1 + length(plot_ages))] <- paste(sort(plot_ages))
    
    # Now write ploti species cohorts  to external text file
    lineik <- paste(spk, paste(plot_ages, collapse=" "))
    # write function to add the file
    #write(lineik, file = "fileTxt", append = TRUE, sep = "\n")
    write.table(lineik,file=fileNameOutText,append=TRUE,row.names=F,col.names=F,sep = "\t",quote=F)
    
  }
  
  startn <- startn + k + 2
  
  # Write an empty line to initial communities text file
  write.table("",file=fileNameOutText,append=TRUE,row.names=F,col.names=F,sep = "\t",quote=F)
  
  }
  
  # Record rowID, dominant species for plot i, and maxplotage for plot i
  newplotclass$rowID[i] <- rowIDi
  newplotclass$sppcode[i] <- domSppi
  newplotclass$nspp[i] <- length(spRbai)
  newplotclass$age[i] <- maxplotage
  
  # Clean up temporary files in loop
  try(rm(ageAdj, ageObsi, baploti, cohort_ages, invyri, k, linei, lineik, maxplotage,
         plot_ages, plotage_pt, ploti, plotiData, rowIDi, sp_cohorts, spk, spRbai), silent=T)
}

## Now create potential young age classes from class means for young forest in 
## Landsat disturbance maps
## Read in GMM classification output. Generate class means and create initial community classes for stands less than 50 years old.
clmeanrba <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\uvm_landis_users_code\\data\\Hanusia_plot_data_init_communities\\VT-MA_sprbacl_table_20220519.csv")

# Subset to species used in simulations
clmeanrba <- clmeanrba %>% dplyr::select(class, baM2ha, n_plots, any_of(sppSim))
# Create a table for "youngcl" to populated recently disturbed areas. Create from classes in gmmout.
classes <- sort(unique(clmeanrba$class))
nclass <- length(classes)

maxrowID <- incomm %>% dplyr::filter(grepl("MapCode", X1)) %>% 
  dplyr::select(X2) %>% unlist() %>% as.numeric() %>% max(., na.rm=T)
maxrowID <- round(maxrowID,digits=-2) # start new MapCodes at this --> 3600
maxrowID0 <- maxrowID
maxdistage <- max(youngAges)

# Specify small statured species - understory or early successional. These will
# Need a different threshold for mean BA by class to be included in young stands.
smallSpecies <- c("acerpens","betupopu","ostrvirg","prunpens","sorbamer")
colSums(clmeanrba[,which(names(clmeanrba) %in% smallSpecies)] > 0.0001)
smallRbaThresh <- 0.0001

# Limit spruce-fir to classes where it makes up 5% of mean RBA
sprucefirclasses <- clmeanrba %>% dplyr::filter(abiebals > 0.05 | picerube > 0.05) %>% 
  dplyr::select(class) %>% unlist %>% unname()

#counter <- nrow(spba) + 1

for (i in 1:nrow(clmeanrba)) {
  # For each class, only populate for species above a threshold.
  # Doesn't work for small understory species. Implement second threshold
  classi <- clmeanrba$class[i]
  orderi <-  order(unlist(clmeanrba[i,4:ncol(clmeanrba)]),decreasing=T)+3
  rbasi <- (clmeanrba)[i,orderi]
  rbasiSmall <- rbasi[which(names(rbasi) %in% smallSpecies)]
  rbasiSmall <- rbasiSmall[which(rbasiSmall > smallRbaThresh)]
  rbasi <- rbasi[which(rbasi > 0.043)] ## Changed to not overpopulate PIRU, ABBA, 2017-05-16

  # Now append small stature species with large overstory species in class
  plotspu <- c(names(rbasi),names(rbasiSmall))
  if (!(clmeanrba$class[i] %in% sprucefirclasses)) { ## Added 2017-05-16
    if ("abiebals" %in% plotspu | "picerube" %in% plotspu) {
      plotspu <- plotspu[-which(plotspu %in% c("abiebals","picerube"))]
    }
  }
  
  for (j in 1:length(youngAges)) {
    yearj <- youngAges[j]
    if (clmeanrba$class[i] > 0) {
      ## Needs to match mapped community assignment code. Should probably pull out to function.
      plotclassj <- paste(1000 + clmeanrba$class[i]*100 + yearj) ## Needs to match mapped community assignment code.
    }
    
    for (k in 1:length(plotspu)) {
      spk <- plotspu[k]
      # Ignore species when the youngAgeClass exceeds their max longevity
      if (yearj >= spp_long$longevity[which(spp_long$sppcode==spk)]) {next()}
      incomm[startn,1] <- paste("MapCode ")
      # Mapcode just increments for each new species-youngAge-class combo...but
      # We keep track of the new synthetic "plotclass" associated with the class and youngAge.
      incomm[startn,2] <- maxrowID + 1 ## Needs to match mapped community assignment code.
      # Mapcodes are nested in plotclasses.
      plot_ages <- c(max(yearj,1))
      incomm[(startn + 1),1] <- paste(spk)
      incomm[startn + 1,2:(1 + length(plot_ages))] <- paste(sort(plot_ages))
      ages3 <- c(ages3,mean(plot_ages))
      newplotclass$rowID[counter] <- maxrowID + 1
      newplotclass$class[counter] <- classi
      newplotclass$class2[counter] <- plotclassj
      newplotclass$age[counter] <- yearj
      newplotclass$nspp[counter] <- 1
      newplotclass$sppcode[counter] <- spk
      
      # Write new lines to file. First, new map code for youngAge class i.
      lineik <- paste("MapCode", maxrowID + 1)
      write.table(lineik,file=fileNameOutText,append=TRUE,row.names=F,col.names=F,sep = "\t",quote=F)
      
      # Now write ploti species cohorts  to external text file
      lineik <- paste(spk, paste(plot_ages, collapse=" "))
      write.table(lineik,file=fileNameOutText,append=TRUE,row.names=F,col.names=F,sep = "\t",quote=F)
      
      # Write an empty line to initial communities text file
      write.table("",file=fileNameOutText,append=TRUE,row.names=F,col.names=F,sep = "\t",quote=F)
      
      # Advance row, counter and mapCode value for initial communities csv and text file.
      startn <- startn + 3
      maxrowID <- maxrowID + 1
      counter <- counter + 1
    }
  }
}


unique(incomm$X1)
mapcodes <- incomm$X2[grep("MapCode",incomm$X1)]
spcodes <- data.frame(spcode = sort(sppSim))

## Prunus pensylvanica is very short-lived. Check assignment for any cohorts older than longevity.
incomm$X2[which(incomm$X1=="prunpens")][which(as.numeric(incomm$X2[which(incomm$X1=="prunpens")]) >= 35)]

newplotclass <- newplotclass[-(is.na(newplotclass$nspp)),]

#close(fileTxt)

# Write more output into processing directory
plotIdOut <- spba %>% dplyr::select(pltID, rowID)

# Trim any unused columns first
incomm <- incomm %>% select_if(function(x){!all(is.na(x))})
write.csv(incomm,paste(data_dir, "//Initial_communities_",study_area_prefix, "_", Sys.Date(),
                       ".csv", sep=""), row.names=F)
# Write out list of rowIDs (mapcodes) that were not used. 
# Plots could be excluded if there were not overstory trees or none of the simulated species present.
write.csv(rowId_not_used,paste(data_dir, "//Init_comm_rowIds_not_used_",study_area_prefix, "_", Sys.Date(),
                               ".csv", sep=""),row.names=F)
write.csv(spba, paste(data_dir, "//", study_area_prefix, "_fia_plots_sp_ba_with_rowID.csv", sep = ""), row.names=F)
write.csv(plotIdOut, paste(data_dir, "//Init_comm_pltIds_mapcodes_key_", study_area_prefix, "_",Sys.Date(),
                           ".csv", sep=""), row.names=F)
write.csv(newplotclass, paste(data_dir, "//Init_comm_youngAge_mapcodes_key_", study_area_prefix, "_",Sys.Date(),
                           ".csv", sep=""), row.names=F)
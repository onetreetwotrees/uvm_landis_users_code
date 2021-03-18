library(raster)
library(sp)
library(rgdal)
library(tidyr)
library(dplyr)
library(ggplot2)

options(dplyr.width = Inf,scipen=999, stringsAsFactors = F) # scipen=0 to turn scientific notation back on again.
source("C:/Users/janer/Dropbox/Defo/rcode/Functions_for_Landsat_defoliation.R")

## Update for Olivia's study area, NH-MA
lstoposub <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Olivia_Mass\\data_shared\\Imputation Inputs\\csv_inputs\\lstopospout_v2.csv",header=T)
## Species codes come from MA CFI, Olivia thinks.

## Check what are the BA units Olivia uses here? Need them to be in metric at this point, ba m2/ha to match with lstoposub.
## GMM output CRS "WGS_1984_UTM_Zone_19N"
plotage0 <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Olivia_Mass\\data_shared\\Imputation Inputs\\csv_inputs\\manh_age_updated_3_18.csv", header=T)
pidkey0 <-  read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Olivia_Mass\\data_shared\\Imputation Inputs\\csv_inputs\\plotids_up.csv",header=T) 
pidkey <- pidkey0 %>% dplyr::select(pid, mid)
spplist <- read.table("C:\\Users\\janer\\Dropbox\\Projects\\Olivia_Mass\\data_shared\\Imputation Inputs\\csv_inputs\\species_3_10_no_header.txt", header=F)
# It looks like Olivia entered the wrong code for pinuresi, changing to PIRE to agree with other tables.
spplist$V12[which(spplist$V1 == "pinuresi")] <- "PIRE"

plotShape <- readOGR(dsn = "C:\\Users\\janer\\Dropbox\\Projects\\Olivia_Mass\\gis", layer = "olivia_inventory_pts_UTM19N")

# Create a table for "youngcl" to populated recently disturbed areas. Create from classes in gmmout.
classes <- sort(unique(lstoposub$class))
# youngAges is based on the Landsat disturbance data set used. Olivia's data goes up to 14 years prior.
youngAges <- 0:14 # Potential forest ages that can be derived from the Landsat disturbance record (Hansen or NAFD)
# Create a vector for Olivia's species codes
spp <- spplist$V12

## Read species codes from Lichstein 2009 Eco Apps SI Table S1
spcd <- read.csv("C:\\Users\\janer\\Dropbox\\SpruceFir\\plotdata\\table_S1.csv",header=T)
spcd <- spcd %>% mutate(spp = tolower(paste(substr(gen,1,4),substr(sp,1,4),sep="")),
                        wascode = toupper(paste(substr(gen,1,3),substr(sp,1,3),sep="")))
spcd$wascode[which(spcd$wascode == "BETPAP")][1] <- "BETCOR"
spcd$jencode <- toupper(paste(substr(spcd$gen,1,2),substr(spcd$sp,1,2),sep=""))

# Set simulation start year.
simyr0 <- 2015 #2018

# Drop some columns that are not needed, 
# rename some columns to work with the rest of code. Olivia converted PLOT_BA for CFI and FIA plots 
# From ft2/ac to m2/ha with conversion factor 4.359
plotage <- plotage0 %>% mutate(std_age = stand_age, bam2ha = UpdatedBA) %>% 
             dplyr::select(mid,pid,inv_yr,bam2ha,std_age)  %>%
             mutate(age2015 = std_age + (simyr0 - inv_yr))
lstoposub <- lstoposub %>% mutate(elev_m = elevation)
plotcoords <- plotShape@data
plotcoords <- cbind(plotcoords, coordinates(plotShape))
names(plotcoords) <- c("pid","eastings","northings")
# Join to add in integer plot index "mid"
plotcoords <- plotcoords %>% inner_join(pidkey) %>% dplyr::select(mid, eastings, northings)

agecl <- lstoposub %>% dplyr::select(pid,mid,class,classpred,elev_m) %>% 
  inner_join(plotcoords, by = c("mid" = "mid")) %>% inner_join(plotage)
# Pull out species rba data, join with class predictions
sprba <- lstoposub %>% dplyr::select(pid,mid,class,classpred,spp[1]:spp[length(spp)])
sprba <- plotage %>% inner_join(sprba)

# Check that the dimensions represent the correct number of plots. 248 here
nrow(agecl)

# Subset plot data to just FIA? Or include CFI?
agecl_fia <- agecl %>% dplyr::filter(nchar(pid)  > 10)

## Now create a model that predicts plot age from plot basal area.
llplotage.lm <- lm(std_age ~ 0 + log(bam2ha), data=agecl_fia)
summary(llplotage.lm)
# Vizualize relationship with scatter plot
newx <- data.frame(bam2ha=seq(from=0.5,to=60,by=0.5))
with(agecl_fia,plot(bam2ha,std_age))
lines(newx$bam2ha,predict(llplotage.lm,newdata=newx))

# Optional: Repeat with simple Bayesian model
#library(brms)
#llplotage.brm <- brm(std_age ~ 0 + log(bam2ha),data=agecl_fia)
#p.brm <- data.frame(predict(llplotage.brm,newdata=newx))
#with(agecl_fia,plot(bam2ha,std_age,pch=21,bg="grey"))
#lines(newx$bam2ha,p.brm$Estimate,lwd=2)
#lines(newx$bam2ha,p.brm$Q2.5)
#lines(newx$bam2ha,p.brm$Q97.5)

## Define new function to round ages off to nearest 5 or 10 year interval
mround <- function(x,base){
  base*round(x/base)
} 

summary(llplotage.lm)

# Read in data from GMM analysis

# Summarize sprba table to get some mean information on species by class
# Note, there is no PIRU in the original species RBA file that Olivia shared, but it is showing up in species list
sppGmm <- sprba %>% dplyr::select(spp[1]:spp[length(spp)]) %>% names()
spba <- sprba  %>% 
  mutate(across(all_of(sppGmm), ~(.* bam2ha)))
meanRba <- sprba %>% dplyr::select(all_of(sppGmm)) %>% summarize_all(list(mean = mean))  %>% 
  as.vector() %>% unlist() %>% unname() %>% as.numeric(.) %>% round(., digits = 3)
meanBa <- spba %>% dplyr::select(sppGmm) %>%
  summarize_all(list(mean = mean)) %>% as.vector() %>% unlist() %>% unname() %>% as.numeric(.) %>% round(., digits = 3)
meanSpp <- data.frame(spp = sppGmm, meanRba = unlist(unname(meanRba)))
meanSpp$meanBa <- unlist(unname(meanBa))
# Try to count the number of plots fo which a species has the highest RBA
# get max RBA species
sprba0 <- sprba %>% dplyr::select(sppGmm)
nplotsmajrba <- colnames(sprba0)[apply(sprba0, 1, which.max)] %>% table()
nplotsmajrba <- as.data.frame(nplotsmajrba)
names(nplotsmajrba) <- c("spp","nplotsmajrba")
meanSpp <- meanSpp %>% full_join(nplotsmajrba)
meanSpp <- meanSpp %>% full_join(spplist, by = c("spp" = "V12")) %>% dplyr::select(spp:V2)
print(meanSpp)

# Calculate class means in spp ba to create informal class names list
clmean0 <- spba %>% group_by(class) %>% summarize_all(mean) %>% data.frame()
clmeanrba <- sprba %>% group_by(class) %>% summarize_all(mean) %>% data.frame() %>%
  dplyr::select(class,sppGmm)

nclass <- length(classes)

## Write informal class names from ordered list of species with BA > 1 m2/ha

class_names <- c()

for (i in 1:nclass) {
  classi <-  clmean0$class[i]
  datai <- clmean0[i,which(clmean0[i,] >= 1)]
  classi <- datai[1]
  plotba <- datai[which(names(datai)=="bam2ha")]
  datai <- datai[which(names(datai) %in% sppGmm)]
  datai <- data.frame(datai)
  datai <- round(datai[order(datai,decreasing=T)],digits = 0)
  datai[length(datai) + 1] <- round(plotba,digits = 0)
  names(datai)[length(datai)] <- "plotba"
  datai <- cbind(classi,datai)
  clname0 <- paste(names(datai),datai,sep="-")
  clname1 <- paste(clname0,collapse = ", ")
  class_names <- c(class_names,clname1)
  rm(clname0,clname1,datai)
}

#write.csv(class_names, "data\\GMM_class_names_spp_meanBA_NH-MA.csv")

# Generate simulation species list from FIA data alone, as this is the only representative data (strat. systematic sample)
spsim2 <- meanSpp$V1
sppcnt <- meanSpp %>% arrange(desc(meanBa))
spsim <- sppGmm
#> spsim
#[1] "acerrubr" "acersacc" "betualle" "betulent" "betupapy" "carycord" "fagugran" "fraxamer" "pinustro" "popugran"
#[11] "poputrem" "prunsero" "queralba" "quercocc" "querrubr" "tiliamer" "tsugcana"

# Hardcode the max longevity by spsim list
longevity <- meanSpp$V2
cbind(spsim,longevity)
# Designate target year for initiation of Landis-II
simyr_0 <- 2015 # Set this for intended start year of Landis simulation

## Set up data frame that will hold ragged array for initial communities text file.
incomm <- data.frame(matrix(NA,nrow=nrow(spba),ncol=20))
incomm[1,1] <- "LandisData"
incomm[1,2] <- "Initial Communities"
incomm[3,1] <- "MapCode 0"
#incomm[which(is.na(incomm)),] <- ""
startn <- 5
ages <- c()
oryrs <- c()

set.seed <- 898945

for (i in 1:dim(spba)[1]) {
  ploti <- spba$pid[i]
  midi <- spba$mid[i]
  baploti <- spba$bam2ha[i]
#  standi <- fvspl$STAND_ID[which(fvspl$STANDPLOT_CN == ploti)]
  invyri <- spba$inv_yr[i]
  # Don't use a plot if there is no record of inventory year. Advance to next plot.
  # Do the same if plot basal area is 0. Not sure why we have rows like that.
  if (is.na(invyri) || baploti == 0) { next() }
  classi <- spba$class[i]
  #binsna <- rep(NA,(133-12+1))
  #.......stopped here
  ## Method for FIA data, if you have tree data loaded, have calculated dbhcm from original tree DBH etc.
  # Deleted
  ## Method for Wason data - only BA by plot available
  if (ploti %in% spba$pid) {
    bas <- spba %>% dplyr::filter(mid == midi) %>% dplyr::select(sppGmm) %>% unname() %>% unlist()
    # Treat smallest observed BA as proxy for individual tree
    minba <- spba %>% dplyr::select(sppGmm) %>% na_if(., 0) %>% min(., na.rm=T)
    minba <- max(minba,0.40) # Used this approach first with Mountain Birdwatch data, but that used a different factor prism approach
    
    if (sum(bas) > 0) {
      ntrees <- bas/minba
      plotsp <- sppGmm[which(bas > 0)]
      ntrees <- ntrees[which(bas > 0)]
      plotdbh <- ntrees # Just need a length vector to coordinate with other data, even though no DBH here.
    }
  } 

  plotsp2 <- meanSpp %>% dplyr::select(spp, V1) %>% inner_join(data.frame(plotsp), by = c("spp"= "plotsp")) %>%
    dplyr::select(V1) %>% as.vector() %>% unname() %>% unlist()

   plotage_pt <- predict(newdata=data.frame(bam2ha = baploti),llplotage.lm, interval = "prediction")
  plotage <- runif(length(plotdbh),min=plotage_pt[1,2],max=plotage_pt[1,3])
  
  if ((simyr_0-invyri) > 0) {plotage <- plotage + (simyr_0-invyri)} # adjust ages estimated at time of inventory to simulation start year, 2010.
  if (invyri-simyr_0 > 0) {plotage <- plotage - (invyri-simyr_0)}
  plotage[plotage < 0] <- abs(plotage[which(plotage < 0)])
  if (length(plotage) > 2) {
    cohorts_3 <- sort(sample(plotage,3))
    dist_1 <- abs(plotage - cohorts_3[1])
    dist_2 <- abs(plotage - cohorts_3[2])
    dist_3 <- abs(plotage - cohorts_3[3])
    dists <- rbind(dist_1,dist_2,dist_3)
    cohort_inds <- apply(dists,2,which.min)
    cohorts_3 <- mround(cohorts_3,1) #cohorts_3 <- mround(cohorts_3,5)
    plotage <- cohorts_3[cohort_inds]
  } else {
    plotage <- mround(plotage,1)#plotage <- mround(plotage[,1],5)
  }
  ages <- c(ages,mean(plotage))
  spba$age[which(spba$mid == as.character(ploti))] <- paste(plotage)
  
  incomm[startn,1] <- paste("MapCode ")
  incomm[startn,2] <- paste(i)
  
  plotsptab <- data.frame(unique(cbind(plotsp,plotage)))
  # Join with spplist, replace 4 letter species code with 8-letter code
  plotsptab <- meanSpp %>% dplyr::select(spp, V1) %>% inner_join(plotsptab, by = c("spp" = "plotsp")) %>%
    rename(., plotsp = V1) #%>% dplyr::select(plotsp, plotage)
  plotspu <- plotsptab$plotsp
  plotspu <- plotspu[plotspu %in% spsim2]
   maxplotage <- max(as.numeric(plotsptab$plotage), na.rm=T) # Modified 2021-03-18
  for (k in 1:length(plotspu)) {
    spk <-plotspu[k]
    plot_ages <- plotsptab$plotage[which(plotsptab$plotsp == spk)]
    plot_ages <- as.numeric(plot_ages)
    plot_ages[which(plot_ages > longevity[which(spsim==spk)])] <- longevity[which(spsim==spk)] - 5
    plot_ages <- sort(unique(plot_ages))
    if (spk == "picerube" & maxplotage %in% plot_ages) {
      plot_ages[which(plot_ages == maxplotage)] <- maxplotage + 1 ## As slowest growing species, give PIRU a boost in spin up
    }
    incomm[(startn + k),1] <- paste(spk)
    incomm[startn + k,2:(1 + length(plot_ages))] <- paste(sort(plot_ages))
  }

  startn <- startn + k + 2
  
}


maxplotnum <- max(as.numeric(incomm$X2[5:dim(incomm)[1]]),na.rm=T)
maxplotnum <- round(maxplotnum,digits=-2) + 100 # start new plot numbers at this --> 300.
maxplotnum0 <- maxplotnum
# What is the max age you could get by sampling plot location from a Landsat disturbance map?
maxdistage <- max(youngAges)

## Create initial community classes for stands less than 50 years old.
ages3 <- c()
# In this version, each 5yr young community of a given class will have x number of plots, 
# That correspond to a single species of the given age for all species > 5% rba in the class.
# The result should mean young stands are "sprinkled" with the species that occur in the mapped class.
newplotclass <- data.frame("NA."=rep(NA,nclass*length(youngAges)*length(spsim)),"mid"=NA,"plotba"=NA,"class"=NA,"age"=NA,
                           "ht"=NA,"nspp"=NA,"plot"=NA)
names(clmeanrba) <- c("class", spsim2)

counter <- 1

for (i in 1:nrow(clmeanrba)) {
  orderi <-  order(clmeanrba[i,2:dim(clmeanrba)[2]],decreasing=T)+1
  rbasi <- (clmeanrba)[i,orderi]
  rbasi <- rbasi[which(rbasi > 0.05)] ## You can modify to include species with diff. min rba.
  #rbasi <- rbasi[which(rbasi > 0.01)]
  plotspu <- names(rbasi)
  
  for (j in 1:length(youngAges)) {
    yearj <- youngAges[j]
    if (clmeanrba[i,1] >0) {
      ## Needs to match mapped community assignment code. Should probably pull out to function.
      plotclassj <- paste(1000 + clmeanrba[i,1]*100 + yearj) ## Needs to match mapped community assignment code.
    }

    for (k in 1:length(plotspu)) {
      spk <-plotspu[k]
      incomm[startn,1] <- paste("MapCode ")
      incomm[startn,2] <- maxplotnum + 1 ## Needs to match mapped community assignment code.
      plot_ages <- c(max(yearj,1))
      incomm[(startn + 1),1] <- paste(spk)
      incomm[startn + 1,2:(1 + length(plot_ages))] <- paste(sort(plot_ages))
      ages3 <- c(ages3,mean(plot_ages))
      newplotclass$plot[counter] <- maxplotnum + 1
      newplotclass$class[counter] <- plotclassj
      newplotclass$age[counter] <- yearj
      newplotclass$nspp <- 1
      startn <- startn + 3
      maxplotnum <- maxplotnum + 1
      counter <- counter + 1
    }
  }
}


unique(incomm$X1)
mapcodes <- incomm$X2[grep("MapCode",incomm$X1)]
spcodes <- data.frame(spcode = sort(spsim2))
spcodes$newcode <- toupper(spcodes$spcode)
spcodes$other <- spsim

for (i in 1:dim(incomm)[1]) {
  if (incomm$X1[i] %in% spcodes$spcode) {
    spind <- which(spcodes$spcode == incomm$X1[i])
    incomm$X1[i] <- spcodes$newcode[spind]
  }
}

## Check assignment for any cohorts older than longevity.
## Didn't do that here.
newplotclass <- newplotclass[-(which(is.na(newplotclass$plot))),]
newplotclass0 <- newplotclass[0,]
mapcodes0 <- as.numeric(mapcodes)[which(as.numeric(mapcodes) <= maxplotnum0)]
mapcodes0 <- cbind(mapcodes0,ages)

## Write results out to text files for later imputation mapping code.
## First write .csv of initial community file as .csv text file. You
## will need to open this in a text edito, Excel can work, Find and replace all
## NA with a blank space, then save as Tab delimited text file with .txt extension
## For Landis-ii.
write.csv(incomm,"data\\landis_initial_comm_data_NH-MA_v0.csv",row.names=F)
write.csv(newplotclass,"data\\landis_age_class_plot_young_stands_NH-MA_v0.csv")
write.csv(mapcodes0, "data\\landis_mapcodes_NH-MA_v0.csv")
write.csv(spcodes, "data\\landis_spcodes_NH-MA_v0.csv", row.names=F)
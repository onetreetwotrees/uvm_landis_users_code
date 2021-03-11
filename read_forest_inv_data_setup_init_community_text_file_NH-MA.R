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
plotage0 <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Olivia_Mass\\data_shared\\Imputation Inputs\\csv_inputs\\manh_age_updated.csv", header=T)
pidkey <-  read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Olivia_Mass\\data_shared\\Imputation Inputs\\csv_inputs\\plotids_up.csv",header=T) 
pidkey <- pidkey %>% dplyr::select(pid, mid)
spplist <- read.table("C:\\Users\\janer\\Dropbox\\Projects\\Olivia_Mass\\data_shared\\Imputation Inputs\\csv_inputs\\species_11_24_no_header.txt", header=F)

# Create a table for "youngcl" to populated recently disturbed areas. Create from classes in gmmout.
classes <- sort(unique(lstoposub$class))
youngAges <- 0:29 # Potential forest ages that can be derived from the Landsat disturbance record (Hansen or NAFD)


## Read species codes from Lichstein 2009 Eco Apps SI Table S1
spcd <- read.csv("C:\\Users\\janer\\Dropbox\\SpruceFir\\plotdata\\table_S1.csv",header=T)
spcd <- spcd %>% mutate(spp = tolower(paste(substr(gen,1,4),substr(sp,1,4),sep="")),
                        wascode = toupper(paste(substr(gen,1,3),substr(sp,1,3),sep="")))
spcd$wascode[which(spcd$wascode == "BETPAP")][1] <- "BETCOR"
spcd$jencode <- toupper(paste(substr(spcd$gen,1,2),substr(spcd$sp,1,2),sep=""))

# Set simulation start year.
simyr0 <- 2018

# Add a new ID column for map-plot-id (mid)
lstoposub <- lstoposub %>% mutate(mid = 1:nrow(lstoposub)) 

# Drop some columns that are not needed
plotage <- plotage0 %>% dplyr::select(pid,inv_yr,baft2ac,std_age) %>% mutate(age2018 = std_age + (simyr0 - inv_yr),
                                                                             bam2ha = baft2ac/ 4.35600) #http://www.firewords.net/UnitCoversions/BasalAreaUnits.htm
agecl <- lstoposub %>% dplyr::select(pid,mid,class,classpred,elev_m) %>% inner_join(plotcoords) %>% inner_join(plotage)
# Pull out species rba data, join with class predictions
sprba <- lstoposub %>% dplyr::select(pid,mid,class,classpred,ABBA:TSCA)
sprba <- plotage %>% inner_join(sprba)

# Check that the dimensions represent the correct number of plots. ADK = 1263 here
nrow(agecl)

agecl_fia <- agecl %>% dplyr::filter(nchar(pid)  > 10)

# Read FIA NY inventory data for ADK study area

## Now create a model that predicts plot age from plot basal area.
#llplotage.lm <- lm(log(STDAGE) ~ log(bam2ha), data=fia_age)
llplotage.lm <- lm(std_age ~ 0 + log(bam2ha), data=agecl_fia)
summary(llplotage.lm)
# Vizualize relationship with scatter plot
newx <- data.frame(bam2ha=seq(from=0.5,to=60,by=0.5))
with(agecl_fia,plot(bam2ha,std_age))
lines(newx$bam2ha,predict(llplotage.lm,newdata=newx))

# Repeat with simple Bayesian model
library(brms)
llplotage.brm <- brm(std_age ~ 0 + log(bam2ha),data=agecl_fia)
p.brm <- data.frame(predict(llplotage.brm,newdata=newx))
with(agecl_fia,plot(bam2ha,std_age,pch=21,bg="grey"))
lines(newx$bam2ha,p.brm$Estimate,lwd=2)
lines(newx$bam2ha,p.brm$Q2.5)
lines(newx$bam2ha,p.brm$Q97.5)

## Define new function to round ages off to nearest 5 or 10 year interval
mround <- function(x,base){
  base*round(x/base)
} 


cmbins <- 12:133## 5:53 ## sort(unique(fvs$DIAMETER,na.rm=T))

summary(llplotage.lm)

# Read in data from GMM analysis
pidkey <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\data_output\\adk_sid_pid_key.csv",header=T)
errall <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\data_output\\adk_lstopospout_gmm_v1.csv",header=T) # These data have been scaled
errclass <- errall %>% dplyr::select(pid,class,classpred)
sppcnt2 <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\data_output\\adk_sppcount_simspp.csv",header=T)
lstoposub <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\data_output\\adk_lstoposub_v3.csv",header=T) # unscaled
spbasub <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\data_output\\adk_spbasub_v3.csv",header=T) # unscaled

spbasub <- spbasub %>% inner_join(errclass) %>% inner_join(lstoposub)
colinderr <- which(names(spbasub) %in% sppcnt2$spp)
resub_ind <- which(names(spbasub) %in% c("class","classpred","elevation","bam2ha"))
colinderr <- c(colinderr,resub_ind)
errspp <- spbasub[,colinderr]
clmean0 <- errspp %>% group_by(class) %>% summarize_all(mean)
# Next line doesn't work
clresubmaj <- errspp %>% group_by(class) %>% summarize_at(vars(classpred),funs(function(x) {names(which.max(table(x)))})) 

nclass <- length(clmean0$class)
## Write informal class names from ordered list of species with BA > 1 m2/ha

class_names <- c()

for (i in 1:nclass) {
  classi <-  clmean0$class[i]
  datai <- clmean0[i,which(clmean0[i,] >= 1)]
  classi <- datai[1]
  plotba <- datai[which(names(datai)=="bam2ha")]
  datai <- datai[-which(names(datai) %in% c("class","bam2ha","elevation","classpred"))]
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

sprucefirclasses <- unique(c(grep("abiebals",class_names),grep("picerube",class_names)))

# Read in spba file to get pid to spcn2 key for fvs and mbw data. Smaller because they only contain plots with ls data.

spbasub$age <- NA

# Generate simulation species list from FIA data alone, as this is the only representative data (strat. systematic sample)
sppcnt <- sppcnt %>% arrange(desc(meanbam2ha))
spsim <- sort(sppcnt$spp[1:26])
#> spsim
#[1] "abiebals" "acerrubr" "acersacc" "betualle" "betupapy" "fagugran" "fraxamer" "fraxnigr" "larilari" "piceabie" "piceglau" "picemari"
#[13] "picerube" "pinubank" "pinuresi" "pinustro" "pinusylv" "popugran" "poputrem" "prunsero" "querrubr" "robipseu" "thujocci" "tiliamer"
#[25] "tsugcana" "ulmuamer"

# Hardcode the max longevity by spsim list
longevity <- c(200,150,350,300,100,350,300,300,180,200,300,300,400,100,300,400,200,100,
               100,250,250,150,400,180,500,300) ## Last three are new MB spp
cbind(spsim,longevity)
# Designate target year for initiation of Landis-II
simyr_0 <- 2015 # Set this for intended start year of Landis simulation

## Set up data frame that will hold ragged array for initial communities text file.
incomm <- data.frame(matrix(NA,nrow=dim(spbasub)[1],ncol=20))
incomm[1,1] <- "LandisData"
incomm[1,2] <- "Initial Communities"
incomm[3,1] <- "MapCode 0"
#incomm[which(is.na(incomm)),] <- ""
startn <- 5
ages <- c()
oryrs <- c()

set.seed <- 45980

for (i in 1:dim(spbasub)[1]) {
  ploti <- spbasub$pid[i]
  sidi <- spbasub$sid[i]
  baploti <- spbasub$bam2ha[i]
#  standi <- fvspl$STAND_ID[which(fvspl$STANDPLOT_CN == ploti)]
  invyri <- spbasub$INVYR[i]
  classi <- spbasub$class[i]
  binsna <- rep(NA,(133-12+1))
  #.......stopped here
  if (sidi %in% tree$PointID) {
    plotdbh <- tree %>% dplyr::filter(PointID == sidi & dbhcm >= 12) %>% select(dbhcm) %>% unname() %>% unlist()
    plotsp <- tree %>% dplyr::filter(PointID == sidi & dbhcm >= 12) %>% select(SpeciesCode)
    plotsp <- spcd %>% inner_join(plotsp,by=c("FVS"="SpeciesCode")) %>% select(spp) %>% unname() %>% unlist()
    # Advance loop if no overstory trees are present
    if (length(plotsp) == 0) next() 
    
    plotdbh2 <- round(plotdbh)
    treecnt <- tree %>% dplyr::filter(PointID == sidi & dbhcm >= 12) %>% select(ba10) %>% unname() %>% unlist()
    plothist <- aggregate(treecnt,by=list(plotdbh2),sum)
    binsna[match(plothist$Group.1,cmbins)] <- plothist$x
    
  }
  if (sidi %in% treemaxl$PLT_CN) {
    plotdbh <- treemaxl %>% dplyr::filter(PLT_CN == sidi) %>% dplyr::select(dbhcm) %>% unname() %>% unlist()
    plotsp <- treemaxl  %>% dplyr::filter(PLT_CN == sidi) %>% dplyr::select(sppcode)  %>% unname() %>% unlist()
    plotdbh2 <- round(plotdbh)
    treecnt <- treemaxl %>% dplyr::filter(PLT_CN == sidi) %>% dplyr::select(tpha) %>% unname() %>% unlist()
    plothist <- aggregate(treecnt,by=list(plotdbh2),sum)
    binsna[match(plothist$Group.1,cmbins)] <- plothist$x
  }
  
  if (sidi %in% sunytree$pid) {
    plotdbh <- sunytree %>% dplyr::filter(pid == sidi) %>% dplyr::select(dbhcm) %>% unname() %>% unlist()
    plotsp <- sunytree  %>% dplyr::filter(pid == sidi) %>% dplyr::select(spp)  %>% unname() %>% unlist()
    plotdbh2 <- round(plotdbh)
    treecnt <- sunytree %>% dplyr::filter(pid == sidi) %>% dplyr::select(tpha) %>% unname() %>% unlist()
    plothist <- aggregate(treecnt,by=list(plotdbh2),sum)
    binsna[match(plothist$Group.1,cmbins)] <- plothist$x
  }
  ## Method for Wason data - only BA by plot available
  if (sidi %in% was$pid) {
    bas <- was %>% dplyr::filter(pid == sidi) %>% dplyr::select(ba) %>% unname() %>% unlist()
    # Treat smallest observed BA as proxy for individual tree
    minba <- was %>% dplyr::filter(ba > 0) %>% dplyr::select(ba) %>% min()
    minba <- max(minba,0.40) # Used this approach first with Mountain Birdwatch data, but that used a different factor prism approach
    
    if (sum(bas) > 0) {
      plottrees <- c()
      
      for (j in 4:dim(wasplotspba2)[2]) {
        ntrees <- wasplotspba2[which(wasplotspba2$sid == sidi),j]/minba
        plottrees <- c(plottrees,rep(names(wasplotspba2)[j],ntrees))
      }
      plotsp <- unique(plottrees)
      plotdbh <- plottrees # Just need a length vector to coordinate with other data, even though no DBH here.
    }
  } 

  plotsp <- plotsp[plotsp %in% spsim]
  if (!(sidi %in% was$pid)) {
  plotinds <- which(plotsp %in% spsim) 
  plotdbh <- plotdbh[plotinds]
  }
  #plotage <- predict(newdata=data.frame(dbhcm=plotdbh),dbhage.lm,interval="prediction")
  plotage_pt <- predict(newdata=data.frame(bam2ha = baploti),llplotage.lm, interval = "prediction")
  plotage <- runif(length(plotdbh),min=plotage_pt[1,2],max=plotage_pt[1,3])
  
  if (simyr_0-invyri > 0) {plotage <- plotage + (simyr_0-invyri)} # adjust ages estimated at time of inventory to simulation start year, 2010.
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
    #plotage <- mround(runif(length(plotdbh),min=plotage[,2],max=plotage[,3]),20)
    plotage <- cohorts_3[cohort_inds]
  } else {
    plotage <- mround(plotage,1)#plotage <- mround(plotage[,1],5)
  }
  ages <- c(ages,mean(plotage))
  spbasub$age[which(spbasub$sid == as.character(ploti))] <- plotage
  
  try(barplot(binsna,names.arg=cmbins,xlab="DBH (cm) "),silent=T)
  try(abline(h=0))
  try(legend("topright",1:length(unique(plotage)),sort(unique(plotage))))
  
  incomm[startn,1] <- paste("MapCode ")
  incomm[startn,2] <- paste(i)
  
  plotsptab <- data.frame(unique(cbind(plotsp,plotage)))
  plotspu <- sort(unique(plotsp))
  plotspu <- plotspu[plotspu %in% spsim]
#  maxplotage <- max(as.numeric(levels(plotsptab$V2)[plotsptab$V2]),na.rm=T) ## Added 2017-05-16
  maxplotage <- max(as.numeric(plotsptab[,2]),na.rm=T) # Modified 2020-02-03
  for (k in 1:length(plotspu)) {
    spk <-plotspu[k]
    plot_ages <- plotsptab[which(plotsptab$plotsp == spk),2]
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
maxplotnum <- round(maxplotnum,digits=-2) # start new plot numbers at this --> 1300.
maxplotnum0 <- maxplotnum
maxdistage <- max(dist_age$age2015,na.rm=T)

## Read in SAS classification output. Generate class means and create initial community classes for stands less than 50 years old.
clmeanrba <- cbind(clmean0[,1],clmean0[,2:29]/clmean0$bam2ha)
#youngyrs <- c(seq(0,maxdistage,by=5))
youngyrs <- sort(unique(dist_age$age2015, na.rm=T))
ages3 <- c()
# Create new dataframe in the same format as the cfi_age_class_plot file that is used by IDL to assign communities.
# In this version, each 5yr young community of a given class will have x number of plots, 
# That correspond to a single species of the given age for all species > 0.01% rba in the class.
# The result should mean young stands are "sprinkled" with the species that occur in the mapped class.
newplotclass <- data.frame("NA."=rep(NA,nclass*length(youngyrs)*length(simspp)),"spcn2"=NA,"plotba"=NA,"class"=NA,"age"=NA,
                           "ht"=NA,"nspp"=NA,"plot"=NA)
counter <- 1

for (i in 1:dim(clmeanrba)[1]) {
  orderi <-  order(clmeanrba[i,2:dim(clmeanrba)[2]],decreasing=T)+1
  rbasi <- (clmeanrba)[i,orderi]
  rbasi <- rbasi[which(rbasi > 0.043)] ## Changed to not overpopulate PIRU, ABBA, 2017-05-16
  #rbasi <- rbasi[which(rbasi > 0.01)]
  plotspu <- names(rbasi)
  if (!(clmeanrba[i,1] %in% sprucefirclasses)) { ## Added 2017-05-16
    if ("abiebals" %in% plotspu | "picerube" %in% plotspu) {
      plotspu <- plotspu[-which(plotspu %in% c("abiebals","picerube"))]
    }
  }
  
  for (j in 1:length(youngyrs)) {
    yearj <- youngyrs[j]
    if (clmeanrba[i,1] < 14) {
      plotclassj <- paste(3000 + clmeanrba[i,1]*100 + yearj) ## Needs to match mapped community assignment code.
    }
    if (clmeanrba[i,1] > 13) {
      plotclassj <- paste(clmeanrba[i,1]*100 + yearj)
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
spcodes <- data.frame(spcode = sort(spsim))
spcodes$newcode <- toupper(spcodes$spcode)

for (i in 1:dim(incomm)[1]) {
  if (incomm$X1[i] %in% spcodes$spcode) {
    spind <- which(spcodes$spcode == incomm$X1[i])
    incomm$X1[i] <- spcodes$newcode[spind]
  }
}

## Prunus pensylvanica is very short-lived. Check assignment for any cohorts older than longevity.
incomm$X2[which(incomm$X1=="PRUNPENS")][which(as.numeric(incomm$X2[which(incomm$X1=="PRUNPENS")]) >= 35)] <- 30

newplotclass <- newplotclass[-(which(is.na(newplotclass$plot))),]
newplotclass0 <- newplotclass[0,]
mapcodes0 <- as.numeric(mapcodes)[which(as.numeric(mapcodes) <= maxplotnum0)]
mapcodes0 <- cbind(mapcodes0,ages)

setwd("C:/Users/janer/Dropbox/Projects/Adirondacks/data_output")
write.csv(incomm,"landis_initial_comm_data_adk_v0.csv",row.names=F)
write.csv(newplotclass,"landis_age_class_plot_young_stands_adk_v0.csv")
## Predict ages from fvsage.lm relationship. Use random draw from a prediction interval to capture variabilty.

## Read in LSTOPO that goes into SAS. Check that pid matches pid from spba and spbasm. Right now it is off by 1! lstoposub3 is okay.
#lstopo <- read.csv("C:\\Users\\janer\\Dropbox\\SpruceFir\\plotdata\\fvs_lstoposub3.csv",header=T)
#lstopo4 <- merge(lstopo,fvskey,by.x="spcn2",by.y="spcn2")
#lstopo4 <- lstopo4[,-44]
#names(lstopo4)[45] <- "pid"
#write.csv(lstopo4,"C:\\Users\\janer\\Dropbox\\SpruceFir\\plotdata\\fvs_lstoposub4.csv",row.names=F)

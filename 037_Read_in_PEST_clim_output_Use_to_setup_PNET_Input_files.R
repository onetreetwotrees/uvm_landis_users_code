#Developed by Dr. Jane Foster
#University of Vermont

#load the necessary libraries
library(raster)
library(sp)
library(rgdal)
library(sqldf)
library(RColorBrewer)
library(rgeos)
library(stringr)
library(Hmisc)
library(dplyr)

## Update these values for gcm, scenario, and your study are prefix (use the same one for all your files)
gcm <- "CCSM4"           # 'CCSM4', 'CESM1-BGC','HADGEM2-ES','MPI-ESM-LR','HadGEM2-ES'
rcp0 <- "rcp85"          # 'historical','rcp26','rcp45','rcp60','rcp85'
studyarea <- "MA-NH"

## UPDATE DIRECTORY PATHS HERE TO SOMETHING THAT MAKES SENSE ON YOUR SYSTEM
## Read in results of GoogleEarth script that calculates zonal stats of climate data by soils class.
#rootdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Olivia_Mass\\data_download\\climate\\"
pestdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\models\\PestCalc\\"
pnetdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\models\\PNET\\"

## Read in files that you need. Site template file for your study area and ecoregions and Hubbard brook PAR file.
## Update path - This file is available on the Shared Google Drive
parhb <- read.table("C:\\Users\\janer\\Dropbox\\SpruceFir\\PNET_C1\\Input\\climate.clim",header=T)
# Read in template for pnet site file with YOUR studyarea ecoregion and spp data...
tempsite <- read.table(paste(pnetdir, studyarea,"_template.site",sep=""),header=F)

# First read in climate table output used by Pest_Calculator to create input files
# Jane's file path shown here. Update to files in your directory structure.
pestclimtmp <- read.table(paste(pestdir, studyarea, "\\NEX_pestclim_", gcm, "_", rcp0,"_out.txt",sep=""), header=T)

### Now set up PNET input files...
## Set up pnet clim file
ecos <- sort(unique(pestclimtmp$Ecoregion))
if (!(exists("yearspest"))) {yearspest <- sort(unique(pestclimtmp$year))}

## Now parse the pest climate dataframe to create annual input files for PnET for Landis-II.
nyearspest <- length(yearspest)
neco <- length(ecos)
years1 <- min(yearspest):max(yearspest)

### Now set up PNET input files...

yearspnet <- yearspest
nyearspnet <- length(yearspnet)
pestclim2 <- pestclimtmp %>% dplyr::filter(year %in% yearspnet)
pnetclim <- data.frame(matrix(nrow=12*length(ecos)*nyearspnet,ncol=7))
names(pnetclim) <- c("Ecoregion","year","doy","Tmax","Tmin","par","pptcm")

# Modify names in parhb to facilitate inner_join
names(parhb)[c(1,2,5)] <- c("year","doy","par")
#pnetclim$doy <- rep(c(15,46,76,107,137,168,198,229,259,290,321,351),nyearspnet)
# Change midday of month to match parhb
parhbdoys <- sort(unique(parhb$doy))#c(15,46,74,105,135,166,196,227,258,288,319,349)
pnetclim$doy <- rep(parhbdoys,nyearspnet)

pnetclim$Ecoregion <- pestclim2$Ecoregion
pnetclim$year <- pestclim2$year
pnetclim$Tmax <- round(pestclim2$avgmaxT,digits=4)
pnetclim$Tmin <- round(pestclim2$avgminT,digits=4)
pnetclim$pptcm <- round(pestclim2$avgppt, digits=4)

set.seed(75)

# Draw par values for each year randomly
for (i in 1:12) {
  doyi <- parhbdoys[i]
  parsi <- parhb %>% dplyr::filter(doy == doyi & year > 1900) %>% dplyr::select(par)
  rparsi <- base::sample(parsi$par,nyearspnet,replace=T)
  rparsi <- data.frame(year=yearspnet,parsi=rparsi,doy=doyi)
  #pnetclim$par[which(pnetclim$doy == doyi)] <- round(rparsi,digits=4)
  pnetclim <- dplyr::left_join(pnetclim,rparsi,by=c("year","doy"))
  pnetclim$par[which(pnetclim$doy == doyi)] <- pnetclim$parsi[which(pnetclim$doy == doyi)]
  pnetclim <- pnetclim[-which(names(pnetclim) == "parsi")]
  #pnetclim <- base::merge(pnetclim,rparsi,by=c("year","doy"))
}


# Create new PNET batch file with commands to call pnet for each year for this scenario...
pnetbatch <- data.frame(cmd=rep(NA,length(yearspnet)))

# Combine files to create input files for each year for PNET model, write out to PNET directory
for (i in 1:length(yearspnet)) {
  pnettemp <- pnetclim[which(pnetclim$year == yearspnet[i]),]
  tempname <- paste(pnetdir, studyarea, "_",gcm,"_",rcp0,"_",
                    yearspnet[i],".clim",sep="")
  tempname2 <- paste(pnetdir, studyarea, "_",gcm,"_",rcp0,"_",
                     yearspnet[i],".site",sep="")
  pnetcmdi <- paste("pnet2r -f -a1 -dvegdata/ ", studyarea, "_",gcm,"_",rcp0,"_",yearspnet[i],sep="")
  pnetbatch$cmd[i] <- pnetcmdi
  write.table(pnettemp,tempname,sep="\t",col.names=F,row.names=F)
  write.table(tempsite,tempname2,sep="\t",col.names=F,row.names=F,quote=F)
  rm(pnettemp,tempname,tempname2)
}

write.table(pnetbatch,paste(pnetdir,"RunPnET_", studyarea," _NEX_",gcm,"_",rcp0,"_many_years.bat",sep=""),
            col.names=F,row.names=F,quote=F)



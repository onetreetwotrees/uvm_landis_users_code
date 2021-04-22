#Developed by Dr. Jane R Foster
#University of Minnesota

#load the necessary libraries
library(raster)
library(sp)
library(rgdal)
library(sqldf)
library(RColorBrewer)
library(rgeos)
library(dplyr)

rm(list=ls())
cat("\014") 
try(dev.off(), silent = T)
gc()

#specify root name for global circulation model and representative concentration pathway rcp
studyarea <- 'MA-NH'
#gcms <- c('TopoWxPrism',rep(c('CCSM4','CESM1-BGC','HadGEM2-ES','MPI-ESM-LR'),2))
gcms <- c('TerraClimate',rep(c('CCSM4','CESM1-BGC','HadGEM2-ES','MPI-ESM-LR'),2))
rcps <- c('historical',rep(c('rcp26','rcp45','rcp85'),2),
          rep(c('rcp85','rcp45','rcp26'),2)) # 'historical','rcp25','rcp45','rcp65','rcp85'
#j=2
gcm <- gcms[1]
rcp0 <- rcps[1]
simyr0 <- 2015 # Specify for a given simulation what the start year should be

## Read in results of GoogleEarth script that calculates zonal stats of climate data by soils class.
dropboxdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\"
#pestdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\models\\PestCalc\\"
#pnetdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\models\\PNET\\"
pestdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Olivia_Mass\\data_shared\\Pest_and_PNET_outputs\\PEST_outputs"
pnetdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Olivia_Mass\\data_shared\\Pest_and_PNET_outputs\\PNET\\"

#jane path, upate for your path and variable names. You want a list of your ecoregion numbers here in the variable "ecoregions"
eco <- read.table(paste("data\\","All_ecoregion.txt", sep=""), header=T)
ecoregions <- sort(unique(eco$Ecoregion))
ecosa <- ecoregions[1:min(length(ecoregions),(20))]
ecosb <- ecoregions[!(ecoregions %in% ecosa)]

## Set years for dynamic inputs
if (length(grep("TerraClimate",gcm)) > 0) {
  yearspest <- 1970:2016
} else yearspest <- 2006:2099

if (gcm == "HadGEM2-ES") {yearspest <- 2006:2098}
nyearspest <- length(yearspest)

## Read some PestCalculator output and stitch tables back together
for (i in 1:length(yearspest)) {
  if (gcm == 'TerraClimate') {
    #tempname <- paste(pestdir,studyarea,"\\",gcm,"_",rcp0,"\\","PestSppEcoregionTable_",studyarea,"_",gcm,"_",rcp0,"_",yearspest[i],"a.txt",sep="")
    #tempname2 <- paste(pestdir,studyarea,"\\",gcm,"_",rcp0,"\\","PestSppEcoregionTable_",studyarea,"_",gcm,"_",rcp0,"_",yearspest[i],"b.txt",sep="")
    tempname <- paste(pestdir,"\\",gcm,"\\",rcp0,"\\","PestSppEcoregionTable",yearspest[i],"a.txt",sep="")
    tempname2 <- paste(pestdir,"\\",gcm,"\\",rcp0,"\\","PestSppEcoregionTable",yearspest[i],"b.txt",sep="")
  } else {
    #tempname <- paste(pestdir,studyarea,"\\NEX_",gcm,"_",substr(rcp0,4,5),"\\PestSppEcoregionTable",yearspest[i],"a.txt",sep="")
    #tempname2 <- paste(pestdir,studyarea,"\\NEX_",gcm,"_",substr(rcp0,4,5),"\\PestSppEcoregionTable",yearspest[i],"b.txt",sep="")
    tempname <- paste(pestdir,"\\",gcm,"\\",rcp0,"\\","PestSppEcoregionTable",yearspest[i],"a.txt",sep="")
    tempname2 <- paste(pestdir,"\\",gcm,"\\",rcp0,"\\","PestSppEcoregionTable",yearspest[i],"b.txt",sep="")
  }
  pesttemp <- read.table(tempname,header=T)
  if (length(ecosb) > 0) {
    pesttemp2 <- read.table(tempname2,header=T)
  }

  if (i == 1 & length(ecosb) > 0) {
    pesttemp3 <- cbind(pesttemp,pesttemp2)
    names(pesttemp3) <- ecoregions
    pesttemp3 <- cbind(data.frame(year=yearspest[i],spp=row.names(pesttemp3)),pesttemp3)
    rm(tempname,tempname2,pesttemp,pesttemp2)
  }
  if (i > 1 & length(ecosb) > 0) {
    pesttemp4 <- cbind(pesttemp,pesttemp2)
    names(pesttemp4) <- ecoregions
    pesttemp4 <- cbind(data.frame(year=yearspest[i],spp=row.names(pesttemp4)),pesttemp4)
    pesttemp3 <- rbind(pesttemp3,pesttemp4)
    rm(tempname,tempname2,pesttemp,pesttemp2,pesttemp4)
  }
  # For studyareas with fewer ecoregions, like Olivias
  if (i == 1 & length(ecosb) == 0) {
    pesttemp3 <- pesttemp
    names(pesttemp3) <- ecoregions
    pesttemp3 <- cbind(data.frame(year=yearspest[i],spp=row.names(pesttemp3)),pesttemp3)
    rm(tempname,tempname2,pesttemp)
  }
  if (i > 1 & length(ecosb) == 0) {
    pesttemp4 <- pesttemp
    names(pesttemp4) <- ecoregions
    pesttemp4 <- cbind(data.frame(year=yearspest[i],spp=row.names(pesttemp4)),pesttemp4)
    pesttemp3 <- rbind(pesttemp3,pesttemp4)
    rm(tempname,tempname2,pesttemp,pesttemp4)
  }
  
  
}

## View the data table you created by stitching together all of the PestCalc output files
## Each row is a species and year combination and the numbered columns are the ecoregions
head(pesttemp3)

## Now read in PNET output data..
yearspnet <- yearspest
pnetspbig <- read.table(paste(pnetdir, studyarea,"_template_site_by_ecoregion.txt", sep=""))
## This next line finds out which column holds spp codes, but only works for current species list. May need to update.
spcol <- which(pnetspbig[1,] == "abiebals" | pnetspbig[1,]== "acerrubr")
nspp <- length(unique(pnetspbig[,spcol]))
splist <- pnetspbig[1:nspp,spcol]
if (is.factor(splist)) {
  splist <- levels(splist)[splist]
}

pnetout <- read.table(paste(pnetdir,studyarea,"_",gcm,"_",rcp0,"_", yearspnet[i],".out",sep=""))
ecoregionsxspp <- pnetout$V1
splistxecoregions <- rep(splist,length(ecoregions))
necoxspp <- length(ecoregionsxspp)

pnetout <- data.frame(matrix(data=NA, nrow=length(ecoregionsxspp) * length(yearspnet),ncol=10))
names(pnetout) <- c("Ecoregion","year","mo","N","NPPleaf","NPPwood","NPProot","runoffcm","unknown","sp")

nyears <- yearspnet[length(yearspnet)]-yearspnet[1]+1
years <- yearspnet

for (i in 1:length(splist)) {
  startrow <- ((i*1-1)*nyears+1)
  pnetout$sp[startrow:(startrow+nyears-1)] <- splist[i]
  pnetout$year[startrow:(startrow+nyears-1)] <- years
}


for (i in 1:length(yearspnet)) {
  tempname <- paste(pnetdir,studyarea,"_",gcm,"_",rcp0,"_", yearspnet[i],".out",sep="")
  pnettemp <- read.table(tempname)
  pnettemp[,10] <- splistxecoregions
  startrow <- ((i*1-1)*necoxspp+1)
  pnetout[startrow:(startrow+necoxspp-1),] <- pnettemp

  rm(pnettemp,tempname,startrow)
}

pnetoutbig <- pnetout

## Set up table to hold parameters for Dynamic Inputs - Biomass Succession - LANDIS-II

#>> Year Ecoregion Species ProbEst MaxANPP MaxB
#>> -------- -------------
simyears <- 2015:2215 # 1970:2016 # Originally 2006:2098, years run under PNET and PEST. Modified to randomly draw from last 20 years through 2200.
#simyears <- 1900:2017 # For historical Prism data, create one file with all years for later
dyntab <- data.frame(matrix(data=NA,nrow=length(ecoregionsxspp) * length(simyears)+4,ncol=6))
names(dyntab) <- c("year","eco","spp","probest","maxanpp","maxbio")

set.seed(9869) # (4678) <- Rep 4 #set.seed(4678) # (348) <- Rep 3 #set.seed(348) # (3295) <- Rep 2 #set.seed (9869) # <- Rep 1
repnum <- "Rep1" ## Should be one of "Rep1", "Rep2", "Rep3", "Rep4" and match the seed number in line above

if (gcm=="TerraClimate") {randyears <- sample(1:length(simyears))}

for (i in 1:length(simyears)) {

  startrow <- ((i*1-1)*necoxspp+1)
  startrow2 <- startrow + 4

  if (simyears[i] %in% yearspnet) {
  dyntab$spp[startrow2:(startrow2+necoxspp-1)] <- pnetoutbig$sp[startrow:(startrow+necoxspp-1)]
  dyntab$year[startrow2:(startrow2+necoxspp-1)] <- simyears[i]
  dyntab$eco[startrow2:(startrow2+necoxspp-1)] <- pnetoutbig$Ecoregion[startrow:(startrow+necoxspp-1)]
  dyntab$maxanpp[startrow2:(startrow2+necoxspp-1)] <- round(pnetoutbig$NPPwood[startrow:(startrow+necoxspp-1)] + pnetoutbig$NPPleaf[startrow:(startrow+necoxspp-1)],digits=0)
  #dyntab$maxanpp[startrow2:(startrow2+necoxspp-1)] <- round(pnetoutbig$NPPwood[startrow:(startrow+necoxspp-1)],digits=0)
  
  for (j in 1:length(ecoregions)) {
    if (j == 1) {pests <- pesttemp3[which(pesttemp3$year == yearspnet[i]),j+2]}
    if (j > 1)  {
      pests <- c(pests,pesttemp3[which(pesttemp3$year == yearspnet[i]),j+2])
    }
  }
  }

  if (simyears[i] > max(yearspnet)) {
    if (gcm == 'TerraClimate') {
      randi <- sample((length(yearspnet)-29):length(yearspnet),1) # Sample from index of last 30 years in Prism historical projections
      
    } else randi <- sample((length(yearspnet)-19):length(yearspnet),1) # Sample from index of last 20 years in NEX gcm projections
    startrow3 <- ((randi*1-1)*necoxspp+1)
    dyntab$spp[startrow2:(startrow2+necoxspp-1)] <- pnetoutbig$sp[startrow3:(startrow3+necoxspp-1)]
    dyntab$year[startrow2:(startrow2+necoxspp-1)] <- simyears[i]
    dyntab$eco[startrow2:(startrow2+necoxspp-1)] <- pnetoutbig$Ecoregion[startrow3:(startrow3+necoxspp-1)]
    dyntab$maxanpp[startrow2:(startrow2+necoxspp-1)] <- round(pnetoutbig$NPPwood[startrow3:(startrow3+necoxspp-1)] + pnetoutbig$NPPleaf[startrow3:(startrow3+necoxspp-1)],digits=0)
    #dyntab$maxanpp[startrow2:(startrow2+necoxspp-1)] <- round(pnetoutbig$NPPwood[startrow3:(startrow3+necoxspp-1)],digits=0)


    for (j in 1:length(ecoregions)) {
      if (j == 1) {pests <- pesttemp3[which(pesttemp3$year == yearspnet[randi]),j+2]}
      if (j > 1)  {
        pests <- c(pests,pesttemp3[which(pesttemp3$year == yearspnet[randi]),j+2])
      }
    }

  }
  dyntab$probest[startrow2:(startrow2+necoxspp-1)] <- pests

  rm(pests,startrow)
}

for (k in 1:length(splist)) {
  sp <- splist[k]
  temp <- dyntab %>% dplyr::filter(spp==sp & year < max(simyears)) %>% dplyr::group_by(year) %>% summarize_at(vars(probest,maxanpp),mean,na.rm=T)
  par(mfrow=(c(2,1)),mar=c(2,2,2,2))
  plot(temp$year,temp$probest,type="l",lwd=2,col="blue",ylim=c(0,1))
  mtext(paste("prob. establishment",sp))
  plot(temp$year,temp$maxanpp,type="l",lwd=2,col="black",ylim=c(0,2500))
  mtext(paste("max anpp",sp))
}
# First time summarizing historical data (TopoWx x TerraClimate here, but have used PRSIM), calculate long-term yr mean for spin-up values in year 0
# dyntab100yrmn <- dyntab %>% filter(year %in% 1917:2017) %>% dplyr::group_by(eco,spp) %>% summarize_at(vars(probest,maxanpp),mean,na.rm=T) %>%
# ungroup() %>% dplyr::mutate(maxbio = maxanpp * 30, year = 0) %>% dplyr::select(year,eco,spp, probest, maxanpp, maxbio) %>%
#mutate(probest = round(probest, digits = 3), maxanpp = round(maxanpp, digits = 0), maxbio = round(maxbio, digits = 0), spp = toupper(spp))
# Write this out the first time you create it.
#if (exists("dyntab100yrmn")) {
#  write.table(dyntab100yrmn, paste(pnetdir,studyarea, "_biomass_succession_dynamic_inputs_TopoWX-TerraClimate_year0_table.txt",sep=""),
#              row.names=F, quote = F)
#}

dyntab$maxbio[5:(dim(dyntab)[1])] <- round(dyntab$maxanpp[5:(dim(dyntab)[1])] * 30)
#dyntab$maxbio[dyntab$spp %in% c("picerube","piceabie")] <- round(dyntab$maxanpp[dyntab$spp %in% c("PICERUBE","PICEMARI")] * 50)

# Adjust years for simulation output and naming conventions
if (gcm != "TerraClimate") {
  dyntab$year <- dyntab$year - simyr0 ## Set years so first simulation year is year 0.
}
dyntab$spp <- toupper(dyntab$spp)
dyntab$probest <- format(dyntab$probest,scientific = F)
dyntab$maxbio[which(dyntab$spp == "ACERPENS")] <- round(as.numeric(dyntab$maxanpp[which(dyntab$spp == "ACERPENS")]) * 10)

#dyntab$maxbio[which(dyntab$probest == "0.000")] <- 0 # For Landis, maxbio should be zero when Prob-est is zero.

## Create table to calculate and hold MinRelBiomass values for Landis input.
## Target biomass thresholds are c(10,35,50,65,80) Mg/ha for shade classes 1,2,3,4,5.
## These biomass thresholds may need to be adjust for different regions.
minrelbios <- c(10,35,50,65,80)
minrelbioshigh <- minrelbios/2
minrelbiotab <- matrix(data=NA,nrow=5,ncol=length(ecoregions)+1)
minrelbiotab[,1] <- 1:5

for (j in 1:length(ecoregions)) {
  ecoj <- ecoregions[j]
  minrelbiotab[,j+1] <- round((minrelbios*100)/max(as.numeric(dyntab$maxbio[dyntab$eco==ecoj]),na.rm=T),digits=2)
}

minrelbiotab <- data.frame(minrelbiotab)
names(minrelbiotab) <- c("shade",ecoregions)

## Read in dyntab for current climate (Prism, TopoWX, TerraClimate or other), calculated earlier, so you can replace year zero parameters with the same values in current runs.
## Spin-up conditions need to be the same.
#dyntab2 <-read.table(paste(pnetdir,studyarea, "_biomass_succession_dynamic_inputs_TopoWX-TerraClimate_year0_table.txt",sep=""),header=T)
dyntab2 <-read.table(paste(pnetdir,studyarea, "_biomass_succession_dynamic_inputs_TopoWX-TerraClimate_year0_table.txt",sep=""),header=T)

if (is.factor(dyntab2[,3])) {
  dyntab2[,3] <- levels(dyntab2[,3])[dyntab2[,3]]
}
startrow <- ((1*1-1)*necoxspp+1)
startrow2 <- startrow + 4
dyntab[startrow2:(startrow2+necoxspp-1),1:6] <- dyntab2[startrow:(startrow+necoxspp-1),1:6]


## Create header for dyntab output file
dyntab[1,1:2] <- c("LandisData","Dynamic Input Data")
dyntab[3,] <- c(">>Year","Ecoregion","Species","ProbEst","MaxANPP","MaxB")
dyntab[4,] <- c(">>____","_________","_______","_______","_______","____")
dyntab[is.na(dyntab)] <- ""
dyntab[1:2,4] <- ""

outfilename <- paste("data\\",studyarea,"_biomass_succession_dynamic_inputs_NEX_",gcm,"_",rcp0,"_",repnum,".txt",sep="")
outfilename2 <- paste("data\\",studyarea,"_MinRelativeBiomassTable_",gcm,"_",rcp0,"_",repnum,".txt",sep="")
write.table(dyntab,outfilename,
            row.names=F,quote=F,sep="\t",col.names=F)
write.table(minrelbiotab, outfilename2,
            row.names=F,quote=F,sep="\t",col.names=F)

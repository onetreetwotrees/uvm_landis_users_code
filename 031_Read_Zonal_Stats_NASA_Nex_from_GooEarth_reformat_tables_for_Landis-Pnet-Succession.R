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
gcm <- "HADGEM2-ES"           # 'CCSM4', 'CESM1-BGC','HADGEM2-ES','MPI-ESM-LR','HadGEM2-ES'
rcp0 <- "rcp45"          # 'historical','rcp26','rcp45','rcp60','rcp85'
studyarea <- "VTMA"
ecos <- c()

## UPDATE DIRECTORY PATHS HERE TO SOMETHING THAT MAKES SENSE ON YOUR SYSTEM
## Read in results of GoogleEarth script that calculates zonal stats of climate data by soils class.
rootdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\uvm_landis_users_code\\data\\climate_Hanusia\\"

# Read in template PnET-Succession climate file from pre-processed Terraclimate & TopoWx data
pnetclim_all <- read.csv("data\\TerraClimate_historical_eco_all.csv")

#if (gcm == "TerraClimate") {

# Read climate summaries of average conditions for soil ecoregions from GEE
  ztmin0 <- read.csv(paste(rootdir,studyarea,"_Nex_mean_extract_",rcp0,"_",gcm,"_tasmin.csv",sep=""),header=T)
  ztmax0 <- read.csv(paste(rootdir,studyarea,"_Nex_mean_extract_",rcp0,"_",gcm,"_tasmax.csv",sep=""),header=T)
  zppt0 <- read.csv(paste(rootdir,studyarea,"_Nex_mean_extract_",rcp0,"_",gcm,"_pr.csv",sep=""),header=T) 
#  zsrad0 <- read.csv(paste(rootdir,studyarea,"_mean_extract_",rcp0,"_",gcm,"_srad.csv",sep=""),header=T)
  
  #}

# Read in CO2 forcing data for CMIP5 and tcp0. CO2 conc is column 3 in ppm.
  rcp_names <- data.frame(co2 = c("RCP3PD","RCP45","RCP6","RCP85","PRE2005"),
                          rcps = c("rcp25","rcp45","rcp65","rcp85","historical"))
  co2 <- read.table(paste("C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\data_download\\CO2\\",
           rcp_names$co2[which(rcp_names$rcps == rcp0)], "_MIDYR_CONC.DAT",sep=""),
           skip=38, header=T)

# Extract date related variables from system.index
sysnchar <- nchar(ztmin0$system.index)
years <- as.numeric(substr(ztmin0$system.index,sysnchar-5,sysnchar-2))
months <- as.numeric(substr(ztmin0$system.index,sysnchar-1,sysnchar))
dates = as.Date(paste(years,months,rep("01",length(years)),sep="-"), "%Y-%m-%d")

# Shift dates one month forward. Will get days-per-month from the months prior to this list.
datesplusone <- c(dates,as.Date(paste(max(years)+1,"01","01",sep="-"), "%Y-%m-%d"))
datesplusone <- datesplusone[-1]
daysmo <- format(datesplusone-1,format="%d")
 
dim(ztmin0)
neco <- length(unlist(gregexpr("ecoregion",ztmin0$groups[1])))
nmonths <- length(months)
nyears <- nmonths/12

## Create new empty data.frames to hold parsed climate summaries
ztmin <- data.frame(matrix(data=NA,nrow=nmonths*neco,ncol=8))
names(ztmin) <- c("system.index","gcm","rcp","year","month","ecoregion","mean","std")
monthsNew <- rep(months,times=rep(neco,nmonths))
yearNew <- rep(years,times=rep(neco,nmonths))
ztmin[,which(names(ztmin) %in% c("year","month"))] <- cbind(yearNew,monthsNew)
ztmax <- ztmin
zppt <- ztmin


for (i in 1:nmonths) {
  ## Extract means and stdDev for tmin for rowI
  tminRowI <- as.character(unlist(ztmin0[i,2][[1]]))
  tmaxRowI <- as.character(unlist(ztmax0[i,2][[1]]))
  pptRowI <- as.character(unlist(zppt0[i,2][[1]]))

  ## Calculate new row range to assign parsed values
  startRow <- (i-1)*neco + 1
  endRow <- (startRow + neco-1)
  c(startRow,endRow)
  ## Parse string for decimal, numeric values, e.g. ecoregions and means or stdDevs
  tminTmp <- as.numeric(unlist(regmatches(tminRowI,gregexpr("(?>-)*[[:digit:]]+\\.*[[:digit:]]*",
                                                            tminRowI, perl=TRUE))))

  ## Extract values from every other positions to get the mean values and convert to degrees C from K
  if (length(ecos) == 0) {
    ecos <- round(tminTmp[seq(from=1,to=length(tminTmp),by=2)])
  }
  tminTmp <- tminTmp[seq(from=2,to=length(tminTmp),by=2)] -273.15 #* 0.1

  ## Repeat for tmax
  tmaxTmp <- as.numeric(unlist(regmatches(tmaxRowI,gregexpr("(?>-)*[[:digit:]]+\\.*[[:digit:]]*",
                                                            tmaxRowI, perl=TRUE))))
  ## Extract values from every other positions to get the mean values and convert to degrees C from K
  if (length(ecos) == 0) {
    ecos <- round(tmaxTmp[seq(from=1,to=length(tmaxTmp),by=2)])
  }
  tmaxTmp <- tmaxTmp[seq(from=2,to=length(tmaxTmp),by=2)] -273.15#* 0.1
  ## Repeat for ppt - This one needs to capture scientific notation E for precip in small units...
  pptTmp <- as.numeric(unlist(regmatches(pptRowI,gregexpr("(?>-)*[[:digit:]]+\\.*[[:digit:]]*E*",
                                                            pptRowI, perl=TRUE))))
  
  # Look for scientific notation and extract exponent if it exists.
  if (any(pptTmp < 0)) {
    pptExp <- pptTmp[seq(from=3,to=length(pptTmp),by=3)]
    pptTmp <- pptTmp[seq(from=2,to=length(pptTmp),by=3)]
    pptTmp <- pptTmp * 10^pptExp
  } else   pptTmp <- pptTmp[seq(from=2,to=length(pptTmp),by=2)]
  
  ## Extract values from every other positions to get the mean values - don't convert here
  if (length(ecos) == 0) {
    ecos <- round(pptTmp[seq(from=1,to=length(pptTmp),by=3)])
  }

  
  ## Now populate locations in parsed table
  ## ztMin
  if (is.factor(ztmin0$system.index)) {
  ztmin$system.index[startRow:endRow] <- levels(ztmin0$system.index)[ztmin0$system.index[i]]
  } else ztmin$system.index[startRow:endRow] <- ztmin0$system.index[i]
  ztmin$gcm[startRow:endRow] <- gcm
  ztmin$rcp[startRow:endRow] <- rcp0
  ztmin$mean[startRow:endRow] <- tminTmp
  ztmin$ecoregion[startRow:endRow] <- ecos
  ## ztMax
  if (is.factor(ztmax0$system.index)) {
  ztmax$system.index[startRow:endRow] <- levels(ztmax0$system.index)[ztmax0$system.index[i]]
  } else ztmax$system.index[startRow:endRow] <- ztmax0$system.index[i]
  ztmax$gcm[startRow:endRow] <- gcm
  ztmax$rcp[startRow:endRow] <- rcp0
  ztmax$mean[startRow:endRow] <- tmaxTmp
  ztmax$ecoregion[startRow:endRow] <- ecos
  ## zPpt
  if (is.factor(zppt$system.index)) {
  zppt$system.index[startRow:endRow] <- levels(zppt0$system.index)[zppt0$system.index[i]]
  } else zppt$system.index[startRow:endRow] <- zppt0$system.index[i]
  zppt$gcm[startRow:endRow] <- gcm
  zppt$rcp[startRow:endRow] <- rcp0
  zppt$mean[startRow:endRow] <- pptTmp
  zppt$ecoregion[startRow:endRow] <- ecos
  
  rm(tminRowI,tmaxRowI,pptRowI,startRow,endRow,
     tminTmp,tmaxTmp,pptTmp)
}

years2 <- sort(unique(zppt$year))

daysmo0 <- daysmo
daysmo <- data.frame(matrix(data=NA,nrow <- length(years2) * 12,ncol=3))
names(daysmo) <- c("year","x","mo")
for (j in 1:length(years2)) {
  datesj <- paste(years2[j],"-",1:12,"-",1,sep="")
  datesj2 = as.Date(datesj, "%Y-%m-%d")
  modaysj <- monthDays(datesj2)
  startrow <-((j-1)*12 + 1)
  endrow <- startrow + 11
  daysmo$year[startrow:endrow] <- years2[j]
  daysmo$mo[startrow:endrow] <- 1:12
  daysmo$x[startrow:endrow] <- modaysj
}

if (exists("pptExp")) {
  ## Convert to desired units. Precip from kg m2 s-1 to mm/day, Multiply by days per month to get to summed precipitation in mm/mo
  zppt <- zppt %>% dplyr::inner_join(daysmo, by=c("year"="year", c("month"="mo"))) %>% dplyr::rename(.,daysmo=x)
  zppt$mean <- zppt$mean * 86400 * zppt$daysmo
  zppt <- zppt %>% dplyr::select(-daysmo)
  ## ztmax and ztmin already converted to degrees C from degrees K.
}


## Calculate ztmean from ztmax and ztminn
ztmean <- ztmax
ztmean$mean <- NA
ztmean$mean <- (ztmax$mean + ztmin$mean)/2

## ztmax and ztmin already converted to degrees C from scaled degrees C.
## Calculate mean climatology, all yrs, Tmax
mzppt <- aggregate(cbind(mean) ~ month +
                     ecoregion,mean,data=zppt)

mztmean <- aggregate(cbind(mean) ~ month +
                       ecoregion,mean,data=ztmean)
mztmax <- aggregate(cbind(mean) ~ month +
                      ecoregion,mean,data=ztmax)


## Calculate total annual precip and tmean all years
zpptyr <- aggregate(cbind(mean) ~ year +
                      ecoregion,sum,data=zppt)
ztmeanyr <- aggregate(cbind(mean) ~ year +
                        ecoregion,mean,data=ztmean)
ztmaxyr <- aggregate(cbind(mean) ~ year +
                       ecoregion,mean,data=ztmax)
ztminyr <- aggregate(cbind(mean) ~ year +
                       ecoregion,mean,data=ztmin)

ecoregions <- unique(zppt$ecoregion)

## Visualize climate summaries to make sure data look okay ################################################
## Plot mean climatologies by class
## Create a color palette for soil ecoregions
marker4 <- c("#9C179EFF","#B52F8CFF","#F89441FF","#FBD424FF","#5D01A6FF","#3B049AFF","#7E03A8FF",
             "#CC4678FF","#2BB07FFF","#433E85FF","#482173FF","#440154FF",
             "#DE5F65FF","#ED7953FF","#85D54AFF","#2D708EFF","#C2DF23FF","#25858EFF",
             "#51C56AFF","#1E9B8AFF","#FDB32FFF","#F0F921FF","#FDE725FF","#38598CFF")




## plot total annual precip over time by class
plot(c(min(zpptyr$year),max(zpptyr$year)),c(min(zpptyr$mean),max(zpptyr$mean)),xlab="year",ylab="Annual Precip. (mm)",type="n")
mtext(paste(gcm, rcp0))
for (i in 1:length(ecoregions)) {
  lines(zpptyr$year[zpptyr$ecoregion == ecoregions[i]],
        zpptyr$mean[zpptyr$ecoregion == ecoregions[i]],
        col=marker4[i],lwd=2)
  text(2000,zpptyr$mean[zpptyr$ecoregion == ecoregions[i]][94],
       labels=paste(ecoregions[i]),col=marker4[i])

}

## plot tmax over time by class
plot(c(min(ztmaxyr$year),max(ztmaxyr$year)),c(min(ztmaxyr$mean),max(ztmaxyr$mean)),xlab="year",ylab="Max Annual T (C)",type="n")
mtext(paste(gcm, rcp0))
for (i in 1:length(ecoregions)) {
  ecoi <- ecos[i]
  rgbi <- marker4[((i-1)*3 + 1):((i-1)*3 + 3)]
  lines(ztmaxyr$year[ztmaxyr$ecoregion == ecoregions[i]],
        ztmaxyr$mean[ztmaxyr$ecoregion == ecoregions[i]],
        col=marker4[i],lwd=2)
  text(2000,ztmaxyr$mean[ztmaxyr$ecos == ecoregions[i]][1],
       labels=paste(ecoregions[i]),col=marker4[i])

}

## plot tmin over time by class
plot(c(min(ztminyr$year),max(ztminyr$year)),c(min(ztminyr$mean),max(ztminyr$mean)),
     xlab="year",ylab="Min Annual T (C)",type="n")
mtext(paste(gcm, rcp0))
for (i in 1:length(ecoregions)) {
  ecoi <- ecos[i]
  rgbi <- marker4[((i-1)*3 + 1):((i-1)*3 + 3)]
  lines(ztminyr$year[ztminyr$ecoregion == ecoregions[i]],
        ztminyr$mean[ztminyr$ecoregion == ecoregions[i]],
        col=marker4[i],lwd=2)
  text(2000,ztminyr$mean[ztminyr$ecos == ecoregions[i]][1],
       labels=paste(ecoregions[i]),col=marker4[i])
  
}
## END - Visualize climate summaries to make sure data look okay ################################################


## Set up Pest calulator clim file
years <- min(years):max(years)
nyears <- length(years)

# Read in PAR estimates from Hubbard Brook in units suitable for PnET
parhb2 <- read.table("C:\\Users\\janer\\Dropbox\\SpruceFir\\PNET_C1\\Input\\HB_Climate_1794.clim",header=T)
parhb <- parhb2[which(parhb2$Year %in% years),]

maxrow <- max(as.numeric(row.names(ztmax[which(ztmax$year==max(years)),])))
maxrow <- which(row.names(ztmax)==maxrow)
minrow <- min(as.numeric(row.names(ztmax[which(ztmax$year==min(years)),])))
minrow <- which(row.names(ztmax)==minrow)

years1 <- min(years):max(years)
ztmax <- ztmax[minrow:maxrow,]
ztmin <- ztmin[minrow:maxrow,]
ztmean <- ztmean[minrow:maxrow,]
zppt <- zppt[minrow:maxrow,]

ztmaxsort <- ztmax %>% dplyr::arrange(ecoregion,year,month)
ztminsort <- ztmin %>% dplyr::arrange(ecoregion,year,month)
ztmeansort <- ztmean %>% dplyr::arrange(ecoregion,year,month)
zpptsort <- zppt %>% dplyr::arrange(ecoregion,year,month)

### Now set up PNET input files...
## Set up pnet clim file
neco <- length(ecos)

### Now set up PNET input files...

yearspnet <- years
nyearspnet <- length(yearspnet)

# Modify names in parhb to facilitate inner_join - NOT USING
names(parhb2)[c(1,2,5)] <- c("year","doy","par")
modoy <- data.frame(mo=1:12,doy=c(15,46,76,107,137,168,198,229,259,290,321,351))

parhb <- parhb2 %>% inner_join(modoy) %>% dplyr::select(year, mo, par) %>% 
  rename(month = mo, PAR=par)

# Calculate monthly means 1991-2020 from Terraclimate & TopoWx observed data
pnetclim_mean <- pnetclim_all %>% dplyr::filter(Year %in% 1991:2020) %>% 
  dplyr::select(-Year) %>% group_by(ecoregion, Month) %>% summarize_all(funs(mean))

## Join all the tables together to compile the columns needed for PnET Succession
pnetclim_rcp <- ztmaxsort %>% dplyr::select(ecoregion, year, month, mean) %>% 
  rename(Tmax = mean) %>% inner_join(ztminsort %>% dplyr::select(ecoregion, year, month, mean)) %>% 
  rename(Tmin = mean) %>% inner_join(zpptsort %>% dplyr::select(ecoregion, year, month, mean)) %>% 
  rename(Prec = mean, Year = year, Month = month) %>% inner_join(pnetclim_mean %>% 
  dplyr::select(ecoregion, Month, PAR)) %>% inner_join(co2 %>% rename(Year = YEARS) %>% 
                                                         dplyr::select(Year, CO2))

## Determine last 20 years of climate data
years_last20 <- years[(length(years)-19):length(years)]
years_needed <- (max(years)+1):2120

set.seed(347)
## Draw random sample of last 20 years with replacement, up to year 2120
rand_years <- sample(years_last20, length(years_needed), replace=T)

## Loop through random years and append annual climate data to CMIP-5 projections
for (i in 1:length(rand_years)) {
  climyear_i <- pnetclim_rcp %>% dplyr::filter(Year == rand_years[i])
  # Update year to new year
  climyear_i$Year <- years_needed[i]
  # Append to bottom of pnetclim_rcp table
  pnetclim_rcp <- pnetclim_rcp %>% bind_rows(climyear_i)
}

## Round climate variables for pnetclim file
pnetclim_rcp$Tmax <- round(pnetclim_rcp$Tmax,digits=4)
pnetclim_rcp$Tmin <- round(pnetclim_rcp$Tmin,digits=4)
pnetclim_rcp$Prec <- round(pnetclim_rcp$Prec,digits=4)
pnetclim_rcp$PAR <- round(pnetclim_rcp$PAR,digits=4)
pnetclim_rcp$CO2 <- round(pnetclim_rcp$CO2,digits=4)

## Now append climate change projection years to observed data from Terraclimate and TopoWx
## All tables should be the same from earliest years to start year of 2020.
pnetclim_all <- pnetclim_all %>% bind_rows(pnetclim_rcp %>% 
                                              mutate(Year = as.character(Year)))

today <- Sys.Date()

## Loop through ecoregions, write out text file with climate data for PnET-Succession Landis-II extension

for (i in 1:length(ecoregions)) {
  # Subset climate table to ecoregion i
  pnetclim_tmp <- pnetclim_all %>% ungroup() %>% dplyr::filter(ecoregion == ecoregions[i]) %>% 
    dplyr::select(-ecoregion)
  # Write ecoregion specific climate to file
  file_name_i <- paste("data\\climate_Hanusia\\Pnet-succession_input_files\\",
                       gcm,"_",rcp0,"_","eco","_",ecoregions[i],".txt", sep="")
  write.table(pnetclim_tmp, file_name_i, row.names=F, quote=F)
  rm(pnetclim_tmp, file_name_i)
}

## Also write out a copy of the pnetclim_all table.
write.csv(pnetclim_all, paste("data\\",
                              gcm,"_",rcp0,"_","eco","_","all",".csv", sep=""),
          row.names=F)


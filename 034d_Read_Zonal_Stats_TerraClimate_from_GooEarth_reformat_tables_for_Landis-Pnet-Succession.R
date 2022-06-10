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
library(viridis)
library(bigleaf)

gcm <- "TerraClimate"   # 'CCSM4', 'CESM1-BGC','HADGEM2-ES','MPI-ESM-LR','HadGEM2-ES'
rcp0 <- "historical"          # 'historical','rcp25','rcp45','rcp65','rcp85'
studyarea <- "VTMA" #"WMNF"
ecos <- c()

## Terraclimate downloaded in 2 sets of files, 1970-2020, 1950-1969. Loop through
## Both sets to read and reformat climate tables, then stitch together and write out files for
## PnET-Succession

for (k in 1:2) {
#setwd("G:\\SpruceFirProject\\GIS\\VT\\soils\\vt_soils_calc\\mosaic_soils_envi")
## Read in results of GoogleEarth script that calculates zonal stats of climate data by soils class.
  if (k == 1) {
  rootdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\uvm_landis_users_code\\data\\"
  ztmin0 <- read.csv(paste(rootdir,studyarea,"_mean_extract_",rcp0,"_",gcm,"_tmmn.csv",sep=""),header=T)
  ztmax0 <- read.csv(paste(rootdir,studyarea,"_mean_extract_",rcp0,"_",gcm,"_tmmx.csv",sep=""),header=T)
  zppt0 <- read.csv(paste(rootdir,studyarea,"_mean_extract_",rcp0,"_",gcm,"_pr.csv",sep=""),header=T)
  zsrad0 <- read.csv(paste(rootdir,studyarea,"_mean_extract_",rcp0,"_",gcm,"_srad.csv",sep=""),header=T)
  } else {
    rootdir <- "data\\climate_Hanusia\\"
    ztmin0 <- read.csv(paste(rootdir,studyarea,"_mean_extract_",rcp0,"_",gcm,"_tmmn","_1950-1969",".csv",sep=""),header=T)
    ztmax0 <- read.csv(paste(rootdir,studyarea,"_mean_extract_",rcp0,"_",gcm,"_tmmx","_1950-1969",".csv",sep=""),header=T)
    zppt0 <- read.csv(paste(rootdir,studyarea,"_mean_extract_",rcp0,"_",gcm,"_pr","_1950-1969",".csv",sep=""),header=T)
    zsrad0 <- read.csv(paste(rootdir,studyarea,"_mean_extract_",rcp0,"_",gcm,"_srad","_1950-1969",".csv",sep=""),header=T)
  }
  #rootdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\data_download\\GEE\\climate\\"

# Extract date related variables from system.index
years <- as.numeric(substr(ztmin0$system.index,1,4))
months <- as.numeric(substr(ztmin0$system.index,5,6))
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
zsrad <- ztmin

for (i in 1:nmonths) {
  ## Extract means and stdDev for tmin for rowI
  tminRowI <- as.character(unlist(ztmin0[i,2][[1]]))
  tmaxRowI <- as.character(unlist(ztmax0[i,2][[1]]))
  pptRowI <- as.character(unlist(zppt0[i,2][[1]]))
  sradRowI <- as.character(unlist(zsrad0[i,2][[1]]))
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
  tminTmp <- tminTmp[seq(from=2,to=length(tminTmp),by=2)] * 0.1
  ## Repeat for tmax
  tmaxTmp <- as.numeric(unlist(regmatches(tmaxRowI,gregexpr("(?>-)*[[:digit:]]+\\.*[[:digit:]]*",
                                                            tmaxRowI, perl=TRUE))))
  ## Extract values from every other positions to get the mean values and convert to degrees C from K
  if (length(ecos) == 0) {
    ecos <- round(tmaxTmp[seq(from=1,to=length(tmaxTmp),by=2)])
  }
  tmaxTmp <- tmaxTmp[seq(from=2,to=length(tmaxTmp),by=2)] * 0.1
  ## Repeat for ppt - This one needs to capture scientific notation E for precip in small units...
  pptTmp <- as.numeric(unlist(regmatches(pptRowI,gregexpr("(?>-)*[[:digit:]]+\\.*[[:digit:]]*E*",
                                                            pptRowI, perl=TRUE))))
  ## Extract values from every other positions to get the mean values - don't convert here
  if (length(ecos) == 0) {
    ecos <- round(pptTmp[seq(from=1,to=length(pptTmp),by=3)])
  }
  pptTmp <- pptTmp[seq(from=2,to=length(pptTmp),by=2)]
  

  sradTmp <- as.numeric(unlist(regmatches(sradRowI,gregexpr("(?>-)*[[:digit:]]+\\.*[[:digit:]]*",
                                                            sradRowI, perl=TRUE))))
  ## Extract values from every other positions to get the mean values and convert to W/m^2
  sradTmp <- sradTmp[seq(from=2,to=length(sradTmp),by=2)] * 0.1
  
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
  
  ## zSrad
  if (is.factor(zsrad$system.index)) {
    zsrad$system.index[startRow:endRow] <- levels(zsrad0$system.index)[zsrad0$system.index[i]]
  } else zsrad$system.index[startRow:endRow] <- zsrad0$system.index[i]
  zsrad$gcm[startRow:endRow] <- gcm
  zsrad$rcp[startRow:endRow] <- rcp0
  zsrad$mean[startRow:endRow] <- sradTmp
  zsrad$ecoregion[startRow:endRow] <- ecos

  rm(tminRowI,tmaxRowI,pptRowI,startRow,endRow,
     tminTmp,tmaxTmp,pptTmp,sradTmp)
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
print(ecoregions)
## Visualize climate summaries to make sure data look okay ################################################
## Plot mean climatologies by class
## Create a color palette for soil ecoregions
marker4 <- c("#9C179EFF","#B52F8CFF","#F89441FF","#FBD424FF","#5D01A6FF","#3B049AFF","#7E03A8FF",
             "#CC4678FF","#2BB07FFF","#433E85FF","#482173FF","#440154FF",
             "#DE5F65FF","#ED7953FF","#85D54AFF","#2D708EFF","#C2DF23FF","#25858EFF",
             "#51C56AFF","#1E9B8AFF","#FDB32FFF","#F0F921FF","#FDE725FF","#38598CFF")

## plot total annual precip over time by class
plot(c(min(zpptyr$year),max(zpptyr$year)),c(min(zpptyr$mean),max(zpptyr$mean)),xlab="year",ylab="Annual Precip. (mm)",type="n")
mtext(gcm)
for (i in 1:length(ecoregions)) {
  lines(zpptyr$year[zpptyr$ecoregion == ecoregions[i]],
        zpptyr$mean[zpptyr$ecoregion == ecoregions[i]],
        col=marker4[i],lwd=2)
  text(2000,zpptyr$mean[zpptyr$ecoregion == ecoregions[i]][94],
       labels=paste(ecoregions[i]),col=marker4[i])
  
}

## plot tmax over time by class
plot(c(min(ztmaxyr$year),max(ztmaxyr$year)),c(min(ztmaxyr$mean),max(ztmaxyr$mean)),xlab="year",ylab="Max Annual T (C)",type="n")
mtext(gcm)
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
plot(c(min(ztminyr$year),max(ztminyr$year)),c(min(ztminyr$mean),max(ztminyr$mean)),xlab="year",ylab="Max Annual T (C)",type="n")
mtext(gcm)
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

## Set up Pest calculator clim file
yearspest <- min(years):max(years)
nyearspest <- length(yearspest)
pestclim <- data.frame(matrix(nrow=12*length(ecoregions)*nyearspest,ncol=8))
names(pestclim) <- c("Ecoregion","month","avgminT","avgmaxT","stddevT","avgppt","stdevppt","year")

# Read in PAR estimates from Hubbard Brook in units suitable for PnET
parhb2 <- read.table("C:\\Users\\janer\\Dropbox\\SpruceFir\\PNET_C1\\Input\\HB_Climate_1794.clim",header=T)
parhb <- parhb2[which(parhb2$Year %in% yearspest),]

maxrow <- max(as.numeric(row.names(ztmax[which(ztmax$year==max(yearspest)),])))
maxrow <- which(row.names(ztmax)==maxrow)
minrow <- min(as.numeric(row.names(ztmax[which(ztmax$year==min(yearspest)),])))
minrow <- which(row.names(ztmax)==minrow)

years1 <- min(yearspest):max(yearspest)
ztmax <- ztmax[minrow:maxrow,]
ztmin <- ztmin[minrow:maxrow,]
ztmean <- ztmean[minrow:maxrow,]
zppt <- zppt[minrow:maxrow,]
zsrad <- zsrad[minrow:maxrow,]

ztmaxsort <- ztmax %>% dplyr::arrange(ecoregion,year,month)
ztminsort <- ztmin %>% dplyr::arrange(ecoregion,year,month)
ztmeansort <- ztmean %>% dplyr::arrange(ecoregion,year,month)
zpptsort <- zppt %>% dplyr::arrange(ecoregion,year,month)
zsradsort <- zsrad %>% dplyr::arrange(ecoregion,year,month)

## Read-in pre-processed Topo-Wx temperature data.
topoclim <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\data_download\\TopoWX\\mean_topowx_by_ecoregion_VTMA.csv",header=T)

## Read in downloaded atmospheric CO-2 record from Mauna Loa 
co2 <- read.csv("data\\climate_Hanusia\\co2_mm_mlo_cleaned.csv")

# Identify any overlapping years, keep observed data from TopoWx or PRISM
yrs_overlap <- unique(years[(years %in% topoclim$year)])

# Rearrange pestclimtmp to combine with pestclim. Basically want tmin, tmax, tmaxsd from Topowx. Precip from TerraClimate.
ecos <- sort(unique(ztminsort$ecoregion))

today <- Sys.Date()

### Now set up PNET input files...
## Set up pnet clim file
yearspnet <- years
nyearspnet <- length(yearspnet)

modoy <- data.frame(mo=1:12,doy=c(15,46,76,107,137,168,198,229,259,290,321,351))

maxrow <- as.numeric(max(row.names(ztmax[which(ztmax$year==max(yearspnet)),])))
maxrow <- which(row.names(ztmax)==maxrow)

years1 <- min(yearspnet):max(yearspnet)
ztmax <- ztmax[1:maxrow,]
ztmin <- ztmin[1:maxrow,]
zppt <- zppt[1:maxrow,]
zsrad <- zsrad[1:maxrow,]

ztmaxsort <- ztmax %>% arrange(ecoregion,year,month)
ztminsort <- ztmin %>% arrange(ecoregion,year,month)
zpptsort <- zppt %>% arrange(ecoregion,year,month)
zsradsort <- zsrad %>% arrange(ecoregion,year,month)
zsradsortsub <- zsradsort %>% dplyr::select(Ecoregion=ecoregion,year=year,mo=month,par=mean) %>% 
  mutate(ppfd = Rg.to.PPFD(par, J_to_mol = 4.6, frac_PAR = 0.5), par=2.1*par)

## New 2021-10-24, try using library bigleaf to convert Global radiation to photosynthetic photon flux density (W/m2 to umol/m2/s)
## User guide says total conversion factor when combined is 2.3. Compare to rule of thumb of 2.1 that Scott shared above.

## Join CO2 record to solar radiation table
zsradsortsub <- zsradsortsub %>% inner_join(co2, by=c("year"="year",
                "mo"="month")) %>% 
  dplyr::select(Ecoregion,year,mo,par, ppfd, averageco2ppm)

## Join all the tables together to compile the columns needed for PnET Succession
pnetclim_all <- ztmaxsort %>% dplyr::select(ecoregion, year, month, mean) %>% 
  rename(Tmax = mean) %>% inner_join(ztminsort %>% dplyr::select(ecoregion, year, month, mean)) %>% 
  rename(Tmin = mean) %>% inner_join(zpptsort %>% dplyr::select(ecoregion, year, month, mean)) %>% 
  rename(Prec = mean) %>% inner_join(zsradsortsub %>% rename(ecoregion = Ecoregion,
  month = mo, PAR = ppfd, CO2 = averageco2ppm) %>% dplyr::select(ecoregion, year, month,
                                                                 PAR, CO2)) %>% 
  rename(Year = year, Month = month)

if (k == 1) {
  ## Rename pnetclim_all, then read in earlier month data and combine into one big file
  ## Before writing out climate files. Final result ~ 1950 - 2020
  pnetclim_all_1970_plus <- pnetclim_all
} else {
    pnetclim_all_1958_1969 <- pnetclim_all
    # Remove incomplete years of data 
    pnetclim_all_1958_1969 <- pnetclim_all_1958_1969 %>% dplyr::filter(Year > 1958)
  }
}

## Append pnetclim_all tables from different time periods to create one table with all years
pnetclim_all <- pnetclim_all_1958_1969 %>% bind_rows(pnetclim_all_1970_plus)

## If you want to substitute PAR values from Hubbard Brook data distributed with PnET-CN, uncomment below
#pnetclim_all <- parhb2 %>% inner_join(modoy, by=c("DOY" = "doy")) %>% 
#  mutate(Month = mo) %>% dplyr::select(Year, Month, PAR) %>% 
#  inner_join(pnetclim_all %>% dplyr::select(-PAR)) %>% 
#  dplyr::select(all_of(names(pnetclim_all))) %>% arrange(ecoregion, Year, Month)
#rcp0 <- "historical_HBEF_PAR"

## Calculate a mean climate for years before 1959
pnetclim_mean <- pnetclim_all %>% ungroup() %>% group_by(ecoregion, Month) %>% summarize_all(., funs(mean)) %>% 
  mutate(Year = '1800-1958') %>% dplyr::select(ecoregion, Year, Month, Tmax:CO2)

pnetclim_all <- pnetclim_mean %>% bind_rows(pnetclim_all %>% 
                mutate(Year = as.character(Year)))

## Round climate variables for pnetclim file
pnetclim_all$Tmax <- round(pnetclim_all$Tmax,digits=4)
pnetclim_all$Tmin <- round(pnetclim_all$Tmin,digits=4)
pnetclim_all$Prec <- round(pnetclim_all$Prec,digits=4)
pnetclim_all$PAR <- round(pnetclim_all$PAR,digits=4)
pnetclim_all$CO2 <- round(pnetclim_all$CO2,digits=4)

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

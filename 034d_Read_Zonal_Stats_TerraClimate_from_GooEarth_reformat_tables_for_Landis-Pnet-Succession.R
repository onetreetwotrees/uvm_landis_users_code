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

gcm <- "TerraClimate"   # 'CCSM4', 'CESM1-BGC','HADGEM2-ES','MPI-ESM-LR','HadGEM2-ES'
rcp0 <- "historical"          # 'historical','rcp25','rcp45','rcp65','rcp85'
studyarea <- "VTMA" #"WMNF"
ecos <- c()

#setwd("G:\\SpruceFirProject\\GIS\\VT\\soils\\vt_soils_calc\\mosaic_soils_envi")
## Read in results of GoogleEarth script that calculates zonal stats of climate data by soils class.
rootdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\uvm_landis_users_code\\data\\"
#rootdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\data_download\\GEE\\climate\\"
pestdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\models\\PestCalc\\"
pnetdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\models\\PNET\\"
if (gcm == "TerraClimate") {
  ztmin0 <- read.csv(paste(rootdir,studyarea,"_mean_extract_",rcp0,"_",gcm,"_tmmn.csv",sep=""),header=T)
  ztmax0 <- read.csv(paste(rootdir,studyarea,"_mean_extract_",rcp0,"_",gcm,"_tmmx.csv",sep=""),header=T)
  zppt0 <- read.csv(paste(rootdir,studyarea,"_mean_extract_",rcp0,"_",gcm,"_pr.csv",sep=""),header=T)
  zsrad0 <- read.csv(paste(rootdir,studyarea,"_mean_extract_",rcp0,"_",gcm,"_srad.csv",sep=""),header=T)

}

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


classes <- unique(zppt$ecoregion)
print(classes)
## Visualize climate summaries to make sure data look okay ################################################
## Plot mean climatologies by class
## Create a color palette for soil classes
marker4 <- c("#9C179EFF","#B52F8CFF","#F89441FF","#FBD424FF","#5D01A6FF","#3B049AFF","#7E03A8FF",
             "#CC4678FF","#2BB07FFF","#433E85FF","#482173FF","#440154FF",
             "#DE5F65FF","#ED7953FF","#85D54AFF","#2D708EFF","#C2DF23FF","#25858EFF",
             "#51C56AFF","#1E9B8AFF","#FDB32FFF","#F0F921FF","#FDE725FF","#38598CFF")

## plot total annual precip over time by class
plot(c(min(zpptyr$year),max(zpptyr$year)),c(min(zpptyr$mean),max(zpptyr$mean)),xlab="year",ylab="Annual Precip. (mm)",type="n")
mtext(gcm)
for (i in 1:length(classes)) {
  lines(zpptyr$year[zpptyr$ecoregion == classes[i]],
        zpptyr$mean[zpptyr$ecoregion == classes[i]],
        col=marker4[i],lwd=2)
  text(2000,zpptyr$mean[zpptyr$ecoregion == classes[i]][94],
       labels=paste(classes[i]),col=marker4[i])
  
}

## plot tmax over time by class
plot(c(min(ztmaxyr$year),max(ztmaxyr$year)),c(min(ztmaxyr$mean),max(ztmaxyr$mean)),xlab="year",ylab="Max Annual T (C)",type="n")
mtext(gcm)
for (i in 1:length(classes)) {
  ecoi <- ecos[i]
  rgbi <- marker4[((i-1)*3 + 1):((i-1)*3 + 3)]
  lines(ztmaxyr$year[ztmaxyr$ecoregion == classes[i]],
        ztmaxyr$mean[ztmaxyr$ecoregion == classes[i]],
        col=marker4[i],lwd=2)
  text(2000,ztmaxyr$mean[ztmaxyr$ecos == classes[i]][1],
       labels=paste(classes[i]),col=marker4[i])
  
}

## plot tmin over time by class
plot(c(min(ztminyr$year),max(ztminyr$year)),c(min(ztminyr$mean),max(ztminyr$mean)),xlab="year",ylab="Max Annual T (C)",type="n")
mtext(gcm)
for (i in 1:length(classes)) {
  ecoi <- ecos[i]
  rgbi <- marker4[((i-1)*3 + 1):((i-1)*3 + 3)]
  lines(ztminyr$year[ztminyr$ecoregion == classes[i]],
        ztminyr$mean[ztminyr$ecoregion == classes[i]],
        col=marker4[i],lwd=2)
  text(2000,ztminyr$mean[ztminyr$ecos == classes[i]][1],
       labels=paste(classes[i]),col=marker4[i])
  
}

## END - Visualize climate summaries to make sure data look okay ################################################

## Set up Pest calulator clim file
yearspest <- min(years):max(years)
nyearspest <- length(yearspest)
pestclim <- data.frame(matrix(nrow=12*length(classes)*nyearspest,ncol=8))
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

## Update, change the way stdev of monthly Temps is calculated, so that it is stdev through time (10-yrs), rather than within ecoregion space.
topoclim <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\data_download\\TopoWX\\mean_topowx_by_ecoregion_VTMA.csv",header=T)

#pestclim <- read.table(paste("C:\\Users\\janer\\Dropbox\\SpruceFir\\soils\\climate_means\\NEX_pestclim_",gcm,"_",rcp0,".txt",sep=""),header=T)
# Identify any overlapping years, keep observed data from TopoWx or PRISM
yrs_overlap <- unique(pestclim$year[(pestclim$year %in% topoclim$year)])
pestclim <- pestclim[which(pestclim$year %in% yrs_overlap),]

pestclimtmp0 <- pestclim %>% dplyr::inner_join(topoclim,by=c("Ecoregion"="eco","year"="year","month"="mo"))

# Rearrange pestclimtmp to combine with pestclim. Basically want tmin, tmax, tmaxsd from Topowx. Precip from TerraClimate.
pestclimtmp <- pestclimtmp0 %>% dplyr::select(Ecoregion,month,tmin,tmax,tmaxsd,avgppt,stdevppt,year)
names(pestclimtmp) <- names(pestclim)


ecos <- sort(unique(pestclim$Ecoregion))
yearspest <- sort(unique(pestclimtmp$year))

species <- unique(pesttemp3$spp)
lwds <- rep(1,length(classes))
lwds[c(9,10,11,25)] <- 2
ltys <- rep(1,length(classes))
ltys[c(17,28,13,26,14,15,16)] <- 2

today <- Sys.Date()

### Now set up PNET input files...
## Set up pnet clim file
yearspnet <- yearspest
nyearspnet <- length(yearspnet)
pnetclim <- data.frame(matrix(nrow=12*length(classes)*nyearspnet,ncol=7))
names(pnetclim) <- c("Ecoregion","year","doy","Tmax","Tmin","par","pptcm")

pnetclim$doy <- rep(c(15,46,76,107,137,168,198,229,259,290,321,351),nyearspnet)
modoy <- data.frame(mo=1:12,doy=c(15,46,76,107,137,168,198,229,259,290,321,351))

#maxrow <- length(which(!is.na(ztmax$MEAN)))
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

zsradsortsub <- zsradsortsub %>% inner_join(modoy) %>% dplyr::select(Ecoregion,year,doy,par, ppfd)

pnetclim$Ecoregion <- ztmaxsort$ecoregion
pnetclim$year <- ztmaxsort$year
pnetclim$Tmax <- round(ztmaxsort$mean,digits=4)
pnetclim$Tmin <- round(ztminsort$mean,digits=4)
pnetclim$pptcm <- round(zpptsort$mean, digits=4)
pnetclim$par <- round(zsradsortsub$ppfd,digits=4)
#pnetclim <- pnetclim %>% inner_join(zsradsortsub)

#for (i in 1:length(classes)) {
#  pnetclim$par[which(pnetclim$Ecoregion == classes[i])] <- round(parhb$PAR,digits=4)
#}

#setwd("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\models\\PNET")
setwd(pnetdir)

# write.csv(pnetclim, "VTMA_TopoWx_Terraclimate_historical_pnetclim.csv", row.names=F)
dirfiles <- dir()

if (length(grep("site_by_ecoregion",dirfiles)) < 1) {
tempsite <- read.table(paste(pnetdir, studyarea,"_template_site.txt", sep=""),header=T)
## Read in soil means by ecoregion to parse and fill in site input file
#soilmn <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\gis\\adk_soilindex2remap_means_w_wiltingpoint.csv",header=T)
soilmn <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\data_download\\GEE\\data_processed\\wmnf_topo_soils_class_means_24.csv", header=T)
#soilmn$waterholdcap <- soilmn$fieldcap - soilmn$WC15Bar # Water holding capacity = field capacity - wilting point (wc15bar)
soilmn$waterholdcap <- soilmn$fieldcap - soilmn$wiltpt # Water holding capacity = field capacity - wilting point (wc15bar)
tempsite0 <- data.frame(matrix(nrow=0,ncol=dim(tempsite)[2]))
names(tempsite0) <- names(tempsite)

# Now loop through ecoregions and populated site template file with all ecoregion information
for (i in 1:length(ecos)) {
  tempi <- tempsite
  ecoi <- ecos[i]
  tempi$Ecoregion <- ecoi
  tempi$Lat <- round(soilmn$latitude[which(soilmn$Index == ecoi)],digits=3)
  tempi$Lon <- abs(round(soilmn$longitude[which(soilmn$Index == ecoi)],digits=3))
  tempi$WaterHoldingCap <- abs(round(soilmn$waterholdcap[which(soilmn$Index == ecoi)],digits=3))
  tempsite0 <- rbind(tempsite0,tempi)
}

write.table(tempsite0,paste(pnetdir, studyarea,"_template_site_by_ecoregion.txt", sep=""),row.names=F,col.names=F,quote=F)
} else tempsite0 <- read.table(paste(pnetdir, studyarea,"_template_site_by_ecoregion.txt", sep=""),header=F)

batchfile <- matrix(nrow=0,ncol=1)

for (i in 1:length(yearspnet)) {
  pnettemp <- pnetclim[which(pnetclim$year == yearspnet[i]),]
  tempname <- paste(pnetdir,studyarea,"_",gcm,"_",rcp0,"_",
                    yearspnet[i],".clim",sep="")
  tempname2 <- paste(pnetdir,studyarea,"_",gcm,"_",rcp0,"_",
                     yearspnet[i],".site",sep="")
  rootname <- paste(studyarea,"_",gcm,"_",rcp0,"_",
                    yearspnet[i],sep="")
  write.table(pnettemp,tempname,sep="\t",col.names=F,row.names=F)
  write.table(tempsite0,tempname2,sep="\t",col.names=F,row.names=F,quote=F)
  batchcall <- paste("pnet2r","-f","-a1","-dvegdata/",rootname,sep=" ")
  batchfile <- rbind(batchfile,batchcall)
  rm(pnettemp,tempname,tempname2)
}

# Write out the batch file for this scenario/model combination
write.table(batchfile,paste("RunPnet_",studyarea,"_",gcm,"_",rcp0,"_many_years.bat",sep=""),row.names=F,col.names=F,quote=F)

# Make some plots of results - required you to have run the batch file created in above line...

#Developed by Dr. Jane Foster
#University of Vermont

#load the necessary libraries
library(raster)
library(sp)
library(rgdal)
library(sqldf)
library(RColorBrewer)
library(stringr)
library(Hmisc)

gcm <- "TopoWxPrism"   # 'CCSM4', 'CESM1-BGC','HADGEM2-ES','MPI-ESM-LR','HadGEM2-ES'
rcp0 <- "historical"          # 'historical','rcp25','rcp45','rcp65','rcp85'
studyarea <- "MA-NH"
ecos <- c()

#setwd("G:\\SpruceFirProject\\GIS\\VT\\soils\\vt_soils_calc\\mosaic_soils_envi")
## Read in results of GoogleEarth script that calculates zonal stats of climate data by soils class.
 rootdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\GEE\\data_download\\climate\\"
 pestdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\models\\PestCalc\\"
 pnetdir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\models\\PNET\\"
 
 
prism <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\data_download\\PRISM\\mean_prism_normals_by_ecoregion_MA-NH.csv",header=T)
topowx <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\data_download\\TopoWX\\mean_topowx_normals_by_ecoregion_MA-NH.csv",header=T)
solarrad <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\models\\PNET\\TerraClimate_historical_mean_totalradiation_by_ecoregion.csv",header=T)
# Convert total radiation to par in units for pnet and subset to 1981-2010, years for normals from prism and topowx
solarrad <- solarrad %>% mutate(par=2.1*mean) %>% filter(year %in% 1981:2010)
# Calculate a temporal sd for 30-years of mean temperature topowx as needed for pnet
tmean <- read.csv("C:\\Users\\janer\\Dropbox\\Climate\\ADK\\mean_topowx_by_ecoregion_adk_1948-2016.csv",header=T)
tmeansd <- tmean %>% dplyr::filter(year %in% 1981:2010) %>% group_by(eco,mo) %>% summarize_at(vars(tmean),funs(mean,sd)) %>%
  dplyr::select(eco,mo,tmeansd2=mean,tmeansd2sd=sd)
topoclim <- read.table(paste(pestdir,"NEX_pestclim_TerraClimate_historical_out.txt",sep=""),header=T)
topoclim <- topoclim %>% dplyr::filter(year %in% 1981:2010)
topoclimpptsd <- topoclim %>% dplyr::filter(year %in% 1981:2010) %>% dplyr::group_by(Ecoregion,month) %>% 
                dplyr::summarize_at(vars(stdevppt),funs(mean(.,na.rm=T)))

ecos <- sort(unique(prism$eco))

# Rename some vars in prism dataframe
prism <- prism %>% dplyr::select(eco:pptsd,tmaxp = tmax,tmaxsdp=tmaxsd,tminp=tmin,tminsdp=tminsd)
solarrad <- solarrad %>% dplyr::select(eco=ecoregion,mo=month,year,par) 
solarradnorm <- solarrad %>% group_by(eco,mo) %>% summarize_at(vars(par),funs(mean,sd)) %>% mutate(par=mean,parsd=sd) %>%
  dplyr::select(eco,mo,par,parsd)
climjoin <- topowx %>% inner_join(prism)
climjoin <- climjoin %>% inner_join(solarradnorm) %>% dplyr::arrange(eco,mo)
climjoin <- climjoin %>% inner_join(tmeansd)

classes <- ecos
## Plot mean climatologies by class
marker = c(brewer.pal(9, "Pastel1"),brewer.pal(12, "Set3"),brewer.pal(9,"Set1"))
marker2 <- c("blue","magenta","beige","grey","maroon","sea green","blue2","orchid",
             "lightblue","lightgreen","yellow","red","black","yellow3","olivedrab","cyan",
             "magenta3","purple2","mediumorchid","maroon4","cornflowerblue","cadetblue",
             "orange","sienna","coral","firebrick","lavender","firebrick2","aquamarine1","aquamarine2",
             "skyblue")
marker3 <- c( 0,   0,   0,   0,   0, 139,   0,   0, 255, 255,   0, 255, 243, 246, 213,
              216, 191, 216, 255,   0, 255, 176,  48,  96,  46, 139,  87, 160,  32, 240,
              255, 127,  80,   0,   0, 255, 218, 112, 214, 160,  82,  45,   0, 174, 255,
              216, 191, 216, 238,   0,   0,  77, 231,  99, 139,   0,   0,   0, 238,   0,
              244, 241,   0,   0, 139,   0,   0,   0, 238,   0,   0, 205, 255,   0,   0,
              0,   0,   0, 205, 205,   0, 139, 139,   0,   0, 238, 238,   0, 205, 205,
              0, 139, 139, 238,   0, 238, 205,   0, 205, 139,   0, 139, 238,  48, 167,
              205,  41, 144, 139,  28,  98, 145,  44, 238, 125,  38, 205,  85,  26, 139,
              255, 165,   0, 238, 154,   0, 205, 133,   0, 139,  90,   0, 238, 121,  66,
              205, 104,  57, 139,  71,  38, 238, 210, 238, 205, 181, 205,   0,   0,   0,
              255, 255, 255, 255,   0,   0, 255, 127,  80, 139,   0,   0, 255, 255,   0,
              0, 255, 255, 255,   0, 255, 176,  48,  96,  46, 139,  87, 160,  32, 240,
              255, 127,  80, 127, 255, 212, 218, 112, 214, 160,  82,  45, 127, 255,   0,
              205,   0,   0, 238,   0,   0, 205,   0,   0, 139,   0,   0,   0, 238,   0,
              0, 205,   0,   0, 139,   0,   0,   0, 238,   0,   0, 205,   0,   0, 139,
              238, 238,   0, 205, 205,   0, 139, 139,   0,   0, 238, 238,   0, 205, 205,
              0, 139, 139, 238,   0, 238, 205,   0, 205, 139,   0, 139, 238,  48, 167,
              205,  41, 144, 139,  28,  98, 145,  44, 238, 125,  38, 205,  85,  26, 139,
              255, 165,   0, 238, 154,   0, 205, 133,   0, 139,  90,   0, 238, 121,  66,
              205, 104,  57, 139,  71,  38, 238, 210, 238, 205, 181, 205,   0,   0,   0,
              255, 255, 255)
marker4 <- c()
for (i in 1:length(ecos)) {
  ecoi <- ecos[i]
  rgbi <- marker3[((ecoi-1)*3 + 1):((ecoi-1)*3 + 3)]
  #rgbi <- paste(rgbi,collapse=",")
  #rgbi <- paste("rgb(",rgbi,")",sep="")
  marker4 <- c(marker4,rgbi)
}



## plot par over time by class
plot(c(1,12),c(min(climjoin$par),max(climjoin$par)),xlab="month",ylab="Mean PAR ()",type="n")
mtext(gcm)
for (i in 1:length(classes)) {
  lines(climjoin$mo[climjoin$eco == classes[i]],
        climjoin$par[climjoin$eco == classes[i]],
        col=marker[i],lwd=2)
  text(2000,climjoin$par[climjoin$eco == classes[i]][94],
       labels=paste(classes[i]),col=marker[i])

}

## Set up Pest calculator clim file
yearspest <- 1981
nyearspest <- length(yearspest)
pestclim <- data.frame(matrix(nrow=12*length(classes)*nyearspest,ncol=8))
names(pestclim) <- c("Ecoregion","month","avgminT","avgmaxT","stddevT","avgppt","stdevppt","year")

## Update, change the way stdev of monthly Temps is calculated, so that it is stdev through time (10-yrs), rather than within ecoregion space.
pptsd <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\data_gee\\climate\\Terraclimate_ppt_sd_normal_1981-2010.csv",header=T)
pptsd <- pptsd %>% dplyr::arrange(eco,mo)

pestclim$Ecoregion <- climjoin$eco
pestclim$month <- climjoin$mo
pestclim$avgminT <- round(climjoin$tmin,digits=2)
pestclim$avgmaxT <- round(climjoin$tmax,digits=2)
pestclim$stddevT <- round(climjoin$tmeansd2sd,digits=1)
pestclim$avgppt <- round(climjoin$ppt, digits=1)
pestclim$stdevppt <- round(pptsd$pptsd,digits=1)
pestclim$year <- yearspest[1]

#write.table(pestclim,paste(pestdir,'NEX_pestclim_',gcm,"_",rcp0,".txt",sep=""),row.names=F)

## Update, change the way stdev of monthly Temps is calculated, so that it is stdev through time (10-yrs), rather than within ecoregion space.
pestclim <- read.table(paste(pestdir,'NEX_pestclim_',gcm,"_",rcp0,".txt",sep=""),header=T)

ecos <- sort(unique(pestclim$Ecoregion))
yearspest <- sort(unique(pestclimtmp$year))

## Now parse the pest climate dataframe to create annual input files for Pest Calculator for Landis-II.
#yearspest <- 2006:2098
nyearspest <- length(yearspest)
ecosa <- ecos[1:(length(ecos)/2)]
ecosb <- ecos[!(ecos %in% ecosa)]

setwd(pestdir)

for (i in 1:length(yearspest)) {
  ## Subset climate file for year i
  pesttemp <- pestclim[which(pestclim$year == yearspest[i] & pestclim$Ecoregion %in% ecosa),1:7]
  pesttemp2 <- pestclim[which(pestclim$year == yearspest[i] & pestclim$Ecoregion %in% ecosb),1:7]
  ## Create file name for input files for Pest Calculator year i.
  tempname <- paste(pestdir,"PestCalcInput_",yearspest[i],"a.txt",sep="")
  tempname2 <- paste(pestdir,"PestCalcInput_",yearspest[i],"b.txt",sep="")
  ## Read in template file to edit and append with climate for year i.
  tempfile <- readLines("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\models\\PestCalc\\PestCalcInput_templatea.txt")
  tempfile2 <- readLines("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\models\\PestCalc\\PestCalcInput_templateb.txt")
   ## Edit output file names within text template for year i.
  fileout <- gsub("PestLogFile.txt",paste("PestLogFile",yearspest[i],"a.txt",sep=""),tempfile)
  fileout <- gsub("PestSppEcoregionTable.txt",paste("PestSppEcoregionTable",yearspest[i],"a.txt",sep=""),fileout)
  ## Repeat for second file. Pest Calculator can't handle more than 20 Ecoregions, so splitting into two calls.
  fileout2 <- gsub("PestLogFile.txt",paste("PestLogFile",yearspest[i],"b.txt",sep=""),tempfile2)
  fileout2 <- gsub("PestSppEcoregionTable.txt",paste("PestSppEcoregionTable",yearspest[i],"b.txt",sep=""),fileout2)
  writeLines(fileout,tempname)
  writeLines(fileout2,tempname2)
  #  file.copy("C:\\Users\\jrfoster\\Dropbox\\SpruceFir\\PestCalc\\PestCalcInput_templatea.txt",
  #            tempname,overwrite=T)
  #  file.copy("C:\\Users\\jrfoster\\Dropbox\\SpruceFir\\PestCalc\\PestCalcInput_templateb.txt",
  #            tempname2,overwrite=T)

  write.table(pesttemp,file=tempname,append=TRUE,row.names=F,col.names=F,sep = "\t")
  write.table(pesttemp2,file=tempname2,append=TRUE,row.names=F,col.names=F,sep = "\t")
  #  write.table(pesttemp,tempname,sep="\t",col.names=F,row.names=F)
  #  write.table(tempsite,tempname2,sep="\t",col.names=F,row.names=F,quote=F)
  rm(pesttemp,pesttemp2,tempname,tempname2,tempfile,tempfile2,fileout,fileout2)
}


## Read some PestCalculator output and stitch tables back together
for (i in 1:length(yearspest)) {
  if (rcp0 == "historical") {
    tempname <- paste(pestdir,"output\\",gcm,"_",rcp0,"\\","PestSppEcoregionTable",yearspest[i],"a.txt",sep="")
    tempname2 <- paste(pestdir,"output\\",gcm,"_",rcp0,"\\","PestSppEcoregionTable",yearspest[i],"b.txt",sep="")
    
  } else {
    tempname <- paste(pestdir,"output\\NEX_",gcm,"_",substr(rcp0,4,5),"\\","PestSppEcoregionTable",yearspest[i],"a.txt",sep="")
    tempname2 <- paste(pestdir,"output\\NEX_",gcm,"_",substr(rcp0,4,5),"\\","PestSppEcoregionTable",yearspest[i],"b.txt",sep="")
  }
  pesttemp <- read.table(tempname,header=T)
  pesttemp2 <- read.table(tempname2,header=T)

  if (i == 1) {
    pesttemp3 <- cbind(pesttemp,pesttemp2)
    names(pesttemp3) <- classes
    pesttemp3 <- cbind(data.frame(year=yearspest[i],spp=row.names(pesttemp3)),pesttemp3)
    rm(tempname,tempname2,pesttemp,pesttemp2)
  }
  if (i > 1) {
    pesttemp4 <- cbind(pesttemp,pesttemp2)
    names(pesttemp4) <- classes
    pesttemp4 <- cbind(data.frame(year=yearspest[i],spp=row.names(pesttemp4)),pesttemp4)
    pesttemp3 <- rbind(pesttemp3,pesttemp4)
    rm(tempname,tempname2,pesttemp,pesttemp2,pesttemp4)
  }
}

species <- unique(pesttemp3$spp)
lwds <- rep(1,length(classes))
lwds[c(9,10,11,25)] <- 2
ltys <- rep(1,length(classes))
ltys[c(17,28,13,26,14,15,16)] <- 2

today <- Sys.Date()

pdf(file=paste(pestdir,"output\\","Prob_establishment_x_spp_",gcm,"_",rcp0,"_",today,".pdf",sep=""),
    width=10,height=8,pointsize=10)
for (p in 1:length(species)) {
  sptest <- species[p]
  test <- pesttemp3[which(pesttemp3$spp==sptest),]

  plot(c(min(yearspest),max(yearspest)),c(0,1),type="n",xlab="year",ylab="Prob. Est. in 100 yrs")
  mtext(sptest)

  for (j  in 3:dim(test)[2]) {
    lines(test$year,test[,j],type='l',col=marker2[j-2],lwd=lwds[j-2],lty=ltys[j-2])
    text(jitter(2001,0.05),test[1,j],names(test)[j],col=marker2[j-2])
  }
}
dev.off()

ltys2 <- rep(1:4,length(species)/3)

pdf(paste(pestdir,"output\\Pest_soil_classes_",min(yearspest),"_",max(yearspest),"_",gcm,"_RCP_",rcp0,"_b_",today,".pdf",sep=""),onefile=T,
    width=10.5,height=8,pointsize=12)

for (p in 1:length(classes)) {
  ploteco = classes[p]
  colp <- which(names(pesttemp3)==ploteco)
  plot(c(min(yearspest)-10,max(yearspest)),c(0,1),type="n",
       xlab="year",ylab="Prob. Establishment 100-yrs")

  for (j in 1:length(species)) {
    lines(pesttemp3$year[pesttemp3$spp == species[j]],pesttemp3[pesttemp3$spp == species[j],colp],col=marker[j],
          lty=ltys2[j],lwd=2)
    minj <- pesttemp3[pesttemp3$spp == species[j],colp][1]
    #minjyr <- pnetout$year[which(pnetout$NPPwood[pnetout$sp == splist[j]] == minj)]
    text(jitter(min(yearspest)-5,.05),minj,species[j],col=marker[j])
  }
  mtext(paste("Ecoregion = ", ploteco,sep=""))
  #legend("topright",species,col=marker,lty=c(ltys),cex=0.7,lwd=2)
}

dev.off()


### Now set up PNET input files...
## Set up pnet clim file
yearspnet <- yearspest
nyearspnet <- length(yearspnet)
#pnetclim <- data.frame(matrix(nrow=12*length(classes)*nyearspnet,ncol=7))
#names(pnetclim) <- c("Ecoregion","year","doy","Tmax","Tmin","par","pptcm")

modoy <- data.frame(mo=1:12,doy=c(15,46,76,107,137,168,198,229,259,290,321,351))
climjoin <- climjoin %>% inner_join(modoy)
climjoin$year <- 1981

# Subset climjoin in the order needed for PNET-II input file
pnetclim <- climjoin %>% dplyr::select(Ecoregion=eco,year,doy,Tmax=tmax,Tmin=tmin,par=par,pptcm=ppt)
# Change units or round variables as needed for input file
pnetclim <- pnetclim %>% mutate(Tmax=round(Tmax,digits=4),Tmin=round(Tmin,digits=4),par=round(par,digits=4),pptcm=round(pptcm/10,digits=4))

setwd("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\models\\PNET")

dirfiles <- dir()

if (length(grep("site_by_ecoregion",dirfiles)) < 1) {
tempsite <- read.table("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\models\\PNET\\adk_template_site.txt",header=T)
## Read in soil means by ecoregion to parse and fill in site input file
soilmn <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\gis\\adk_soilindex2remap_means_w_wiltingpoint.csv",header=T)
soilmn$waterholdcap <- soilmn$fieldcap - soilmn$WC15Bar # Water holding capacity = field capacity - wilting point (wc15bar)
tempsite0 <- data.frame(matrix(nrow=0,ncol=dim(tempsite)[2]))
names(tempsite0) <- names(tempsite)

# Now loop through ecoregions and populated site template file with all ecoregion information
for (i in 1:length(ecos)) {
  tempi <- tempsite
  ecoi <- ecos[i]
  tempi$Ecoregion <- ecoi
  tempi$Lat <- round(soilmn$lat[which(soilmn$soilindex2 == ecoi)],digits=3)
  tempi$Lon <- abs(round(soilmn$lon[which(soilmn$soilindex2 == ecoi)],digits=3))
  tempi$WaterHoldingCap <- abs(round(soilmn$waterholdcap[which(soilmn$soilindex2 == ecoi)],digits=3))
  tempsite0 <- rbind(tempsite0,tempi)
}

write.table(tempsite0,"C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\models\\PNET\\adk_template_site_by_ecoregion.txt",row.names=F,col.names=F,quote=F)
} else tempsite0 <- read.table("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\models\\PNET\\adk_template_site_by_ecoregion.txt",header=F)

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

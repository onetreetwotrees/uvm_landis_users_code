#Developed by Dr. Jane Foster
#University of Vermont

#load the necessary libraries
library(raster)
library(rgdal)
library(sp)
library(dplyr)

#;--------------------------------------------------------------------------------------------

#  ;   OPEN AND READ INPUT FILES
# Function to cast variables stored as a factor to numeric in the correct way
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
as.char.factor <- function(x) {as.character(levels(x))[x]}

#;	Set a directory for desired rasters image files.
imgdir <- "C:\\Users\\localjrfoster\\Dropbox\\Projects\\Adirondacks\\gis\\"
setwd(imgdir)

zones <- raster(paste(imgdir,"LandClass20180627_UTM_lccode.bin",sep=""))

#   Calculate the extent covered by desired shapefiles/study areas. You find these for your study area.
#   This example shows eastings and northings in m.
ex  <- raster(xmn=532400, xmx=608900, ymn=4870000, ymx=4969800,crs=crs(zones),resolution=30)
zones2 <- crop(x=zones,y=ex)

# Write out subsetted Raster as GeoTiff. Check that datatype is okay for you.
writeRaster(zones2, filename="adk_mgmt_zones_mask", format="GTiff", 
            overwrite=T,datatype='INT2S')

gc()

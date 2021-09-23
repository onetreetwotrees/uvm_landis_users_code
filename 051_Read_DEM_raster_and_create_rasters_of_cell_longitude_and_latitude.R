library(rgdal)
library(raster)
library(sp)
#library(viridis)

## Read in raster of DEM
dem <- raster('File directory//path//filename')
#dem01 <- raster('C://Users//janer//Dropbox//Projects//Inspires//data_download//GEE//Topographic_Indices_Hanusia_from_SRTM_utm18_WGS84-0000000000-0000000000.tif',
#                band = 1)
#dem02 <- raster('C://Users//janer//Dropbox//Projects//Inspires//data_download//GEE//Topographic_Indices_Hanusia_from_SRTM_utm18_WGS84-0000011008-0000000000.tif',
#                band = 1)
#dem <- mosaic(dem01, dem02, fun=median, na.rm=T)
## Write mosaicked DEM to file.
#writeRaster(dem, 'C://Users//janer//Dropbox//Projects//Inspires//data_download//GEE//DEM_Hanusia_from_SRTM_utm18_WGS84.tif', 
#            format = "GTiff")
  
## Create function to extract lonlat from raster cell
## Function copied from  https://github.com/SWotherspoon/SGAT/blob/master/R/Raster.R

lonlatFromCell <- function(raster,cells,spatial=FALSE) {
  if(is.na(projection(raster)) || isLonLat(raster)) {
    xyFromCell(raster,cells,spatial=spatial)
  } else {
    p <- spTransform(xyFromCell(raster,cells,spatial=TRUE),
                     CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
    if(spatial) p else coordinates(p)
  }
}

## Create copies of dem raster to hold cell latitude and longitude
dem.lat <- dem.long <- dem
## Extract long and lat from raster cells using function and cast to data.frame
dem.df.ll <- data.frame(lonlatFromCell(dem, 1:ncell(dem)))
## Now set values for rasters of dem.long and dem.lat
dem.lat <- setValues(dem.lat, values = dem.df.ll$y)
dem.long <- setValues(dem.long, values = dem.df.ll$x)

## Try using latitude raster to modify how numbers are displayed in DEM
## First, set a latitude that will divide your northern and southern regions
## Modify this for your study region
lat.divide <- 43.5

dem.class <- dem
## First, create a simple example by converting DEM to classified map with 2 classes
dem.class[which(dem[] > 450)] <- 2
dem.class[which(dem[] <= 450)] <- 1
## View both rasters in the plot window
plot(dem)
plot(dem.class)
## View latitude raster
plot(dem.lat)
dem.lat

## Example code, add a single scalar value to dem.class for all areas above a certain latitude
## Notice that only difference b/w right and left side of "<-" is the scalar value to add (5)
dem.class[which(dem.lat[] > lat.divide)] <- 5 + dem.class[which(dem.lat[] > lat.divide)]

## Now plot the result
plot(dem.class)

## Example of how to write a modified raster to new Geotiff file.
## Change variable name, directory path and new file name for your application.
writeRaster(dem.class, 'C://Users//janer//Dropbox//Projects//Inspires//data_download//GEE//DEM_Hanusia_from_SRTM_utm18_WGS84_class_example.tif', 
            format = "GTiff")


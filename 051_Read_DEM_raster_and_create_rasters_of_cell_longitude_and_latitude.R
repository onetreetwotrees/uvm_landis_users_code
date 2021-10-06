library(rgdal)
library(raster)
library(sp)
#library(viridis)

## Read in raster of DEM
dem <- raster('File directory//path//filename')
soil <- readOGR(dsn = "C://Users//janer//Dropbox//Projects//Inspires//data_shared//Soil class outputs AUG 2021-20210915T170256Z-001//Soil class outputs AUG 2021",
               layer = "soilmu_a_Merge_colors_25Aug2021")
#dem01 <- raster('C://Users//janer//Dropbox//Projects//Inspires//data_download//GEE//Topographic_Indices_Hanusia_from_SRTM_utm18_WGS84-0000000000-0000000000.tif',
#                band = 1)
#dem02 <- raster('C://Users//janer//Dropbox//Projects//Inspires//data_download//GEE//Topographic_Indices_Hanusia_from_SRTM_utm18_WGS84-0000011008-0000000000.tif',
#                band = 1)
#dem <- mosaic(dem01, dem02, fun=median, na.rm=T)
## Write mosaicked DEM to file.
#writeRaster(dem, 'C://Users//janer//Dropbox//Projects//Inspires//data_download//GEE//DEM_Hanusia_from_SRTM_utm18_WGS84.tif', 
#            format = "GTiff")

## Get the map projection info for dem raster and save as crs object
crs.dem <- crs(dem)  
## Reproject soil polygons to same map projection as dem using crs object
## First, make sure you have a numeric field of class numbers to rasterize
soil$soilIndex2 <- as.numeric(soil$solndx2)

soil.crs.dem <- spTransform(soil, crs.dem)
## Rasterize soil polygons to same extent and resolution as dem raster
soil.Rast <- rasterize(soil.crs.dem, dem, field = 'soilIndex2', background = 0)
## Example of how to write a modified raster to new Geotiff file.
## Change variable name, directory path and new file name for your application.
writeRaster(soil.Rast, 'C://Users//janer//Dropbox//Projects//Inspires//data_shared//Soil class outputs AUG 2021-20210915T170256Z-001//Soil class outputs AUG 2021//Hanusia_soilmu_a_Merge_colors_25Aug2021.tif', 
            format = "GTiff")


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

## Now try this process on existing soils class raster rasterized above.
soil.Rast2 <- soil.Rast
soil.Rast2[which(dem.lat[] > lat.divide)] <- 100 + soil.Rast[which(dem.lat[] > lat.divide)]

## Now plot the result
plot(soil.Rast2)

## Now re-mask the background
soil.Rast2[which(soil.Rast[] == 0)] <- 0
## Checking the datatype before writing out to new raster
dataType(soil.Rast2)
? datatype
## Now transform map projection to .prj most compatible with Google Earth Engine
## Make sure resampling method is nearest neighbor ('ngb') for categorical soil variables
## Default, is bilinear resampling which is an average that is not appropriate for categorical vars.
soil.Rast2.wgs84 <- projectRaster(soil.Rast2, crs = CRS("+init=epsg:4326"), method = 'ngb')

## Example of how to write a modified raster to new Geotiff file.
## Change variable name, directory path and new file name for your application.
##writeRaster(dem.class, 'C://Users//janer//Dropbox//Projects//Inspires//data_download//GEE//DEM_Hanusia_from_SRTM_utm18_WGS84_class_example.tif', 
##            format = "GTiff")
writeRaster(soil.Rast2, 'C://Users//janer//Dropbox//Projects//Inspires//data_shared//Soil class outputs AUG 2021-20210915T170256Z-001//Soil class outputs AUG 2021//Hanusia_soilmu_a_Merge_colors_25Aug2021_add100_utm18N.tif', 
            format = "GTiff", datatype = 'INT2U') ## Save to raster file, specify unsigned integer dataType
## Write out the copy of modified soils raster in WGS84 datum. Use this one for GEE climate summaries.
writeRaster(soil.Rast2.wgs84, 'C://Users//janer//Dropbox//Projects//Inspires//data_shared//Soil class outputs AUG 2021-20210915T170256Z-001//Soil class outputs AUG 2021//Hanusia_soilmu_a_Merge_colors_25Aug2021_add100_wgs84.tif', 
            format = "GTiff", datatype = 'INT2U')##, overwrite = T) ## Save to raster file, specify unsigned integer dataType


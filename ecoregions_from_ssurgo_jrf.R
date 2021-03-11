## This script reads SSURGO data to extract cut points for slope and depth
## The cut points are applied to SSURGO data to create ecoregion classes
## Classes represent combinations of soil texture, slope and depth

library(Hmisc)
library(plyr)
library(dplyr)

#options(tibble.width = Inf)
options(dplyr.width = Inf,scipen=999, stringsAsFactors = F) # scipen=0 to turn scientific notation back on again.

# Read SR SSURGO
## CHECK PATH
#D2 <- read.csv("data\\soilmu_a_Merge_26Feb2021.csv",header=T)
D2h <- read.csv("data\\mu_summary_wilt_hanusia_2021-03-10.csv", header=T)
D2 <- read.csv("data\\mu_summary_VT-MA.csv", header=T)
## Problem with mean elevation. Steps: (1) Open shape file and DEM in Arcmap. (2) Go to catalog, spatial analyst, zonal stats by table.
## (3) Specify shapefile for first entry, use FID as the field to summarize zonal stats. Specify DEM as the raster to summarize.
## Compute just the MEAN and tell it to ignore null or nodata values. It will probably default to writing the output to 
## a table in your default geodatabase.(4) To view the output, you can use a "join" with the shapefile. You will join with FID
## as the join field. When you open the joined attribute table, the last column will be the mean elevation. Try vizualizing this
## field. If you notice that Some of the soil polygons in southern unit are not getting summarized (mean = NULL), that is the problem I encountered.
## I don't know why, could be a problem with Shapefile? Maybe try this and see if you can figure it out. I put the topographic
## indices raster stack in the share drive data_Hanusia. Elevation is the first band of that raster stack.
#elev <- read.csv("data_shared\\Hanusia_files_for_soil_class_analysis-20210303\\Benn-Berk_soilmu_a_Merge_mean_elev_by_FID_zonal_stats.csv",header=T)
elev <- read.csv("data\\soil_mu_a_vtma_mu_summary_elev_by_oid.txt", header=T) # "VALUE" relates to objectID in merged shapefile of ssurgo soils data.

# Edit some field names and formats - specific to Hanusia's table
names(D2)
#[1] "ï..OID"     "AREASYMBOL" "SPATIALVER" "MUSYM"      "MUKEY"      "Shape_Leng" "Shape_Area" "MUKEY2"     "mukey_1"    "FirstOfslo"
#[11] "FirstOfbro" "SumOfksat_" "SumOfsand_" "SumOfsilt_" "SumOfclay_" "SumOffield" "SumOfmaxde" "FirstOftex" "FirstOft_1" "FirstOft_2"
#[21] "AvgOfwfift" "AvgOfwfi_1" "AvgOfwfi_2"
#names(D2)[1] <- "OID"

# Hanusia - To DO: Rename or reformat columns to the names specified in the next lines. Your starting column names are not the same,
# So you'll have to read through and update to agree with your column names. I'm not sure why yours got truncated, problem in Arcmap?
# You may not have to cast them to character or numeric if they already have that format, but the desired format is as.numeric, when specified.
## Simplify some variable names and make sure they are classified as.numeric
D2$slopegradw <- D2$FirstOfslo
D2$slopegradw <- D2$FirstOfslopegradwta # as.numeric()
D2$brockdepmi <- as.numeric(D2$FirstOfbrockdepmin)
D2$ksat <- as.numeric(D2$SumOfksat_weight1)
D2$sand <- as.numeric(D2$SumOfsand_weight1)
D2$silt <- as.numeric(D2$SumOfsilt_weight1)
D2$clay <- as.numeric(D2$SumOfclay_weight1)
D2$fieldcap <- as.numeric(D2$SumOffieldcap_weight1)
D2$wiltpoint <- as.numeric(D2$SumOfwiltpt_weight1)
D2$maxdepth <- as.numeric(D2$SumOfmaxdepth_weight)
## Calculate sum of sand, silt and clay, divide and multiply times 100 to get a percentage of each.
D2$compsum <- D2$sand + D2$silt + D2$clay
D2$sand <- D2$sand / D2$compsum * 100
D2$silt <- D2$silt / D2$compsum * 100
D2$clay <- D2$clay / D2$compsum * 100
## Create empty variable "TextClass" to be populated by rule-based classifier below
D2$TextClass <- "None"
attach(D2)

# Calc texture classes - This is the section in the code that creates soil texture classes from thresholds in sand, silt, clay
D2$TextClass[silt+(1.5*clay) < 15] <- "Sand"
D2$TextClass[(silt+(1.5*clay) >= 15) & (silt + (2*clay)<30)] <- "LoamySand"
D2$TextClass[((clay >=7)&(clay<20)&(sand>52)&(silt+(2*clay)>=30))] <- "SandyLoam"
D2$TextClass[((clay<7)&(silt<50)&(silt+(2*clay)>=30))] <- "SandyLoam"
D2$TextClass[((clay >=7)&(clay<27)&(silt>=28)&(silt<50)&(sand<=52))] <- "Loam"
D2$TextClass[((silt >=50)&(clay>=12)&(clay<27))] <- "SiltLoam"
D2$TextClass[((silt >=50)&(silt<80)&(clay<12))] <- "SiltLoam"
D2$TextClass[((silt >=80)&(clay<12))] <- "Silt"
D2$TextClass[((clay >=20)&(clay<35)&(silt<28)&(sand>45))] <- "SandyClayLoam"
D2$TextClass[((clay >=27)&(clay<40)&(sand>20)&(sand<=45))] <- "ClayLoam"
D2$TextClass[((clay >=27)&(clay<40)&(sand<=20))] <- "SiltyClayLoam"
D2$TextClass[((clay >=35)&(sand>45))] <- "SandyClay"
D2$TextClass[((clay >=40)&(silt>=40))] <- "SiltyClay"
D2$TextClass[((clay >=40)&(sand<=45)&(silt<40))] <- "Clay"

## Quick vizualize barplot of TextClass in your data
barplot(table(D2$TextClass))


## Link elevation data from shapefile back to mu_summary table by mukey
elev$elev <- elev$MEAN
## Join you merged shapefile attribute table (imported as "elev" here) with table D2, excluding any overlapping variables
elev2 <- elev %>% dplyr::select(!(which(names(elev) %in% names(D2)))) %>% inner_join(D2, by = c("MUKEY" = "mukey"))

# Figure out cut points to subdivide soil texture classes based on slope, depth or with elevation, if you calculated zonal means by oid
slopecuts <- quantile(elev2$slopegradw,probs=seq(0,1,1/3),na.rm=T)[3]
depthcuts <- quantile(elev2$maxdepth,probs=seq(0,1,0.2),na.rm=T)[2:3]
#depthcuts <- c(30)
# Vizualize zoil depth
with(elev2, hist(maxdepth, nclass=40)) # If most soils are deeper than 40cm, where most root action is, maybe not worth using as divider
elevcuts <- quantile(elev2$elev,probs=seq(0,1,0.1), na.rm=T)[c(6,10)]
## For New England, when dividing elevation into thirds, consider using 650 as upper cut, as it approximates mean for ecotone
elevcuts <- c(elevcuts[1],650)

# Apply cut points
elev2$slopebin <- as.numeric(cut2(elev2$slopegradw,unique(slopecuts)))
elev2$depthbin <- as.numeric(cut2(elev2$maxdepth,unique(depthcuts)))
elev2$elevbin <- as.numeric(cut2(elev2$elev,unique(elevcuts)))

# Identify unique combinations
## In this step you can add more of the cut point variables, like slopebin or depthbin to create more classes if desired
Dout <- plyr::count(elev2[,c("TextClass","elevbin")])
print(Dout)

sumArea <- elev2 %>% group_by(TextClass, elevbin) %>% summarize_at(vars(Shape_Area), list(~ sum(.,na.rm=T)))
names(sumArea)[3] <- "sum_Shape_Area"

# Ecoregion index
Dout$soilindex <- row.names(Dout)
Dout <- Dout %>% inner_join(sumArea)

# Append ecoregion classes to SSURGO data
Dcomb <- merge(elev2,Dout,by=c("TextClass","elevbin"))
# Compute available water holding capacity (AWC or WHC) as difference between Field Capacity and Wilting Point
Dcomb$WHC <- Dcomb$fieldcap - Dcomb$wiltpoint

# Compute weighted mean of soil variables (weighted by polygon area)
Dout2 <- Dcomb %>% group_by(soilindex) %>% summarize_at(vars(fieldcap,wiltpoint,WHC,slopegradw,brockdepmi,ksat,sand,silt,clay,maxdepth,elev),list(~ weighted.mean(., Shape_Area,na.rm=T)))
Dout3 <- Dout %>% inner_join(Dout2) %>% dplyr::select(soilindex,TextClass,elevbin,elev,freq,fieldcap:maxdepth)
Dcombout <- Dcomb %>% dplyr::select(OBJECTID,elev,elevbin,soilindex) %>%
            arrange(OBJECTID)
soilindex2remap <- Dcombout$soilindex2
soilindex2remap[which(soilindex2remap == 4)] <- 5
soilindex2remap[which(soilindex2remap == 11)] <- 10
soilindex2remap[which(soilindex2remap == 14)] <- 13
soilindex2remap[which(soilindex2remap == 17)] <- 16
soilindex2remap[which(soilindex2remap == 24)] <- 23
soilindex2remap[which(soilindex2remap == 31)] <- 32
soilindex2remap[which(soilindex2remap == 34)] <- 33
soilindex2remap[which(soilindex2remap == 38)] <- 39
soilindex2remap[which(soilindex2remap == 41)] <- 40
soilindex2remap[which(soilindex2remap == 42)] <- 43

Dcombout$soilindex2remap <- soilindex2remap
Dout4 <- Dout3 %>% filter(soilindex2 %in% unique(soilindex2remap))
freq4 <- data.frame(table(soilindex2remap))
Dout4 <- Dout4 %>% inner_join(freq4,by=c("soilindex2" = "soilindex2remap"))
## CHECK PATH
#outfile <- "C:/BRM/LANDIS_II/Projects/SavageRiver/savageriver_eco_index_031717.csv"
#write.csv(Dcomb, file=outfile,row.names=FALSE)
#write.csv(Dcombout,"C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\gis\\adk_soilindex2.csv",row.names=F)
#write.csv(Dout3,"C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\gis\\adk_soilindex2_means.csv",row.names=F)
#write.csv(Dout4,"C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\gis\\adk_soilindex2remap_means.csv",row.names=F)

aggregate(x=Dcomb,by=list(Dcomb$depthbin),FUN="mean")

# Need to add new Wilting Point values from w15bar SSURGO variable
library(sp)
library(rgdal)

# Function to cast variables stored as a factor to numeric in the correct way
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
as.char.factor <- function(x) {as.character(levels(x))[x]}

# Read in shapefile of merged county data 
w15bar <-readOGR(dsn="C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\gis\\ssurgo_from_Emma-20180731",layer="ny6_merge_wc15bar")
# Read previously calculated means (with issues) for mean lat and long
latlong0 <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\gis\\ADK_soils2_means_long_lat_wcs15bar.csv",header=T)


w15bardf <- w15bar@data
w15bardf <- w15bardf %>% select(-OBJECTID,-MUKEY)

# Loopt through and Convert Factors to character for both tables
for (i in 1:dim(w15bardf)[2]) {
  if (is.factor(w15bardf[,i])) {
    w15bardf[,i] <- as.char.factor(w15bardf[,i])
  }
}
str(w15bardf)
w15bardf$SPATIALVER <- as.numeric(w15bardf$SPATIALVER)

for (i in 1:dim(Dcomb)[2]) {
  if (is.factor(Dcomb[,i])) {
    Dcomb[,i] <- as.char.factor(Dcomb[,i])
  }
}

# Create a new MUKEY field to collate all the MUKEYS from previously joined tables
Dcomb$MUKEY_3 <- NA
mukey_cols <- c(grep("mukey",names(Dcomb),grep("MUKEY",names(Dcomb))))
nmukeys <- c()
mukeys <- list()

for (i in 1:dim(Dcomb)[1]) {
  mukey_vals <- unique(as.character(Dcomb[i,mukey_cols]))
  nchar_vals <- nchar(mukey_vals)
  mukey_vali <- mukey_vals[which(nchar_vals > 2)][1]
  Dcomb$MUKEY_3[i] <- mukey_vali
  nmukeys <- c(nmukeys,length(which(nchar_vals > 2)))
  mukeys <- c(mukeys,mukey_vals[which(nchar_vals > 2)])
}
table(nmukeys)

w15bardf2 <- w15bardf %>% select(MUKEY_1,WC15Bar) %>% filter(MUKEY_1 %in% sort(unique(Dcomb$MUKEY_3)))

# Join tables to get Wilting point summarized
Dcomb2 <- Dcomb %>% select(-OBJECTID) %>% left_join(w15bardf2,by=c("MUKEY_3"="MUKEY_1")) %>% distinct(.)
Dcomb3 <- Dcomb2 %>% filter(nchar(MUKEY_3) > 2)
dim(Dcomb3)

#Dcomb3 <- Dcomb3 %>% filter(!(ksat==0 & sand==0 & clay==0 & fieldcap==0 & maxdepth==0))
# Two classes (1,2) are affected by lack of attributes associated with unfinished MUKEYS in Franklin County
# Replace soil means for those classes with means calculated here 
w15barout2 <- Dcomb3 %>% group_by(soilindex2) %>% summarize_at(vars(elev,fieldcap,slopegradw,brockdepmi,ksat,sand,silt,clay,maxdepth,WC15Bar,freq.y),funs(weighted.mean(., Shape_Area,na.rm=T))) %>%
              arrange(as.numeric(soilindex2)) %>% filter(soilindex2 %in% Dout4$soilindex2)
w15out <- w15barout2 %>% select(soilindex2,WC15Bar)
new12means <- w15barout2 %>% filter(soilindex2 %in% c("1","2")) %>% select(elev,freq.y,fieldcap,slopegradw,brockdepmi,ksat,sand,silt,clay,maxdepth)
Dout5 <- Dout4 %>% inner_join(w15out)
Dout5[1:2,min(which(names(Dout5) %in% names(new12means))):max(which(names(Dout5) %in% names(new12means)))] <- new12means

# Now add mean Lat and Long calculated previously
latlong <- latlong0 %>% select(soilindex2=soil,lat=lat,long=long) %>% mutate(soilindex2 = as.character(soilindex2))
Dout5 <- Dout5 %>% inner_join(latlong)
#write.csv(Dout5,"C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\gis\\adk_soilindex2remap_means_w_wiltingpoint.csv",row.names=F)


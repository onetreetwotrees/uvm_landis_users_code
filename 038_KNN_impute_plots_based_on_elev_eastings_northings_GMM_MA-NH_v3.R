#Developed by Dr. Jane Foster
#University of Vermont

#load the necessary libraries
library(raster)
library(rgdal)
library(sp)
library(dplyr)

# Read in data tables for KNN process
lstoposub <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Olivia_Mass\\data_shared\\Imputation Inputs\\csv_inputs\\lstopospout_v2.csv",header=T)
gmmout <- lstoposub %>% dplyr::select(mid, pid, class, classpred)
pidkey <-  read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Olivia_Mass\\data_shared\\Imputation Inputs\\csv_inputs\\plotids_up.csv",header=T) 
pidkey <- pidkey %>% dplyr::select(pid, mid)

# Create a table for "youngcl" to populated recently disturbed areas. Create from classes in gmmout.
classes <- sort(unique(gmmout$class))
youngAges <- 0:29 # Potential forest ages that can be derived from the Landsat disturbance record (Hansen or NAFD)
# Probably need to move creation of this table lower to where spp have already been read in
#youngcl <- data.frame(nspp = rep(1, length(classes) * length(youngAges)))
#youngcl <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\data_output\\landis_age_class_plot_young_stands_adk_v0.csv",header=T)
#sid_not_used <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\data_output\\adk_sid_not_used_in_init_comm_v0.csv",header=T)
#sid_not_used <- sid_not_used$x
#pid_not_used <- pidkey$pid[which(pidkey$sid %in% sid_not_used)]

# Change the working directory
#setwd("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\data_gee\\Landsat")

# Subset and join tables for predictors and GMM classification results
agecl <- lstoposub %>% dplyr::select(mid,pid,latitude,longitude,elevation) %>% inner_join(gmmout, by = c("mid" = "mid")) %>% 
                        inner_join(pidkey, by=c("mid" = "mid")) %>%
          dplyr::select(class,classpred,latitude,longitude,elevation,pid,mid)

# Check that the dimensions represent the correct number of plots (rows in lstoposub). MA-NH = 248
nrow(agecl)

# View structure of agecl data table
#head(agecl)
#sid class classpred  latitude longitude  elevation pid
#class classpred latitude longitude elevation pid mid
#1     1         1 42.38031 -71.78667       145   1   1
#2     4         3 42.32141 -71.80919       216   2   2
#3     3         6 42.31588 -71.81404       208   3   3

#OLD> head(agecl)
#NA.              spcn2   plotba class resub_in Latitude    elevm plot
#1   1 1128599010602_0001 29.84389     4        4 43.25025 711.6967    1
#2   2 1128599010602_0002 32.13958     4       19 43.24861 688.1342    2
#3   3 1128599010602_0003 27.54821     4        4 43.24751 683.2827    3
#4   4 1128599010602_0004 41.32231     6        4 43.24737 665.5792    4
#5   5 1128599010602_0005 45.91368    19       10 43.24717 636.8677    5
#6   6 1128599010602_0006 22.95684     6        6 43.24639 651.5295    6

# make a list of species codes for species you want to simulate in your data
#spsim <- c("abiebals","acerpens","acerrubr","acersacc","betualle","betupapy","betupopu","fagugran","fraxamer","fraxnigr","larilari",
#"ostrvirg","piceabie","picemari","picerube","pinuresi","pinustro","pinusylv","popugran","poputrem","prunsero","querrubr",
#"thujocci","tiliamer","tsugcana","ulmuamer")
# You can type out the list here or use a command like the one below that you have customized to your data table/scheme
spsim <- names(lstoposub)[grep("ACRU",names(lstoposub)):grep("TSCA", names(lstoposub))]

#now create the raster to hold input initcomm map and stack from which to extract probability values - predictors for Knn
## Also create raster to read in the forest class map map. This will be used to screen for plots that only belong to same class...
## All rasters should be masked using the rs raster. Set zeros in rs to zeros here.
setwd("C:\\Users\\janer\\Dropbox\\Projects\\Olivia_Mass\\data_shared\\Imputation Inputs\\rasters")
rasterList <- dir()
class1 <- raster(rasterList[grep("GMM", rasterList)])
elev1 <- raster(rasterList[grep("elev", rasterList)])
age1 <- raster(rasterList[grep("hansen", rasterList)])
mask1 <- raster(rasterList[grep("NLCD", rasterList)]) # Was adk_mask_0.bin but updated to adk_mask.bin 2020-02-26 to include woody wetlands and shrub class 52,90
#extent.rs <- extent(elev1) # Olivia's class raster does not have the same dimensions as elev or age. Crop those to the class size I guess.
extent.rs <- extent(class1)
elev0 <- elev1

elev1 <- crop(elev0,extent.rs)
age1 <- crop(age1, extent.rs)
mask1 <- crop(mask1, extent.rs)
extent(elev1) <- extent(class1) # Line everything up to compare rasters on a cell by cell basis. xy dimensions should be exactly the same.

# Apply the mask to all files
class1[which(mask1[]==0)] <- 0
mask1[which(class1[]==0)] <- 0
# Save an original version of age raster for sampling at points (maintain NA)
ageNA <- age1
age1[which(mask1[]==0)] <- 0
# Age map is supposed to represent the age of disturbed areas at desired start year of simulation. Set to 2015 and subtract from Hansen raster
# added for Olivia's data
# First set any later disturbed area to 2015 to get approximate age
age1[which(age1[] > 2015)] <- 2015
age1 <- age1 - 2015
# Mask out zeros again
age1[which(age1[] == -2015)] <- 0
age1 <- abs(age1)
plot(age1)

# Determine all unique disturbance ages from disturbance map
youngyrs <- sort(unique(age1))

# Make rasters that hold the eastings and northings for each pixel, then scale them mean=0,sd=1
eastings0 <- init(elev1,'x') #init(elev1,'x') # Changed 2020-02-26
eastings1 <- scale(eastings0)
northings0 <- init(elev1,'y') #init(elev1, 'y') # Changed 2020-02-26
northings1 <- scale(northings0)
elev1 <- scale(elev1) #scale(elev1)

# stack three spatial bands together
enelev <- stack(elev1,eastings1,northings1)
names(enelev) <- c("elevpid","eastpid","northpid")
enelev <- brick(enelev)


#now let's load our point locations into a spatialpointsdataframe
pidxy <- lstoposub %>% dplyr::inner_join(pidkey, by = c("mid" = "mid")) %>% dplyr::select(mid,longitude,latitude) #select(pid,longitude,latitude) # Changed 2020-02-26 to agree with init comm text file
xycols <- which(names(pidxy) %in% c("longitude","latitude"))
pidcol <- which(names(pidxy) == "mid") #which(names(pidxy) == "pid") # Changed 2020-02-26 to agree with init comm text file

#create a matrix with our information
#convert the matrix into a spatialpointsdataframe
#convert coordinates to SpatialPoints
xysp <- SpatialPoints(pidxy[,xycols], proj4string= CRS("+proj=longlat +ellps=WGS84"))

#combine the attributes to the now projected points
locs <- SpatialPointsDataFrame(xysp, data.frame(Site=pidxy[,pidcol]))

#Subset locs to just the plots in classification in lstoposub
locs <- locs[which(locs@data$Site %in% agecl$mid),]#locs[which(locs@data$Site %in% lstoposub$pid),]

#Change projection of vector pts to match raster - adjust this for specific project data.
crs.rs <- crs(class1)
locs.rs <- spTransform(locs, crs.rs)  # spTransform makes the projection

#locate raster xy of coordinates specified in loc pts file
v <- cellFromXY(class1, locs.rs)
# Sample age1 raster for disturbance based ages. Then write out result in csv.
agev <- age1[v]
agevout <- locs.rs %>% as.data.frame() %>% mutate(age2015 = agev)
#write.csv(agevout,"..//..//data_output//age1_sampled_from_disturbance_age_map.csv",row.names=F)
# Remove NAs, e.g. plots that fall off the raster map (or should I do this part with the larger map?)
mid_mask <- locs.rs$Site
if (any(is.na(v))) {
  mid_mask <- mid_mask[-which(is.na(v))]
  v <- v[-which(is.na(v))]
}


#locate any zeros in the raster map, as these are inactive in LANDIS.
#You will use these locations to remask raster after next step.
xy0 <- which(class1[v] == 0)

#locate any pid plot class values that do not occur on raster map, from masking or other reason.
#Not used in this version

# Now reassign the initial community class at each plot location to the plot identity (pid)
cl1 <- sort(unique(class1)) ## First get list of unique classes in class1
# Exclude zero, the mask value
cl1 <- cl1[which(cl1 > 0)]
cl1

## Get training data - class assignment for all plots (mid) in agecl data
traincl3 <- as.integer(agecl$class)
pid3 <- agecl$mid 
traincl3 <- as.integer(traincl3)

elevpid <- c(elev1[v])
eastpid <- c(eastings1[v])
northpid <- c(northings1[v])
elevpid <- data.frame(elevpid)
elevpid$eastpid <- eastpid
elevpid$northpid <- northpid
elevpid$plot <- pid3[1:length(eastpid)]
head(elevpid)

## Join scaled elevation to class info, so training data can have elev,eastings, northings for distance knn info
#agecl <- merge(agecl,elevpid,all=T) # Old method
agecl0 <- agecl
agecl <- agecl0 %>% inner_join(elevpid, by=c("mid"="plot"))
train3 <- subset(agecl,select=c("elevpid","eastpid","northpid"))

# Now that training data is extracted, subset enelev data to the study area subset
enelev <- crop(enelev, extent.rs)

# Reformat table youngcl so that it can be appended to agecl from plot data
youngcl_tmp <- data.frame(matrix(nrow=nrow(youngcl),ncol=ncol(agecl),data=NA))
names(youngcl_tmp) <- names(agecl)
youngcl_tmp$class <- youngcl_tmp$classpred <- youngcl$class
youngcl_tmp$mid <- youngcl$plot
# Now extend triancl3 and pid3 with synthetic class values and pids for young disturbed pixels - generated in read_TFLG_for_inv_data_v2 - line741-
traincl3 <- c(traincl3,youngcl_tmp$class)
pid3 <- c(pid3,youngcl_tmp$mid)

## Now create a loop to go through each class in class1 map, subset training data to just plots in that class, then do
## something with elevation to better match samples
library(class)
set.seed(984357)

k <- mask1
rowstarts <- seq(1,nrow(k),by=10)
nnn = 10 # Set Number of nearest neighbors for knn!
maxage = max(age1[],na.rm=T) # 

# If you want to test following loop with one row, use click(age1,cell=T) and click on raster plot
# of age 1 on a disturbed area to get cell number. (Age1 value should not be NA or 0 for clicked point)
# Next use rowFromCell(age1, cellnum) to get a row to set j to and run inside code...
# Look at vector "rowstarts" and set j to the location of desired row (eg you want rowj to be close to row)

s1 <- Sys.time()
for (j in 1:length(rowstarts)) {
  rowj <- rowstarts[j]
  kblock <- getValuesBlock(mask1,row=rowj,nrows=10)
  cltmp <- getValuesBlock(class1,row=rowj,nrows=10)
  cutsj <- getValuesBlock(age1,row=rowj,nrows=10)
  elevj <- getValuesBlock(enelev,row=rowj,nrows=10)
  #elevval <- getValuesBlock(elev1,row=rowj,nrows=10)
  pixloc <- which(cltmp > 0)
  cuts2 <- cutsj[pixloc]
  cltmp2 <- cltmp[pixloc]
  enelev2 <- elevj[pixloc,]
  #elevvaltmp2 <- elevval[pixloc]
  

  for (i in cl1) {  
    cli <- pid3[which(traincl3 == cl1[i])] ## Subset training plots to those that belong to current class i.
    if (i %in% sprucefirclasses) {
      traini <- train3[which(traincl3==cl1[i]),] ## Subset training data to those cells that belong to current class i.
    } else {
      cli <- cli[!(cli %in% nospfir)]
      traini <- train3[which(traincl3==cl1[i]),]
      #traini[,c(sprucefirclasses)] <- 0
      traini <- traini[!(cli %in% nospfir),]
    }
  
    # Remove NAs - not sure why these occur - added 2020-02-26
    #traini <- traini[!(is.na(traini$elevpid)),]
    pixloc2 <- which(cltmp2 == cl1[i])
    #s1 <- Sys.time()
    if (length(pixloc2) > 0) {
    # Use k-nearest neighbor to assign 1 of 5 nearest plots in the same class to each pixel
      if (length(pixloc2) > 1) {
        if (length(cli) < nnn) {nnn = length(cli)}
        ki <- as.integer(as.character(knn(traini, enelev2[pixloc2,], cli,k=nnn,prob=F,use.all=F)))
      }
      if (length(pixloc2) == 1 & length(pixloc) > 1) { ## Catch case where many pixels in block are active, but only 1 in class.
        enelev3 <- data.frame(matrix(enelev2[pixloc2,],nrow=1))
        names(enelev3) <- names(enelev)                      
        ki <- as.integer(as.character(knn(traini, enelev3, cli,k=nnn,prob=F,use.all=F)))
      }
      if (length(pixloc2) == 1 & length(pixloc) == 1) { ## Catch case where only 1 pixel in block is active
        enelev3 <- data.frame(matrix(enelev2,nrow=1))
        names(enelev3) <- names(enelev)                      
        ki <- as.integer(as.character(knn(traini, enelev3, cli,k=nnn,prob=F,use.all=F)))
      }
    
    ## Now reassign any young pixels (where age in cuts2 < maxage) to young age classes...
    # Need new young age codes for this set of plot data. Should start at number higher than n plots.
    if (any(cuts2[pixloc2] > 0 & cuts2[pixloc2] <= maxage,na.rm=T)) {
      if (cl1[i] < 14) {
        cutinds2 <- which(cuts2[pixloc2] > 0 & cuts2[pixloc2] <= maxage & cltmp2[pixloc2] < 14) # Update from "read_TFLG_for_inv_data_v2.R" lines 741-
        tempclass2 = as.integer(3000 + cltmp2[pixloc2][cutinds2]*100 + round(cuts2[pixloc2][cutinds2],digits=0))
        tempclasses <- sort(unique(tempclass2))
        for (m in 1:length(tempclasses)) {
          tmpcli <- tempclasses[m]
          indsm <- which(tempclass2 == tmpcli)
          np <- length(indsm) 
          npids <- length(pid3[which(traincl3 == tmpcli)])
          if (np == 1 & npids > 1) {ki[cutinds2][indsm] <- sample(pid3[which(traincl3 == tmpcli)],2,replace=T)[1]
          } ## "sample" doesn't work as desired when n = 1.
          if (np > 1 & npids > 1) {
            ki[cutinds2][indsm] = sample(pid3[which(traincl3 == tmpcli)],np,replace=T)
          }
          if (npids == 1) {## PROBLEM is that the number of PIDs is only 1. Need to MODIFY to catch this
            ki[cutinds2][indsm] <- pid3[which(traincl3 == tmpcli)] 
          }
          rm(tmpcli,indsm,np)
        }
      rm(cutinds2,tempclass2,tempclasses)
      }
      if (cl1[i] > 13) {
        cutinds2 <- which(cuts2[pixloc2] > 0 & cuts2[pixloc2] <= maxage & cltmp2[pixloc2] > 13)
        tempclass2 = as.integer(cltmp2[pixloc2][cutinds2]*100 + round(cuts2[pixloc2][cutinds2],digits=0))
        tempclasses <- unique(tempclass2)
        for (m in 1:length(tempclasses)) {
          tmpcli <- tempclasses[m]
          indsm <- which(tempclass2 == tmpcli)
          np <- length(indsm)
          npids <- length(pid3[which(traincl3 == tmpcli)])
          if (np == 1 & npids > 1) {ki[cutinds2][indsm] <- sample(pid3[which(traincl3 == tmpcli)],2,replace=T)[1]
          } ## "sample" doesn't work as desired when n = 1.
          if (np > 1 & npids > 1) {
            ki[cutinds2][indsm] = sample(pid3[which(traincl3 == tmpcli)],np,replace=T)
          }
          if (npids == 1) {## PROBLEM is that the number of PIDs is only 1. Need to MODIFY to catch this
            ki[cutinds2][indsm] <- pid3[which(traincl3 == tmpcli)] 
          }
          rm(tmpcli,indsm,np)
        }
        rm(cutinds2,tempclass2,tempclasses)
      }
    }
    
    #Sys.time()-s1

    kblock[pixloc][pixloc2] <- ki
    rm(ki,pixloc2)
    }
  }
  
  k[rowj:(rowj+9),] <- kblock
  rm(rowj,cltmp,pixloc,cltmp2,kblock)
}
Sys.time()-s1
## This took 1.857 mins!
## This took 10.94 hours
kcopy <- k


#Need another line here, don't want to replace younger forests id's > 1752 with older plot veg stuff.
keepvals <- k[v][which(k[v] > 1263)] ## Only happens in FVS plots that were cut before 2010. Legacy code - not sure how to update for ADK.
keeplocs <- which(k[v] > 1263)


k[v] <- mid_mask
k[v][xy0] <- 0
k[v][keeplocs] <- keepvals

## Then need to remask area outside park. Should do this step at start to make k go faster. Or mask probs file.
k[which(mask1[]==0)] <- 0

## Check for any errant pids, that are not in the list of possible plots in pid3 
k[(which(!(k[] %in% pid3) & (k[] != 0)))] ## Appears to be one mystery value of 1,occuring at northern edges of landsat replace!
k[(which(!(k[] %in% pid3) & (k[] != 0)))] <- sample(pid3[which(traincl3 == 3)],2,replace=T)[1] # replace with most common class

#Write raster to new initial communities file.
#rf <- writeRaster(rs, filename="gmnf_init_comm_v16b_masked_v2.bin", format="ENVI", 
#                  overwrite=TRUE,datatype='INT2S')
kout <- writeRaster(k, filename="adk_init_comm_v0.bin", format="ENVI", 
                                      overwrite=TRUE,datatype='INT2S')
#//////////////////////////////////////////////////////
## Seem to be losing some class representation that we want. Maybe I need to rewrite to only select nearest 
## neighbors within the same class. Do this by loading in class map and looping through each class, doing a 
## knn within that class...
#library(rsMove)

# Try segmenting the age raster to get at distribution of disturbance sizes by year
# Should have done this just for the wind disturbances maybe
#age1_allNA[which(age1_allNA[] == -999)] <- NA
#age1_seg <- segRaster(age1_allNA,break.point = 1, min.value=1) # Started at 3:41 pm

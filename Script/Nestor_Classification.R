##################################################################################################################################3
## Classification of water bodies adapted to the MB2 project Eagle program
## Based on Image Classification with RandomForests in R by Alí Santacruz
## Author: Néstor Gualsaqui
## Submission: April 2019
##################################################################################################################################
## Background information 
## Study area:
## Esmeraldas river, Ecuador 

## Datasets: 
## Landsat8 2017.05.06
## Landsat8 2019.02.05
##################################################################################################################################
## Output: 
## Water mask
## Thematic map
##################################################################################################################################

############################# Set work directory #############################

## Set work directory 
setwd("C:/EAGLE_DATA/RS_proy/R")

## Download the required data 
## Install package
devtools::install_github("16EAGLE/getSpatialData")

## load packages
library(getSpatialData)
library(raster)
library(sf)
library(sp)

## Define the area of study
aoi3 <- read_sf(dsn = "C:/EAGLE_DATA/RS_proy/Data/Shapes", layer="aoi3")     # load the shapefile  
aoi3 <- aoi3[[2]]                                                            # AOI as sp object
plot(aoi3)

## Define the area of study
set_aoi(aoi3)
view_aoi()

## Login to USGS and set the archive directory
login_USGS(username = "Nestor_Gualsaqui")                                    # write the password to get access                               
set_archive("C:/EAGLE_DATA/RS_proy/Data/Landsat/2017")

## Get available products and select one image
product_names <- getLandsat_names()

## Use getLandsat_query to search for data, using the session aoi            
query <- getLandsat_query(time_range = c("2017-05-02", "2017-07-02"),        # Set the desired time range 
                              name = product_names[7])

## Record preview 
View(query)
getLandsat_preview(query[1,])

## Print available levels for a record
query[1,]$levels_available

## Download record 1 with level "l1" (will direct to AWS automaticaly)
files <- getLandsat_data(records = query[1,], level = "l1", source = "auto")

############################# Download finished ############################# 

## Define a command which checks if the package is installed 
## and if not, it installs the package automatically
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1]))
  {install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }

loadandinstall("maptools")
loadandinstall("randomForest")
loadandinstall("rgdal")
loadandinstall("mgcv")
loadandinstall("nlme")
loadandinstall("raster")
loadandinstall("lattice")
loadandinstall("ggplot2")
loadandinstall("sp")
loadandinstall("caret")
loadandinstall("randomForest")
loadandinstall("e1071")

############################# Start of the classification process ############################# 

## Import some bands in a big stack of bands:

raster1 <- raster("C:/EAGLE_DATA/RS_proy/Data/Landsat/2017/LC08_L1TP_011059_20170506_20170515_01_T1_B1.tif")
raster2 <- raster("C:/EAGLE_DATA/RS_proy/Data/Landsat/2017/LC08_L1TP_011059_20170506_20170515_01_T1_B2.tif")
raster3 <- raster("C:/EAGLE_DATA/RS_proy/Data/Landsat/2017/LC08_L1TP_011059_20170506_20170515_01_T1_B3.tif")
raster4 <- raster("C:/EAGLE_DATA/RS_proy/Data/Landsat/2017/LC08_L1TP_011059_20170506_20170515_01_T1_B4.tif")
raster5 <- raster("C:/EAGLE_DATA/RS_proy/Data/Landsat/2017/LC08_L1TP_011059_20170506_20170515_01_T1_B5.tif")
raster7 <- raster("C:/EAGLE_DATA/RS_proy/Data/Landsat/2017/LC08_L1TP_011059_20170506_20170515_01_T1_B7.tif")

## Stack the bands 
satImage3 <- stack(raster1, raster2, raster3, raster4, raster5, raster7)

## make short band names 
names(satImage3) <- paste0("B", c(1:5, 7))

## Read the trainind data, shapefile
trainData <- shapefile("C:/EAGLE_DATA/RS_proy/Data/Shapes/water/water_Polygon")

### Name of the attribute of the land cover type 
attName <- "Class"

## Extrat the pixel values in the training data for every band loaded 
## and store them in a data frame dfAll with the corresponding land cover class

dfAll=data.frame(matrix(vector(), nrow = 0, ncol = length(names(satImage3))+1))
for(i in 1: length(unique(trainData[[attName]]))){
  category <- unique(trainData[[attName]])[i]
  categorymap <- trainData[trainData[[attName]]==category,]
  dataSet <- extract(satImage3, categorymap)
     dataSet <- sapply(dataSet, function(x) {cbind(x, Class = rep(category, nrow(x)))})
  df <- do.call("rbind", dataSet)
  dfAll <- rbind(dfAll, df)
}

## Subset the data, this will help for reducing the time process of the random forest 
## subset the data generating 1000 random samples:
nsamples <- 1000
sdfAll <- dfAll[sample(1:nrow(dfAll), nsamples), ]


## Model fitting and image classification
modFit_rf <- train(as.factor(Class) ~ B4 + B5 + B3, method = "rf", data = sdfAll)

## Make a raster with predictions from the fitted model object 
loadandinstall("snow")

beginCluster()
preds_rf <- clusterR(satImage3, raster::predict, args = list(model = modFit_rf))
endCluster()
 plot(preds_rf)
 
## Export result as a geotiff 
writeRaster(preds_rf,filename = "C:/EAGLE_DATA/RS_proy/data_export/test.tif", overwrite=TRUE)

############################# Make same process for the image from 2019
############################# Creation of thematic Map ############################# 

loadandinstall("sf")
loadandinstall("ggplot2")
loadandinstall("tmap")
loadandinstall("tmaptools")
loadandinstall("leaflet")
loadandinstall("dplyr")
loadandinstall("ggsn") 
loadandinstall("GISTools")

# Open Vector Layer in order to crop the result raster
cropextent <- readOGR(dsn = "C:/EAGLE_DATA/RS_proy/Data/Shapes", layer= "aoi3")

# crop the raster using the vector extent
preds_rf_crop <- crop(preds_rf, cropextent)
preds_rf_crop

plot(preds_rf_crop)

### save the crop raster 
writeRaster(preds_rf_crop,filename = "C:/EAGLE_DATA/RS_proy/data_export/class_fin.tif", overwrite=TRUE)

## Open the new raster cropped  
watermask <- raster("C:/EAGLE_DATA/RS_proy/data_export/class_fin.tif")
plot(watermask)

## Check the imported data before any further analysis:
summary(watermask)
summary(watermask, maxsamp = ncell(watermask))

## Convert the result to a dataframe for visualizing this data using ggplot2
watermask_df <- as.data.frame(watermask, xy = TRUE)

## Standard dataframe format
str(watermask_df)

## Use ggplot to creat a map
map_17 <- ggplot() +
  geom_raster(data = watermask_df , aes(x = x, y = y, fill = class_fin)) +
      scale_fill_viridis_c(breaks = c(1,2), labels=c("Water bodies", "No water")) +
  coord_quickmap() +  labs(x = "Longitude", y = "Latitude",
                           fill = "Class name") +
    ggtitle("Classification water bodies Esmeraldas")+ 
  theme(legend.background = element_rect(fill = alpha('white', 0)),
        legend.key.height = unit(0.3, "in"))

plot(map_17)
plot(map_19)

## Multiple ggplots on one single page
## Install package
loadandinstall("ggpubr")

ggarrange(map_17, map_19, 
          labels = c("2017", "2019"),
          ncol = 2, nrow = 1)

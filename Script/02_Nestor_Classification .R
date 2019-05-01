
############################# SECOND IMAGE 2019  ############################# 

## Define a command which checks if the package is installed and if not, it installs the package automatically
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }

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

## export some bands 

raster1_19 <- raster("C:/EAGLE_DATA/RS_proy/Data/Landsat/2019/LC08_L1TP_011059_20190205_20190221_01_T1_B1.tif")
raster2_19 <- raster("C:/EAGLE_DATA/RS_proy/Data/Landsat/2019/LC08_L1TP_011059_20190205_20190221_01_T1_B2.tif")
raster3_19 <- raster("C:/EAGLE_DATA/RS_proy/Data/Landsat/2019/LC08_L1TP_011059_20190205_20190221_01_T1_B3.tif")
raster4_19 <- raster("C:/EAGLE_DATA/RS_proy/Data/Landsat/2019/LC08_L1TP_011059_20190205_20190221_01_T1_B4.tif")
raster5_19 <- raster("C:/EAGLE_DATA/RS_proy/Data/Landsat/2019/LC08_L1TP_011059_20190205_20190221_01_T1_B5.tif")
raster7_19 <- raster("C:/EAGLE_DATA/RS_proy/Data/Landsat/2019/LC08_L1TP_011059_20190205_20190221_01_T1_B7.tif")

## Stack the bands together
satImage5 <- stack(raster1_19, raster2_19, raster3_19, raster4_19, raster5_19, raster7_19)

## make short the band names 
names(satImage5) <- paste0("B", c(1:5, 7))


## Read the trainind data, shapefile
trainData <- shapefile("C:/EAGLE_DATA/RS_proy/Data/Shapes/water/water_Polygon")
attName <- "Class"


## Extrat the pixel values in the training data for every band loaded 
## and store them in a data frame dfAll with the corresponding land cover class


dfAll19=data.frame(matrix(vector(), nrow = 0, ncol = length(names(satImage5))+1))
for(i in 1: length(unique(trainData[[attName]]))){
  category19 <- unique(trainData[[attName]])[i]
  categorymap19 <- trainData[trainData[[attName]]==category19,]
  dataSet19 <- extract(satImage5, categorymap19)
  dataSet19 <- sapply(dataSet19, function(x) {cbind(x, Class = rep(category19, nrow(x)))})
  df19 <- do.call("rbind", dataSet19)
  dfAll19 <- rbind(dfAll19, df19)
}

## Subset the data, this will help for reducing the time process of the random forest 
## subset the data generating 1000 random samples:

nsamples <- 1000
sdfAll19 <- dfAll19[sample(1:nrow(dfAll19), nsamples), ]


## Model fitting and image classification

modFit_rf19 <- train(as.factor(Class) ~ B4 + B5 + B3, method = "rf", data = sdfAll19)

loadandinstall("snow")

beginCluster()
preds_rf19 <- clusterR(satImage5, raster::predict, args = list(model = modFit_rf19))
endCluster()
plot(preds_rf19)

## Export as geotiff 
writeRaster(preds_rf19,filename = "C:/EAGLE_DATA/RS_proy/data_export/test19.tif", overwrite=TRUE)

############################# Creat a thematic Map ############################# 

loadandinstall("sf")
loadandinstall("ggplot2")
loadandinstall("tmap")
loadandinstall("tmaptools")
loadandinstall("leaflet")
loadandinstall("dplyr")
loadandinstall("ggsn") #add scalbbar 
loadandinstall("GISTools")


## Open Vector Layer in order to crop the result raster
cropextent <- readOGR(dsn = "C:/EAGLE_DATA/RS_proy/Data/Shapes", layer= "aoi3")


## crop the raster using the vector extent
preds_rf_crop19 <- crop(preds_rf19, cropextent)
preds_rf_crop19

plot(preds_rf_crop19)

### save the crop raster 
writeRaster(preds_rf_crop19,filename = "C:/EAGLE_DATA/RS_proy/data_export/class_fin19.tif", overwrite=TRUE)

## Open the new raster cropped and classified 
watermask19 <- raster("C:/EAGLE_DATA/RS_proy/data_export/class_fin19.tif")
plot(watermask19)

## check your imported data before any further analysis:
summary(watermask19)
summary(watermask19, maxsamp = ncell(watermask19))

## to visualise this data in R using ggplot2, 
## convert it to a dataframe
watermask_df19 <- as.data.frame(watermask19, xy = TRUE)

## standard dataframe format
str(watermask_df19)

## use ggplot to creat a map
map_19 <- ggplot() +
  geom_raster(data = watermask_df19 , aes(x = x, y = y, fill = class_fin19)) +
  scale_fill_viridis_c(breaks = c(1,2), labels=c("Water bodies", "No water")) +
  coord_quickmap() +  labs(x = "Longitude", y = "Latitude",
                           fill = "Class name") +
  ggtitle("Classification water bodies Esmeraldas")+ 
  theme(legend.background = element_rect(fill = alpha('white', 0)),
        legend.key.height = unit(0.3, "in"))
plot(map_19)

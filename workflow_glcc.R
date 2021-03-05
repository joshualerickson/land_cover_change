# load libraries
library(rgee)
ee_Initialize(email = "joshualerickson@gmail.com", gcs = TRUE)
library(sf)
library(AOI)
library(raster)
library(arcgisbinding)
arc.check_product()
library(tidyverse)
#devtools::install_github("joshualerickson/exploreRGEE")
library(exploreRGEE)

#get the bbox for analysis, e.g. Region 1.
region_1_bbox <- st_bbox(c(xmin = -117.190616, xmax = -96.554411, ymax = 49.001390, ymin = 44.26879), crs = st_crs(4326))

# geom for earth engine
geom <- ee$Geometry$Rectangle(region_1_bbox)
r1_bb <- st_as_sfc(region_1_bbox) %>% st_as_sf()
# for graphing
state_r1 <- aoi_get(state = c("Montana", "Idaho", "Wyoming", "South Dakota", "North Dakota"))

# plot showing boundary
ggplot() + geom_sf(data = state_r1, fill = NA) +
  geom_sf(data = r1_bb, fill = NA, color = 'red') + theme_bw()

# get huc12's within boundary
r1_huc12 <- arc.open("T:/FS/Reference/EDW/Hydrology_WCATT_WBD_WatershedConditionClassification_EDW.lyr")

# now get the global forest change from 2011 up to 2019

gfc = ee$Image('UMD/hansen/global_forest_change_2019_v1_7')

# need to mask out years from 2011 to 2019

gfc_11_19_mask = gfc$select('lossyear')$gte(11)

gfc_11_19 = gfc$updateMask(gfc_11_19_mask)

gfc_dataMask = gfc$select('datamask')$eq(1)

gfc_11_19 = gfc_11_19$updateMask(gfc_dataMask) #this is the final image to use

gfc = gfc$updateMask(gfc_dataMask) #if you want

# now visualise to see the difference
Map$addLayer(gfc_11_19, visParams = list(bands = 'loss',min = 0, max = 1, palette = c('red'))) | Map$addLayer(gfc, visParams = list(bands = 'lossyear',min = 0, max = 1, palette = 'blue'))


# get nlcd
nlcd = ee$Image("USGS/NLCD/NLCD2011")
nlcd_tree <- nlcd$select('percent_tree_cover')
nlcd_tree_mask <- nlcd_tree$gte(.90) # I really think above 0 is ideal
nlcd_tree = nlcd_tree$updateMask(nlcd_tree_mask)
nlcd_tree = nlcd_tree$updateMask(gfc_dataMask)

Map$addLayer(nlcd_tree$clip(geom))

#get spatial ref
st_read("T:/FS/Reference/GIS/r01/Data/SpatialReference/R1SpatialReference.gdb")

#change proj and addBands together

gfc_nlcd <- gfc_11_19$addBands(nlcd_tree)

gfc_nlcd <- gfc_nlcd$select(c('percent_tree_cover', 'loss', 'lossyear'))

#download

gfc_nlcd_5070 <- ee_as_raster(gfc_nlcd,
                              region = geom,
                              dsn = "gfc_nlcd_final",
                              scale = 30,
                              crs = 'EPSF:5070',
                              container = "jle_rasters",
                              via = "gcs",
                              lazy = TRUE)

# now let's bring it in. I downloaded it to box in a folder called images.

gfc_nlcd_tif <- raster("images/gfc_nlcd_gte.tif")

gfc_nlcd_lossyear <- raster("images/gfc_nlcd_gte.tif", band = 3)
gfc_nlcd_loss <- raster("images/gfc_nlcd_gte.tif", band = 2)
gfc_nlcd_tree_cov <- raster("images/gfc_nlcd_gte.tif", band = 1)
plot(gfc_nlcd_lossyear)



#now bring in the forest service only shape

usfs_huc_12s <- read_sf("images/clip_w_usfs_huc12_final.shp")
fire_perim <- read_sf("T:/DataCenter/Citrix/Home01/joshualerickson/My Documents/ArcGIS/Default.gdb",
                      layer = 'fire_perim')
st_crs(fire_perim_crs)

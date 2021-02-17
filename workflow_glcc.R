# load libraries
library(rgee)
ee_Initialize(email = "joshualerickson@gmail.com", gcs = TRUE)
library(sf)
library(AOI)
library(tidyverse)
library(leaflet)
library(resourceviz)
library(arcgisbinding)
arc.check_product()

#get the bbox for analysis, e.g. Region 1.
region_1_bbox <- st_bbox(c(xmin = -117.190616, xmax = -96.554411, ymax = 49.001390, ymin = 44.26879), crs = st_crs(4326))

# geom for earth engine
geom <- ee$Geometry$Rectangle(region_1_bbox)

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

gfc_11_19_mask = gfc$select('lossyear')$gt(11)

gfc_11_19 = gfc$updateMask(gfc_11_19_mask)

gfc_dataMask = gfc$select('datamask')$eq(1)

gfc_11_19 = gfc_11_19$updateMask(gfc_dataMask) #this is the final image to use

# now visualise to see the difference
Map$addLayer(gfc_11_19, visParams = list(bands = 'loss',min = 0, max = 1, palette = c('red'))) | Map$addLayer(gfc, visParams = list(bands = 'lossyear',min = 0, max = 1, palette = 'blue'))


# get nlcd
nlcd = ee$Image("USGS/NLCD/NLCD2011")
nlcd_tree <- nlcd$select('percent_tree_cover')
nlcd_tree_mask <- nlcd_tree$gt(.90) # I really think above 0 is ideal
nlcd_tree = nlcd_tree$updateMask(nlcd_tree_mask)
nlcd_tree = nlcd_tree$updateMask(gfc_dataMask)

Map$addLayer(nlcd_tree$clip(geom))

#get spatial ref
st_read("T:/FS/Reference/GIS/r01/Data/SpatialReference/R1SpatialReference.gdb")

#change proj and addBands together

gfc_mask_reProj <- gfc_11_19$reproject(crs = 'EPSG:5070')
nlcd_tree_reProj <- nlcd_tree$reproject(crs = 'EPSG:5070')

gfc_nlcd <- gfc_mask_reProj$addBands(nlcd_tree_reProj)

gfc_nlcd <- gfc_nlcd$select(c('percent_tree_cover', 'loss', 'lossyear'))

#download

gfc_nlcd_5070 <- ee_as_raster(gfc_nlcd,
                              region = geom,
                              dsn = "gfc_nlcd_5070",
                              scale = 30,
                              crs = 'EPSG:5070',
                              container = "jle_rasters",
                              via = "gcs",
                              lazy = TRUE)


areaImage = lossImage$multiply(ee$Image$pixelArea())
g_areaImage = gainImage$multiply(ee$Image$pixelArea())



stats <- areaImage$addBands(lossYear)$reduceRegions(
  reducer = ee$Reducer$sum()$group(groupField = 1),
  collection = ee_eur_hucs,
  scale = 30
)

statsFormatted = ee$List(stats$get('groups'))$map(rgee::ee_utils_pyfunc(
  function(el) {
    d = ee$Dictionary(el)
    return(ee$Number(d$get('group'))$format("20%02d"), d$get('sum'))
  }))
statsDictionary = print(statsFormatted$flatten()$getInfo())
print(statsDictionary)
meta = statsDictionary$getInfo()
meta$getInfo()
sums <- data.frame()
new_j <- print(statsFormatted, type = 'json')
ee_dict <- ee$Dictionary(list(min=0,max=1))
library(jsonify)
library(jsonlite)

from_json(ee_dict)
print(ee_dict,type="json")
jsonify::from_json(statsFormatted)
for(i in 1:length(meta$features)){

  huc_name <- meta$features[[i]]$properties$name
  sum_gr <- meta$features[[i]]$properties$groups
  df_gr <- sum_gr %>% flatten() %>% data.frame() %>%
    mutate(huc = huc_name) %>% pivot_longer(-huc)

}

sum_gr %>% flatten() %>% data.frame() %>%
  mutate(huc = "dkjl") %>% pivot_longer(-huc) %>% view()
stInfo <- ee_as_sf(stats)

stInfo %>% st_drop_geometry() %>% view()

meta <- stats$getInfo()
ee_print(statsDictionary)
stats <- areaImage$reduceRegions(
  reducer = ee$Reducer$sum(),
  collection = ee_eur_hucs,
  scale = 30
)

g_stats <- g_areaImage$reduceRegions(
  reducer = ee$Reducer$sum(),
  collection = ee_eur_hucs,
  scale = 30
)
hucs <- rgee::ee_as_sf(stats)
g_hucs <- ee_as_sf(g_stats)
hucs <- hucs %>% mutate(sqMi_loss = sum*3.861e-7, acres_loss = sum*0.0002471040001481)
g_hucs <- g_hucs %>% mutate(sqMi_gain = sum*3.861e-7, acres_gain = sum*0.0002471040001481)
all_hucs <- hucs %>% full_join(st_drop_geometry(g_hucs), by = c("areaacres", "areasqkm", "huc12", "humod", "hutype", "id", "name", "noncontrib", "states", "tohuc")) %>% st_as_sf
all_hucs %>% mutate(diff = acres_gain-acres_loss) %>%
  pivot_longer(c("acres_loss", "acres_gain", "diff"), names_to = "measure") %>%
  ggplot() + geom_sf(aes(fill = value, geometry = geometry)) + facet_wrap(~measure)


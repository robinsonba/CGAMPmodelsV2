# ---
# title: CGAMP V2.0 - create raster template for covariates
# author: Barry Robinson
# created: Sept 4, 2025
# ---

#NOTES#############################################################################################
#This script extracts the Bird Conservation Regions that define study area and buffers them by 100km
#Buffered polygon is cliped by North American coastline to exclude any ocean
#It then creates a 1km resolution raster template within the buffered BCRs in North_America_Albers_Equal_Area_Conic (ESRI:102008) projection
#This template raster will subsequently be used in Google Earth Engine to create all spatial covariates
###################################################################################################

#1. Load packages
library(tidyverse) #basic data wrangling
library(terra) #basic raster handling
library(sf) #basic shapefile handling
library(rnaturalearth) #accessing country boundaries

#2. Load BCR shapefile and Central Grasslands Roadmap shapefile
st_layers("gis/BCRs.gdb")
bcr <- st_read("gis/BCRs.gdb", layer = "BCR_Terrestrial_Master") |>
  st_transform(crs = "ESRI:102008")
cgr <- st_read("gis/CGRoadmap/Grasslands_Roadmap_boundary_Aug_2021.shp") |>
  st_transform(crs = "ESRI:102008")


#3. extract BCRs to be included, clip by Central Grasslands Roadmap (CGR) boundary, and buffer by 100km
#clipping with CGR boundary to exclude portions of some BCRs that are considered to fall outside of the central great plains
bcr.to.include <- c(11,16,17,18,19,20,21,22,34,35,36,37)
bcr <- filter(bcr, bcr_label %in% bcr.to.include)|>
  st_intersection(cgr)

#load North America boundary to exclude Gulf of Mexico
na <- ne_countries(scale = "large", continent = "North America") |>
  st_transform(crs = "ESRI:102008")

bcr.buff <- st_buffer(bcr, dist = 100*1000) |>
  st_intersection(na)
plot(bcr.buff)


#4. create raster template encompossing bcr.buff for developing spatial covariates
template <- rast(ext = ext(bcr.buff), resolution = 1000, crs = crs(bcr.buff))
cgamp.ras <- rasterize(bcr.buff, template, field = 1)


#5. export bcr, bcr.buff, and cgamp.ras
dir.create("gis/CGAMPV2_boundaries")
bcr |> select(bcr_label, bcr_label_name) |>
  rename(number = bcr_label, name = bcr_label_name) |>
  st_write("gis/CGAMPV2_boundaries/CGAMPV2_BCR.shp")

bcr.buff |> select(bcr_label, bcr_label_name) |>
  rename(number = bcr_label, name = bcr_label_name) |>
  st_write("gis/CGAMPV2_boundaries/CGAMPV2_BCR_buffered.shp")

writeRaster(cgamp.ras, "gis/CGAMPV2_boundaries/CGAMPV2_rasterTemplate.tif")


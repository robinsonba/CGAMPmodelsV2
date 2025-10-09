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
library(dplyr)

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

#5. split cgamp.rad between Canada and USA because landcover covariates need to be extracted separately
#load cgamp.ras if need be
cgamp.ras <- rast("gis/CGAMPV2_boundaries/CGAMPV2_rasterTemplate.tif")

#load boundaries for Canada and USA
canada <- ne_countries(scale = "large", country = "canada", returnclass = "sf") |> 
  st_union() |> 
  st_sf() |>
  st_transform(crs = "ESRI:102008")
usa <- ne_states(country = "united states of america", returnclass = "sf") %>%
  filter(name != "Alaska" & name != "Hawaii") |>
  st_union() |> 
  st_sf() |> 
  st_transform(crs = "ESRI:102008")
plot(canada)
plot(usa)

# Convert sf objects to SpatVector before cropping
canada_vect <- vect(canada)
usa_vect <- vect(usa)

#crop cgamp.ras with Canada
cgamp.canada <- terra::crop(cgamp.ras, canada) |>
  mask(mask = canada)
#the Canadian polygon goes much further south, east, and west than I need it to, so I need to crop even further
extent <- ext(cgamp.canada)
new_extent <- ext(-1350000, 126000, 1025000, extent$ymax)
cgamp.canada <- crop(cgamp.canada, new_extent)
plot(cgamp.canada)

writeRaster(cgamp.canada, "gis/CGAMPV2_boundaries/CGAMPV2_rasterTemplate_Can.tif")

#crop with USA
cgamp.usa <- terra::crop(cgamp.ras, usa) |>
  mask(mask = usa)
plot(cgamp.usa)
#USA also needs a little more croping on the norther edge
extent <- ext(cgamp.usa)
new_extent <- ext(extent$xmin, extent$xmax, extent$ymin, 1200000)
cgamp.usa <- crop(cgamp.usa, new_extent)
plot(cgamp.usa)

writeRaster(cgamp.usa, "gis/CGAMPV2_boundaries/CGAMPV2_rasterTemplate_USA.tif")

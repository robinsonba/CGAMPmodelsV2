#This script downloads climate normals and annual climate covariates from AdaptWest and snaps them to the CGAMPV2 template raster
library(terra)
library(climr)
library(elevatr)
library(curl)
library(tidyverse)
library(ClimateNAr)
library(tools)
library(purrr)
library(sf)

#load template raster and download DEM
cgamp <- rast("gis/CGAMPV2_boundaries/CGAMPV2_rasterTemplate.tif")

#1. Climate Normals #######################################################################################################
#Download climate normals for 1991-2020 peroid. The climr package doesn't support degree day calculations yet, so
#downloading directly from AdaptWest Data basin.
url  <- "https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6v73/normals/Normal_1991_2020_bioclim.zip"
dest <- "gis/Covariate_rasters/temp/Normal_1991_2020_bioclim.zip"
h <- new_handle()
handle_setopt(h, timeout = 0)  # 0 = no timeout
curl_download(url, destfile = dest, handle = h, quiet = FALSE)

#unzip
files <- unzip(dest, list = T)
print(files$Name)
uzip_pattern <- "bFFP|eFFP|DD_0|DD5|DD18|MWMT|Tave_wt|Tave_sm|MAP|MSP|PPT_wt"
uzipFiles <- keep(files$Name, ~grepl(uzip_pattern, .x))
exdir <- "gis/Covariate_rasters/Climate"
unzip(dest, files = uzipFiles, exdir = exdir)
file.remove(dest)

#reproject, crop, mask, and rename all climate normals, then save a multiband raster
rasterFiles <- list.files(exdir, full.names = T, recursive = T)

normals <- rast(rasterFiles) |>
  project(y = cgamp) |>
  crop(y = cgamp) |>
  mask(mask = cgamp)

rasterNames <- sub(".*2020_", "", rasterFiles)
names(normals) <- tools::file_path_sans_ext(rasterNames)
writeRaster(normals, paste0(exdir, "/climateNormals.tif"))
normals <- rast(paste0(exdir, "/climateNormals.tif"))


#2. Annual Climate Variables #################################################################################
#ClimateNAr requires a DEM to create rasters for annual climate variables
#DEM needs to be in WGS84, so reprojection CGAMP template
cgamp_wgs <- project(cgamp, "EPSG:4326", method="near")
dem <- elevatr::get_elev_raster(cgamp_wgs, z = 6)

#Aggregate to ~5km resolution to save even more RAM and save raster
dem_5km  <- dem |> 
  rast() |> #change to SpatRaster format
  aggregate(fact = 5, fun = "mean")
demRasterName = "dem_CGAMP_wgs"
writeRaster(dem_5km, paste0("gis/CGAMPV2_boundaries/", demRasterName, ".tif"))

#remove unnecessary layers and garbage collect to save RAM
rm(dem)
gc()

#define years and variables of interest and extract
years <- 1999:2023
variables <- c("PPT_sp", "PPT_wt", "MSP", "Tave_sp", "Tave_sm")
outDir <- "gis/Covariate_rasters/temp"
annuals <- ClimateNAr(inputFile = "gis/CGAMPV2_boundaries/dem_CGAMP_wgs.tif",
                      periodList = years,
                      varList = variables,
                      outDir = outDir)

#Import annual climate rasters, reproject to match CGAMP, save as raster stacks for each variable
#Some variables are associated with year of survey (y) and others the year prior (y-1). Ensure to keep separate
#define y and y-1 variables
yVar <- c("PPT_sp", "PPT_wt", "Tave_sp")
y_1Var <- c("MSP", "Tave_sm")

#extract appropriate files names and paths
yList <- list.files(file.path(outDir, demRasterName), 
                    pattern = paste(yVar, collapse = "|"), 
                    full.names = T, 
                    recursive = T)


y_1List <- list.files(file.path(outDir, demRasterName), 
                    pattern = paste(y_1Var, collapse = "|"), 
                    full.names = T, 
                    recursive = T)

#load as a list of raster stacks stacking years within variables
yRasters <- lapply(yVar, FUN = function(x) {
  tmpList = yList[grep(x, yList)]
  tmpYears = gsub("\\D", "", tmpList) #extract years
  tmpRast = rast(tmpList)
  names(tmpRast) = paste(x, tmpYears, sep = "_") #ensure rasters have the correct variable and year combo
  return(tmpRast)
})
names(yRasters) <- yVar

#1999 was only included to accomodate lag effect for 2000 in the y_1 rasters...remove 1999 from yRasters
for (i in 1:length(yRasters)) {
  yRasters[[i]] <- yRasters[[i]][[!grepl("1999", names(yRasters[[i]]))]]
}

y_1Rasters <- lapply(y_1Var, FUN = function(x) {
  tmpList = y_1List[grep(x, y_1List)]
  tmpYears = (gsub("\\D", "", tmpList) |>
          as.numeric()) + 1 #add 1 to years because these are for y-1
  tmpRast = rast(tmpList)
  names(tmpRast) = paste(x, tmpYears, sep = "_")
  return(tmpRast)
})
names(y_1Rasters) <- y_1Var

#2024 needs to be removed from y_1Rasters
for (i in 1:length(y_1Rasters)) {
  y_1Rasters[[i]] <- y_1Rasters[[i]][[!grepl("2024", names(y_1Rasters[[i]]))]]
}

#reproject, crop, and mask to match cgamp template and save
saveDir <- "gis/Covariate_rasters/Climate/Annuals"
dir.create(saveDir, showWarnings = F)
yRast_cgamp <- lapply(yVar, FUN = function(x) {
  tmpRaster = project(yRasters[[x]], cgamp) |>
    crop(y = cgamp) |>
    mask(mask = cgamp)
  writeRaster(tmpRaster, filename = file.path(saveDir, paste0(x, ".tif")))
  return(rast(file.path(saveDir, paste0(x, ".tif"))))
})

y_1Rast_cgamp <- lapply(y_1Var, FUN = function(x) {
  tmpRaster = project(y_1Rasters[[x]], cgamp) |>
    crop(y = cgamp) |>
    mask(mask = cgamp)
  writeRaster(tmpRaster, filename = file.path(saveDir, paste0(x, ".tif")))
  return(rast(file.path(saveDir, paste0(x, ".tif"))))
})


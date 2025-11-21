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
#ClimateNAr needs the DEM to be in WGS84, so reprojection CGAMP template
cgamp_wgs <- project(cgamp, "EPSG:4326", method="near")
dem <- elevatr::get_elev_raster(cgamp_wgs, z = 6)

# ext_wgs <- ext(project(cgamp, y = "EPSG:4326"))
# templateWGS <- rast(ext_wgs, res = 0.045, crs = "EPSG:4326")
dem_wgs  <- dem |> 
  rast() |> #change to SpatRaster format
  resample(y = cgamp, method = "bilinear") |> #resample to 1000m to save RAM
  crop(y = cgamp) |>
  mask(mask = cgamp) |>
  aggregate(fact = 5, fun = "mean") |> # aggregate to 5km resolution to save even more RAM
  project(y = templateWGS, method = 'bilinear') #reproject to WGS lat/long, which is required for ClimateNAr
demRasterName = "dem_CGAMP_wgs"
writeRaster(dem_wgs, paste0("gis/CGAMPV2_boundaries/", demRasterName, ".tif"))

#remove unnecessary layers and garbage collect to save RAM
rm(dem_wgs, dem)
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
#Some variables are associated with year of survey (y) and other the year prior (y-1). Ensure to keep separate
#define y and y-1 variables
yVar <- c("PPT_sp", "PPT_wt", "Tave_sp")
y_1Var <- c("MSP", "Tave_sm")

yList <- list.files(file.path(outDir, demRasterName), 
                    pattern = paste(yVar, collapse = "|"), 
                    full.names = T, 
                    recursive = T)

#extract appropriate files names
y_1List <- list.files(file.path(outDir, demRasterName), 
                    pattern = paste(y_1Var, collapse = "|"), 
                    full.names = T, 
                    recursive = T)

#load as a list of raster stacks
yRasters <- lapply(yVar, FUN = function(x) {
  tmpList = yList[grep(x, yList)]
  tmpYears = gsub("\\D", "", tmpList) #extract years
  tmpRast = rast(tmpList)
  names(tmpRast) = paste(x, tmpYears, sep = "_") #ensure rasters have the correct variable and year combo
  return(tmpRast)
})
names(yRasters) <- yVar

y_1Rasters <- lapply(y_1Var, FUN = function(x) {
  tmpList = y_1List[grep(x, y_1List)]
  tmpYears = (gsub("\\D", "", tmpList) |>
          as.numeric()) + 1 #add 1 to years because these are for y-1
  tmpRast = rast(tmpList)
  names(tmpRast) = paste(x, tmpYears, sep = "_")
  return(tmpRast)
})
names(y_1Rasters) <- y_1Var

#reproject, crop, and mask to match cgamp template and save
saveDir <- "gis/Covariate_rasters/Climate/Annuals"
yRast_cgamp <- lapply(yVar, FUN = function(x) {
  tmpRaster = project(yRasters[[x]][[1]], cgamp) #|>
    # crop(y = cgamp) |>
    # mask(mask = cgamp)
  #writeRaster(tmpRaster, filename = file.path(saveDir, paste0(x, ".tif")))
  #return(rast(file.path(saveDir, paste0(x, ".tif"))))
    return(tmpRaster)
})

y_1Rast_cgamp <- lapply(y_1Var, FUN = function(x) {
  tmpRaster = project(y_1Rasters[[x]], cgamp) |>
    crop(y = cgamp) |>
    mask(mask = cgamp)
  writeRaster(tmpRaster, filename = file.path(saveDir, paste0(x, ".tif")))
  return(rast(file.path(saveDir, paste0(x, ".tif"))))
})




rasterList <- lapply(years, FUN = function(y) {
  return(list.files(file.path(outDir, demRasterName, paste0("Year_", y)), full.names = T))
})
years <- as.character(years)
names(rasterList) <- years

#initially import as a list of lists of lists, with outer list grouping rasters together by year and inner list 
#grouping variables together by y and y-1
annualRasters <- lapply(years, FUN = function(x) {
  tempFiles = rasterList[x]
  tempRasters = lapply(tempFiles, FUN = rast)
  names(tempRasters) = file_path_sans_ext(basename(tempFiles)) #assign raster names to elements of the list
  yRasters = tempRasters[names(tempRasters) %in% c("PPT_sp", "PPT_wt", "Tave_sp")] #year of covariates
  y_1Rasters = tempRasters[names(tempRasters) %in% c("MSP", "Tave_sm")] #y-1 covariates
  output = list(yRasters, y_1Rasters)
  names(output) = c("y", "y_1")
  return(output)
})


annualRasters <- lapply(rasterList, FUN = function(x) {
  tempRasters = lapply(x, FUN = rast)
  names(tempRasters) = file_path_sans_ext(basename(x)) #assign raster names to elements of the list
  yRasters = tempRasters[names(tempRasters) %in% c("PPT_sp", "PPT_wt", "Tave_sp")] #year of covariates
  y_1Rasters = tempRasters[names(tempRasters) %in% c("MSP", "Tave_sm")] #y-1 covariates
  output = list(yRasters, y_1Rasters)
  names(output) = c("y", "y_1")
  return(output)
})
names(annualRasters) <- years

yRasters <- lapply(annualRasters, FUN = function(x) {
  
})

test <- purrr::transpose(annualRasters)

#separate variables based on those associated with year of survey (y) and those with year prior to survey (y-1)
#y surveys = PPT_sp, PPT_wt, Tave_sp



#create a function to download ready made rasters directly from ClimateNA
download_climatena <- function(years, variables, out_dir) {
  dir.create(out_dir, showWarnings = FALSE)
  base_url <- "https://www.cacpd.org/CMIP6v73/annual"
  for (yr in years) {
    for (var in variables) {
      url <- paste0(base_url, "/", yr, "/", var, ".asc.gz")
      dest <- file.path(out_dir, paste0(var, "_", yr, ".asc.gz"))
      message("Downloading: ", url)
      try(
        download.file(url, dest, mode = "wb"),
        silent = TRUE
      )
    }
  }
}

#define years and variables for above function

download_climatena(years = years, variables = variables, out_dir = "gis/Covariate_rasters/temp")







#download desired variables for years of interest.
annuals <- downscale(
  xyz = dem_wgs,
  obs_years = 1999:2023,
  gcm_hist_years = 1999:2023,
  gcms = list_gcms(),
  ensemble_mean = T,
  vars = c("PPT_sp", "PPT_wt", "MSP", "Tave_sp", "Tave_sm")
  )

#Downscale observed annual data for your survey years. climr needs a point location table to extract historical values
#it doesn't seem to work with a raster, so need to convert DEM layer to points
pts <- as.data.frame(dem_wgs, na.rm = FALSE, xy = TRUE) %>%
  mutate(id = row_number()) %>%
  select(id, lon = x, lat = y, elev = file5c1461fc4a36)

#define years and variables
obs_years <- 1999:2023
vars <- c("PPT_sp", "PPT_wt", "MSP", "Tave_sp", "Tave_sm")

#extract data
annual_obs <- downscale(
  xyz = pts,
  obs_years = obs_years,
  vars = vars,
)

#Convert point data back into rasters
raster_list <- list()
for (v in vars) {
  for (y in obs_years) {
    subset_vals <- annual_obs %>%
      filter(variable == v, year == y) %>%
      pull(value)
    
    r <- dem_wgs #raster template to populate
    r[] <- subset_vals
    
    # Name the layer
    names(r) <- paste0(v, "_", y)
    
    # Store in list
    raster_list[[paste0(v, "_", y)]] <- r
  }
}

# 4️⃣ Combine all layers into a single SpatRaster stack
annual_stack <- rast(raster_list)

#reproject, crop, and mask to cgamp template and export.


ext(yRasters[[1]])
ext(project(yRasters[[1]], crs(cgamp)))  # transformed footprint
ext(cgamp)

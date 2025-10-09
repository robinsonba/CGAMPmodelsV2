# ---
# title: CGAMP V2.0 - download AAFC semi-decadal land use rasters to be uploaded to GEE
# author: Barry Robinson
# created: Oct 7, 2025
# ---

#NOTES#############################################################################################
#This script downloads and unzips AAFC's semi-decadal land use rasters, and mosaics them into a 
#single image for each time period. 
###################################################################################################
#load packages
library(rvest)
library(dplyr)
library(purrr)
library(terra)
library(parallel)

#define year range
years <- as.character(seq(2000, 2020, 5))
dest_path <- file.path("gis/Covariate_rasters/temp","extracted")
dir.create(dest_path, showWarnings = F)

####Only run lines 25-47 if files haven't been downloaded and unzipped##################################
#define URL to download from
# url <- "https://agriculture.canada.ca/atlas/data_donnees/landuse/data_donnees/tif/"
# #define list of utm zones wanted
# utm <- seq(11,15,1)
# utm <- sprintf("%02d", utm) #add leading zeros, if needed
# utm <- paste0("u",utm) |> paste(collapse = "|")
# 
# 
# #lapply through each year and download all files from UTM zones 10-16 
# lapply(years, function(i) {
#   webpage = read_html(paste0(url,i,"/"))
#   links = webpage %>%
#     html_elements("a") %>%
#     html_attr("href")
#   wanted = links[grepl(utm,links)]
#   GetMe = paste0(url, i, "/", wanted)
#   dest_path = file.path("gis/Covariate_rasters/temp", wanted) 
#   lapply(seq_along(GetMe), 
#          function(x) {download.file(GetMe[x], dest_path[x], mode = "wb")})
# })
# 
#unzip all files
# archive_paths <- list.files("gis/Covariate_rasters/temp", pattern = "tar.gz", full.names = T)
# lapply(seq_along(archive_paths), function(x) {untar(tarfile = archive_paths[x], exdir = dest_path, extras = "--strip-components=1")})
# unlink(archive_paths)
#delete all files that aren't .tif
# deleteList = unlist(lapply(years, function(x) {list.files(dest_path, pattern = x, full.names = T) %>%
#   keep(grepl(".tfw", .) | nchar(.) > 54)}))
# unlink(deleteList)
#######################################################################################################

#create list of years, each with a list of rasters associated with each year
# rasterfiles <- lapply(years, FUN = function(x) {
#   tmpList = list.files(dest_path, pattern = x, full.names = T, recursive = T) %>%
#     keep(grepl(".tif", .)) %>% keep(nchar(.) == 68) #keep only the single .tif file for each
#   return(lapply(tmpList, rast))
# })

#reproject each raster to match cgamp template raster and mosaic together
#ensure native resolution of 30m is maintained
cgamp <-rast("gis/CGAMPV2_boundaries/CGAMPV2_rasterTemplate.tif")
cgampCRS <- crs(cgamp)

#define a function that can be run in parallel to:
# 1. load all rasters associated with a given year (yyyy)
# 2. reproject to cgamp template
reprojRast <- function(yyyy) {
  #1. load rasters for a given year
  tmpList = list.files(dest_path, pattern = yyyy, full.names = T)
  nativeRast = lapply(tmpList, rast)
  #2. reproject and save. Running as a loop to save memory
  for (i in 1:length(nativeRast)) {
    tmprast = project(x = nativeRast[[i]], y = cgampCRS, res = res(nativeRast[[i]]))
    filename = paste0("gis/Covariate_rasters/Landcover/",names(nativeRast[[i]]),"_",i,".tif")
    writeRaster(tmprast, filename = filename)
    rm(tmprast)
    gc()
  }
}

#implement function in parallel
cl = makeCluster(length(years))
clusterEvalQ(cl, {library(terra)})
clusterExport(cl,c("cgampCRS", "dest_path"))
reprojList = parLapply(cl=cl, X=years, fun=reprojRast)
stopCluster(cl)
gc()

#load reprojected rasters, keeping each year as an element in a list
reprojLU <- lapply(years, function(x) {
  return(sprc(list.files("gis/Covariate_rasters/Landcover/", pattern = x, full.names = T)))
})

#mosaic together
mosaicRast <- lapply(reprojLU, function(x) {
  tmpmerge = terra::merge(x)
  filename = paste0("gis/Covariate_rasters/Landcover/",substr(names(x)[1],1,6),".tif")
  writeRaster(tmpmerge, filename)
  rm(tmpmerge)
  gc()
  return(rast(filename))
  })

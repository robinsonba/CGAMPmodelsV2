#This script summarizes the SPEI drought variables within Grasslands National Park across 2 time periods
#2007-2015 and 2017-2024


library(sf)
library(terra)
library(tidyverse)
library(dplyr)
library(ggplot2)

#load drought rasters (y = year of survey, y-1 = year prior)
#March-May y 
spring <- rast("gis/Covariate_rasters/SPEI/spei3Jun.tif")
#Apr-Aug y-1
sumY_1 <- rast("gis/Covariate_rasters/SPEI/spei5SepY_1.tif")
#12 month
oneY <- rast("gis/Covariate_rasters/SPEI/spei12May.tif")
#48 month
fourY <- rast("gis/Covariate_rasters/SPEI/spei48May.tif")

#put in a list
spei <- list(spring, sumY_1, oneY, fourY)


#load GNP boundary and reproject to SPEI projection
gnp <- st_read("gis/CLAB_SK_2023-09-08.shp") %>%
  filter(CLAB_ID == "GRAS") %>%
  st_transform(crs(spring))
plot(gnp)


#create separate stacks for each time period for each spei variable, clipped to GNP
historic <- lapply(spei, FUN = function(x) {
  tmp = crop(x, gnp)
  tmp = tmp[[as.character(2007:2015)]]
  tmp = tmp/1000
  mean = app(tmp, mean, na.rm = T)
  sd = app(tmp, sd, na.rm = T)
  return(c(mean, sd))
})

current <- lapply(spei, FUN = function(x) {
  tmp = crop(x, gnp)
  tmp = tmp[[as.character(2017:2023)]]
  tmp = tmp/1000
  mean = app(tmp, mean, na.rm = T)
  sd = app(tmp, sd, na.rm = T)
  gmean = global()
  return(c(mean, sd))
})

#Calculate global mean of all pixels the cover GNP for each year
speiAll <- lapply(spei, FUN = function(x) {
  tmp = crop(x, gnp)
  tmp =tmp/1000
  mu = global(tmp, fun = mean, na.rm = T)
  sig = global(tmp, fun = sd, na.rm = T)
  df = cbind(mu, sig)
  df$year = rownames(df)
  return(df)
})

#create a table summarizing all SPEI metrics over time
names(speiAll) <- c("spring", "sumY_1", "oneY", "fourY")

speiTable <- lapply(names(speiAll), function(x) {
  df = speiAll[[x]]
  df$metric = x
  return(df)
}) |>
  do.call(rbind, args = _)
write.csv(speiTable, "GNP_spei.csv", row.names = F)

#plot the results

ggplot(speiAll[["spring"]], aes(x = year, y = mean)) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = 0.3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Year",
    y = "Spring SPEI (March-May)",
  ) +
  theme_bw()

ggplot(speiAll[["sumY_1"]], aes(x = year, y = mean)) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = 0.3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Year",
    y = "Summer SPEI (April-August y-1)",
  ) +
  theme_bw()

ggplot(speiAll[["oneY"]], aes(x = year, y = mean)) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = 0.3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Year",
    y = "SPEI 12 months prior to May",
  ) +
  theme_bw()

ggplot(speiAll[["fourY"]], aes(x = year, y = mean)) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = 0.3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Year",
    y = "SPEI 48 months prior to May",
  ) +
  theme_bw()

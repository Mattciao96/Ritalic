library(httr)
library(jsonlite)
library(dplyr)
library(sp)
library(rgdal)
library(raster)


getLicPoints <- function(sp_name){
  res <- GET('https://italic.units.it/?procedure=occurrencedata', query = list(sp = sp_name))
  data = fromJSON(rawToChar(res$content))
  data <- bind_rows(data, .id = "herbarium")
  return(data)
}

matchLicName <- function(sp_name){
  res <- GET('https://italic.units.it/?procedure=matchapi', query = list(sp = sp_name))
  data = fromJSON(rawToChar(res$content))
  data = data[2]
  data = data$match
  return(data)
}

dfToSpatialPoints <- function(getLicPointsOutput){
  points <- getLicPointsOutput
  points$lat <- suppressWarnings(as.numeric(points$lat))
  points$long <- suppressWarnings(as.numeric(points$long))
  points <- na.omit(points)
  coordinates(points)<-~long+lat
  crs(points) <- CRS("+proj=longlat +datum=WGS84")
  return(points)
}




#### Libraries ####

## Data manipulation
library(data.table)
library(Hmisc)
library(reshape2)
library(tidyverse)

## Mapping
library(leaflet)
library(gplots)
library(rgdal)
library(rmapshaper)
library(RColorBrewer)
library(sf)
library(USAboundaries)

## Census API (you should have an API key; assign it to "api_key" in a file "src/api_key.R")
library(censusapi)
source("src/api_key.R")

Sys.setenv(CENSUS_KEY = api_key)

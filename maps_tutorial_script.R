# Coding Club Maps Tutorial Script
# https://ourcodingclub.github.io/2016/12/11/maps_tutorial.html
# John Godlee
# ourcodingclub@gmail.com
# 2017_11_09

# Set working directory
setwd("~/Downloads/CC-6-Maps-master")

# Packages ----
library(readr)  # For reading in files
library(dplyr)  # For formatting and cleaning data
library(rgdal)  # For manipulating map data
library(raster)  # For clipping shapefile polygons
library(devtools)  # For installing packages from altenative sources, e.g. Github
devtools::install_github("dkahle/ggmap")
devtools::install_github("oswaldosantos/ggsn")
library(ggmap)  # For plotting map data, downloading map tiles from online sources
library(ggsn)  # For adding scalebars and north arrows.

# Import bird data ----

vulture <- read.csv("Gyps_rueppellii_GBIF.csv", sep="\t")
penguin <- read.csv("Spheniscus_dermersus_GBIF.csv", sep="\t")

# Clean up bird data
# Keep only the columns we need
vars <- c("gbifid", "scientificname", "locality", "decimallongitude",
					"decimallatitude", "coordinateuncertaintyinmeters")

vulture_trim <- vulture %>% dplyr::select(one_of(vars))
penguin_trim <- penguin %>% dplyr::select(one_of(vars))
# `one_of()` is part of `select()` and selects all columns specified in `vars`

# Combine the dataframes
pc_trim <- bind_rows(vulture_trim, penguin_trim)

# Check column names and content
str(pc_trim)

# Check that species names are consistent
unique(pc_trim$scientificname)
# Needs cleaning up

# Clean up "scientificname" to make names consistent
pc_trim$scientificname <- pc_trim$scientificname %>%
	recode("Gyps rueppellii (A. E. Brehm, 1852)" = "Gyps rueppellii",
				 "Gyps rueppellii subsp. erlangeri Salvadori, 1908" = "Gyps rueppellii",
				 "Gyps rueppelli rueppelli" = "Gyps rueppellii",
				 "Spheniscus demersus (Linnaeus, 1758)" = "Spheniscus demersus")

# Checking names
unique(pc_trim$scientificname)
# Done

# Preliminary plot of bird data
ggplot(pc_trim, aes(x = decimallongitude, y = decimallatitude, colour = scientificname)) +
	geom_point()

# Trim data to remove USA entries
pc_trim_us <- pc_trim %>% filter(decimallongitude > -50)

# Replot
ggplot(pc_trim_us, aes(x = decimallongitude, y = decimallatitude, colour = scientificname)) +
	geom_point()


# Plot using data from `maps` package ---- 
# Get map data
map_world <- borders("world", fill = "grey90", colour = "black")

# Make plot
ggplot() +
	map_world +  # Add world map
	geom_point(data = pc_trim_us,  # Add and plot species data
						 aes(x = decimallongitude, y = decimallatitude, colour = scientificname)) +
	coord_quickmap() +  # Define aspect ratio of the map, so it doesn't get stretched when resizing
	theme_classic() +  # Remove ugly grey background
	theme(legend.position = "top")  # Position the legend at the top of the plot

# Make a vector of country names
saf_countries <- c("South Africa", "Namibia", "Botswana", "Zimbabwe")

# Call the vector in `borders()`
map_saf <- borders("world", regions = saf_countries, fill = "grey90", colour = "black")

ggplot() +
	map_saf +  # Add map
	geom_point(data = pc_trim_us,  # Add and plot speices data
						 aes(x = decimallongitude, y = decimallatitude, colour = scientificname)) +
	coord_quickmap() +  # Define aspect ratio of the map, so it doesn't get stretched when resizing
	xlim(8, 35) +  # Set x axis limits, xlim(min, max)
	ylim(-35, -15) +  # Set y axis limits
	theme_classic() +  # Remove ugly grey background
	theme(legend.position = "top")  # Position the legend at the top of the plot

# Plotting with `ggmap`

# Make bounding box
bbox <- c(min(pc_trim_us$decimallongitude) - 2,
					min(pc_trim_us$decimallatitude) - 2,
					max(pc_trim_us$decimallongitude) + 2,
					max(pc_trim_us$decimallatitude) + 2
)

# Get map data
map_penguin <- get_map(location = bbox, source = "stamen", maptype = "toner-lite")

# Preliminary plot
ggmap(map_penguin)

# Combine with point data
ggmap(map_penguin) +
	geom_point(data = pc_trim_us,
						 aes(x = decimallongitude,
						 		y = decimallatitude,
						 		colour = scientificname),
						 alpha = 0.6,                     # `alpha=` sets the transparency of `geom_point()`, from 0 (transparent) to 1 (opaque)
						 size = 2) +                      # `size=` sets the diameter of `geom_point()`
	xlab(expression("Decimal Longitude ("*degree*")")) +  # Wrapping the label in `expression()` and using *degree* lets us add a degree symbol
	ylab(expression("Decimal Latitude ("*degree*")"))
```

# Plotting detailed zoomed data in ggmap ----
# Read in data
play_areas <- read.csv("play_areas.csv")

# Get map
edi_map <- get_map(location = "Edinburgh", zoom = 12, source = "google", maptype = "hybrid")

# Plot with data
ggmap(edi_map) +
	geom_point(data = play_areas, aes(x = long, y = lat), size = 4, colour = "#06BA00")

# Using shapefiles ----

# Read in data
brown_trout <- read.csv("Brown_Trout_GBIF_clip.csv")

# Preliminary plot
ggplot(brown_trout, mapping = aes(x = decimallongitude, y = decimallatitude)) + geom_point(alpha = 0.5)

# Define bounding box
bbox <- c(-40, 30, 40, 85)

# Get map tiles
Map_trout <- get_map(location = bbox, source = "google", maptype = "terrain", zoom = 3, color = "bw")

# Preliminary map
ggmap(Map_trout) +
	geom_point(colour = "blue", alpha = 0.5,
						 aes(x = decimallongitude, y = decimallatitude),
						 data = brown_trout) +
	theme_bw() +
	xlab("Longitude") +
	ylab("Latitude")

# Load shapefiles
shpData_FEOW <- readOGR(dsn = "FEOW-TNC", layer = "FEOWv1_TNC")

# Check CRS
proj4string(shpData_FEOW)

# Transform CRS
shpData_FEOW <- spTransform(shpData_FEOW, CRS("+proj=longlat +datum=WGS84"))

# Create bounding box for clipping
clip_box <- as(extent(min(brown_trout$decimallongitude) -15,
											max(brown_trout$decimallongitude) + 10,
											min(brown_trout$decimallatitude),
											max(brown_trout$decimallatitude)), "SpatialPolygons")

# Clip polygons
shpData_FEOW_clipped <- intersect(shpData_FEOW, clip_box)

# Plot 
plot(shpData_FEOW_clipped)

# Identify ID
str(shpData_FEOW_clipped@data)

# Fortify Spatialpolygons -> dataframe
shpData_FEOW_clipped_fort <- fortify(shpData_FEOW_clipped, region = "ECOREGION")  # this could take a while

# Make a plot
map_FEOW <- ggplot() +
	coord_map() +
	geom_map(data = shpData_FEOW_clipped_fort,
					 map = shpData_FEOW_clipped_fort,
					 aes(x = long, y = lat, map_id = id, group = group, fill = id),
					 color = "black", size = 0.5) +
	geom_point(colour = "red", alpha = 0.5, size = 0.5,
						 aes(x = decimallongitude, y = decimallatitude),
						 data = brown_trout) +
	theme_classic() +
	theme(legend.position="none") +
	xlab("Longitude") +
	ylab("Latitude")

# Add annotations
map_FEOW_annot <- map_FEOW +
	annotate("rect", xmin = 20 , xmax = 35, ymin = 55, ymax = 65, fill="red", alpha=0.5) +
	annotate("text", x = 27.5, y = 61, size = 10, label = "Restock Area")

map_FEOW_scale <- map_FEOW_annot +
	scalebar(location="topleft", data = shpData_FEOW_clipped_fort,
					 dd2km = TRUE, dist = 500, model='WGS84',
					 height = 0.01)

north2(map_FEOW_scale, x = 0.2, y = 0.2, scale = 0.1, symbol = 1)


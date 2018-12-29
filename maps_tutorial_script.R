# Coding Club Maps Tutorial Script
# https://ourcodingclub.github.io/2016/12/11/maps_tutorial.html
# John Godlee
# ourcodingclub@gmail.com
# 2018_12_29

# Set working directory
setwd("Path_to_folder")

# Load packages
library(ggplot2)  # ggplot() fortify()
library(dplyr)  # %>% select() filter() bind_rows()
library(rgdal)  # readOGR() spTransform()
library(raster)  # intersect()
library(ggsn)  # north2() scalebar()
library(rworldmap)  # getMap()

# Load bird data
vulture <- read.csv("Gyps_rueppellii_GBIF.csv", sep = "\t")
penguin <- read.csv("Spheniscus_dermersus_GBIF.csv", sep = "\t")

# Keep only the columns we need
vars <- c("gbifid", "scientificname", "locality", "decimallongitude",
	"decimallatitude", "coordinateuncertaintyinmeters")

vulture_trim <- vulture %>% dplyr::select(one_of(vars))
penguin_trim <- penguin %>% dplyr::select(one_of(vars))
# `one_of()` is part of `dplyr` and selects all columns specified in `vars`

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

# Preliminary plot
ggplot(pc_trim, aes(x = decimallongitude, y = decimallatitude, 
	colour = scientificname)) +
	geom_point()

# Remove records in Americas
pc_trim_us <- pc_trim %>% filter(decimallongitude > -50)

# Plot again
ggplot(pc_trim_us, aes(x = decimallongitude, y = decimallatitude, 
	colour = scientificname)) +
	geom_point()

# Get world map
world <- getMap(resolution = "low")

# Plot data with map
ggplot() +
	geom_polygon(data = world, 
		aes(x = long, y = lat, group = group),
		fill = NA, colour = "black") + 
	geom_point(data = pc_trim_us,  # Add and plot species data
		aes(x = decimallongitude, y = decimallatitude, 
			colour = scientificname)) +
	coord_quickmap() +  # Prevents stretching when resizing
	theme_classic() +  # Remove ugly grey background
	xlab("Longitude") +
	ylab("Latitude") + 
	guides(colour=guide_legend(title="Species"))

# Make a vector of country names
saf_countries <- c("South Africa", "Namibia", "Botswana", "Zimbabwe")

# Call the vector in `borders()`
world_saf <- world[world@data$ADMIN %in% saf_countries, ]

ggplot() +
	geom_polygon(data = world_saf, 
		aes(x = long, y = lat, group = group),
		fill = NA, colour = "black") + 
	geom_point(data = pc_trim_us,  # Add and plot speices data
		aes(x = decimallongitude, y = decimallatitude, 
			colour = scientificname)) +
	coord_quickmap() + 
	xlim(8, 35) +  # Set x axis limits, xlim(min, max)
	ylim(-35, -15) +  # Set y axis limits
	theme_classic() +  # Remove ugly grey background
	xlab("Longitude") +
	ylab("Latitude") + 
	guides(colour=guide_legend(title="Species"))

# Read in trout data
brown_trout <- read.csv("Brown_Trout_GBIF_clip.csv")

# Preliminary plot
ggplot(brown_trout, mapping = aes(x = decimallongitude, y = decimallatitude)) +
	geom_point(alpha = 0.5)

# Get map data
clipper_europe <- as(extent(-10, 32, 30, 72), "SpatialPolygons")
proj4string(clipper_europe) <- CRS(proj4string(world))
world_clip <- raster::intersect(world, clipper_europe)
world_clip_f <- fortify(world_clip)

# Plot map
ggplot() + 
	geom_polygon(data = world_clip_f, 
		aes(x = long, y = lat, group = group),
		fill = NA, colour = "black") + 
	geom_point(colour = "blue", alpha = 0.5,
		aes(x = decimallongitude, y = decimallatitude),
		data = brown_trout) +
	theme_bw() +
	xlab("Longitude") +
	ylab("Latitude") + 
	coord_quickmap()

# Read shapefile
shpdata_FEOW <- readOGR(dsn = "FEOW-TNC", layer = "FEOWv1_TNC")

# Check CRS
proj4string(shpdata_FEOW)

# Transform CRS
shpdata_FEOW <- spTransform(shpdata_FEOW, CRS("+proj=longlat +datum=WGS84"))

# Clip spatial polygons
shpdata_FEOW_clip <- raster::intersect(shpdata_FEOW, clipper_europe)

# Test plot 
plot(shpdata_FEOW_clip)
str(shpdata_FEOW_clip)

# Fortify for ggplot2
shpdata_FEOW_clip_f <- fortify(shpdata_FEOW_clip, region = "ECOREGION")

# ggplot of ecoregions
map_FEOW <- ggplot() +
	geom_polygon(data = shpdata_FEOW_clip_f,
		aes(x = long, y = lat, group = group, fill = id),
		color = "black", size = 0.5) +
	geom_point(colour = "red", alpha = 0.5, size = 0.5,
		aes(x = decimallongitude, y = decimallatitude),
		data = brown_trout) +
	theme_classic() +
	theme(legend.position="bottom") +
	theme(legend.title=element_blank()) + 
	xlab("Longitude") +
	ylab("Latitude") + 
	coord_quickmap()

# Add annotations
map_FEOW_annot <- map_FEOW +
	annotate("rect", xmin = 20 , xmax = 35, ymin = 55, ymax = 65, fill="red", alpha=0.5) +
	annotate("text", x = 27.5, y = 61, size = 10, label = "Restock Area")

# Add scalebar
map_FEOW_scale <- map_FEOW_annot +
	scalebar(location="topleft", data = shpdata_FEOW_clip_f,
		dd2km = TRUE, dist = 250, model='WGS84',
		height = 0.01)

# Add north arrow
map_FEOW_scale 
north2(map_FEOW_scale, x = 0.2, y = 0.2, scale = 0.1, symbol = 1)

# See how many points are in which polygons

point_match <- over(
	SpatialPoints(
		coords = data.frame(brown_trout$decimallongitude, brown_trout$decimallatitude),
		proj4string = CRS(proj4string(shpdata_FEOW_clip))
		), 
	shpdata_FEOW_clip)

point_match %>%
	group_by(ECOREGION) %>%
	tally() %>%
	arrange(desc(n))



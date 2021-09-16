#### draw map ####

## credits
## much of the code here was graciously provided by Markus Min (mmin@uw.edu)

# library(tidyverse)
# library(readxl)
library(here)
library(rgdal)
library(broom)
library(ggplot2)
# library(ggpubr)
# library(sf)

## figure save directory
fig_dir <- here("figures")

## ----Base map of Puget sound--------------------------------------------------

# Load data
usa_spdf <- readOGR(dsn = here("analysis", "map_files", "USA_adm0.shp"))

# Convert to df(ish)
usa_spdf_fort <- tidy(usa_spdf)

# Load BC data
BC_shp <- readOGR(dsn = here("map_files", "canada", "lpr_000b16a_e.shp"))
# proj4string(BC_shp)
BC_shp_transform <- spTransform(BC_shp, "+init=epsg:4326")
BC_spdf_fort <- tidy(BC_shp_transform)


# Create map in ggplot

PS_map <- ggplot(usa_spdf_fort, aes(x = long, y = lat, group = group))+
  # Create grid using geom_vline and geom_hline
  # geom_vline(xintercept = seq(-123.1, min_lon, 1/nm), color = "gray50", size = 0.1)+
  # geom_hline(yintercept = seq(min_lat, 48.5, 1/60), color = "gray50", size = 0.1)+
  #base map
  geom_polygon(color = "gray20")+
  ylab("Latitude")+
  coord_fixed(ylim = c(47.1,48.25),  xlim = c(-123.1,-122.1), ratio = 1.3)+
  theme(plot.background = element_rect(fill = "white"))

greater_PS_map <- ggplot(usa_spdf_fort, aes(x = long, y = lat, group = group))+
  # Create grid using geom_vline and geom_hline
  # geom_vline(xintercept = seq(-123.1, min_lon, 1/nm), color = "gray50", size = 0.1)+
  # geom_hline(yintercept = seq(min_lat, 48.5, 1/60), color = "gray50", size = 0.1)+
  #base map
  geom_polygon(color = "gray20", fill = "gray20")+
  geom_polygon(data = BC_spdf_fort, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  ylab("Latitude")+
  coord_fixed(ylim = c(47.1,48.8),  xlim = c(-124.7,-122.1), ratio = 1.3)+
  theme(plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# Load data
usa_spdf <- readOGR(dsn = here("map_files", "USA_adm0.shp"))
# Convert to df(ish)
usa_spdf_fort <- tidy(usa_spdf)
# Load BC data
BC_shp <- readOGR(dsn = here("map_files", "canada", "lpr_000b16a_e.shp"))
# proj4string(BC_shp)
BC_shp_transform <- spTransform(BC_shp, "+init=epsg:4326")
BC_spdf_fort <- tidy(BC_shp_transform)
# Create map in ggplot
greater_PS_map <- ggplot(usa_spdf_fort, aes(x = long, y = lat, group = group))+
  geom_polygon(color = "gray70", fill = rgb(251, 234, 194, max=255))+
  geom_polygon(data = BC_spdf_fort, aes(x = long, y = lat, group = group), inherit.aes = FALSE, color = "gray70", fill = rgb(251, 234, 194, max=255)) +
  ylab("Latitude")+
  xlab("Longitude")+
  coord_fixed(ylim = c(47.1,48.9),  xlim = c(-124.8,-122.1), ratio = 1.3)+
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill="white", color = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = c(-124.5, -124, -123.5, -123, -122.5), expand = c(0,0), labels=c(expression(paste(124.5*degree,"W")),
                                                                                               expression(paste(124*degree,"W")),
                                                                                               expression(paste(123.5*degree,"W")),
                                                                                               expression(paste(123*degree,"W")),
                                                                                               expression(paste(122.5*degree,"W"))))+
  scale_y_continuous(breaks = seq(47.5, 48.5, 0.5), expand = c(0,0), labels=c(expression(paste(47.5*degree,"N")),
                                                                              expression(paste(48*degree,"N")),
                                                                              expression(paste(48.5*degree,"N"))))+
  # Add line for DPS boundary - from 48.121536, -123.288602 (Green Point) to 48.411782, -123.294625 (McMicking Point in Victoria)
  annotate("segment", x = -123.288602, xend = -123.294625, y = 48.121536, yend = 48.411782, lty = 2, color = "firebrick4")+
  annotate("text", label = "DPS Boundary", x = -123.288602, 48.07, size = 4, color = "firebrick4")


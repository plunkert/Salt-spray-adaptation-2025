# Create map of accessions used in leaf surface study

require(tidyr)
require(readxl)
require(ggmap)
require(mapproj)
require(mapdata)
require(maptools)
require(sp)
require(raster)
require(rgdal)
require(dismo)
require(maps)

# Read in Mimulus collection data and pull coordinates for relevant accessions

setwd("~/Documents/GitHub/Leaf-surface-traits-2024/") # Change this
all_pops <- read_excel("./Data/Mimulus_Collections_8_05_24.xlsx", sheet = "CollectionCompilation_080524")

# List accessions used
inland_pops <- c("TOR", "RGR", "LMC", "SWC", "OAE")
coastal_pops <- c("HEC", "OPB", "SWB", "PGR", "BHE")

dat <- all_pops[which(all_pops$Species=="M. guttatus" & (all_pops$`Population Code` %in% coastal_pops | all_pops$`Population Code` %in% inland_pops)),]

# When two collection entries exist, choose the one by D. Lowry
dat <- dat[which(dat$Collector == "D. Lowry" | dat$Collector == "D. Lowry/K. Wright"),]

# Remove unnecessary columns
dat <- subset(dat, select = c(`Population Code`, Latitude, Longitude))
colnames(dat) <- c("pop_code", "Latitude", "Longitude")
dat$ecotype <- case_when(dat$pop_code %in% coastal_pops ~ "coastal",
                         dat$pop_code %in% inland_pops ~ "inland")


range(dat$Latitude)
range(dat$Longitude)

base = get_map(location=c(-125,33,-115,47), zoom=8, maptype="alidade_smooth")
base = get_map(location=c(-125,33,-115,47), zoom=8, maptype="outdoors")
base = get_map(location=c(-125,33,-115,47), zoom=8, 
               maptype="stamen_terrain_background", color="bw")

us_states <- map_data("state")
ca_or_nv_wa <- us_states[which(us_states$region %in% c("california","oregon", "nevada", "washington")),]

base <- ggplot(data = ca_or_nv_wa,
       mapping = aes(x = long, y = lat,
                     group = group))+
  geom_polygon(colour="gray", fill = "white") +
  guides(fill = FALSE)+
  coord_map(projection = "mercator")+
  #coord_fixed()+
  theme(axis.title=element_blank(),
        panel.background=element_rect(fill = "lightsteelblue2", colour = "black"),
        panel.border=element_blank(),
        panel.grid=element_line(colour = "lightsteelblue2"))

points_map <- base + geom_point(data=dat, aes(x=dat$Longitude, y=dat$Latitude, fill = dat$ecotype), inherit.aes = FALSE, cex=3, pch=21)+
  scale_fill_manual(values = c('#514663', '#cacf85'))

ggsave(
  filename = 'Leaf_surface_accession_map.png', 
  plot = points_map,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  bg = 'white'
)


  
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
require(ggrepel)

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

dat$adjust <- case_when(dat$ecotype=="coastal" ~ -1.5,
                        dat$ecotype=="inland" ~ 1.5)

points_map <- base + geom_point(data=dat, aes(x=Longitude, y=Latitude, fill = ecotype, shape = ecotype), inherit.aes = FALSE, cex=5)+
  scale_fill_manual(values = c('#514663', '#cacf85'), name=NULL)+
  scale_shape_manual(values = c(21,24), labels=c("Coastal", "Inland"), name=NULL) # define shape/color scales

points_map <- base +
  geom_point(data=dat, aes(x=Longitude, y=Latitude, fill = ecotype, shape = ecotype), inherit.aes = FALSE, cex=5)+
  geom_text_repel(data= dat,aes(x=Longitude, y=Latitude, label=pop_code), nudge_x=dat$adjust, fontface = "bold", segment.color = 'transparent', size=4.5, inherit.aes = FALSE) +
  scale_fill_manual(values = c('#514663', '#cacf85'), name=NULL)+
  scale_shape_manual(values = c(21,24), labels=c("Coastal", "Inland"), name=NULL) # define shape/color scales


ggsave(
  filename = 'Leaf_surface_accession_map.png', 
  plot = points_map,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  bg = 'white'
)


  
# Create map of accessions used in leaf surface study. Indicate ecotype using colors
# and latitudinal pairs using like shapes for the two accessions in each pair.

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
dat <- read_excel("./Results/Tables/Table S1 leaf_surface_supp_table_accession_list.xlsx")

# List accessions used
inland_pops <- c("TOR", "RGR", "LMC", "SWC", "OAE")
coastal_pops <- c("HEC", "OPB", "SWB", "PGR", "BHE")

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

dat$adjust <- case_when(dat$Ecotype=="coastal" ~ -1.2,
                        dat$Ecotype=="inland" ~ 1.2)

shapes <- as.integer(c(21, 22, 25, 21, 23, 24, 23, 25, 22, 24))

points_map <- base +
  geom_point(data=dat, aes(x=Longitude, y=Latitude, fill = Ecotype), color = "black", shape = shapes, inherit.aes = FALSE, cex=5)+
  geom_text_repel(data= dat,aes(x=Longitude, y=Latitude, label=Population_Code), nudge_x=dat$adjust, fontface = "bold", segment.color = 'transparent', size=4.5, inherit.aes = FALSE) +
  scale_fill_manual(values = c('#514663', '#cacf85'), labels=c("Coastal", "Inland"), name=NULL)+
  scale_color_manual("black", "black", name=NULL)

ggsave(
  filename = 'Leaf_surface_accession_map.svg', 
  plot = points_map,
  device = 'svg',
  path = './Results/Figures/SVGs_for_MS/',
  scale = 1,
  bg = 'white'
)


  
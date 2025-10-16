# Test whether LMC and SWB secrete salt from the leaf surface differentially

# load required packages
require(readxl)
require(tidyr)
require(dplyr)
require(ggplot2)
# read in salt secretion conductivity and leaf area data
setwd("~/Documents/GitHub/Leaf-surface-traits-2024/")

secretion <- read_xlsx(
  path = './Data/secretion_test_LMC_SWB.xlsx', # Change path here
  sheet = 'Secretion_test') %>% mutate(geno = as.factor(geno), shriveled = shriveled=="Y")

# Make separate treatment and replicate columns
secretion$treatment <- substr(secretion$trt_rep, 0, 1)
secretion$rep <- substr(secretion$trt_rep, 2, 1000000L)

# add columns for uS/cm^2 and TDS/cm^2. uS and TDS are not 1:1 with NaCl but hopefully an okay proxy
secretion$uS_per_area <- secretion$conductivity/secretion$leaf_area_cm
secretion$tds_per_area <- secretion$tds/secretion$leaf_area_cm

boxplot(data=secretion[secretion$treatment != "m",], secretion$uS_per_area ~ secretion$treatment)

secretion$geno_trt <- paste(secretion$geno, secretion$treatment, sep="_")

secretion %>%
  filter(treatment != "m") %>%
  ggplot() +
  aes(x = geno_trt, fill = geno_trt, y = uS_per_area) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Accession') +
  scale_y_continuous(
    name = 'Rinsate Conductivity over Leaf Area (uS/cm^2)', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85', '#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
tds_per_area
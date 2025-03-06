# MLP 2/17/2025. This script wrangles data, fits models, makes plots for leaf surface traits that
# require leaf area in order to be interpretable. Calculates succulence and 
# determines result of leaf dunk assay.

require(tidyverse)
require(readxl)
require(ggpubr)
require(readxl)


# Read leaf area data into R
area <- read_excel("./Data/leaf_area_data.xlsx", sheet="Sheet1")
area$pop_code <- as.factor(area$pop_code)
table(area$pop_code)

# Read other leaf surface data (leaf mass, dunk assay, etc) into R
dat <- read_excel("./Data/leaf_surface_data.xlsx", sheet="Sheet1")
colnames(dat)[1] <- "pop_code"
# Merge with area (effectively, add area column to larger dataframe)
dat_area <- merge(area, dat, by = c("pop_code", "rep"))

# Add salt exposure, ecotype
dat_area <- dat_area %>% 
  mutate(salt_exposure = case_when(
    pop_code %in% c('PGR', 'BHE') ~ "protected",
    pop_code %in% c('SWB', 'OPB', 'HEC') ~ "exposed",
    pop_code %in% c('SWC', 'LMC', 'TOR', 'OAE', 'RGR') ~ "inland")
  )

# Add ecotype column
dat_area <- dat_area %>% mutate(ecotype = as.factor(if_else(salt_exposure=="inland", "inland", "coastal")),
                              salt_exposure = as.factor(salt_exposure),
                              pop_code=as.factor(pop_code))


# Calculate succulence as grams of water per cm^2 leaf area
dat_area$succulence <- (dat_area$fresh.mass - dat_area$dry.mass)/dat_area$leaf_area_cm2

# Calculate result of dunk assay as grams of water that hung onto leaf surface / leaf area
dat_area$dunk_result <- (dat_area$dunked.mass - dat_area$fresh.mass)/(2*dat_area$leaf_area_cm2)

# Dunk assay felt error-prone because of the way you hold the leaf. Does accession explain
# variation in dunk result?
summary(lm(data=dat_area, dunk_result ~ pop_code)) # wow, very much, r^2=0.254

# plot succulence as function of ecotype
dat_area %>% ggplot() +
  aes(x = ecotype, fill = ecotype, y = succulence) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Succulence (g H2O / cm^2)', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

# plot dunk result as function of ecotype
dunk_assay_ecotype <- dat_area %>% ggplot() +
  aes(x = ecotype, fill = ecotype, y = dunk_result) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Dunk Assay Result (g H2O / cm^2)', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

ggsave(
  filename = 'Dunk_Assay_Ecotype.png', 
  plot = dunk_assay_ecotype,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 3,
  height = 6,
  bg = 'white'
)


summary(lm(data=dat_area, succulence ~ ecotype))

summary(lm(data=dat_area, dunk_result ~ ecotype))

# Fit nested anovas for dunk assay and succulence
m_nest <- aov(data=dat_area, dunk_result ~ ecotype/pop_code)
summary(m_nest)
coefficients(m_nest)

m_nest <- aov(data=dat_area, succulence ~ ecotype/pop_code)
summary(m_nest)
coefficients(m_nest)


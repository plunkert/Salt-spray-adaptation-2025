# Primary contributor: Cam
# Purpose: Preliminary analysis of stomata data to compare BHE and OAE populations

require(tidyverse)
require(readxl)

# Data wrangling
stomata =  read_csv('./Data/stomatal_density_data_06-13-24.csv') %>%
  # Remove missing data
  filter(!is.na(stomata_count)) %>% 
  # Separate file_name column
  separate(
    col = file_name, 
    sep = '_',
    into = c('pop.code', 'replicate', 'leaf_side', 'view_rep')
  ) %>%
  # Get average stomata count for each leaf side
  group_by(pop.code, replicate, leaf_side) %>%
  summarize(stomata_count = mean(.data[['stomata_count']], na.rm = TRUE)) %>%
  ungroup() %>%
  # Reshape data
  pivot_wider(
    id_cols = c(pop.code, replicate), 
    names_from = leaf_side,
    values_from = stomata_count,
    names_prefix = "stomata_count_",
  ) %>%
  # Create more stomata columns
  mutate(
    # Total amount of stomata on both sides
    stomata_count_total = stomata_count_ab + stomata_count_ad, 
    # Adaxial vs. abaxial ratio, 1 = equal stomata, >1 more stomata on abaxial side
    amphistomy = stomata_count_ad / stomata_count_ab 
  )
  
#View(stomata)

# Compare Abaxial vs. Adaxial Stomata Counts for each pop
oae_bhe_amphistomy = stomata %>%
  filter(pop.code %in% c('BHE', 'OAE')) %>%
  mutate(pop.code = if_else(pop.code == 'BHE', 'Inland (BHE)', 'Coastal (OAE)')) %>%
  ggplot() +
  aes(x = pop.code, fill = pop.code, y = stomata_count_ab / stomata_count_total * 100) +
  geom_boxplot(outliers = F) +
  geom_hline(yintercept = 50, linetype = 'longdash') +
  # Labels
  scale_x_discrete(name = 'Population') +
  scale_y_continuous(
    name = 'Percent of Stomata on Adaxial Surface', 
    limits = c(40, 60)
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
  )
oae_bhe_amphistomy

ggsave(
  filename = 'Prelim_Amphistomy.png', 
  plot = oae_bhe_amphistomy,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 3,
  height = 6,
  bg = 'white'
)



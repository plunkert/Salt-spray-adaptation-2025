require(tidyverse)
require(readxl)

coastal_pops = c('BHE', 'SWB', 'PGR', 'HEC', 'OPB')
inland_pops = c('SWC', 'LMC', 'TOR', 'OAE', 'RGR')


contact_angle =  read_xlsx(
  path = './Data/Contact Angle Measurements.xlsx', # Change path here
  sheet = 'Sheet1'
) %>%
  mutate(
    ecotype = case_when(
      pop.code %in% coastal_pops ~ "coastal",
      pop.code %in% inland_pops ~ "inland",
      .default = NA_character_
    ),
    salt_exposure = case_when(
      pop.code %in% c('PGR', 'BHE') ~ "protected",
      pop.code %in% c('SWB', 'OPB', 'HEC') ~ "exposed",
      pop.code %in% inland_pops ~ "inland",
      .default = NA_character_
    ),
    surface_type = case_when(
      pop.code %in% c('PGR', 'BHE') ~ 'gooey',
      pop.code == 'SWB' ~ 'middling',
      pop.code %in% c('OPB', 'HEC') ~ 'glaborous',
      pop.code %in% inland_pops ~ "inland",
      .default = NA_character_
    )
  )

View(contact_angle)

# View coastal_data
contact_angle %>%
  ggplot() +
  aes(x=contact_angle) +
  geom_histogram()

# Coastal vs. inland and ad vs. ab
contact_angle %>%
  filter(leaf_side == 'ad') %>%
  ggplot() +
  aes(x=contact_angle) +
  facet_wrap(vars(ecotype), nrow = 2) +
  geom_histogram()

contact_angle %>%
  filter(leaf_side == 'ab') %>%
  ggplot() +
  aes(x=contact_angle) +
  facet_wrap(vars(ecotype), nrow = 2) +
  geom_histogram()

# How much of each pop?
contact_angle %>%
  filter(!is.na(contact_angle)) %>%
  group_by(pop.code) %>%
  count()

# Comparing pairs
contact_angle %>%
  filter(pop.code == 'RGR' | pop.code == 'OPB') %>%
  filter(leaf_side == 'ad') %>%
  ggplot() +
  aes(x=contact_angle) +
  facet_wrap(vars(ecotype), nrow = 2) +
  geom_boxplot()

# Comparing protected and exposed
contact_angle %>%
  filter(salt_exposure != "inland") %>%
  ggplot() +
  aes(x=contact_angle) +
  facet_wrap(vars(salt_exposure), nrow = 2) +
  geom_boxplot()

contact_angle %>%
  filter(surface_type != "inland") %>%
  ggplot() +
  aes(x=contact_angle) +
  facet_wrap(vars(surface_type), nrow = 2) +
  geom_boxplot()

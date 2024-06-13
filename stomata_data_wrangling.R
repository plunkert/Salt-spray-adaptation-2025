require(tidyverse)
require(readxl)

stomata =  read_csv('./Data/stomatal_density_data_06-13-24.csv') %>%
  filter(!is.na(stomata_count)) %>%
  separate(col = file_name, sep = '_',
           into = c('pop.code', 'replicate', 'leaf_side', 'view_rep')) %>%
  group_by(pop.code, replicate, leaf_side) %>%
  summarize(stomata_count = mean(.data[['stomata_count']], na.rm = TRUE)) %>%
  ungroup()
View(stomata)

stomata %>%
  filter(pop.code %in% c('OAE', 'BHE')) %>%
  filter(leaf_side == 'ad') %>%
  ggplot() +
  aes(x=stomata_count) +
  facet_wrap(vars(pop.code), nrow = 2) +
  geom_boxplot()

stomata %>%
  filter(pop.code %in% c('OAE', 'BHE')) %>%
  ggplot() +
  aes(x=stomata_count) +
  facet_wrap(vars(pop.code, leaf_side), nrow = 2) +
  geom_boxplot()

stomata %>%
  filter(pop.code %in% c('OAE', 'BHE')) %>%
  ggplot() +
  aes(y=stomata_count, x=pop.code) +
  facet_wrap(vars(pop.code, leaf_side), nrow = 2) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9)

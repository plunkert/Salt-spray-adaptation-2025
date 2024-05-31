require(tidyverse)
require(stringr)
require(readxl)

leaf_surface_data = read_excel(path = './Data/leaf_surface_data.xlsx', sheet = 'Sheet1')

contact_angle_images = leaf_surface_data %>% 
  select(ad.file, ab.file, pop.code, rep) %>%
  pivot_longer(cols = c('ad.file', 'ab.file'), names_to = 'leaf_side', values_to = 'filename') %>%
  arrange(filename) %>%
  mutate(leaf_side = str_sub(leaf_side, start = 1L, end = 2))


write_csv(contact_angle_images, file = './Data/contact_angle_images.csv')

set.seed(1)
slice_sample(contact_angle_images, n = 10) %>% 
  arrange(filename)


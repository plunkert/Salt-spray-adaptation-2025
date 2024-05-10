require(tidyverse)
require(readxl)

leaf_surface_data = read_excel(path = './Data/leaf_surface_data.xlsx', sheet = 'Sheet1')

leaf_surface_data %>% 
  select(ad.file, ab.file, pop.code, rep) %>%
  pivot_longer(cols = c('ad.file', 'ab.file'), names_to = 'leaf_side', values_to = 'filename') %>%
  arrange(filename) %>%
  write_csv(file = './Data/contact_angle_images.csv')

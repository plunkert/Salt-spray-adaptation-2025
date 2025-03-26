# Primary contributor: Cam
# Purpose: Preliminary analysis of stomata data to compare BHE and OAE populations
# Madison update Dec. 2024 include all 10 populations
# Madison update Feb. 2025 incorporate stomatal size data
require(tidyverse)
require(readxl)
require(ggpubr)

# Data wrangling

stomata =  read_excel('./Data/stomatal density data.xlsx') %>%
  # Remove missing data
  filter(!is.na(stomata_count)) %>% 
  # Separate file_name column
  separate(
    col = file_name, 
    sep = '_',
    into = c('pop_code', 'replicate', 'leaf_side', 'view_rep')
  ) %>%
  # Get average stomata count for each leaf side
  group_by(pop_code, replicate, leaf_side) %>%
  summarize(stomata_count = mean(.data[['stomata_count']], na.rm = TRUE)) %>%
  ungroup() %>%
  # Reshape data
  pivot_wider(
    id_cols = c(pop_code, replicate), 
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
  
View(stomata)

table(as.factor(stomata$pop_code))

# Compare Abaxial vs. Adaxial Stomata Counts for each pop
oae_bhe_amphistomy = stomata %>%
  filter(pop_code %in% c('BHE', 'OAE')) %>%
  mutate(pop_code = if_else(pop_code == 'BHE', 'Coastal (BHE)', 'Inland (OAE)')) %>%
  ggplot() +
  aes(x = pop_code, fill = pop_code, y = stomata_count_ad / stomata_count_total * 100) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
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
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
oae_bhe_amphistomy

# Add salt exposure columns
stomata <- stomata %>% 
  mutate(salt_exposure = case_when(
    pop_code %in% c('PGR', 'BHE') ~ "protected",
    pop_code %in% c('SWB', 'OPB', 'HEC') ~ "exposed",
    pop_code %in% c('SWC', 'LMC', 'TOR', 'OAE', 'RGR') ~ "inland")
  )

# Add ecotype column
stomata <- stomata %>% mutate(ecotype = as.factor(if_else(salt_exposure=="inland", "inland", "coastal")),
                              salt_exposure = as.factor(salt_exposure),
                              pop_code=as.factor(pop_code))

# distribution of adaxial stomata
stomata %>%
  ggplot() +
  aes(x=stomata_count_ad) +
  facet_wrap(vars(ecotype), nrow = 2) +
  geom_histogram()

# plot adaxial stomata for ecotypes without regard for specific populations
stomata_ad_boxplot = stomata %>%
  filter(!is.na(stomata_count_ad)) %>%
  ggplot() +
  aes(x = ecotype, fill = ecotype, y = stomata_count_ad) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Stomatal Count on Adaxial Surface', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
stomata_ad_boxplot

t.test(data=stomata, stomata_count_ad ~ ecotype)


# adaxial stomata, but break coastals into protected and exposed
stomata_ad_exposure_boxplot = stomata %>%
  filter(!is.na(stomata_count_ad)) %>%
  ggplot() +
  aes(x = salt_exposure, fill = salt_exposure, y = stomata_count_ad) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Salt Exposure') +
  scale_y_continuous(
    name = 'Stomatal Count on Adaxial Surface', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85', "grey")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
stomata_ad_exposure_boxplot

# amphistomy ~ ecotype
amphistomy_boxplot = stomata %>%
  filter(!is.na(amphistomy)) %>%
  ggplot() +
  aes(x = ecotype, fill = ecotype, y = amphistomy) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Amphistomy (adaxial/abaxial)', 
  ) +
  geom_hline(yintercept=1, linetype="dotted")+
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
amphistomy_boxplot



# amphistomy, but break coastals into protected and exposed
stomata_amphistomy_exposure_boxplot = stomata %>%
  filter(!is.na(amphistomy)) %>%
  ggplot() +
  aes(x = salt_exposure, fill = salt_exposure, y = amphistomy) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Salt Exposure') +
  scale_y_continuous(
    name = 'Amphistomy', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85', "grey")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
stomata_amphistomy_exposure_boxplot

# plot abaxial stomata for ecotypes without regard for specific populations
stomata_ab_boxplot = stomata %>%
  filter(!is.na(stomata_count_ab)) %>%
  ggplot() +
  aes(x = ecotype, fill = ecotype, y = stomata_count_ab) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Stomatal Count on Abaxial Surface', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
stomata_ab_boxplot
t.test(data=stomata, stomata_count_ab ~ ecotype)

t.test(data=stomata, amphistomy ~ ecotype)


ggsave(
  filename = 'Amphistomy_Ecotype.png', 
  plot = amphistomy_boxplot,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 3,
  height = 6,
  bg = 'white'
)



# make a function that takes the dataframe with a population pair and a variable of interest
plot_each_pair <- function (pair, var, ylab) {
  pops=unique(pair$pop_code)
  coastal <- c("BHE","HEC", "OPB", "PGR", "SWB")
  codes <- unique(pair$pop_code)
  na.omit(pair) %>%
    ggplot() +
    aes(x = ecotype, fill = ecotype, y = pair[[var]]) +
    geom_boxplot(outliers = F) +
    geom_jitter(position=position_jitter(0.1)) +
    # Labels
    scale_x_discrete(name = 'Ecotype') +
    scale_y_continuous(
      name = ylab
    )+
    ggtitle(paste(as.character(codes[which(codes %in% coastal)]), " / ", as.character(codes[which(codes %in% inland)])))+
    
    ggtitle(paste(as.character(unique(pair$pop_code))[1], " / ", as.character(unique(pair$pop_code))[2]))+
    # Style
    scale_fill_manual(values = c('#514663', '#cacf85')) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_text(size=12),
      axis.title = element_text(size=12)
      )
}

# Read in stomatal size data
stom_size <- read.csv("./Data/stomata_lengths_leaf_surface_data.csv")
stom_size$file_name[stom_size$file_name == ""] <- NA 
stom_size <- stom_size %>% fill(file_name) %>%
  mutate(
    stomate_length_um = as.numeric(stomate_length_um)
    ) %>% na.omit() %>%
  separate(
    col = file_name,
    sep = '_',
    into = c('pop_code', 'replicate', 'leaf_side', 'view_rep')
  )

# temporarily throw out the 2cm stomate, obviously not right
stom_size <- stom_size[which(stom_size$stomate_length_um < 1000),]

hist(stom_size$stomate_length_um)

# indicate ecotype
# stom_size$ecotype <- as.factor(case_when(
#   stom_size$pop_code %in% c('SWB', 'OPB', 'HEC', 'PGR', 'BHE') ~ "coastal",
#   stom_size$pop_code %in% c('SWC', 'LMC', 'TOR', 'OAE', 'RGR') ~ "inland"))
                 
# get mean stomatal size for each peel
stom_size_means <- stom_size %>% 
  group_by(pop_code, replicate, view_rep, leaf_side) %>% 
  summarise(mean=mean(stomate_length_um)) %>%
  # get stomatal size for each side of the leaf on the same row
  pivot_wider(id_cols = c(pop_code, replicate),
              names_from=leaf_side,
              values_from=mean,
              names_prefix = "stomate_size_")

# merge mean stomatal size with stomatal count data
stom_all <- merge(stomata, stom_size_means, by = c("pop_code", "replicate"))

# plot adaxial stomatal length as function of ecotype
stom_all %>% ggplot() +
  aes(x = salt_exposure, fill = salt_exposure, y = stomate_size_ad) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Adaxial Stomata Length (um)', 
  ) +
  # Style
#  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )


# plot abaxial stomatal length as function of ecotype
ab_stom_length <- stom_all %>% ggplot() +
  aes(x = ecotype, fill = ecotype, y = stomate_size_ab) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Abaxial Stomata Length (um)', 
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
  filename = 'Ab_Stomata_Length_Ecotype.png', 
  plot = ab_stom_length,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 6,
  height = 6,
  bg = 'white'
)

t.test(data=stom_all, stomate_size_ad ~ ecotype)
t.test(data=stom_all, stomate_size_ab ~ ecotype)

# Estimate area of a stomate in mm^2 following Muir et al. 2023 Am Nat. 
# Approximate stomatal area as A = 0.5*L^2. Includes conversion um to mm.
stom_all$stomate_area_ad <- 0.5 * (stom_all$stomate_size_ad/1000)^2
stom_all$stomate_area_ab <- 0.5 * (stom_all$stomate_size_ab/1000)^2

# Estimate stomatal density in stomata/mm^2. Images are 2048 pixels x 2880 pixels and
# there are 4e-4 mm/pi. Field of view is 0.9437 mm^2.
stom_all$stom_density_ad <- stom_all$stomata_count_ad/0.94372
stom_all$stom_density_ab <- stom_all$stomata_count_ab/0.94372

# Estimate fraction of leaf epidermal area allocated to stomata following Muir et al. 2023 Am Nat.
stom_all$stom_ad_fraction <- stom_all$stom_density_ad * stom_all$stomate_area_ad
stom_all$stom_ab_fraction <- stom_all$stom_density_ab * stom_all$stomate_area_ab

# plot stomatal density as a function of ecotype
ad_stom_density_ecotype_boxplot <- stom_all %>% ggplot() +
  aes(x = ecotype, fill = ecotype, y = stom_density_ad) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Adaxial stomatal density (stomata/mm^2)', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

ab_stom_density_ecotype_boxplot <- stom_all %>% ggplot() +
  aes(x = ecotype, fill = ecotype, y = stom_density_ab) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Abaxial stomatal density (stomata/mm^2)', 
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
  filename = 'Ad_Stomata_Ecotype.png', 
  plot = ad_stom_density_ecotype_boxplot,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 3,
  height = 6,
  bg = 'white'
)

ggsave(
  filename = 'Ab_Stomata_Ecotype.png', 
  plot = ab_stom_density_ecotype_boxplot,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 3,
  height = 6,
  bg = 'white'
)

# plot fraction of epidermis dedicated to stomata as a function of ecotype
stom_all %>% ggplot() +
  aes(x = ecotype, fill = ecotype, y = stom_ad_fraction) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Fraction of Adaxial Epidermis Area Allocated to Stomata', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
stom_all %>% ggplot() +
  aes(x = ecotype, fill = ecotype, y = stom_ab_fraction) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Fraction of Abaxial Epidermis Area Allocated to Stomata', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

# Look at each population pair separately

# subset dataframe for each population pair
oae_bhe <- stom_all[which(stom_all$pop_code == "BHE" | stom_all$pop_code == "OAE"),]
tor_pgr <- stom_all[which(stom_all$pop_code == "PGR" | stom_all$pop_code == "TOR"),]
lmc_swb <- stom_all[which(stom_all$pop_code == "SWB" | stom_all$pop_code == "LMC"),]
rgr_opb <- stom_all[which(stom_all$pop_code == "OPB" | stom_all$pop_code == "RGR"),]
swc_hec <- stom_all[which(stom_all$pop_code == "HEC" | stom_all$pop_code == "SWC"),]
pairs <- list(swc_hec, rgr_opb, lmc_swb, oae_bhe, tor_pgr)
plots <- rep(NA, 5)
for (i in 1:length(pairs)){
  plot <- as.data.frame(pairs[i]) %>%
    filter(!is.na(stomata_count_ad)) %>%
    ggplot() +
    aes(x = ecotype, fill = ecotype, y = stomata_count_ad) +
    geom_boxplot(outliers = F) +
    geom_jitter(position=position_jitter(0.1)) +
    # Labels
    scale_x_discrete(name = 'Ecotype') +
    scale_y_continuous(
      name = 'Stomatal Count on Adaxial Surface', 
    ) +
    # ggtitle(as.character(unique(pairs[[i]]$pop_code)))+
    # Style
    scale_fill_manual(values = c('#514663', '#cacf85')) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_text(size=12),
      axis.title = element_text(size=14),
    )
  plot
  #plots[[i]] <- plot
}


plots <- lapply(pairs, plot_each_pair, "stom_density_ad", "Stomatal density (stomata/mm^2")
ad_pair_plots <- ggarrange(plotlist=plots, ncol=2, nrow=3)
ggsave(
  filename = 'Ad_Stomata_Pairs.png', 
  plot = ad_pair_plots,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 6,
  height = 6,
  bg = 'white'
)

plots <- lapply(pairs, plot_each_pair, "stomate_size_ab")
ad_pair_plots <- ggarrange(plotlist=plots, ncol=2, nrow=3)
ggsave(
  filename = 'Ab_Stomata_Length_Pairs.png', 
  plot = ad_pair_plots,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 6,
  height = 6,
  bg = 'white'
)

plots <- lapply(pairs, plot_each_pair, "stomate_size_ab")
ab_pair_plots <- ggarrange(plotlist=plots, ncol=2, nrow=3)
ggsave(
  filename = 'Ab_Stomata_Length_Pairs.png', 
  plot = ad_pair_plots,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 6,
  height = 6,
  bg = 'white'
)

# Fit nested ANOVAs to model effect of pop nested w/in ecotype on stomatal density, 
# stomatal size, and fraction of epidermis allocated to stomata.

# adaxial stomatal density
m_nest <- aov(data=stom_all, stom_density_ad ~ ecotype/pop_code)
summary(m_nest)
coefficients(m_nest)

# abaxial stomatal density
m_nest <- aov(data=stom_all, stom_density_ab ~ ecotype/pop_code)
summary(m_nest)
coefficients(m_nest)

# adaxial stomatal length
m_nest <- aov(data=stom_all, stomate_size_ad ~ ecotype/pop_code)
summary(m_nest)
coefficients(m_nest)

# amphistomy
m_nest <- aov(data=stom_all, amphistomy ~ ecotype/pop_code)
summary(m_nest)
coefficients(m_nest)

# abaxial stomatal length
m_nest <- aov(data=stom_all, stomate_size_ab ~ ecotype/pop_code)
summary(m_nest)
coefficients(m_nest)

# adaxial area allocated to stomata
m_nest <- aov(data=stom_all, stom_ad_fraction ~ ecotype/pop_code)
summary(m_nest)
coefficients(m_nest)

m_nest <- aov(data=stom_all, stom_ab_fraction ~ ecotype/pop_code)
summary(m_nest)
coefficients(m_nest)


# Primary contributor: Cam
# Purpose: Preliminary analysis of stomata data to compare BHE and OAE populations
# Madison update Dec. 2024 include all 10 populations
# Madison update Feb. 2025 incorporate stomatal size data
require(tidyverse)
require(readxl)
require(ggpubr)
require(emmeans)
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
  
table(as.factor(stomata$pop_code))

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

# add latitudinal pair column
stomata$pair <- case_when(stomata$pop_code == "OPB" | stomata$pop_code == "RGR" ~ "OPB/RGR",
                          stomata$pop_code == "HEC" | stomata$pop_code == "SWC" ~ "HEC/SWC",
                          stomata$pop_code == "BHE" | stomata$pop_code == "OAE" ~ "BHE/OAE",
                          stomata$pop_code == "SWB" | stomata$pop_code == "LMC" ~ "SWB/LMC",
                          stomata$pop_code == "PGR" | stomata$pop_code == "TOR" ~ "PGR/TOR",
                                 .default=NA_character_) %>% as.factor()

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
    axis.title = element_text(size=12),
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
    axis.title = element_text(size=12),
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
    axis.title = element_text(size=12),
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
    axis.title = element_text(size=12),
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
    axis.title = element_text(size=12),
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

# throw out the 2cm stomate, obviously not right
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
ad_stom_length <- stom_all %>% ggplot() +
  aes(x = ecotype, fill = ecotype, y = stomate_size_ad) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Stomate Length (um)', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=12),
  )


# plot abaxial stomatal length as function of ecotype
ab_stom_length <- stom_all %>% ggplot() +
  aes(x = ecotype, fill = ecotype, y = stomate_size_ab) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Stomate Length (um)', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=12),
  )

ggsave(
  filename = 'Ad_Stomata_Length_Ecotype.png', 
  plot = ad_stom_length,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 3,
  height = 6,
  bg = 'white'
)

ggsave(
  filename = 'Ab_Stomata_Length_Ecotype.png', 
  plot = ab_stom_length,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 3,
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
    name = 'Stomatal density (stomata/mm^2)', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=12),
  )

ab_stom_density_ecotype_boxplot <- stom_all %>% ggplot() +
  aes(x = ecotype, fill = ecotype, y = stom_density_ab) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Stomatal density (stomata/mm^2)', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=12),
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
ad_stom_fraction_ecotype <- stom_all %>% ggplot() +
  aes(x = ecotype, fill = ecotype, y = stom_ad_fraction) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Stomata Area / Epidermis Area', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=12),
  )
ab_stom_fraction_ecotype <- stom_all %>% ggplot() +
  aes(x = ecotype, fill = ecotype, y = stom_ab_fraction) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Stomata Area / Epidermis Area', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=12),
  )

ggsave(
  filename = 'Ad_Stomata_Area_FractionEcotype.png', 
  plot = ad_stom_fraction_ecotype,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 3,
  height = 6,
  bg = 'white'
)

ggsave(
  filename = 'Ab_Stomata_Area_FractionEcotype.png', 
  plot = ab_stom_fraction_ecotype,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 3,
  height = 6,
  bg = 'white'
)

# Look at each population pair separately

# subset dataframe for each population pair
oae_bhe <- stom_all[which(stom_all$pop_code == "BHE" | stom_all$pop_code == "OAE"),]
tor_pgr <- stom_all[which(stom_all$pop_code == "PGR" | stom_all$pop_code == "TOR"),]
lmc_swb <- stom_all[which(stom_all$pop_code == "SWB" | stom_all$pop_code == "LMC"),]
rgr_opb <- stom_all[which(stom_all$pop_code == "OPB" | stom_all$pop_code == "RGR"),]
swc_hec <- stom_all[which(stom_all$pop_code == "HEC" | stom_all$pop_code == "SWC"),]
pairs <- list(swc_hec, rgr_opb, lmc_swb, oae_bhe, tor_pgr)


# make a function that takes the dataframe with a population pair and a variable of interest
plot_each_pair <- function (pair, var, ylab) {
  pops=unique(pair$pop_code)
  coastal <- c("BHE","HEC", "OPB", "PGR", "SWB")
  inland <- c("OAE", "SWC", "RGR", "TOR", "LMC")
  codes <- unique(pair$pop_code)
  pair %>% ggplot() +
    aes(x = ecotype, fill = ecotype, y = !!sym(var)) +
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

plots <- lapply(pairs, plot_each_pair, "stomate_size_ab", "Abaxial stomate length (um)")
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

plots <- lapply(pairs, plot_each_pair, "stom_ad_fraction", "Fraction adaxial epidermis allocated to stomata")
ad_pair_plots <- ggarrange(plotlist=plots, ncol=2, nrow=3)
ggsave(
  filename = 'Ad_Stomata_Area_Fraction.png', 
  plot = ad_pair_plots,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 6,
  height = 6,
  bg = 'white'
)

plots <- lapply(pairs, plot_each_pair, "stom_ab_fraction", "Fraction abaxial epidermis allocated to stomata")
ab_pair_plots <- ggarrange(plotlist=plots, ncol=2, nrow=3)
ggsave(
  filename = 'Ab_Stomata_Area_Fraction.png', 
  plot = ab_pair_plots,
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
m_nest_ad_dens <- aov(data=stom_all, stom_density_ad ~ ecotype/pop_code)
summary(m_nest_ad_dens)
coefficients(m_nest_ad_dens)

# abaxial stomatal density
m_nest_ab_dens <- aov(data=stom_all, stom_density_ab ~ ecotype/pop_code)
summary(m_nest_ab_dens)
coefficients(m_nest_ab_dens)

# adaxial stomatal length
m_nest_ad_length <- aov(data=stom_all, stomate_size_ad ~ ecotype/pop_code)
summary(m_nest_ad_length)
coefficients(m_nest_ad_length)

# abaxial stomatal length
m_nest_ab_length <- aov(data=stom_all, stomate_size_ab ~ ecotype/pop_code)
summary(m_nest_ab_length)
coefficients(m_nest_ab_length)

# amphistomy
m_nest_amph <- aov(data=stom_all, amphistomy ~ ecotype/pop_code)
summary(m_nest_amph)
coefficients(m_nest_amph)

# adaxial area allocated to stomata
m_nest_ad_frac <- aov(data=stom_all, stom_ad_fraction ~ ecotype/pop_code)
summary(m_nest_ad_frac)
coefficients(m_nest_ad_frac)

# abaxial area allocated to stomata
m_nest_ab_frac <- aov(data=stom_all, stom_ab_fraction ~ ecotype/pop_code)
summary(m_nest_ab_frac)
coefficients(m_nest_ab_frac)

# Make ecotype plots showing only LSMs from nested ANOVA

# Indicate which pairs are which based on point shape
shapes <- c(21, 22, 23, 24, 25, 25, 21, 23, 22, 24)

ad_dens_plot <- emmip(m_nest_ad_dens, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                        dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                      fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Stomatal Density (mm^-2)') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12)) 


ad_length_plot <- emmip(m_nest_ad_length, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                      dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                    fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Stomatal Length (um)') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12)) 

ad_frac_plot <- emmip(m_nest_ad_frac, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                        dotarg = list(shape = shapes, cex = 5, col="black",
                                      fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge=0.4) +
  ylab('Stomatal Area / Epidermis Area') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12))

ad_gsw_plot <- emmip(m_nest_gsw, pop ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                     dotarg = list(shape = shapes, cex = 5, col="black",
                                   fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('gsw (umol m^-2 s^-1)') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12))

amph_plot <- emmip(m_nest_amph, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                     dotarg = list(shape = shapes, cex = 5, col="black",
                                   fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Amphistomy (adaxial/abaxial)') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12))

ggsave(ggarrange(ad_dens_plot, ad_length_plot, ad_frac_plot, 
                 ad_gsw_plot, amph_plot, nrow=2, ncol=3), 
       filename = "stomatal_figs_lsms_ecotype.svg", 
       path = "./Results/Figures/SVGs_for_MS/",
       device="svg", width = 10, height = 7, units = "in")

# Let's make a comparable supplemental figure for abaxial traits
ab_dens_plot <- emmip(m_nest_ab_dens, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                      dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                    fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Stomatal Density (mm^-2)') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12)) 


ab_length_plot <- emmip(m_nest_ab_length, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                        dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                      fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Stomatal Length (um)') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12)) 

ab_frac_plot <- emmip(m_nest_ab_frac, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                      dotarg = list(shape = shapes, cex = 5, col="black",
                                    fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge=0.4) +
  ylab('Stomatal Area / Epidermis Area') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12))

ggsave(ggarrange(ab_dens_plot, ab_length_plot, ab_frac_plot, 
                nrow=1, ncol=3), 
       filename = "stomatal_figs_abaxial_lsms_ecotype.svg", 
       path = "./Results/Figures/SVGs_for_MS/",
       device="svg", width = 10, height = 3.5, units = "in")


# check for differences between population pairs
#m <- glmmTMB(stom_density_ad ~ ecotype * pair, family = "gaussian", data = stom_all)
#summary(m)
#eg <- emmeans(m, specs = "pop_code")

# try Tukey as a post-hoc test. Use only pop code bc including ecotype causes
# it to make up groups that don't exist, like inland OPB
m <- aov(data=stom_all, stom_density_ad ~ pop_code)
summary(m)
TukeyHSD(m)

m <- aov(data=stom_all, stomate_size_ad ~ pop_code)
summary(m)
TukeyHSD(m)

# difference in stomatal size with coastal < inland for the following pairs:
# SWC/HEC (p=0.0030098), TOR/PGR (p=0.0253589), 
# not: OAE/BHE (p=1), OPB/RGR (other direction, p=0.0860436), SWB/LMC (p=0.9996284)

# Get mean stomatal trait values for each accession so I can compare them
# to the sodium uptake results
vars <- c('amphistomy', 'succulence', 'stom_density_ad', 'stom_density_ab', 'stomate_size_ab', 'stomate_size_ad',  'stom_ad_fraction', 'stom_ab_fraction')

stom_all %>% group_by(pop_code) %>% summarise(mean=mean(c('amphistomy', 'succulence', 'stom_density_ad', 'stom_density_ab', 'stomate_size_ab', 'stomate_size_ad',  'stom_ad_fraction', 'stom_ab_fraction')))

stom_all %>% drop_na(amphistomy, stom_density_ad, stom_density_ab, stomate_size_ab, stomate_size_ad,  stom_ad_fraction, stom_ab_fraction) %>% 
  group_by(pop_code) %>% summarise(avg=mean(.data[[var]]))

stom_summary <- stom_all %>%
  group_by(pop_code) %>%
  summarise(across(everything(), mean, na.rm=TRUE))

stom_summary <- stom_summary[, c(1,6, 9, 10, 13:16)]
write.csv(stom_summary, file="./Data/stomata_accession_means.csv", row.names = FALSE)

# read in stomatal conductance baseline (before spray treatments) data
# only adaxial side measured
baseline <- read.csv("./Data/stomatal_conductance_data/processed_baseline_gsw.csv")
      
# fit nested anova to test for differences in gsw between ecotypes  
m_nest_gsw <- aov(data=baseline, gsw ~ ecotype/pop)
summary(m_nest_gsw)
coefficients(m_nest_gsw) # being inland increases gsw by 0.08831 mol/*m^2/s

# plot adaxial gsw ~ ecotype
gsw_boxplot <- baseline %>%
  filter(!is.na(baseline$gsw)) %>%
  ggplot() +
  aes(x = ecotype, fill = ecotype, y = gsw) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'gsw (umol m^-2 s^-1)', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=12),
  )

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

# adaxial stomatal traits for main text figure
ggarrange(ad_stom_density_ecotype_boxplot, ad_stom_length, ad_stom_fraction_ecotype, 
          gsw_boxplot, amphistomy_boxplot, nrow=2, ncol=3)

# abaxial stomatal traits for supplemental figure
ggarrange(ad_stom_density_ecotype_boxplot, ab_stom_length, ab_stom_fraction_ecotype)

# set up contrasts
HEC_SWC <- c(0,1,0,0,0,0,0,0,-1,0)
OPB_RGR <- c(0,0,0,0,1,0,-1,0,0,0)
SWB_LMC <- c(0,0,-1,0,0,0,0,1,0,0)
BHE_OAE <- c(1,0,0,-1,0,0,0,0,0,0)
PGR_TOR <- c(0,0,0,0,0,1,0,0,0,-1)

mat <- cbind(HEC_SWC, OPB_RGR, SWB_LMC, BHE_OAE, PGR_TOR)

m <- lm(stom_density_ad ~ pop_code, data=stom_all)
summary(m)
m.emm <- emmeans(m, ~ pop_code)
contrast(m.emm, method=list(mat), adjust='bh')

m <- lm(stom_density_ab ~ pop_code, data=stom_all)
summary(m)
m.emm <- emmeans(m, ~ pop_code)
contrast(m.emm, method=list(mat), adjust='bh')

m <- lm(stomate_size_ad ~ pop_code, data=stom_all)
summary(m)
m.emm <- emmeans(m, ~ pop_code)
contrast(m.emm, method=list(mat), adjust='bh')

m <- lm(stomate_size_ab ~ pop_code, data=stom_all)
summary(m)
m.emm <- emmeans(m, ~ pop_code)
contrast(m.emm, method=list(mat), adjust='bh')

m <- lm(amphistomy ~ pop_code, data=stom_all)
summary(m)
m.emm <- emmeans(m, ~ pop_code)
contrast(m.emm, method=list(mat), adjust='bh')

m <- lm(stom_ad_fraction ~ pop_code, data=stom_all)
summary(m)
m.emm <- emmeans(m, ~ pop_code)
contrast(m.emm, method=list(mat), adjust='bh')

m <- lm(stom_ab_fraction ~ pop_code, data=stom_all)
summary(m)
m.emm <- emmeans(m, ~ pop_code)
contrast(m.emm, method=list(mat), adjust='bh')

m <- lm(gsw ~ pop, data=baseline)
summary(m)
m.emm <- emmeans(m, ~ pop)
contrast(m.emm, method=list(mat), adjust='bh')

########### Let's make tables!!! ###############
# Make ANOVA tables for stomatal traits!

library(kableExtra)

# tell kable not to plot NAs
options(knitr.kable.NA = '')

# make vector of sources of variation
sov <- c("Ecotype", "Accession (Ecotype)", "Error")

# make vector to describe what effect sizes indicate
effect_meaning <- c("Inland", "", "")

# vector of main and nested effects for adaxial stomatal density
effects <- c(as.numeric(m_nest_ad_dens$coefficients[2]), "", "")
anova_ad_dens_tbl <- as.data.frame(cbind(sov, effect_meaning, effects, anova(m_nest_ad_dens)$Df, anova(m_nest_ad_dens)$F, anova(m_nest_ad_dens)$`Pr(>F)`))
colnames(anova_ad_dens_tbl) <- c("Source of variation", "Effect of", "Effect size", "df", "F", "p-value")

# adaxial stomatal density table!
anova_ad_dens_tbl %>% mutate(`Effect size` = round(as.numeric(`Effect size`), 5),
                         F = round(as.numeric(F), 2),
                         `p-value` = case_when(as.numeric(`p-value`) < 0.00001 ~ "<0.00001",
                                               .default = as.character(round(as.numeric(`p-value`), 5)))) %>%
  kbl(caption = "Adaxial Stomatal Density") %>% kable_classic() %>% 
  row_spec(which(as.numeric(anova_ad_dens_tbl$`p-value`) < 0.05), bold=T)

# vector of main and nested effects for adaxial stomatal length
effects <- c(as.numeric(m_nest_ad_length$coefficients[2]), "", "")
anova_ad_length_tbl <- as.data.frame(cbind(sov, effect_meaning, effects, anova(m_nest_ad_length)$Df, anova(m_nest_ad_length)$F, anova(m_nest_ad_length)$`Pr(>F)`))
colnames(anova_ad_length_tbl) <- c("Source of variation", "Effect of", "Effect size", "df", "F", "p-value")

# make adaxial stomatal length table!
anova_ad_length_tbl %>% mutate(`Effect size` = round(as.numeric(`Effect size`), 5),
                             F = round(as.numeric(F), 2),
                             `p-value` = case_when(as.numeric(`p-value`) < 0.00001 ~ "<0.00001",
                                                   .default = as.character(round(as.numeric(`p-value`), 5)))) %>%
  kbl(caption = "Adaxial Stomatal Length") %>% kable_classic() %>% 
  row_spec(which(as.numeric(anova_ad_dens_tbl$`p-value`) < 0.05), bold=T)

# vector of main and nested effects for fraction of epidermis area allocated to stomata
effects <- c(as.numeric(m_nest_ad_frac$coefficients[2]), "", "")
anova_ad_frac_tbl <- as.data.frame(cbind(sov, effect_meaning, effects, anova(m_nest_ad_frac)$Df, anova(m_nest_ad_frac)$F, anova(m_nest_ad_frac)$`Pr(>F)`))
colnames(anova_ad_frac_tbl) <- c("Source of variation", "Effect of", "Effect size", "df", "F", "p-value")

# make adaxial stomatal fraction table!
anova_ad_frac_tbl %>% mutate(`Effect size` = round(as.numeric(`Effect size`), 5),
                               F = round(as.numeric(F), 2),
                               `p-value` = case_when(as.numeric(`p-value`) < 0.00001 ~ "<0.00001",
                                                     .default = as.character(round(as.numeric(`p-value`), 5)))) %>%
  kbl(caption = "Fraction of Adaxial Epidermis Area Allocated to Stomata") %>% kable_classic() %>% 
  row_spec(which(as.numeric(anova_ad_dens_tbl$`p-value`) < 0.05), bold=T)


# gsw table

# vector of main and nested effects for gsw
effects <- c(as.numeric(m_nest_gsw$coefficients[2]), "", "")
anova_gsw_tbl <- as.data.frame(cbind(sov, effect_meaning, effects, anova(m_nest_gsw)$Df, anova(m_nest_gsw)$F, anova(m_nest_gsw)$`Pr(>F)`))
colnames(anova_gsw_tbl) <- c("Source of variation", "Effect of", "Effect size", "df", "F", "p-value")

# make adaxial stomatal fraction table!
anova_gsw_tbl %>% mutate(`Effect size` = round(as.numeric(`Effect size`), 5),
                             F = round(as.numeric(F), 2),
                             `p-value` = case_when(as.numeric(`p-value`) < 0.00001 ~ "<0.00001",
                                                   .default = as.character(round(as.numeric(`p-value`), 5)))) %>%
  kbl(caption = "Adaxial Stomatal Conductance") %>% kable_classic() %>% 
  row_spec(which(as.numeric(anova_ad_dens_tbl$`p-value`) < 0.05), bold=T)

# amphistomy table
# vector of main and nested effects for amphistomy
effects <- c(as.numeric(m_nest_amph$coefficients[2]), "", "")
anova_amph_tbl <- as.data.frame(cbind(sov, effect_meaning, effects, anova(m_nest_amph)$Df, anova(m_nest_amph)$F, anova(m_nest_amph)$`Pr(>F)`))
colnames(anova_amph_tbl) <- c("Source of variation", "Effect of", "Effect size", "df", "F", "p-value")

# adaxial stomatal density table!
anova_amph_tbl %>% mutate(`Effect size` = round(as.numeric(`Effect size`), 5),
                             F = round(as.numeric(F), 2),
                             `p-value` = case_when(as.numeric(`p-value`) < 0.00001 ~ "<0.00001",
                                                   .default = as.character(round(as.numeric(`p-value`), 5)))) %>%
  kbl(caption = "Amphistomy") %>% kable_classic() %>% 
  row_spec(which(as.numeric(anova_ad_dens_tbl$`p-value`) < 0.05), bold=T)


# Supplemental ANOVA table for abaxial stomatal traits 

# vector of main and nested effects for adaxial stomatal density
effects <- c(as.numeric(m_nest_ab_dens$coefficients[2]), "", "")
anova_ab_dens_tbl <- as.data.frame(cbind(sov, effect_meaning, effects, anova(m_nest_ab_dens)$Df, anova(m_nest_ab_dens)$F, anova(m_nest_ab_dens)$`Pr(>F)`))
colnames(anova_ab_dens_tbl) <- c("Source of variation", "Effect of", "Effect size", "df", "F", "p-value")

# abaxial stomatal density table!
anova_ab_dens_tbl %>% mutate(`Effect size` = round(as.numeric(`Effect size`), 5),
                             F = round(as.numeric(F), 2),
                             `p-value` = case_when(as.numeric(`p-value`) < 0.00001 ~ "<0.00001",
                                                   .default = as.character(round(as.numeric(`p-value`), 5)))) %>%
  kbl(caption = "Abaxial Stomatal Density") %>% kable_classic() %>% 
  row_spec(which(as.numeric(anova_ab_dens_tbl$`p-value`) < 0.05), bold=T)

# vector of main and nested effects for adaxial stomatal length
effects <- c(as.numeric(m_nest_ab_length$coefficients[2]), "", "")
anova_ab_length_tbl <- as.data.frame(cbind(sov, effect_meaning, effects, anova(m_nest_ab_length)$Df, anova(m_nest_ab_length)$F, anova(m_nest_ab_length)$`Pr(>F)`))
colnames(anova_ab_length_tbl) <- c("Source of variation", "Effect of", "Effect size", "df", "F", "p-value")

# make adaxial stomatal length table!
anova_ab_length_tbl %>% mutate(`Effect size` = round(as.numeric(`Effect size`), 5),
                               F = round(as.numeric(F), 2),
                               `p-value` = case_when(as.numeric(`p-value`) < 0.00001 ~ "<0.00001",
                                                     .default = as.character(round(as.numeric(`p-value`), 5)))) %>%
  kbl(caption = "Abaxial Stomatal Length") %>% kable_classic() %>% 
  row_spec(which(as.numeric(anova_ab_dens_tbl$`p-value`) < 0.05), bold=T)

# vector of main and nested effects for fraction of epidermis area allocated to stomata
effects <- c(as.numeric(m_nest_ab_frac$coefficients[2]), "", "")
anova_ab_frac_tbl <- as.data.frame(cbind(sov, effect_meaning, effects, anova(m_nest_ab_frac)$Df, anova(m_nest_ab_frac)$F, anova(m_nest_ab_frac)$`Pr(>F)`))
colnames(anova_ab_frac_tbl) <- c("Source of variation", "Effect of", "Effect size", "df", "F", "p-value")

# make abaxial stomatal fraction table!
anova_ab_frac_tbl %>% mutate(`Effect size` = round(as.numeric(`Effect size`), 5),
                             F = round(as.numeric(F), 2),
                             `p-value` = case_when(as.numeric(`p-value`) < 0.00001 ~ "<0.00001",
                                                   .default = as.character(round(as.numeric(`p-value`), 5)))) %>%
  kbl(caption = "Fraction of Abaxial Epidermis Area Allocated to Stomata") %>% kable_classic() %>% 
  row_spec(which(as.numeric(anova_ab_dens_tbl$`p-value`) < 0.05), bold=T)



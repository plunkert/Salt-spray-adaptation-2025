# Primary contributor: Cam
# Purpose: Preliminary analysis of stomata data to compare BHE and OAE populations
# Madison update Dec. 2024 include all 10 populations
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
  
View(stomata)

table(as.factor(stomata$pop.code))

# Compare Abaxial vs. Adaxial Stomata Counts for each pop
oae_bhe_amphistomy = stomata %>%
  filter(pop.code %in% c('BHE', 'OAE')) %>%
  mutate(pop.code = if_else(pop.code == 'BHE', 'Coastal (BHE)', 'Inland (OAE)')) %>%
  ggplot() +
  aes(x = pop.code, fill = pop.code, y = stomata_count_ad / stomata_count_total * 100) +
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
    pop.code %in% c('PGR', 'BHE') ~ "protected",
    pop.code %in% c('SWB', 'OPB', 'HEC') ~ "exposed",
    pop.code %in% c('SWC', 'LMC', 'TOR', 'OAE', 'RGR') ~ "inland")
  )

# Add ecotype column
stomata <- stomata %>% mutate(ecotype = as.factor(if_else(salt_exposure=="inland", "inland", "coastal")),
                              salt_exposure = as.factor(salt_exposure),
                              pop.code=as.factor(pop.code))

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
  filename = 'Ad_Stomata_Ecotype.png', 
  plot = stomata_ad_boxplot,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 3,
  height = 6,
  bg = 'white'
)

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

# Look at each population pair separately

# subset dataframe for each population pair
oae_bhe <- stomata[which(stomata$pop.code == "BHE" | stomata$pop.code == "OAE"),]
tor_pgr <- stomata[which(stomata$pop.code == "PGR" | stomata$pop.code == "TOR"),]
lmc_swb <- stomata[which(stomata$pop.code == "LMC" | stomata$pop.code == "SWB"),]
rgr_opb <- stomata[which(stomata$pop.code == "RGR" | stomata$pop.code == "OPB"),]
swc_hec <- stomata[which(stomata$pop.code == "SWC" | stomata$pop.code == "HEC"),]
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
   # ggtitle(as.character(unique(pairs[[i]]$pop.code)))+
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

# make a function that takes the dataframe with a population pair and a variable of interest
plot_each_pair <- function (pair, var) {
  pops=unique(pair$pop.code)
  na.omit(pair) %>%
    ggplot() +
    aes(x = ecotype, fill = ecotype, y = pair[[var]]) +
    geom_boxplot(outliers = F) +
    geom_jitter(position=position_jitter(0.1)) +
    # Labels
    scale_x_discrete(name = 'Ecotype') +
    scale_y_continuous(
      name = var 
    ) +
    ggtitle(paste(as.character(unique(pair$pop.code))[1], " / ", as.character(unique(pair$pop.code))[2]))+
    # Style
    scale_fill_manual(values = c('#514663', '#cacf85')) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_text(size=12),
      axis.title = element_text(size=14)
      )
}

# Compare adaxial stomata for each population pair
plots <- lapply(pairs, plot_each_pair, "stomata_count_ad")
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

# Compare abaxial stomata for each population pair
plots <- lapply(pairs, plot_each_pair, "stomata_count_ab")
ab_pair_plots <- ggarrange(plotlist=plots, ncol=2, nrow=3)
ggsave(
  filename = 'Ab_Stomata_Pairs.png', 
  plot = ab_pair_plots,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 6,
  height = 6,
  bg = 'white'
)

# Compare stomatal ratio for each population pair
plots <- lapply(pairs, plot_each_pair, "amphistomy")
amphistomy_pair_plots <- ggarrange(plotlist=plots, ncol=2, nrow=3)
ggsave(
  filename = 'Amphistomy_Pairs.png', 
  plot = amphistomy_pair_plots,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 6,
  height = 6,
  bg = 'white'
)




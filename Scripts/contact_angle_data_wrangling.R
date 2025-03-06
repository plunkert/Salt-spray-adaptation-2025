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
  ) %>%
  pivot_wider(
    id_cols = c(pop.code, rep, ecotype, salt_exposure), 
    names_from = leaf_side,
    values_from = contact_angle,
    names_prefix = "contact_angle_"
  )

View(contact_angle)

# View coastal_data
contact_angle %>%
  ggplot() +
  aes(x=contact_angle_ad) +
  geom_histogram()

# Coastal vs. inland and ad vs. ab
contact_angle %>%
  ggplot() +
  aes(x=contact_angle_ad) +
  facet_wrap(vars(ecotype), nrow = 2) +
  geom_histogram()

contact_angle %>%
  ggplot() +
  aes(x=contact_angle_ab) +
  facet_wrap(vars(ecotype), nrow = 2) +
  geom_histogram()

# How much of each pop?
contact_angle %>%
  filter(!is.na(contact_angle_ad)) %>%
  group_by(pop.code) %>%
  count()

# Comparing pairs
contact_angle %>%
  filter(pop.code == 'RGR' | pop.code == 'OPB') %>%
  ggplot() +
  aes(x=contact_angle_ad) +
  facet_wrap(vars(ecotype), nrow = 2) +
  geom_boxplot()

# Comparing protected and exposed
contact_angle %>%
  filter(salt_exposure != "inland") %>%
  ggplot() +
  aes(x=contact_angle_ad) +
  facet_wrap(vars(salt_exposure), nrow = 2) +
  geom_boxplot()

contact_angle %>%
  filter(salt_exposure != "inland") %>%
  ggplot() +
  aes(x=contact_angle_ad) +
  facet_wrap(vars(salt_exposure), nrow = 2) +
  geom_boxplot()


contact_ad_boxplot = contact_angle %>%
  filter(!is.na(contact_angle_ad)) %>%
  ggplot() +
  aes(x = ecotype, fill = ecotype, y = contact_angle_ad) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Contact Angle (degrees) on Adaxial Surface', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
contact_ad_boxplot

ggsave(
  filename = 'Ad_contact_angle_ecotype.png', 
  plot = contact_ad_boxplot,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 3,
  height = 6,
  bg = 'white'
)

contact_ab_boxplot = contact_angle %>%
  filter(!is.na(contact_angle_ab)) %>%
  ggplot() +
  aes(x = ecotype, fill = ecotype, y = contact_angle_ab) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Contact Angle (degrees) on Abaxial Surface', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
contact_ab_boxplot

ggsave(
  filename = 'Ab_contact_angle_ecotype.png', 
  plot = contact_ab_boxplot,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 3,
  height = 6,
  bg = 'white'
)

# t-tests
t.test(data=contact_angle, contact_angle_ad ~ ecotype)
t.test(data=contact_angle, contact_angle_ab ~ ecotype)

# fit nested ANOVA
m_nest_ad <- aov(data=contact_angle, contact_angle_ad ~ ecotype/pop.code)
summary(m_nest_ad)
coefficients(m_nest_ad)

m_nest_ab <- aov(data=contact_angle, contact_angle_ab ~ ecotype/pop.code)

summary(m_nest_ab)
coefficients(m_nest_ab)

summary(lm(data=contact_angle, contact_angle_ab ~ ecotype + ecotype:pop.code))


# Look at each pair of populations
# subset dataframe for each population pair
oae_bhe <- contact_angle[which(contact_angle$pop.code == "BHE" | contact_angle$pop.code == "OAE"),] %>% na.omit()
tor_pgr <- contact_angle[which(contact_angle$pop.code == "PGR" | contact_angle$pop.code == "TOR"),] %>% na.omit()
lmc_swb <- contact_angle[which(contact_angle$pop.code == "LMC" | contact_angle$pop.code == "SWB"),] %>% na.omit()
rgr_opb <- contact_angle[which(contact_angle$pop.code == "RGR" | contact_angle$pop.code == "OPB"),] %>% na.omit()
swc_hec <- contact_angle[which(contact_angle$pop.code == "SWC" | contact_angle$pop.code == "HEC"),] %>% na.omit()
pairs <- list(swc_hec, rgr_opb, lmc_swb, oae_bhe, tor_pgr)

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
plot_each_pair(tor_pgr, "contact_angle_ad")
plots <- lapply(pairs, plot_each_pair, "contact_angle_ad")
ad_contact_angle_pair_plots <- ggarrange(plotlist=plots, ncol=2, nrow=3)
ggsave(
  filename = 'Adaxial_Contact_Angle_Pairs.png', 
  plot = ad_contact_angle_pair_plots,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 6,
  height = 6,
  bg = 'white'
)

plots <- lapply(pairs, plot_each_pair, "contact_angle_ab")
ab_contact_angle_pair_plots <- ggarrange(plotlist=plots, ncol=2, nrow=3)
ggsave(
  filename = 'Abaxial_Contact_Angle_Pairs.png', 
  plot = ad_contact_angle_pair_plots,
  device = 'png',
  path = './Results/Figures/',
  scale = 1,
  width = 6,
  height = 6,
  bg = 'white'
)


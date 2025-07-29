require(tidyverse)
require(readxl)

coastal_pops = c('BHE', 'SWB', 'PGR', 'HEC', 'OPB')
inland_pops = c('SWC', 'LMC', 'TOR', 'OAE', 'RGR')

setwd("~/Documents/GitHub/Leaf-surface-traits-2024/") # change this

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

# Nested ANOVA table for contact angle

library(kableExtra)

# tell kable not to plot NAs
options(knitr.kable.NA = '')

# make vector of sources of variation
sov <- c("Ecotype", "Accession (Ecotype)", "Error")

# make vector to describe what effect sizes indicate
effect_meaning <- c("Inland", "", "")

# vector of main and nested effects for adaxial contact angle
effects <- c(as.numeric(m_nest_ad$coefficients[2]), "", "")
anova_ad_angle_tbl <- as.data.frame(cbind(sov, effect_meaning, effects, anova(m_nest_ad)$Df, anova(m_nest_ad)$F, anova(m_nest_ad)$`Pr(>F)`))
colnames(anova_ad_angle_tbl) <- c("Source of variation", "Effect of", "Effect size", "df", "F", "p-value")

# adaxial contact angle density table!
anova_ad_angle_tbl %>% mutate(`Effect size` = round(as.numeric(`Effect size`), 5),
                             F = round(as.numeric(F), 2),
                             `p-value` = case_when(as.numeric(`p-value`) < 0.00001 ~ "<0.00001",
                                                   .default = as.character(round(as.numeric(`p-value`), 5)))) %>%
  kbl(caption = "Adaxial Contact Angle") %>% kable_classic() %>% 
  row_spec(which(as.numeric(anova_ad_angle_tbl$`p-value`) < 0.05), bold=T)

# vector of main and nested effects for abaxial contact angle
effects <- c(as.numeric(m_nest_ab$coefficients[2]), "", "")
anova_ab_angle_tbl <- as.data.frame(cbind(sov, effect_meaning, effects, anova(m_nest_ab)$Df, anova(m_nest_ab)$F, anova(m_nest_ab)$`Pr(>F)`))
colnames(anova_ab_angle_tbl) <- c("Source of variation", "Effect of", "Effect size", "df", "F", "p-value")

# adaxial contact angle density table!
anova_ab_angle_tbl %>% mutate(`Effect size` = round(as.numeric(`Effect size`), 5),
                              F = round(as.numeric(F), 2),
                              `p-value` = case_when(as.numeric(`p-value`) < 0.00001 ~ "<0.00001",
                                                    .default = as.character(round(as.numeric(`p-value`), 5)))) %>%
  kbl(caption = "Abaxial Contact Angle") %>% kable_classic() %>% 
  row_spec(which(as.numeric(anova_ab_angle_tbl$`p-value`) < 0.05), bold=T)


# Plot showing LSMs of contact angle for each accession

shapes <- c(21, 22, 23, 24, 25, 25, 21, 23, 22, 24)

ad_contact_plot <- emmip(m_nest_ad, pop.code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                      dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                    fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Adaxial Contact Angle') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12)) +
  ylim(53,80)

ab_contact_plot <- emmip(m_nest_ab, pop.code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                         dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                       fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Abaxial Contact Angle') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12)) +
  ylim(53,80)

# dunk plot is loaded into R enviro from succulence_dunk_area_wrangling.R script (fix this!)
ggsave(ggarrange(ad_contact_plot, ab_contact_plot, dunk_plot, 
                 nrow=1, ncol=3), 
       filename = "hydrophobicity_lsms_ecotype.svg", 
       path = "./Results/Figures/SVGs_for_MS/",
       device="svg", width = 10, height = 3.5, units = "in")


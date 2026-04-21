# Analysis of stomatal and hydrophobicity traits from coastal and inland accessions in a common garden.
# This script wrangles and cleans data, performs calculations related to stomatal 
# allocation and leaf water droplet adhesion assay, fits nested ANOVAs for the differences 
# between ecotypes in leaf surface traits, and produces figures and tables describing these results.

# Consolidating into a single analysis for all leaf traits (but not including 
# anything with salt/control treatments).

require(tidyverse)
require(readxl)
require(ggpubr)
require(emmeans)
require(ggplot2)
require(dplyr)
require(kableExtra)
require(webshot2)
require(lme4)
# Which accession numbers are coastal and inland?
coastal_pops = c('BHE', 'SWB', 'PGR', 'HEC', 'OPB')
inland_pops = c('SWC', 'LMC', 'TOR', 'OAE', 'RGR')

# Read in contact angle data
setwd("~/Documents/GitHub/Salt-spray-adaptation-2025/")

contact_angle =  read_xlsx(
  path = './Data/Contact Angle Measurements.xlsx', # Change path here
  sheet = 'Sheet1'
) %>%
  mutate(
    ecotype = case_when(
      pop.code %in% coastal_pops ~ "coastal",
      pop.code %in% inland_pops ~ "inland",
      .default = NA_character_
    )
  ) %>%
  pivot_wider(
    id_cols = c(pop.code, rep, ecotype), 
    names_from = leaf_side,
    values_from = contact_angle,
    names_prefix = "contact_angle_"
  )

# Read in stomatal count data

stomata_counts <-  read_excel('./Data/stomatal density data.xlsx', sheet="Sheet1") %>%
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
    # Adaxial vs. abaxial ratio, 1 = equal stomata, >1 more stomata on abaxial side
    amphistomy = stomata_count_ad / stomata_count_ab,
    # Stomatal density in stomata/mm^2 by dividing by field of view
    stom_density_ad = stomata_count_ad/0.94372,
    stom_density_ab = stomata_count_ab/0.94372,
    ecotype = case_when(
      pop_code %in% coastal_pops ~ "coastal",
      pop_code %in% inland_pops ~ "inland",
      .default = NA_character_
      )
  )

# Read in stomatal size data

stom_size <- read_excel('./Data/stomata_lengths_leaf_surface_data.xlsx', sheet="Sheet1")
stom_size$file_name[stom_size$file_name == ""] <- NA
stom_size <- stom_size %>% fill(file_name) %>%
  mutate(
    stomate_length_um = as.numeric(stomate_length_um)
  ) %>%
  separate(
    col = file_name,
    sep = '_',
    into = c('pop_code', 'replicate', 'leaf_side', 'view_rep')
  )

# throw out the 2cm stomate, obviously not right. Get mean stomatal size for each peel.
stom_size <- stom_size[which(stom_size$stomate_length_um < 1000),] %>%
  group_by(pop_code, replicate, view_rep, leaf_side) %>% 
  summarise(mean=mean(stomate_length_um)) %>%
  # get stomatal size for each side of the leaf on the same row
  pivot_wider(id_cols = c(pop_code, replicate),
              names_from=leaf_side,
              values_from=mean,
              names_prefix = "stomate_size_")

# merge mean stomatal size with stomatal count data
stom_all <- merge(stomata_counts, stom_size, by = c("pop_code", "replicate"))

# Estimate area of a stomate in mm^2 following Muir et al. 2023 Am Nat. 
# Approximate stomatal area as A = 0.5*L^2. Includes conversion um to mm.
stom_all$stomate_area_ad <- 0.5 * (stom_all$stomate_size_ad/1000)^2
stom_all$stomate_area_ab <- 0.5 * (stom_all$stomate_size_ab/1000)^2

# Estimate fraction of leaf epidermal area allocated to stomata following Muir et al. 2023 Am Nat.
stom_all$stom_ad_fraction <- stom_all$stom_density_ad * stom_all$stomate_area_ad
stom_all$stom_ab_fraction <- stom_all$stom_density_ab * stom_all$stomate_area_ab

# Read in stomatal conductance (gsw) data. Adaxial only.
gsw <- read_excel("./Data/processed_baseline_gsw.xlsx")

# Read in leaf area data into R
area <- read_excel("./Data/leaf_area_data.xlsx", sheet="Sheet1")
area$pop_code <- as.factor(area$pop_code)

# Read other leaf surface data (leaf mass, leaf water drop adhesion assay, etc) into R
dat <- read_excel("./Data/leaf_surface_data.xlsx", sheet="Sheet1")
colnames(dat)[1] <- "pop_code"
# Merge with area (effectively, add area column to larger dataframe)
dat_area <- merge(area, dat, by = c("pop_code", "rep")) %>%
  mutate(ecotype = case_when(
    pop_code %in% coastal_pops ~ "coastal",
    pop_code %in% inland_pops ~ "inland",
    .default = NA_character_
    ),
    # Calculate succulence as grams of water per cm^2 leaf area
  succulence = (fresh.mass - dry.mass)/leaf_area_cm2,
  
  # Calculate result of dunk assay as grams of water that hung onto leaf surface / leaf area
  adhesion_result = (dunked.mass - fresh.mass)/(2*leaf_area_cm2),
  
  lma = 10000*dry.mass/leaf_area_cm2, # calculate LMA and convert to g/m^2
)

# Add factor variable indicating latitudinal pair

dat_area$pair <- case_when(dat_area$pop_code == "OPB" | dat_area$pop_code == "RGR" ~ "OPB/RGR",
                           dat_area$pop_code == "HEC" | dat_area$pop_code == "SWC" ~ "HEC/SWC",
                           dat_area$pop_code == "BHE" | dat_area$pop_code == "OAE" ~ "BHE/OAE",
                           dat_area$pop_code == "SWB" | dat_area$pop_code == "LMC" ~ "SWB/LMC",
                           dat_area$pop_code == "PGR" | dat_area$pop_code == "TOR" ~ "PGR/TOR",
                      .default=NA_character_) %>% as.factor()

contact_angle$pair <- case_when(contact_angle$pop.code == "OPB" | contact_angle$pop.code == "RGR" ~ "OPB/RGR",
                           contact_angle$pop.code == "HEC" | contact_angle$pop.code == "SWC" ~ "HEC/SWC",
                           contact_angle$pop.code == "BHE" | contact_angle$pop.code == "OAE" ~ "BHE/OAE",
                           contact_angle$pop.code == "SWB" | contact_angle$pop.code == "LMC" ~ "SWB/LMC",
                           contact_angle$pop.code == "PGR" | contact_angle$pop.code == "TOR" ~ "PGR/TOR",
                           .default=NA_character_) %>% as.factor()

stom_all$pair <- case_when(stom_all$pop_code == "OPB" | stom_all$pop_code == "RGR" ~ "OPB/RGR",
                           stom_all$pop_code == "HEC" | stom_all$pop_code == "SWC" ~ "HEC/SWC",
                           stom_all$pop_code == "BHE" | stom_all$pop_code == "OAE" ~ "BHE/OAE",
                           stom_all$pop_code == "SWB" | stom_all$pop_code == "LMC" ~ "SWB/LMC",
                           stom_all$pop_code == "PGR" | stom_all$pop_code == "TOR" ~ "PGR/TOR",
                           .default=NA_character_) %>% as.factor()


# Forcing the levels of each coastal/inland pair to have 

## Fitting nested ANOVAs that include latitudinal pair

# contact angle both leaf sides, slough of models
m_nest_ang_ad_1 <- aov(data=contact_angle, contact_angle_ad ~ ecotype/pop.code)
m_nest_ang_ad_2 <- aov(data=contact_angle, contact_angle_ad ~ pair/pop.code + ecotype)
#m_nest_ang_ad_3 <- aov(data=contact_angle, contact_angle_ad ~ pair + ecotype/pop.code)
#m_nest_ang_ad_4 <- aov(data=contact_angle, contact_angle_ad ~ pair + ecotype)
#m_nest_ang_ad_6 <- aov(data=contact_angle, contact_angle_ad ~ pair/pop.code)
#m_nest_ang_ad_7 <- aov(data=contact_angle, contact_angle_ad ~ ecotype)
#m_nest_ang_ad_8 <- aov(data=contact_angle, contact_angle_ad ~ ecotype * pair)
#m <- lm(data=contact_angle, contact_angle_ad ~ pair + ecotype)

#Tentative plan: keep original figures and tables, but also run pairwise contrasts as
# post-hoc test for differences between pairs

anova(m, m_nest_ang_ad_3)
AIC(m_nest_ang_ad_2, m_nest_ang_ad_3)

# Let's see if LSMs are different between m_nest_ang_ad_1 and m_nest_ang_ad_2
rg_1 <- ref_grid(m_nest_ang_ad_1)
rg_2 <- ref_grid(m_nest_ang_ad_2)

emmeans(rg_1, ~ pop.code)
emmeans(rg_2, ~ pair/pop.code)

emmeans(rg_1, ~ "ecotype")
emmeans(rg_2, ~ "ecotype")

out <- lmer(data=contact_angle, contact_angle_ad ~ ecotype + (1|pair/pop.code)) #isSingular warning


# is this our guy??? But not explicit about the nesting structure
out_pair <- lmer(data=contact_angle, contact_angle_ad ~ pair + ecotype + (1|pop.code))
drop1(out_pair, test="Chisq")

# Residual diagnostics
res.vals <- resid(out_pair) # out$residuals didn't work, trying resid function
pred.vals <- fitted(out_pair) # Fitted values
# Make qqnorm plot and see if points are along diagonal
qqnorm(res.vals, pch = 19)
qqline(res.vals, col = 'red')

plot(pred.vals, res.vals, main = "Residuals vs. pred.vals values", 
     las = 1, xlab = "Predicted values", ylab = "Residuals", pch = 19)
abline(h = 0)

out_pair_pair <- lmer(data=contact_angle, contact_angle_ad ~ pair + ecotype + (1|pair/pop.code))# does not converge
confint(out)
confint(out_pair)
AIC(out, out_pair)

out_pair_pair <- glmmTMB(data=contact_angle, contact_angle_ad ~ ecotype + (1|pair/pop.code))
summary(out_pair_pair)




out_pair_pair_another <- glmmTMB(data=contact_angle, contact_angle_ad ~ ecotype + (1|pair))

AIC(out_pair_pair_another, out_pair_pair)

# You should calculate accession means removing the effect of pair
# take out effect of pair and accession nested w/in pair

# try with contrasts
HEC_SWC <- c(0,1,0,0,0,0,0,0,-1,0)
OPB_RGR <- c(0,0,0,0,1,0,-1,0,0,0)
SWB_LMC <- c(0,0,-1,0,0,0,0,1,0,0)
BHE_OAE <- c(1,0,0,-1,0,0,0,0,0,0)
PGR_TOR <- c(0,0,0,0,0,1,0,0,0,-1)

mat <- cbind(HEC_SWC, OPB_RGR, SWB_LMC, BHE_OAE, PGR_TOR)
summary(m)
m.emm <- emmeans(m_nest_ang_ad_5, ~ ecotype)
contrast(m.emm, method=list(mat), adjust='bh')



m_nest_ang_ab <- aov(data=contact_angle, contact_angle_ab ~ pair/pop.code + ecotype)


# stomatal density both leaf sides
m_nest_dens_ad <- aov(data=stom_all, stom_density_ad ~ ecotype/pop_code)
m_nest_dens_ad_pair <- aov(data=stom_all, stom_density_ad ~ pair/pop_code + ecotype)

m_nest_dens_ab <- aov(data=stom_all, stom_density_ab ~ ecotype/pop_code)
  
# stomatal length both leaf sides
m_nest_length_ad <- aov(data=stom_all, stomate_size_ad ~ ecotype/pop_code)
m_nest_length_ab <- aov(data=stom_all, stomate_size_ab ~ ecotype/pop_code)

# fraction epidermis allocated to stomata both leaf sides
m_nest_frac_ad <- aov(data=stom_all, stom_ad_fraction ~ ecotype/pop_code)
m_nest_frac_ab <- aov(data=stom_all, stom_ab_fraction ~ ecotype/pop_code)

# adaxial stomatal conductance
m_nest_gsw <- aov(data=gsw, gsw ~ ecotype/pop)

# amphistomy
m_nest_amph <- aov(data=stom_all, amphistomy ~ ecotype/pop_code)

# succulence
m_nest_succ <- aov(data=dat_area, succulence ~ ecotype/pop_code)

# LMA
m_nest_lma <- aov(data=dat_area, lma ~ ecotype/pop_code)

# Leaf water drop adhesion assay
m_nest_adhesion <- aov(data=dat_area, adhesion_result ~ ecotype/pop_code)

# Making figures <3

shapes <- c(21, 22, 23, 24, 25, 25, 21, 23, 22, 24)

ang_ad_plot <- emmip(m_nest_ang_ad, pop.code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                     dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                   fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Abaxial Contact Angle (°)') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12)) +
  ylim(42,80)

ang_ab_plot <- emmip(m_nest_ang_ab, pop.code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                     dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                   fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Abaxial Contact Angle (°)') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12)) +
  ylim(42,80)

stom_dens_ad_plot <- emmip(m_nest_dens_ad, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                     dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                   fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Stomatal Density') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12))

stom_dens_ab_plot <- emmip(m_nest_dens_ab, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                           dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                         fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Stomatal Density') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12))

stom_length_ad_plot <- emmip(m_nest_length_ad, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                          dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                        fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Stomatal Length (um)') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12))

stom_length_ab_plot <- emmip(m_nest_length_ab, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                             dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                           fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Stomatal Length (um)') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12))

stom_frac_ad_plot <- emmip(m_nest_frac_ad, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                             dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                           fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Stomatal Area / Epidermis Area') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12))

stom_frac_ab_plot <- emmip(m_nest_frac_ab, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                           dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                         fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Stomatal Area / Epidermis Area') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12))

gsw_plot <- emmip(m_nest_gsw, pop ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                           dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                         fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('gsw (umol m^-2 s^-1)') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12))

amph_plot <- emmip(m_nest_amph, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                   dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                 fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Amphistomy (adaxial/abaxial)') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12)) +
  geom_hline(yintercept=1, linetype="dotted")
  
succulence_plot <- emmip(m_nest_succ, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                     dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                   fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Succulence (g H2O / cm2 leaf') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12))

adhesion_plot <- emmip(m_nest_adhesion, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                         dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                       fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('Leaf Water Drop Adhesion Assay (g H2O/cm2)') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12))

lma_plot <- emmip(m_nest_lma, pop_code ~ ecotype,CIs=TRUE, col=c(rep('#514663', 5), rep('#cacf85', 5)),
                       dotarg = list(shape = shapes, cex = 5, col="black", position="jitter",
                                     fill = c(rep('#514663', 5), rep('#cacf85', 5))), type = "response", plotit = T, dodge = 0.4) +
  ylab('LMA (g/m2)') + xlab("Ecotype") +
  theme(axis.text = element_text(size = 12))

# Now put them together for MS


# wettability figure
ggsave(ggarrange(ang_ad_plot, ang_ab_plot, adhesion_plot, 
                 nrow=1, ncol=3), 
       filename = "hydrophobicity_lsms_ecotype.svg", 
       path = "./Results/Figures/SVGs_for_MS/",
       device="svg", width = 10, height = 3.5, units = "in")

# adaxial stomatal traits figure
ggsave(ggarrange(stom_dens_ad_plot, stom_length_ad_plot, stom_frac_ad_plot, 
                 gsw_plot, amph_plot, nrow=2, ncol=3), 
       filename = "stomatal_figs_lsms_ecotype.svg", 
       path = "./Results/Figures/SVGs_for_MS/",
       device="svg", width = 10, height = 7, units = "in")

# abaxial stomatal traits supplemental figure

ggsave(ggarrange(stom_dens_ab_plot, stom_length_ab_plot, stom_frac_ab_plot, 
                 nrow=1, ncol=3), 
       filename = "stomatal_figs_abaxial_lsms_ecotype.svg", 
       path = "./Results/Figures/SVGs_for_MS/",
       device="svg", width = 10, height = 3.5, units = "in")


# succulence and LMA supplemental figure
ggsave(ggarrange(succulence_plot, lma_plot, nrow=1, ncol=2),
       filename = "LMA_succulence_lsms_ecotype.svg",
       path = "./Results/Figures/SVGs_for_MS/",
       device="svg", width = 8, height = 3.5, units = "in")


# Let's make tables!!

# tell kable not to plot NAs
options(knitr.kable.NA = '')


# Make a function that takes a nested ANOVA and outputs a formatted table and saves a CSV
anovaTable <- function(model, title){
  # make vector of sources of variation
  sov <- c("Ecotype", "Accession (Ecotype)", "Error")
  # make vector to describe what effect sizes indicate
  effect_meaning <- c("Inland", "", "")
  # vector of main and nested effects
  effects <- c(as.numeric(model$coefficients[2]), "", "")
  tbl <- as.data.frame(cbind(sov, effect_meaning, effects, anova(model)$Df, 
                             anova(model)$F, anova(model)$`Pr(>F)`))
  colnames(tbl) <- c("Source of variation", "Effect of", "Effect size", "df", "F", "p-value")
  unformatted <- tbl %>% mutate(`Effect size` = round(as.numeric(`Effect size`), 5),
                               F = round(as.numeric(F), 2),
                               `p-value` = case_when(as.numeric(`p-value`) < 0.00001 ~ "<0.00001",
                                                     .default = as.character(round(as.numeric(`p-value`), 5))))
  write.csv(unformatted, file = paste("./Results/Tables/tables_CSV_format/", title, "_ecotype_anova_table.csv", sep=""), row.names=FALSE)
  unformatted %>% 
    kbl(caption = title) %>% kable_classic() %>% 
    row_spec(which(as.numeric(tbl$`p-value`) < 0.05), bold=T) %>%
    save_kable(paste("./temp/", title, "_ecotype_anova_table.html", sep="")) %>% 
    webshot(paste("./Results/Tables/", title, "_ecotype_anova_table.pdf", sep=""), vwidth = 496)
  
}


anovaTable(m_nest_adhesion, "Leaf Water Droplet Adhesion Assay")
anovaTable(m_nest_amph, "Amphistomy")
anovaTable(m_nest_ang_ab, "Abaxial Contact Angle")
anovaTable(m_nest_ang_ad, "Adaxial Contact Angle")
anovaTable(m_nest_dens_ab, "Abaxial Stomatal Density")
anovaTable(m_nest_dens_ad, "Adaxial Stomatal Density")
anovaTable(m_nest_frac_ab, "Fraction of Abaxial Epidermis Allocated to Stomata")
anovaTable(m_nest_frac_ad, "Fraction of Adaxial Epidermis Allocated to Stomata")
anovaTable(m_nest_gsw, "Adaxial Stomatal Conductance")
anovaTable(m_nest_length_ab, "Abaxial Stomatal Length")
anovaTable(m_nest_length_ad, "Adaxial Stomatal Length")
anovaTable(m_nest_lma, "LMA")
anovaTable(m_nest_succ, "Succulence")

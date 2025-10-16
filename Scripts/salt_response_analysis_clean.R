# Sodium treatment response analysis: includes leaf mass, area, and MP-AES, only analyses
# related to MP-AES and succulence.

# Consolidating analyses of coastal/inland ecotypes under salt and control 
# treatments into a single script and reducing repetitive code.

require(tidyverse)
require(readxl)
require(ggpubr)
require(emmeans)
require(ggplot2)
require(dplyr)
require(kableExtra)
require(webshot)

# Which accession numbers are coastal and inland?
coastal_pops = c('BHE', 'SWB', 'PGR', 'HEC', 'OPB')
inland_pops = c('SWC', 'LMC', 'TOR', 'OAE', 'RGR')

# Read in leaf mass and area data
setwd("~/Documents/GitHub/Leaf-surface-traits-2024/")

mass_area <- read_excel("~/Documents/GitHub/Leaf-surface-traits-2024/Data/MP-AES_leaf_mass_area.xlsx",
                  sheet="Sheet1")
# get pop_code, treatment, and replicate columns from leaf_id
mass_area$pop_code = substr(mass_area$leaf_id, 1, 3)
mass_area$treatment = case_when(
  grepl(" R", mass_area$leaf_id) ~ "rinsed_salt",
  grepl(" S", mass_area$leaf_id) ~ "salt",
  grepl(" W", mass_area$leaf_id) ~ "water",
  .default = NA_character_
) %>% as.factor() %>% relevel(ref="water")
mass_area$ecotype = case_when(
  mass_area$pop_code %in% coastal_pops ~ "coastal",
  mass_area$pop_code %in% inland_pops ~ "inland",
  .default = NA_character_
) %>% as.factor()

# indicate ecotype and treatment combos for plotting
mass_area$eco_trt <- paste(mass_area$ecotype, mass_area$treatment, sep = " ")
mass_area$eco_trt <- factor(mass_area$eco_trt, levels = c("coastal rinsed_salt", 
                                              "coastal water", "coastal salt",
                                              "inland water", "inland salt"))
mass_area$replicate <- substr(mass_area$leaf_id, 6, 1000000L)
mass_area$mpaes_g <- as.numeric(mass_area$mpaes_g)

# get LMA and succulence
mass_area$lma <- mass_area$dry_weight_g*10000/mass_area$area_cm2 # convert to g/m^2
mass_area$succulence <- (mass_area$fresh_weight_g - mass_area$dry_weight_g)/mass_area$area_cm2

# Read in MP-AES data
mpaes <- read.csv("./Data/coastal_inland_exclusion_test_mpaes.csv", header=FALSE)
colnames(mpaes) <- mpaes[2,]
mpaes <- mpaes[-c(1,2),]
colnames(mpaes)[5] <- "element_label"
mpaes$Concentration <- as.numeric(mpaes$Concentration)
mpaes$Intensity <- as.numeric(mpaes$Intensity)

# plot standard curve for Na (concentration in ppm ~ intensity)
standards <- filter(mpaes, Type == "STD")
na_std <- filter(standards, element_label == "Na" & !is.na(Intensity) & !is.na(Concentration))
na_curve_out <- lm(data=na_std, Intensity ~ Concentration)
summary(na_curve_out)
coefficients(na_curve_out) # slope and intercept of standard curve

na_std_curve <- na_std %>% ggplot() +
  geom_point(aes(x = Concentration, y = Intensity))+
  # Labels
  scale_y_continuous(
    name = 'Intensity') +
  scale_x_continuous(name = "Concentration (ppm)")+
  geom_abline(aes(slope=coefficients(na_curve_out)[2],intercept=coefficients(na_curve_out)[1]), color="darkgrey")+
  theme_minimal() + ggtitle("Standard Curve for Na") +
  theme(
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

# export standard curve as svg
ggsave(na_std_curve, 
       filename = "Na_standard_curve.svg", 
       path = "./Results/Figures/SVGs_for_MS/",
       device="svg", width = 6, height = 6, units = "in")

# Put samples in a separate dataframe from blanks, standards, etc.
mpaes_samples <- mpaes[which(mpaes$Type == "Sample"),]

# merge mpaes results with leaf area and mass information
mpaes_samples_mass <- merge(mpaes_samples, mass_area, by.x="Label", by.y="leaf_id")

# Calculate molarity and % dry weight for each element
# add molar mass column
mpaes_samples_mass$molar_mass <- case_when(mpaes_samples_mass$element_label=="Ca" ~ 40.08,
                                           mpaes_samples_mass$element_label=="Co" ~ 58.93319,
                                           mpaes_samples_mass$element_label=="Cu" ~ 63.55,
                                           mpaes_samples_mass$element_label=="Fe" ~ 55.84,
                                           mpaes_samples_mass$element_label=="K" ~ 39.0983,
                                           mpaes_samples_mass$element_label=="Mg" ~ 24.305,
                                           mpaes_samples_mass$element_label=="Mn" ~ 54.93804,
                                           mpaes_samples_mass$element_label=="Na" ~ 22.989769,
                                           mpaes_samples_mass$element_label=="Ni" ~ 58.693,
                                           mpaes_samples_mass$element_label=="P" ~ 30.973,
                                           mpaes_samples_mass$element_label=="Zn" ~ 65.4,
                                           .default = NA
)
# density of mineralization solution (measured from samples after run completed) is 1.062 g/ml.
# dimensional analysis to calculate molarity in the original leaf from ppm in MP-AES
mpaes_samples_mass$molarity <- mpaes_samples_mass$Concentration * 5 * 1000 * mpaes_samples_mass$dry_weight_g/ 
  (mpaes_samples_mass$molar_mass * 1.062 * mpaes_samples_mass$mpaes_g *(mpaes_samples_mass$fresh_weight_g - mpaes_samples_mass$dry_weight_g) * 10^6)

# calculate moles of ion per gram of dry leaf mass
mpaes_samples_mass$umol_per_dry_gram <- (mpaes_samples_mass$Concentration * 5)/
  (mpaes_samples_mass$molar_mass*mpaes_samples_mass$mpaes_g * 1.062)

# how effective was the rinse?
mpaes_samples_mass %>%
  filter(element_label == "Na" & pop_code=="HEC") %>% ggplot() +
  aes(x = treatment, fill = treatment, y = molarity) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment') +
  scale_y_continuous(
    name = 'Concentration of Na (M)')+
  # Style
  scale_fill_manual(values = c('cyan3', 'yellow', "salmon")) +
  theme_minimal() + ggtitle("Test for Efficacy of Rinsing Surface Salt in HEC Leaves") +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
# the rinse was so effective!

# calculate umol of element that enters per unit leaf area
mpaes_samples_mass$umol_per_area <- (mpaes_samples_mass$Concentration * 5 * mpaes_samples_mass$dry_weight_g)/
  (mpaes_samples_mass$molar_mass*mpaes_samples_mass$mpaes_g * 1.062 * mpaes_samples_mass$area_cm2)

# Simplify things by showing only Na. Get rid of rinsed samples
mpaes_Na <- mpaes_samples_mass[which(mpaes_samples_mass$element_label == "Na" & 
                                       mpaes_samples_mass$treatment != "rinsed_salt"),]

## Fitting nested ANOVAs
m_nest_suc <- aov(data=mpaes_Na, succulence ~  treatment * ecotype/pop_code)
m_nest_lma <- aov(data=mpaes_Na, lma ~ treatment * ecotype/pop_code)
m_nest_M <- aov(data=mpaes_Na, molarity ~ treatment * ecotype/pop_code)
m_nest_umol <- aov(data=mpaes_Na, umol_per_area ~ treatment * ecotype/pop_code)


## Let's make tables!!

# tell kable not to plot NAs
options(knitr.kable.NA = '')

# Make a function that takes a nested ANOVA and outputs a formatted table
anovaTable <- function(model, title){
  # make vector of sources of variation
  sov <- c("Treatment", "Ecotype", "Treatment:Ecotype", "Treatment:Ecotype:Accession", "Error")
  # make vector to describe what effect sizes indicate
  effect_meaning <- c("Salt", "Inland", "Salt:Inland", "", "")
  # vector of main and nested effects
  effects <- c(as.numeric(model$coefficients[2]), as.numeric(model$coefficients[3]),
               as.numeric(model$coefficients[4]), "", "")
  tbl <- as.data.frame(cbind(sov, effect_meaning, effects, anova(model)$Df, 
                             anova(model)$F, anova(model)$`Pr(>F)`))
  colnames(tbl) <- c("Source of variation", "Effect of", "Effect size", "df", "F", "p-value")
  tbl %>% mutate(`Effect size` = round(as.numeric(`Effect size`), 5),
                 F = round(as.numeric(F), 2),
                 `p-value` = case_when(as.numeric(`p-value`) < 0.00001 ~ "<0.00001",
                                       .default = as.character(round(as.numeric(`p-value`), 5)))) %>%
    kbl(caption = title) %>% kable_classic() %>% 
    row_spec(which(as.numeric(tbl$`p-value`) < 0.05), bold=T) %>%
    save_kable(paste("./temp/", title, "_ecotype_anova_table.html", sep="")) %>% 
    webshot(paste("./Results/Tables/", title, "_ecotype_anova_table.pdf", sep=""), vwidth = 496)
  
}

anovaTable(m_nest_umol, "Sodium Exclusion")
anovaTable(m_nest_suc, "Succulence")
anovaTable(m_nest_lma, "LMA")
anovaTable(m_nest_M, "Concentration of Na (M)")







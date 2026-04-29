# Analysis of stomatal and hydrophobicity traits from coastal and inland accessions in a common garden.
# This script wrangles and cleans data, performs calculations related to stomatal 
# allocation and leaf water droplet adhesion assay, fits generalized linear mixed models for the differences 
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
require(glmmTMB)
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

gsw$pair <- case_when(gsw$pop == "OPB" | gsw$pop == "RGR" ~ "OPB/RGR",
                      gsw$pop == "HEC" | gsw$pop == "SWC" ~ "HEC/SWC",
                      gsw$pop == "BHE" | gsw$pop == "OAE" ~ "BHE/OAE",
                      gsw$pop == "SWB" | gsw$pop == "LMC" ~ "SWB/LMC",
                      gsw$pop == "PGR" | gsw$pop == "TOR" ~ "PGR/TOR",
                           .default=NA_character_) %>% as.factor()

# Fit models
#hydrophobicity models
out_ang_ad <- glmmTMB(data=contact_angle, contact_angle_ad ~ ecotype + (1|pair/pop.code))
summary(out_ang_ad)
out_ang_ab <- glmmTMB(data=contact_angle, contact_angle_ab ~ ecotype + (1|pair/pop.code))
summary(out_ang_ab)
out_adhesion <- glmmTMB(data=dat_area, adhesion_result ~ ecotype + (1|pair/pop_code))
summary(out_adhesion)

#stomatal trait models

# stomatal density both leaf sides
out_dens_ad <- glmmTMB(data=stom_all, stom_density_ad ~ ecotype + (1|pair/pop_code))
summary(out_dens_ad)
out_dens_ab <- glmmTMB(data=stom_all, stom_density_ab ~ ecotype + (1|pop_code))
summary(out_dens_ab)
# stomatal length both leaf sides
out_len_ad <- glmmTMB(data=stom_all, stomate_size_ad ~ ecotype + (1|pair/pop_code))
summary(out_len_ad)
out_len_ab <- glmmTMB(data=stom_all, stomate_size_ab ~ ecotype + (1|pair/pop_code))
summary(out_len_ab)

# fraction epidermis allocated to stomata both leaf sides
out_frac_ad <- glmmTMB(data=stom_all, stom_ad_fraction ~ ecotype + (1|pair/pop_code))
summary(out_frac_ad)
out_frac_ab <- glmmTMB(data=stom_all, stom_ab_fraction ~ ecotype + (1|pair/pop_code))
summary(out_frac_ab)

# adaxial stomatal conductance
out_gsw <- glmmTMB(data=gsw, gsw ~ ecotype + (1|pair/pop))
summary(out_gsw)

# amphistomy
out_amph <- glmmTMB(data=stom_all, amphistomy ~ ecotype + (1|pair/pop_code))
summary(out_amph)

# succulence model (for 15 reps w/o salt treatment)
out_succ_ecotype <- glmmTMB(data=dat_area, succulence ~ ecotype + (1|pair/pop_code))
summary(out_succ_ecotype)


ad_ang_plot <- emmeans(out_ang_ad, specs="ecotype") %>% as.data.frame() %>% ggplot() +
  aes(x=ecotype, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Ecotype",y="Adaxial Contact Angle (°)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")


ab_ang_plot <- emmeans(out_ang_ab, specs="ecotype") %>% as.data.frame() %>% ggplot() +
  aes(x=ecotype, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Ecotype",y="Abaxial Contact Angle (°)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

adhesion_plot <- emmeans(out_adhesion, specs="ecotype") %>% as.data.frame() %>% ggplot() +
  aes(x=ecotype, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Ecotype",y="Leaf Water Drop Adhesion Assay (g H2O / cm2)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position=c(0.75,0.9))

hydrophobicity_fig <- plot_grid(ad_ang_plot, ab_ang_plot, adhesion_plot, ncol = 3, labels = c("A", "B", "C"), label_size=18, align="hv")
save_plot("./Results/Figures/hydrophobicity_lsms_ecotype.svg", plot=hydrophobicity_fig, base_width = 9, base_height = 5)


# Adaxial stomatal trait figures for main text
ad_dens_plot <- emmeans(out_dens_ad, specs="ecotype") %>% as.data.frame() %>% ggplot() +
  aes(x=ecotype, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Ecotype",y="Adaxial Stomatal Density (mm-2)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

ad_len_plot <- emmeans(out_len_ad, specs="ecotype") %>% as.data.frame() %>% ggplot() +
  aes(x=ecotype, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Ecotype",y="Adaxial Stomatal Length (μm)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

ad_frac_plot <- emmeans(out_frac_ad, specs="ecotype") %>% as.data.frame() %>% ggplot() +
  aes(x=ecotype, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Ecotype",y="Stomatal Area / Epidermis Area")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position=c(0.75,0.77))

gsw_plot <- emmeans(out_gsw, specs="ecotype") %>% as.data.frame() %>% ggplot() +
  aes(x=ecotype, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Ecotype",y="gsw (μmol m-2 s-1)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

amphistomy_plot <- emmeans(out_amph, specs="ecotype") %>% as.data.frame() %>% ggplot() +
  aes(x=ecotype, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Ecotype",y="Amphistomy (adaxial/abaxial)")+
  geom_hline(yintercept=1, linetype="dashed")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

ad_stom_fig <- plot_grid(ad_dens_plot, ad_len_plot, ad_frac_plot, gsw_plot, amphistomy_plot, 
                         ncol = 3, nrow=2, labels = "AUTO", label_size=18,align="hv")
save_plot("./Results/Figures/stomata_ad_lsms_ecotype.svg", plot=ad_stom_fig, base_width = 9, base_height = 5)

# Abaxial stomatal trait figures for supplement
ab_dens_plot <- emmeans(out_dens_ab, specs="ecotype") %>% as.data.frame() %>% ggplot() +
  aes(x=ecotype, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Ecotype",y="Abaxial Stomatal Density (mm-2)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

ab_len_plot <- emmeans(out_len_ab, specs="ecotype") %>% as.data.frame() %>% ggplot() +
  aes(x=ecotype, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Ecotype",y="Abaxial Stomatal Length (μm)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

ab_frac_plot <- emmeans(out_frac_ab, specs="ecotype") %>% as.data.frame() %>% ggplot() +
  aes(x=ecotype, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Ecotype",y="Stomatal Area / Epidermis Area")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position=c(0.75, 0.9))

ab_stom_fig <- plot_grid(ab_dens_plot, ab_len_plot, ab_frac_plot, 
                         ncol = 3, labels = "AUTO", label_size=18,align="hv")
save_plot("./Results/Figures/stomata_ab_lsms_ecotype.svg", plot=ab_stom_fig, base_width = 9, base_height = 5)

succ_ecotype_plot <- emmeans(out_succ_ecotype, specs="ecotype") %>% as.data.frame() %>% ggplot() +
  aes(x=ecotype, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Ecotype",y="Succulence (g H2O / cm2)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")



# Make a function that takes a model and outputs a CSV of fixed effects, std error, and their significance
glmmtmbTableEco <- function(model, title){
  summary(model)
  # make vector of effects and what they mean
  effect <- c("Intercept(coastal)", "Ecotype(inland)")
  tbl <- as.data.frame(cbind(effect, coef(summary(model))$cond[,c(1:4)]))
  
  colnames(tbl) <- c("Effect", "Estimate", "SE", "z-value", "p-value")
  csv <- tbl %>% mutate(Estimate = round(as.numeric(Estimate), 4),
                        SE = round(as.numeric(SE), 4),
                        `z-value` = round(as.numeric(SE), 1),
                        `p-value` = case_when(as.numeric(`p-value`) < 0.0001 ~ "<0.0001",
                                              .default = as.character(round(as.numeric(`p-value`), 4))))
  write.csv(csv, file = paste("./Results/Tables/tables_CSV_format/", title, "_glmmTMB_table.csv", sep=""), row.names=FALSE)
}

glmmtmbTableEco(out_ang_ad, "Adaxial Contact Angle")
glmmtmbTableEco(out_ang_ab, "Abaxial Contact Angle")
glmmtmbTableEco(out_adhesion, "Leaf Water Drop Adhesion Assay")

glmmtmbTableEco(out_dens_ad, "Adaxial Stomatal Density")
glmmtmbTableEco(out_dens_ab, "Abaxial Stomatal Density")
glmmtmbTableEco(out_len_ad, "Adaxial Stomatal Length")
glmmtmbTableEco(out_len_ab, "Abaxial Stomatal Length")
glmmtmbTableEco(out_gsw, "gsw_ad")
glmmtmbTableEco(out_amph, "Amphistomy")
glmmtmbTableEco(out_frac_ad, "Fraction Stomata Adaxial")
glmmtmbTableEco(out_frac_ab, "Fraction Stomata Abaxial")
glmmtmbTableEco(out_succ_ecotype, "Succulence Ecotype")

# Make plots that show accession means for each trait!
# Make dataframes with means and standard errors for each trait

ang_ad_means <- contact_angle %>% group_by(pop.code) %>% 
  summarise(mean = mean(contact_angle_ad), se = sd(contact_angle_ad)/sqrt(length(contact_angle_ad)))
ang_ab_means <- contact_angle %>% filter(!is.na(contact_angle_ab)) %>% group_by(pop.code) %>% 
  summarise(mean = mean(contact_angle_ab), se = sd(contact_angle_ab)/sqrt(length(contact_angle_ab)))
adhesion_means <- dat_area %>% filter(!is.na(adhesion_result)) %>% group_by(pop_code) %>% 
  summarise(mean = mean(adhesion_result), se = sd(adhesion_result)/sqrt(length(adhesion_result)))

stom_dens_ad_means <- stom_all %>% group_by(pop_code) %>% 
  summarise(mean = mean(stom_density_ad), se = sd(stom_density_ad)/sqrt(length(stom_density_ad)))
stom_dens_ab_means <- stom_all %>% group_by(pop_code) %>% 
  summarise(mean = mean(stom_density_ab), se = sd(stom_density_ab)/sqrt(length(stom_density_ab)))

stom_len_ad_means <- stom_all %>% group_by(pop_code) %>% 
  summarise(mean = mean(stomate_size_ad), se = sd(stomate_size_ad)/sqrt(length(stomate_size_ad)))
stom_len_ab_means <- stom_all %>% group_by(pop_code) %>% 
  summarise(mean = mean(stomate_size_ab), se = sd(stomate_size_ab)/sqrt(length(stomate_size_ab)))

stom_frac_ad_means <- stom_all %>% group_by(pop_code) %>% 
  summarise(mean = mean(stom_ad_fraction), se = sd(stom_ad_fraction)/sqrt(length(stom_ad_fraction)))
stom_frac_ab_means <- stom_all %>% group_by(pop_code) %>% 
  summarise(mean = mean(stom_ab_fraction), se = sd(stom_ab_fraction)/sqrt(length(stom_ab_fraction)))

gsw_means <- gsw %>% group_by(pop) %>% 
  summarise(mean = mean(gsw), se = sd(gsw)/sqrt(length(gsw)))

amph_means <- stom_all %>% group_by(pop_code) %>%
  summarise(mean = mean(amphistomy), se = sd(amphistomy)/sqrt(length(amphistomy)))

ecotype_vec <- c("coastal", "coastal", "inland", "inland", "coastal", "coastal", "inland", "coastal",
                 "inland", "inland")

shapes <- c(21,22,25,21,23,24,23,25,22,24)

acc_ang_ad_plot <- ang_ad_means %>% ggplot() + aes(x=ecotype_vec, y=mean, fill=ecotype_vec) +
  scale_fill_manual(values=c('#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85'))+
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se, col=ecotype_vec), 
                  position=position_jitter(width=0.3), cex=1,
                  linetype='solid', shape=shapes) +
  labs(x="Ecotype", y="Adaxial Contact Angle (°)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

acc_ang_ab_plot <- ang_ab_means %>% ggplot() + aes(x=ecotype_vec, y=mean, fill=ecotype_vec) +
  scale_fill_manual(values=c('#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85'))+
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se, col=ecotype_vec), 
                  position=position_jitter(width=0.3), cex=1,
                  linetype='solid', shape=shapes) +
  labs(x="Ecotype", y="Abaxial Contact Angle (°)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

acc_adhesion_plot <- adhesion_means %>% ggplot() + aes(x=ecotype_vec, y=mean, fill=ecotype_vec) +
  scale_fill_manual(values=c('#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85'))+
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se, col=ecotype_vec), 
                  position=position_jitter(width=0.3), cex=1,
                  linetype='solid', shape=shapes) +
  labs(x="Ecotype", y="Leaf Water Drop Adhesion Result (g H2O / cm2)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position=c(0.25,0.9))

hydrophobicity_supp_acc_fig <- plot_grid(acc_ang_ad_plot, acc_ang_ad_plot, acc_adhesion_plot, ncol = 3, labels = "AUTO", label_size=18, align="hv")
save_plot("./Results/Figures/hydrophobicity_acc_supp_fig.svg", plot=hydrophobicity_supp_acc_fig, base_width = 9, base_height = 5)

acc_stom_dens_ad_plot <- stom_dens_ad_means %>% ggplot() + aes(x=ecotype_vec, y=mean, fill=ecotype_vec) +
  scale_fill_manual(values=c('#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85'))+
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se, col=ecotype_vec), 
                  position=position_jitter(width=0.3), cex=1,
                  linetype='solid', shape=shapes) +
  labs(x="Ecotype", y="Adaxial Stomatal Density (mm-2)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

acc_stom_dens_ab_plot <- stom_dens_ab_means %>% ggplot() + aes(x=ecotype_vec, y=mean, fill=ecotype_vec) +
  scale_fill_manual(values=c('#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85'))+
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se, col=ecotype_vec), 
                  position=position_jitter(width=0.3), cex=1,
                  linetype='solid', shape=shapes) +
  labs(x="Ecotype", y="Abaxial Stomatal Density (mm-2)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

acc_stom_len_ad_plot <- stom_len_ad_means %>% ggplot() + aes(x=ecotype_vec, y=mean, fill=ecotype_vec) +
  scale_fill_manual(values=c('#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85'))+
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se, col=ecotype_vec), 
                  position=position_jitter(width=0.3), cex=1,
                  linetype='solid', shape=shapes) +
  labs(x="Ecotype", y="Adaxial Stomatal Length (µm)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

acc_stom_len_ab_plot <- stom_len_ab_means %>% ggplot() + aes(x=ecotype_vec, y=mean, fill=ecotype_vec) +
  scale_fill_manual(values=c('#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85'))+
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se, col=ecotype_vec), 
                  position=position_jitter(width=0.3), cex=1,
                  linetype='solid', shape=shapes) +
  labs(x="Ecotype", y="Abaxial Stomatal Length (µm)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

acc_frac_ad_plot <- stom_frac_ad_means %>% ggplot() + aes(x=ecotype_vec, y=mean, fill=ecotype_vec) +
  scale_fill_manual(values=c('#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85'))+
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se, col=ecotype_vec), 
                  position=position_jitter(width=0.3), cex=1,
                  linetype='solid', shape=shapes) +
  labs(x="Ecotype", y="Stomatal Area / Epidermis Area (adaxial)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position=c(0.7,0.8))


acc_frac_ab_plot <- stom_frac_ab_means %>% ggplot() + aes(x=ecotype_vec, y=mean, fill=ecotype_vec) +
  scale_fill_manual(values=c('#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85'))+
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se, col=ecotype_vec), 
                  position=position_jitter(width=0.3), cex=1,
                  linetype='solid', shape=shapes) +
  labs(x="Ecotype", y="Stomatal Area / Epidermis Area (abaxial)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

acc_gsw_plot <- gsw_means %>% ggplot() + aes(x=ecotype_vec, y=mean, fill=ecotype_vec) +
  scale_fill_manual(values=c('#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85'))+
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se, col=ecotype_vec), 
                  position=position_jitter(width=0.3), cex=1,
                  linetype='solid', shape=shapes) +
  labs(x="Ecotype", y="gsw (µmol m-2 s-1)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

acc_amph_plot <- amph_means %>% ggplot() + aes(x=ecotype_vec, y=mean, fill=ecotype_vec) +
  scale_fill_manual(values=c('#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85'))+
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se, col=ecotype_vec), 
                  position=position_jitter(width=0.3), cex=1,
                  linetype='solid', shape=shapes) +
  labs(x="Ecotype", y="Amphistomy (adaxial/abaxial)")+
  theme_bw()+
  geom_hline(yintercept=1, linetype="dashed")+
  theme(axis.text = element_text(size = 16), legend.position="none")

stomata_acc_supp_fig <- plot_grid(acc_stom_dens_ad_plot, acc_stom_len_ad_plot, acc_frac_ad_plot,
                                  acc_stom_dens_ab_plot, acc_stom_len_ab_plot, acc_frac_ab_plot,
                                  acc_gsw_plot, acc_amph_plot,
                                  ncol = 3, labels = "AUTO", label_size=18, align="hv")

save_plot("./Results/Figures/stomata_acc_supp_fig.svg", plot=stomata_acc_supp_fig, base_width = 9, base_height = 9)


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



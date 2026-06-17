# Sodium treatment response analysis: includes leaf mass, area, and MP-AES, only analyses
# related to MP-AES and succulence.

# Consolidating analyses of coastal/inland ecotypes under salt and control 
# treatments into a single script and reducing repetitive code.
  
  require(tidyverse)
  require(readxl)
  require(ggpubr)
  require(cowplot)
  require(emmeans)
  require(ggplot2)
  require(dplyr)
  require(kableExtra)
  require(webshot)
  
  # Which accession numbers are coastal and inland?
  coastal_pops = c('BHE', 'SWB', 'PGR', 'HEC', 'OPB')
  inland_pops = c('SWC', 'LMC', 'TOR', 'OAE', 'RGR')
  
  # Read in leaf mass and area data
  setwd("~/Documents/GitHub/Salt-spray-adaptation-2025/")
  
  mass_area <- read_excel("~/Documents/GitHub/Salt-spray-adaptation-2025/Data/MP-AES_leaf_mass_area.xlsx",
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
  mpaes <- read_excel("./Data/coastal_inland_exclusion_test_mpaes.xlsx", sheet="Sheet1")
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

# Add pair variable to dataframe

mpaes_Na$pair <- case_when(mpaes_Na$pop_code == "OPB" | mpaes_Na$pop_code == "RGR" ~ "OPB/RGR",
                           mpaes_Na$pop_code == "HEC" | mpaes_Na$pop_code == "SWC" ~ "HEC/SWC",
                           mpaes_Na$pop_code == "BHE" | mpaes_Na$pop_code == "OAE" ~ "BHE/OAE",
                           mpaes_Na$pop_code == "SWB" | mpaes_Na$pop_code == "LMC" ~ "SWB/LMC",
                           mpaes_Na$pop_code == "PGR" | mpaes_Na$pop_code == "TOR" ~ "PGR/TOR",
                           .default=NA_character_) %>% as.factor()


## Fitting linear models

out_suc <- glmmTMB(data=mpaes_Na, succulence ~ ecotype*treatment + (1|pair/pop_code))
out_lma <- glmmTMB(data=mpaes_Na, lma ~ ecotype*treatment + (1|pair/pop_code))
out_excl <- glmmTMB(data=mpaes_Na, umol_per_area ~ ecotype*treatment + (1|pair/pop_code))

# LMA had wacky responses. Let's see if mass and area respond individually:
out_mass <- glmmTMB(data=mpaes_Na, dry_weight_g ~ ecotype*treatment + (1|pair/pop_code))
summary(out_mass)
out_area <- glmmTMB(data=mpaes_Na, area_cm2 ~ ecotype*treatment + (1|pair/pop_code))
summary(out_area)
# For molarity only, removed pair from model due to non-Hessian convergence warning
# Random effects explained very little variance and fixed effects estimates were the same
out_M <- glmmTMB(data=mpaes_Na, molarity ~ ecotype*treatment + (1|pop_code))




# Plot EMMs for main text Fig 3.
M_plot <- emmeans(out_M, specs=c("ecotype", "treatment")) %>% as.data.frame() %>% ggplot() +
  aes(x=treatment, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Spray Treatment",y="Concentration of Na (M)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")
  
umol_plot <- emmeans(out_excl, specs=c("ecotype", "treatment")) %>% as.data.frame() %>% ggplot() +
  aes(x=treatment, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Spray Treatment",y="umol Na per cm^2 leaf")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

succ_plot <- emmeans(out_suc, specs=c("ecotype", "treatment")) %>% as.data.frame() %>% ggplot() +
  aes(x=treatment, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Spray Treatment",y="Succulence (g H2O / cm^2)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position=c(.75,.90))

salt_excl_fig <- plot_grid(M_plot, umol_plot, succ_plot, ncol = 3, labels = c("A", "B", "C"), label_size=18, align="hv")

save_plot("./Results/Figures/salt_exclusion_lsms.svg", plot=salt_excl_fig, base_width = 9, base_height = 5)

lma_plot <- emmeans(out_lma, specs=c("ecotype", "treatment")) %>% as.data.frame() %>% ggplot() +
  aes(x=treatment, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Spray Treatment",y="LMA (g / m2)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="right")
save_plot("./Results/Figures/lma_lsms.svg", plot=lma_plot, base_width = 5, base_height = 5)


# Plot accession means and standard errors for supplemental salt spray exclusion figure
# Make dataframes with means and standard errors for each trait

acc_excl_means <- mpaes_Na %>% group_by(pop_code, treatment) %>% summarise(mean_umol = mean(umol_per_area),
                                              se_umol = sd(umol_per_area)/sqrt(length(umol_per_area)))

acc_excl_means$ecotype <- case_when(acc_excl_means$pop_code %in% coastal_pops ~ "coastal",
                                    acc_excl_means$pop_code %in% inland_pops ~ "inland")

acc_M_means <- mpaes_Na %>% filter(!is.na(molarity)) %>% group_by(pop_code, treatment) %>% summarise(mean_M = mean(molarity),
                                                                        se_M = sd(molarity)/sqrt(length(molarity)))
acc_M_means$ecotype <- case_when(acc_M_means$pop_code %in% coastal_pops ~ "coastal",
                                 acc_M_means$pop_code %in% inland_pops ~ "inland")

acc_succ_means <- mpaes_Na %>% filter(!is.na(succulence)) %>% group_by(pop_code, treatment) %>% summarise(mean_succ = mean(succulence),
                                                                                                     se_succ = sd(succulence)/sqrt(length(succulence)))
acc_succ_means$ecotype <- case_when(acc_succ_means$pop_code %in% coastal_pops ~ "coastal",
                                    acc_succ_means$pop_code %in% inland_pops ~ "inland")

acc_lma_means <- mpaes_Na %>% filter(!is.na(lma)) %>% group_by(pop_code, treatment) %>% summarise(mean_lma = mean(lma),
                                                                                                          se_lma = sd(lma)/sqrt(length(lma)))
acc_lma_means$ecotype <- case_when(acc_lma_means$pop_code %in% coastal_pops ~ "coastal",
                                   acc_lma_means$pop_code %in% inland_pops ~ "inland")

shapes <- c(21,21,22,22,25,25,21,21,23,23,24,24,23,23,25,25,22,22,24,24)

acc_M_plot <- acc_M_means %>% ggplot() + aes(x=treatment, y=mean_M, fill=ecotype)+
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(aes(ymin = mean_M - se_M, ymax = mean_M + se_M, col=ecotype), 
                  position=position_jitter(width=0.3), cex=1,
                  linetype='solid', shape=shapes) +
  labs(x="Spray Treatment",y="Concentration of Na (M)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")


acc_excl_plot <- acc_excl_means %>% ggplot() + aes(x=treatment, y=mean_umol, fill=ecotype) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(aes(ymin = mean_umol - se_umol, ymax = mean_umol + se_umol, col=ecotype), 
                  position=position_jitter(width=0.3), cex=1,
                  linetype='solid', shape=shapes) +
labs(x="Spray Treatment",y="μmol Na per cm2 leaf")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

acc_succ_plot <- acc_succ_means %>% ggplot() + aes(x=treatment, y=mean_succ, fill=ecotype) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(aes(ymin = mean_succ - se_succ, ymax = mean_succ + se_succ, col=ecotype), 
                  position=position_jitter(width=0.3), cex=1,
                  linetype='solid', shape=shapes) +
  labs(x="Spray Treatment",y="Succulence (g H2O / cm2)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position=c(.75,.90))

salt_excl_supp_fig <- plot_grid(acc_M_plot, acc_excl_plot, acc_succ_plot, ncol = 3, labels = c("A", "B", "C"), label_size=18, align="hv")
save_plot("./Results/Figures/salt_exclusion_accession_means.svg", plot=salt_excl_supp_fig, base_width = 9, base_height = 5)

acc_lma_plot <- acc_lma_means %>% ggplot() + aes(x=treatment, y=mean_lma, fill=ecotype)+
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(aes(ymin = mean_lma - se_lma, ymax = mean_lma + se_lma, col=ecotype), 
                  position=position_jitter(width=0.3), cex=1,
                  linetype='solid', shape=shapes) +
  labs(x="Spray Treatment",y="LMA (g/m2)")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position=c(.15,.90))

save_plot("./Results/Figures/lma_accession_means.svg", plot=acc_lma_plot, base_width = 5, base_height = 5)



## Let's make tables!!


# tell kable not to plot NAs
options(knitr.kable.NA = '')

# Make a function that takes a model and outputs a CSV of fixed effects, std error, and their significance
glmmtmbTable <- function(model, title){
  summary(model)
  # make vector of effects and what they mean
  effect <- c("Intercept(coastal water)", "Ecotype(inland)", "Treatment(salt)", "Treatment(salt):Ecotype(inland)")
  tbl <- as.data.frame(cbind(effect, coef(summary(model))$cond[,c(1:4)]))
  
  colnames(tbl) <- c("Effect", "Estimate", "SE", "z-value", "p-value")
  csv <- tbl %>% mutate(Estimate = round(as.numeric(Estimate), 4),
                 SE = round(as.numeric(SE), 4),
                 `z-value` = round(as.numeric(SE), signif(3)),
                 `p-value` = case_when(as.numeric(`p-value`) < 0.0001 ~ "<0.0001",
                                       .default = as.character(round(as.numeric(`p-value`), 4))))
  write.csv(csv, file = paste("./Results/Tables/tables_CSV_format/", title, "_glmmTMB_table.csv", sep=""), row.names=FALSE)
}
glmmtmbTable(out_M, "molarity")
glmmtmbTable(out_excl, "umol_per_area")
glmmtmbTable(out_suc, "succulence")
glmmtmbTable(out_lma, "lma")


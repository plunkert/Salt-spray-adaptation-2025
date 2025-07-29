# 4/20/25. Analyze fresh leaf mass, dry leaf mass, and leaf area data for water and salt 
# treated leaves used for MP-AES analysis.

# 5/3/25. Combine with the MP-AES dataset

# load required packages
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)

# list coastal, inland pops
coastal_pops = c('PGR', 'BHE', 'SWB', 'HEC', 'OPB')
inland_pops = c('TOR', 'OAE', 'LMC', 'SWC', 'RGR')

# read in leaf mass and area data
setwd("~/Documents/GitHub/Leaf-surface-traits-2024/")
dat <- read_excel("~/Documents/GitHub/Leaf-surface-traits-2024/Data/MP-AES_leaf_mass_area.xlsx",
                  sheet="Sheet1")

# get pop_code, treatment, and replicate columns from leaf_id
dat$pop_code = substr(dat$leaf_id, 1, 3)
dat$treatment = case_when(
     grepl(" R", dat$leaf_id) ~ "rinsed_salt",
     grepl(" S", dat$leaf_id) ~ "salt",
     grepl(" W", dat$leaf_id) ~ "water",
     .default = NA_character_
   ) %>% as.factor() %>% relevel(ref="water")
dat$ecotype = case_when(
      dat$pop_code %in% coastal_pops ~ "coastal",
      dat$pop_code %in% inland_pops ~ "inland",
      .default = NA_character_
    ) %>% as.factor()

# indicate membership into latitudinal pairs
# let's try indicating latitudinal pair instead of accession per se
dat$pair <- case_when(dat$pop_code == "OPB" | dat$pop_code == "RGR" ~ "OPB/RGR",
                      dat$pop_code == "HEC" | dat$pop_code == "SWC" ~ "HEC/SWC",
                      dat$pop_code == "BHE" | dat$pop_code == "OAE" ~ "BHE/OAE",
                      dat$pop_code == "SWB" | dat$pop_code == "LMC" ~ "SWB/LMC",
                      dat$pop_code == "PGR" | dat$pop_code == "TOR" ~ "PGR/TOR",
                                  .default=NA_character_) %>% as.factor()


# indicate ecotype and treatment combos for plotting
dat$eco_trt <- paste(dat$ecotype, dat$treatment, sep = " ")
dat$eco_trt <- factor(dat$eco_trt, levels = c("coastal rinsed_salt", 
                                              "coastal water", "coastal salt",
                                              "inland water", "inland salt"))
dat$replicate <- substr(dat$leaf_id, 6, 1000000L)
dat$mpaes_g <- as.numeric(dat$mpaes_g)

# get LMA and succulence
dat$lma <- dat$dry_weight_g*10000/dat$area_cm2 # convert to g/m^2
dat$succulence <- (dat$fresh_weight_g - dat$dry_weight_g)/dat$area_cm2

# let's see if succulence is affected by salt treatment!
inland_succulence <- dat %>%
  filter(treatment != "rinsed_salt" & ecotype == "inland") %>%
  ggplot() +
  aes(x = treatment, fill = treatment, y = succulence) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment') +
  scale_y_continuous(
    name = 'Succulence (g H2O / cm^2)', limits = c(0, 0.045), 
  ) +
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal() + ggtitle("Inland") +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

succulence <- dat %>% filter(treatment != "rinsed_salt") %>%
  ggplot() +
  aes(x = eco_trt, fill = treatment, y = succulence) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment and Ecotype') +
  scale_y_continuous(
    name = 'Succulence (g H2O / cm^2)', limits=c(0,0.045) 
  ) +
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=16),
    axis.title = element_text(size=16),
  )

ggsave(
  filename = 'Succulence_ecotype.png', 
  plot = succulence,
  device = 'png',
  path = './Results/Figures/',
  bg = 'white'
)

# ooookay, opposite of the pilot study that just had PGR, where succulence increased with salt treatment

# How does each accession's succulence respond to salt treatment?

dat %>%
  filter(pop_code == "SWB" & treatment != "rinsed_salt") %>%
  ggplot() +
  aes(x = treatment, fill = treatment, y = succulence) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment') +
  scale_y_continuous(
    name = 'Succulence (g H2O / cm^2)', limits = c(0, 0.045) 
  ) +
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal()+
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
# PGR, HEC, SWC, RGR, LMC, OAE, TOR decreases succulence with salt treatment
# BHE, SWB, OPB barely/do not respond

# Is LMA affected by salt treatment? On this timescale, I don't expect it to be

lma <- dat %>%
  filter(treatment != "rinsed_salt") %>%
  ggplot() +
  aes(x = eco_trt, fill = treatment, y = lma) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment') +
  scale_y_continuous(
    name = 'LMA (g/cm^2)')+
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

mass <- dat %>%
  filter(treatment != "rinsed_salt") %>% ggplot() +
  aes(x = eco_trt, fill = treatment, y = dry_weight_g) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment') +
  scale_y_continuous(
    name = 'Dry mass (g)')+
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

area <- dat %>%
  filter(treatment != "rinsed_salt") %>% ggplot() +
  aes(x = eco_trt, fill = treatment, y = area_cm2) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment') +
  scale_y_continuous(
    name = 'Area (cm2)')+
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )


lm(data=dat, dry_weight_g ~ treatment*ecotype) %>% summary()

m_nest <- aov(data=dat, dry_weight_g ~ treatment * ecotype/pop_code)
summary(m_nest)
m_nest <- aov(data=dat, area_cm2 ~ treatment * ecotype/pop_code)
summary(m_nest)

# how you'd fit a model with random effects
#lme(dat, concentration ~ treatment * ecotype + (1|pop_code))

# check for effect of latitudinal pair following Sylvie's advice
#library(glmmTMB)
#m <- glmmTMB(data=dat, lma ~ ecotype + pair, family="gaussian")
#summary(m)


ggarrange(mass, area)
ggsave(
  filename = 'lma_ecotype_trt.png', 
  plot = lma,
  device = 'png',
  path = './Results/Figures/',
  bg = 'white'
)

# Weird! Is this a reasonable increase in LMA to be caused by the saltwater alone?
dat %>% group_by(treatment, ecotype) %>% summarise(mean=mean(lma)) 

# inlands increase LMA by 0.00189 g/cm2, which is 32.34 umol NaCl
# But only ~6umol Na per cm^2 in inland salt treatment. Maybe a lot more
# chloride enters? Or is the plant actually putting more resources into the
# leaf on that timescale?

# what's up with bimodal dist. of coastal LMA?
View(dat[which(dat$treatment == "water"),])

# just OPB and HEC making very expensive leaves. Latitudinal thing from Zambiasi & Lowry 2024?
# consistent with later flowering in high latitude coastals

# read in MP-AES data
mpaes <- read.csv("./Data/coastal_inland_exclusion_test_mpaes.csv", header=FALSE)
colnames(mpaes) <- mpaes[2,]
mpaes <- mpaes[-c(1,2),]
colnames(mpaes)[5] <- "element_label"
mpaes$Concentration <- as.numeric(mpaes$Concentration)
mpaes$Intensity <- as.numeric(mpaes$Intensity)

# plot standard curve for Na (concentration in ppm ~ intensity)
standards <- filter(mpaes, Type == "STD")
na_std <- filter(standards, element_label == "Na" & !is.na(Intensity) & !is.na(Concentration))
k_std <- filter(standards, element_label == "K" & !is.na(Intensity) & !is.na(Concentration))
ca_std <- filter(standards, element_label == "Ca" & !is.na(Intensity) & !is.na(Concentration))

na_curve_out <- lm(data=na_std, Intensity ~ Concentration)
k_curve_out <- lm(data=k_std, Intensity ~ Concentration)
ca_curve_out <- lm(data=ca_std, Intensity ~ Concentration)

coefficients(na_curve_out) # slope and intercept of standard curve
coefficients(k_curve_out)
coefficients(ca_curve_out)

na_std_curve <- na_std %>% ggplot() +
  geom_point(aes(x = Concentration, y = Intensity))+
  # Labels
  scale_y_continuous(
    name = 'Intensity') +
  scale_x_continuous(name = "Concentration (ppm)")+
  # Style
  theme_minimal() + ggtitle("Standard Curve for Na") +
  theme(
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

k_std_curve <- filter(standards, element_label == "K") %>% ggplot() +
  geom_point(aes(x = Concentration, y = Intensity))+
  # Labels
  scale_y_continuous(
    name = 'Intensity') +
  scale_x_continuous(name = "Concentration (ppm)")+
  # Style
  theme_minimal() + ggtitle("Standard Curve for K") +
  theme(
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

ca_std_curve <- filter(standards, element_label == "Ca") %>% ggplot() +
  geom_point(aes(x = Concentration, y = Intensity))+
  # Labels
  scale_y_continuous(
    name = 'Intensity') +
  scale_x_continuous(name = "Concentration (ppm)")+
  # Style
  theme_minimal() + ggtitle("Standard Curve for Ca") +
  theme(
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

ggarrange(na_std_curve, k_std_curve, ca_std_curve, ncol=3)

# Conclude: We can use this MP-AES run to measure Na, K, and Ca concentrations

# Put samples in a separate dataframe from blanks, standards, etc.
mpaes_samples <- mpaes[which(mpaes$Type == "Sample"),]

# merge mpaes results with leaf area and mass information
mpaes_samples_mass <- merge(mpaes_samples, dat, by.x="Label", by.y="leaf_id")

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

# Some exploration: plot Na concentration in ppm as a function of treatment
mpaes_samples_mass %>%
  filter(treatment != "rinsed_salt" & element_label == "Na") %>%
  ggplot() +
  aes(x = treatment, fill = treatment, y = Concentration) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment') +
  scale_y_continuous(
    name = 'Concentration (ppm)')+
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
# salt treatment worked!

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

# let's remove the rinsed ones, that's all we needed to verify
mpaes_samples_mass <- filter(mpaes_samples_mass, treatment != "rinsed_salt")

# calculate umol of element that enters per unit leaf area
mpaes_samples_mass$umol_per_area <- (mpaes_samples_mass$Concentration * 5 * mpaes_samples_mass$dry_weight_g)/
  (mpaes_samples_mass$molar_mass*mpaes_samples_mass$mpaes_g * 1.062 * mpaes_samples_mass$area_cm2)

# Simplify things by showing only Na
mpaes_Na <- mpaes_samples_mass[which(mpaes_samples_mass$element_label == "Na"),]


# test whether treatment and ecotype interactively affect succulence

# can I model effect of treatment * (pop code nested w/in ecotype)?
m_nest_suc <- aov(data=mpaes_Na, succulence ~  treatment * ecotype/pop_code)
summary(m_nest_suc)
coefficients(m_nest_suc)

# Let's try a plot showing LSMs
shapes <- rep(c(21, 22, 23, 24, 25, 25, 21, 23, 22, 24), 2)

lsms <- emmip(m_nest_suc, pop_code ~ ecotype*treatment,CIs=TRUE, plotit=FALSE)

lsms$color <- ifelse(lsms$ecotype == "coastal", '#514663', '#cacf85')

succulence_plot <- emmip(m_nest_suc, pop_code ~ ecotype*treatment,CIs=TRUE, col = lsms$color,
                         dotarg = list(shape = shapes, cex = 5, col="black",
                                       fill = lsms$color), 
                        linearg = list(linetype="solid", col="black"), type = "response", nesting.order=TRUE, plotit = T, dodge = 0.4) +
  ylab('Succulence (g H2O / cm^2)') + xlab("Spray Treatment and Ecotype") +
  theme(axis.text = element_text(size = 12)) +
  scale_x_discrete(limits = c("coastal water", "coastal salt", "inland water", "inland salt"))


m_nest_lma <- aov(data=mpaes_Na, lma ~ treatment * ecotype/pop_code)
summary(m_nest_lma)
coefficients(m_nest_lma)

lma_lsm_plot <- emmip(m_nest_lma, pop_code ~ ecotype*treatment,CIs=TRUE, col = lsms$color,
                         dotarg = list(shape = shapes, cex = 5, col="black",
                                       fill = lsms$color), 
                         linearg = list(linetype="solid", col="black"), type = "response", plotit = T, dodge = 0.4) +
  ylab('LMA (g/m^2)') + xlab("Spray Treatment and Ecotype") +
  theme(axis.text = element_text(size = 12)) +
  scale_x_discrete(limits = c("coastal water", "coastal salt", "inland water", "inland salt"))

ggsave(lma_lsm_plot, 
       filename = "lma_lsm_ecotype_salt.svg", 
       path = "./Results/Figures/SVGs_for_MS/",
       device="svg", width = 4, height = 3.5, units = "in")


# Plot Na for each ecotype and treatment combination
M <- mpaes_Na %>% ggplot() +
  aes(x = eco_trt, fill = treatment, y = molarity) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment and Ecotype') +
  scale_y_continuous(
    name = 'Concentration of Na (M)') +
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=16),
    axis.title = element_text(size=16),
  )

# plot 

# Plot LSMs for each accession in salt and water
lma_plot <- emmip(m_nest_lma, pop_code ~ ecotype*treatment,CIs=TRUE, col = lsms$color,
                  dotarg = list(shape = shapes, cex = 5, col="black",
                                fill = lsms$color), 
                  linearg = list(linetype="solid", col="black"), type = "response", nesting.order=TRUE, plotit = T, dodge = 0.4) +
  ylab('Succulence (g H2O / cm^2)') + xlab("Spray Treatment and Ecotype") +
  theme(axis.text = element_text(size = 12)) +
  scale_x_discrete(limits = c("coastal water", "coastal salt", "inland water", "inland salt"))

# test for statistical interaction!!

m_nest_M <- aov(data=mpaes_Na, molarity ~ treatment * ecotype/pop_code)
summary(m_nest_M)

M_plot <- emmip(m_nest_M, pop_code ~ ecotype*treatment,CIs=TRUE, col = lsms$color,
                         dotarg = list(shape = shapes, cex = 5, col="black",
                                       fill = lsms$color), 
                         linearg = list(linetype="solid", col="black"), type = "response", nesting.order=TRUE, plotit = T, dodge = 0.4) +
  ylab('Concentration of Na (M)') + xlab("Spray Treatment and Ecotype") +
  theme(axis.text = element_text(size = 12)) +
  scale_x_discrete(limits = c("coastal water", "coastal salt", "inland water", "inland salt"))


# But coastals are also waterier, which would reduce Na concentration.
# Let's plot micromoles Na per unit leaf area to understand salt exclusion

mpaes_samples_mass$umol_per_area <- (mpaes_samples_mass$Concentration * 5 * mpaes_samples_mass$dry_weight_g)/
  (mpaes_samples_mass$molar_mass*mpaes_samples_mass$mpaes_g * 1.062 * mpaes_samples_mass$area_cm2)

umol <- mpaes_Na %>% ggplot() +
  aes(x = eco_trt, fill = treatment, y = umol_per_area) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment and Ecotype') +
  scale_y_continuous(
    name = 'umol Na per cm^2 leaf') +
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=16),
    axis.title = element_text(size=16),
  )

m_nest_umol <- aov(data=mpaes_Na, umol_per_area ~ treatment * ecotype/pop_code)
summary(m_nest_umol)

umol_plot <- emmip(m_nest_umol, pop_code ~ ecotype*treatment,CIs=TRUE, col = lsms$color,
                dotarg = list(shape = shapes, cex = 5, col="black",
                              fill = lsms$color), 
                linearg = list(linetype="solid", col="black"), type = "response", nesting.order=TRUE, plotit = T, dodge = 0.4) +
  ylab('umol Na per cm^2 leaf') + xlab("Spray Treatment and Ecotype") +
  theme(axis.text = element_text(size = 12)) +
  scale_x_discrete(limits = c("coastal water", "coastal salt", "inland water", "inland salt"))

ggsave(ggarrange(M_plot, umol_plot, succulence_plot, 
                 nrow=1, ncol=3), 
       filename = "salt_response_lsms_ecotype.svg", 
       path = "./Results/Figures/SVGs_for_MS/",
       device="svg", width = 10, height = 3.5, units = "in")


# perform t-tests to see if differences in salt spray exclusion as umol per unit leaf area
# are consistent among pairs of populations
ttests <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(ttests) <- c('coastal_pop', 'inland_pops', 'mean_coastal', 'mean_inland', 't', 'df', 'unadjustedP')
for (i in 1:5) {
  pair <- mpaes_Na[which((mpaes_Na$pop_code==coastal_pops[i] | mpaes_Na$pop_code==inland_pops[i]) & mpaes_Na$treatment == "salt"),]
  test <- t.test(data = pair, umol_per_area ~ ecotype)
  ttests[i,] <- c(coastal_pops[i], inland_pops[i], test$estimate[1], test$estimate[2], test$statistic, test$parameter, test$p.value)
}

ttests$adjustedP <- p.adjust(c(ttests$unadjustedP), method = "BH")

# When we make individual pairwise comparisons as t-tests between paired populations with Benjamini-Hochberg 
# there's no significantly different

# Does each coastal exclude salt better than its respective inland?

# make a function to plot how each pair of accessions responds to salt
plot_each_pair <- function(coastal, inland, var, ylab, ylim) {
  pair <- filter(mpaes_Na, pop_code == inland | pop_code == coastal)
  pair %>% ggplot() +
    aes(x = eco_trt, fill = treatment, y = pair[[var]]) +
    geom_boxplot(outliers = F) +
    geom_jitter(position=position_jitter(0.1)) +
    # Labels
    scale_x_discrete(name = 'Spray treatment') +
    scale_y_continuous(
      name = ylab, limits = c(0, ylim)
    )+
    ggtitle(paste(coastal, " / ", inland))+
    # Style
    scale_fill_manual(values = c('cyan3', 'salmon')) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_text(size=12),
      axis.title = element_text(size=12)
    )
}

p1 <- plot_each_pair(coastal = "PGR", inland = "TOR", var="umol_per_area", ylab = "umol Na per cm2", ylim=12.5)
p2 <- plot_each_pair(coastal = "BHE", inland = "OAE", var="umol_per_area", ylab = "umol Na per cm2", ylim=12.5)
p3 <- plot_each_pair(coastal = "SWB", inland = "LMC", var="umol_per_area", ylab = "umol Na per cm2", ylim=12.5)
p4 <- plot_each_pair(coastal = "HEC", inland = "SWC", var="umol_per_area", ylab = "umol Na per cm2", ylim=12.5)
p5 <- plot_each_pair(coastal = "OPB", inland = "RGR", var="umol_per_area", ylab = "umol Na per cm2", ylim=12.5)
ggarrange(p1, p2, p3, p4, p5, nrow=3, ncol=2)

p1 <- plot_each_pair(coastal = "PGR", inland = "TOR", var="succulence", ylab = "succulence (g H2O/cm2)", ylim=0.045)
p2 <- plot_each_pair(coastal = "BHE", inland = "OAE", var="succulence", ylab = "succulence (g H2O/cm2)", ylim=0.045)
p3 <- plot_each_pair(coastal = "SWB", inland = "LMC", var="succulence", ylab = "succulence (g H2O/cm2)", ylim=0.045)
p4 <- plot_each_pair(coastal = "HEC", inland = "SWC", var="succulence", ylab = "succulence (g H2O/cm2)", ylim=0.045)
p5 <- plot_each_pair(coastal = "OPB", inland = "RGR", var="succulence", ylab = "succulence (g H2O/cm2)", ylim=0.045)
ggarrange(p1, p2, p3, p4, p5, nrow=3, ncol=2)

p1 <- plot_each_pair(coastal = "PGR", inland = "TOR", var="molarity", ylab = "Sodium Concentration (M)", ylim=1)
p2 <- plot_each_pair(coastal = "BHE", inland = "OAE", var="molarity", ylab = "Sodium Concentration (M)", ylim=1)
p3 <- plot_each_pair(coastal = "SWB", inland = "LMC", var="molarity", ylab = "Sodium Concentration (M)", ylim=1)
p4 <- plot_each_pair(coastal = "HEC", inland = "SWC", var="molarity", ylab = "Sodium Concentration (M)", ylim=1)
p5 <- plot_each_pair(coastal = "OPB", inland = "RGR", var="molarity", ylab = "Sodium Concentration (M)", ylim=1)
ggarrange(p1, p2, p3, p4, p5, nrow=3, ncol=2)

mpaes_Na %>% filter(pop_code == "SWB" | pop_code == "LMC") %>% ggplot() +
  aes(x = eco_trt, fill = treatment, y = umol_per_area) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment') +
  scale_y_continuous(
    name = 'umol') +
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

# what are the succulence for each accession under each treatment?
mpaes_Na %>% drop_na(succulence) %>% group_by(pop_code, treatment) %>% summarise(mean=mean(succulence)) %>% View()

# Holds true for PGR/TOR, LMC/SWB, BHE/OAE. Not SWC/HEC, and OPB/RGR is cutting it fine

# Let's plot umol sodium per leaf area against succulence so we can understand
# the contributions of those two variables to molarity
mpaes_Na %>% filter(treatment == "salt" & (pop_code == "OPB" | pop_code == "RGR")) %>% ggplot() +
  geom_point(aes(x = succulence, y = umol_per_area, color=ecotype))+
  # Labels
  scale_y_continuous(
    name = 'umol sodium per cm2') +
  scale_x_continuous(name = 'succulence')+
  # Style
  theme_minimal() +
  theme(
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

conc_plots <- ggarrange(M, umol, succulence, nrow=1, ncol=3)

ggsave(
  filename = 'Na_concentration_plots.png', 
  plot = conc_plots,
  width=15,
  height=8.5,
  device = 'png',
  path = './Results/Figures/',
  bg = 'white'
)


# just calculate total moles of the element in the leaf?
#mpaes_samples_mass$mass_element <- (mpaes_samples_mass$Concentration * 5 * mpaes_samples_mass$dry_weight_g)/
  
# try a version where we color by population to see what each population pair does
pops <- c(coastal_pops, inland_pops)
plotlist <- c(rep(NA,10))
for(i in 1:10){
  plot <- filter(mpaes_Na, pop_code==coastal_pops[i] & treatment != "rinsed_salt") %>%
    ggplot() + aes(x = treatment, fill = treatment, y = molarity) +
    geom_boxplot(outliers = F) +
    geom_jitter(position=position_jitter(0.1), aes(fill=factor(pops[i]))) +
  # Labels
    scale_x_discrete(name = 'Spray Treatment') +
    scale_y_continuous(
    name = 'Concentration of Na (M)', limits = c(0,1)) +
    # Style
    scale_fill_manual(values = c('cyan3', 'salmon')) +
    theme_minimal() + ggtitle("Coastal") +
    theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
  plotlist[i] <- plot
}

# Let's get leaf water content: (fresh mass - dry mass) / fresh mass
mpaes_samples_mass$lwc <- (mpaes_samples_mass$fresh_weight_g - mpaes_samples_mass$dry_weight_g) / mpaes_samples_mass$fresh_weight_g

# LWC affected by salt spray?
inland <- filter(mpaes_samples_mass, treatment != "rinsed_salt" & ecotype=="inland" & `Element Label`=="Na") %>% ggplot() +
  aes(x = treatment, fill = treatment, y = lwc) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment') +
  scale_y_continuous(
    name = 'Leaf Water Content', limits=c(0,1)) +
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal() + ggtitle("Inland") +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

coastal <- filter(mpaes_samples_mass, treatment != "rinsed_salt" & ecotype=="coastal" & `Element Label`=="Na") %>% ggplot() +
  aes(x = treatment, fill = treatment, y = lwc) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment') +
  scale_y_continuous(
    name = 'Leaf Water Content', limits=c(0,1)) +
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal() + ggtitle("Coastal") +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
ggarrange(coastal, inland)

# let's plot leaf water content against sodium concentration
coastal <- filter(mpaes_samples_mass, treatment != "rinsed_salt" & ecotype=="coastal" & element_label=="Na") %>% ggplot() +
  geom_point(aes(x = molarity, y = lwc, color=treatment))+
  # Labels
  scale_y_continuous(
    name = 'Leaf Water Content', limits=c(0,1)) +
  scale_x_continuous(name = 'Sodium Concentration (M)', limits=c(0,1))+
  # Style
  theme_minimal() + ggtitle("Coastal") +
  theme(
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

inland <- filter(mpaes_samples_mass, treatment != "rinsed_salt" & ecotype=="inland" & element_label=="Na") %>% ggplot() +
  geom_point(aes(x = molarity, y = lwc, color=treatment))+
  # Labels
  scale_y_continuous(
    name = 'Leaf Water Content', limits=c(0,1)) +
  scale_x_continuous(name = 'Sodium Concentration (M)', limits=c(0,1))+
  # Style
  theme_minimal() + ggtitle("Inland") +
  theme(
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

# I don't like those much conceptually because water is involved in both variables
# but not in the same ways. Let's plot mass of sodium ~ mass of water

coastal <- filter(mpaes_samples_mass, treatment != "rinsed_salt" & ecotype=="coastal" & element_label=="Na") %>% ggplot() +
  geom_point(aes(x = (5*Concentration*dry_weight_g/(mpaes_g*10^6)), y = (fresh_weight_g - dry_weight_g), color=treatment))+
  # Labels
  scale_y_continuous(
    name = 'Mass of water (g)', limits=c(0,0.7)) +
  scale_x_continuous(name = 'Mass of Na (g)', limits=c(0,0.003))+
  # Style
  theme_minimal() + ggtitle("Coastal") +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

inland <- filter(mpaes_samples_mass, treatment != "rinsed_salt" & ecotype=="inland" & element_label=="Na") %>% ggplot() +
  geom_point(aes(x = (5*Concentration*dry_weight_g/(mpaes_g*10^6)), y = (fresh_weight_g - dry_weight_g), color=treatment))+
  # Labels
  scale_y_continuous(
    name = 'Mass of water (g)', limits=c(0,0.7)) +
  scale_x_continuous(name = 'Mass of Na (g)', limits=c(0,0.003))+
  # Style
  theme_minimal() + ggtitle("Inland") +
  theme(
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
ggarrange(coastal, inland)

# What happens to K in different ecotypes and treatments? Calculate as moles K per g dry weight
coastal <- filter(mpaes_samples_mass, treatment != "rinsed_salt" & ecotype=="coastal" & element_label == "K") %>% ggplot() +
  aes(x = treatment, fill = treatment, y = umol_per_dry_gram) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment') +
  scale_y_continuous(
    name = 'umol K per gram dry leaf', limits=c(0,1100)) +
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal() + ggtitle("Coastal") +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

inland <- filter(mpaes_samples_mass, treatment != "rinsed_salt" & ecotype=="inland" & element_label == "K") %>% ggplot() +
  aes(x = treatment, fill = treatment, y = umol_per_dry_gram) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment') +
  scale_y_continuous(
    name = 'umol K per gram dry leaf', limits=c(0,1100)) +
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal() + ggtitle("Inland") +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
ggarrange(coastal, inland)


# What happens to Ca in different ecotypes and treatments?
coastal <- filter(mpaes_samples_mass, treatment != "rinsed_salt" & ecotype=="coastal" & element_label == "Ca") %>% ggplot() +
  aes(x = treatment, fill = treatment, y = umol_per_dry_gram) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment') +
  scale_y_continuous(
    name = 'umol Ca per gram dry leaf', limits=c(0,1400)) +
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal() + ggtitle("Coastal") +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

inland <- filter(mpaes_samples_mass, treatment != "rinsed_salt" & ecotype=="inland" & `Element Label` == "Ca") %>% ggplot() +
  aes(x = treatment, fill = treatment, y = umol_per_dry_gram) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Spray Treatment') +
  scale_y_continuous(
    name = 'umol Ca per gram dry leaf', limits=c(0,1400)) +
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon')) +
  theme_minimal() + ggtitle("Inland") +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )
ggarrange(coastal, inland)

# Did leaves with the highest concentrations of salt sustain the most damage?
mpaes_samples_mass$damage <- mpaes_samples_mass$damage=="Y"
sum(mpaes_samples_mass$damage)

par(mfrow=c(2,1))
hist(mpaes_samples_mass[which(mpaes_samples_mass$damage & mpaes_samples_mass$treatment == "salt" & mpaes_samples_mass$element_label=="Na"), "molarity"], xlab="Concentration of Na (M) in Damaged Leaves", xlim=c(0,1))
hist(mpaes_samples_mass[which(!mpaes_samples_mass$damage & mpaes_samples_mass$treatment == "salt" & mpaes_samples_mass$element_label=="Na"), "molarity"], xlab="Concentration of Na (M) in Undamaged Leaves", xlim=c(0,1))
dev.off()

par(mfrow=c(2,1))
hist(mpaes_samples_mass[which(mpaes_samples_mass$damage & mpaes_samples_mass$treatment == "salt" & mpaes_samples_mass$element_label=="Na"), "succulence"], xlab="Concentration of Na (M) in Damaged Leaves", xlim=c(0,0.1))
hist(mpaes_samples_mass[which(!mpaes_samples_mass$damage & mpaes_samples_mass$treatment == "salt" & mpaes_samples_mass$element_label=="Na"), "succulence"], xlab="Concentration of Na (M) in Undamaged Leaves", xlim=c(0,0.1))
dev.off()

# Are leaves with higher salt concentrations more likely to be damaged?

mpaes_samples_mass[which(mpaes_samples_mass$damage & mpaes_samples_mass$treatment == "salt" & mpaes_samples_mass$element_label=="Na"), ] %>%
  glm(family="binomial", formula = damage ~ umol_per_area) %>% summary()

# no, not really

#with more water loss?

mpaes_samples_mass[which(mpaes_samples_mass$damage & mpaes_samples_mass$treatment == "salt" & mpaes_samples_mass$element_label=="Na"), ] %>%
  glm(family="binomial", formula = damage ~ succulence) %>% summary()

# Let's see if stomatal traits 
# grab mean salt exclusion for each accession

# read in mean stomatal traits
stom <- read.csv("./Data/stomata_accession_means.csv")

# get means

na_summary_salt <- filter(mpaes_Na, treatment=="salt") %>%
  group_by(pop_code) %>%
  summarise(across(c("umol_per_area", "lma", "succulence", "umol_per_dry_gram", "molarity"), mean, na.rm=TRUE))

stom_na_summary <- cbind(stom, na_summary_salt[,2:6])

plot(data=stom_na_summary, umol_per_area ~ stomate_size_ad, pch=" ")
text(x=stom_na_summary$stomate_size_ad, y=stom_na_summary$umol_per_area, 
     labels=stom_na_summary$pop_code, col=(stom_na_summary$pop_code %in% coastal_pops)+1)

lm(data=stom_na_summary, molarity ~ stomate_size_ad) %>% summary()


########### Let's make tables!!! ###############
# Make ANOVA table with Na concentration (M), umol Na per cm^2 leaf, and succulence

library(kableExtra)

# tell kable not to plot NAs
options(knitr.kable.NA = '')

# make vector of sources of variation
sov <- c("Treatment", "Ecotype", "Treatment:Ecotype", "Treatment:Ecotype:Accession", "Error")

# make vector to describe what effect sizes indicate
effect_meaning <- c("Salt", "Inland ", "Salt * Inland", "", "")

# vector of main and interacting effects for succulence
effects <- c(as.numeric(m_nest_suc$coefficients[2]), as.numeric(m_nest_suc$coefficients[3]), as.numeric(m_nest_suc$coefficients[4]), "", "")
anova_suc_tbl <- as.data.frame(cbind(sov, effect_meaning, effects, anova(m_nest_suc)$Df, anova(m_nest_suc)$F, anova(m_nest_suc)$`Pr(>F)`))
colnames(anova_suc_tbl) <- c("Source of variation", "Effect of", "Effect size", "df", "F", "p-value")

# make succulence table!
anova_suc_tbl %>% mutate(`Effect size` = round(as.numeric(`Effect size`), 5),
                         F = round(as.numeric(F), 2),
                         `p-value` = case_when(as.numeric(`p-value`) < 0.00001 ~ "<0.00001",
                                               .default = as.character(round(as.numeric(`p-value`), 5)))) %>%
  kbl(caption = "Succulence") %>% kable_classic() %>% 
  row_spec(which(as.numeric(anova_suc_tbl$`p-value`) < 0.05), bold=T)

# vector of main and interacting effects for umol sodium per leaf area
effects <- c(as.numeric(m_nest_umol$coefficients[2]), as.numeric(m_nest_umol$coefficients[3]),
             as.numeric(m_nest_umol$coefficients[4]), "", "")
anova_umol_tbl <- as.data.frame(cbind(sov, effect_meaning, effects, anova(m_nest_umol)$Df, anova(m_nest_umol)$F, anova(m_nest_umol)$`Pr(>F)`))
colnames(anova_umol_tbl) <- c("Source of variation", "Effect of", "Effect size", "df", "F", "p-value")

# make umol per leaf area table!
anova_umol_tbl %>% mutate(`Effect size` = round(as.numeric(`Effect size`), 3),
                         F = round(as.numeric(F), 2),
                         `p-value` = case_when(as.numeric(`p-value`) < 0.00001 ~ "<0.00001",
                                               .default = as.character(round(as.numeric(`p-value`), 5)))) %>%
  kbl(caption = "Sodium Exclusion") %>% kable_classic() %>% 
  row_spec(which(as.numeric(anova_umol_tbl$`p-value`) < 0.05), bold=T)

# vector of main and interacting effects for molarity of Na
effects <- c(as.numeric(m_nest_M$coefficients[2]), as.numeric(m_nest_M$coefficients[3]),
             as.numeric(m_nest_M$coefficients[4]), "", "")
anova_M_tbl <- as.data.frame(cbind(sov, effect_meaning, effects, anova(m_nest_M)$Df, anova(m_nest_M)$F, anova(m_nest_M)$`Pr(>F)`))
colnames(anova_M_tbl) <- c("Source of variation", "Effect of", "Effect size", "df", "F", "p-value")

# make molarity table!
anova_M_tbl %>% mutate(`Effect size` = round(as.numeric(`Effect size`), 3),
                          F = round(as.numeric(F), 2),
                          `p-value` = case_when(as.numeric(`p-value`) < 0.00001 ~ "<0.00001",
                                                .default = as.character(round(as.numeric(`p-value`), 5)))) %>%
  kbl(caption = "Concentration of Na (M)") %>% kable_classic() %>% 
  row_spec(which(as.numeric(anova_M_tbl$`p-value`) < 0.05), bold=T)





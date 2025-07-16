# Stomata data wrangling for gsw of coastal and inland under salt or DI water spray
# Baseline gsw, then let dry, measured again, rinsed, measured again to capture wicking

#loading required packages
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(ggpubr)

# read in baseline gsw
setwd("~/Documents/GitHub/Leaf-surface-traits-2024/")
baseline <- read_excel("./Data/stomatal_conductance_data/raw_corrected_Wicking CP IA_M Plunkert_2025_04_01_12_05_01_1.xlsx")
colnames(baseline) <- baseline[1,]
baseline <- baseline[3:nrow(baseline),c("Time", "bgrwp", "Rep", "Treatment", "gsw")]
colnames(baseline)[3] <- "rep"
# check that numbers of observations match experimental design
table(as.factor(baseline$bgrwp))

# Some missing data in accessions, OPB has an extra point. Will reconcile w/ other dataframe.
baseline$bgrwp <- as.numeric(baseline$bgrwp)

baseline$pop <- case_when(
  baseline$bgrwp == 1 ~ "HEC",
  baseline$bgrwp == 2 ~ "OPB",
  baseline$bgrwp == 3 ~ "BHE",
  baseline$bgrwp == 4 ~ "SWB",
  baseline$bgrwp == 5 ~ "PGR",
  baseline$bgrwp == 11 ~ "SWC",
  baseline$bgrwp == 12 ~ "RGR",
  baseline$bgrwp == 13 ~ "OAE",
  baseline$bgrwp == 14 ~ "LMC",
  baseline$bgrwp == 15 ~ "TOR") %>% as.factor()

baseline$rep <- as.numeric(baseline$rep)
baseline$gsw <- as.numeric(baseline$gsw)

baseline$id <- paste(as.character(baseline$pop), as.character(baseline$rep), baseline$Treatment, sep="_")

table(as.factor((baseline$id)))

# label these all baseline to help with pivot wider later
baseline$timepoint <- rep("baseline", nrow(baseline))

# Missing:    Plant   With
# BHE_3_water 2       BHE W6
# HEC_2_water 3       HEC W6
# HEC_4_water 11      HEC W4
# HEC_5_water 11      HEC W5
# HEC_6_water 3       HEC W2
# TOR_1_salt  12      TOR S4
# TOR_4_salt  12      TOR S1

# make ecotype variable
coastal_pops <- c("SWB", "BHE", "OPB", "HEC", "PGR")
inland_pops <- c("LMC", "OAE", "RGR", "SWC", "TOR")
baseline$ecotype <- case_when(baseline$pop %in% coastal_pops ~ "coastal",
                              baseline$pop %in% inland_pops ~ "inland")

# just a quick check for differences in baseline gsw between ecotypes!

baseline %>%
  filter(!is.na(baseline$gsw)) %>%
  ggplot() +
  aes(x = ecotype, fill = ecotype, y = gsw) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Baseline Adaxial gsw', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

m_nest_gsw <- aov(data=baseline, gsw ~ ecotype/pop)
summary(m_nest_gsw)
coefficients(m_nest_gsw) # being inland increases gsw by 0.08831 mol/*m^2/s

# write processed gsw data to a new file to read into script with other stomata data
write.csv(baseline, "./Data/stomatal_conductance_data/processed_baseline_gsw.csv", row.names=FALSE)

# What does each population pair do?
filter(baseline, ! is.na(gsw) & (pop == "PGR" | pop == "TOR")) %>%
  filter(!is.na(gsw)) %>%
  ggplot() +
  aes(x = ecotype, fill = ecotype, y = gsw) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Ecotype') +
  scale_y_continuous(
    name = 'Baseline Adaxial gsw', 
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#cacf85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )

# most pairs have inland gsw > coastal gsw by at least a little, except OPB gsw dramatically > RGR
# OPB conveniently also has larger stomata than RGR - connected?

# Load in after spray and after rinse data
after <- read_excel("raw_corrected_Wicking CP IA_M Plunkert_2025_04_01_16_05_40_1.xlsx")
colnames(after) <- after[1,]
after <- after[3:nrow(after),c("Obs#", "Time", "remark","genotype", "rep", "Treatment", "gsw")]
colnames(after)[1] <- "obs_num"
colnames(after)[4] <- "pop"
after$rep <- as.numeric(after$rep)
after$pop <- as.numeric(after$pop)
after$pop <- case_when(
  after$pop == 1 ~ "BHE",
  after$pop == 2 ~ "HEC",
  after$pop == 3 ~ "LMC",
  after$pop == 4 ~ "OAE",
  after$pop == 5 ~ "OPB",
  after$pop == 6 ~ "PGR",
  after$pop == 7 ~ "RGR",
  after$pop == 8 ~ "SWB",
  after$pop == 9 ~ "SWC",
  after$pop == 10 ~ "TOR")

table(after$pop)

after$id <- paste(after$pop, after$rep, after$Treatment, sep="_")

# label timepoint for after spray and after rinse
after <- arrange(after, as.numeric(obs_num))
after$timepoint <- case_when(
  !duplicated(after$id) ~ "after_spray",
  duplicated(after$id) ~ "after_rinse"
)

# Combine baseline and after spray 
dat_long <- rbind(baseline[,c("Time", "rep", "Treatment", "id", "pop", "gsw", "timepoint")], 
                  after[,c("Time", "rep", "Treatment", "id", "pop", "gsw", "timepoint")])

dat_long$gsw <- as.numeric(dat_long$gsw)
dat_long$timepoint <- factor(dat_long$timepoint, levels = c("baseline", "after_spray", "after_rinse"))
# make dataframe where baseline, after spray, and after rinse for each plant are all
# on the same row

dat_long$ecotype <- case_when(dat_long$pop %in% coastal_pops ~ "coastal",
                              dat_long$pop %in% inland_pops ~ "inland")

# Add column for treatment and ecotype combos
dat_long$treatment_ecotype <- as.factor(paste(dat_long$ecotype, dat_long$Treatment))

table(dat_long$id)

# Start by getting after spray and after rinse on the same row
after_wider <- after %>% pivot_wider(id_cols = c(pop, rep, Treatment, id), 
                                     names_from = "timepoint", names_prefix = "gsw_", values_from="gsw")

dat_wide <- pivot_wider(dat_long, id_cols=c(pop, rep, Treatment, id), 
                        names_from="timepoint", names_prefix="gsw_", values_from="gsw")
  
dat_wide$ecotype <- case_when(dat_wide$pop %in% coastal_pops ~ "coastal",
                              dat_wide$pop %in% inland_pops ~ "inland")

dat_wide$treatment_ecotype <- as.factor(paste(dat_wide$ecotype, dat_wide$Treatment))

# Plot gsw at three timepoints
dat_long %>%
  ggplot(aes(x=timepoint, y=gsw, colour=as.factor(paste(dat_long$ecotype, dat_long$Treatment))))+
  geom_line(aes(group=id))


# Calculate and plot timepoint and ecotype combos
ggplot(
  filter(dat_long, timepoint=="baseline"), aes(x=treatment_ecotype, y=gsw, group=treatment_ecotype)) + geom_boxplot() + 
  geom_jitter(width=0.1, height=0) +
  geom_hline(yintercept=0)+
  ggtitle("Stomatal Conductance after NaCl or Water Spray")


# very concerning that coastal salt gsw < coastal water gsw before treatment

# is salt gsw < water gsw across the board?
ggplot(
    filter(dat_long, timepoint=="baseline"), aes(x=Treatment, y=gsw, group=Treatment)) + geom_boxplot() + 
  geom_jitter(width=0.1, height=0) +
  geom_hline(yintercept=0)+
  ggtitle("Stomatal conductance before spray")

t.test(baseline$gsw ~ baseline$Treatment)

# ugh okay so comparisons between treatments will all have to be relative to baseline

ggplot(
  filter(dat_long, timepoint=="after_spray"), aes(x=treatment_ecotype, y=gsw, group=treatment_ecotype)) + geom_boxplot() + 
  geom_jitter(width=0.1, height=0) +
  geom_hline(yintercept=0)+
  ggtitle("Stomatal Conductance after NaCl or Water Spray")

ggplot(
  filter(dat_long, timepoint=="after_rinse"), aes(x=treatment_ecotype, y=gsw, group=treatment_ecotype)) + geom_boxplot() + 
  geom_jitter(width=0.1, height=0) +
  geom_hline(yintercept=0)+
  ggtitle("Stomatal Conductance after NaCl or Water Spray and Rinse")


# Let's look at change from baseline to after spray
dat_wide$change_baseline_after_spray <- dat_wide$gsw_after_spray - dat_wide$gsw_baseline
dat_wide$change_baseline_after_rinse <- dat_wide$gsw_after_rinse - dat_wide$gsw_baseline
dat_wide$change_after_spray_after_rinse <- dat_wide$gsw_after_rinse - dat_wide$gsw_after_spray

ggplot(
  dat_wide, aes(x=treatment_ecotype, y=change_baseline_after_spray, group=treatment_ecotype)) + geom_boxplot() + 
  geom_jitter(width=0.1, height=0) +
  geom_hline(yintercept=0)+
  ggtitle("After spray gsw - Baseline gsw")

ggplot(
  dat_wide, aes(x=treatment_ecotype, y=change_baseline_after_rinse, group=treatment_ecotype)) + geom_boxplot() + 
  geom_jitter(width=0.1, height=0) +
  geom_hline(yintercept=0)+
  ggtitle("After rinse gsw - Baseline gsw")

ggplot(
  dat_wide, aes(x=treatment_ecotype, y=change_after_spray_after_rinse, group=treatment_ecotype)) + geom_boxplot() + 
  geom_jitter(width=0.1, height=0) +
  geom_hline(yintercept=0)+
  ggtitle("After rinse gsw - After spray gsw")


# Suprised we don't pick up much wicking. Let's look at SWB by itself
ggplot(
  filter(dat_wide, pop == "SWB"), aes(x=treatment_ecotype, y=change_baseline_after_spray, group=treatment_ecotype)) + geom_boxplot() + 
  geom_jitter(width=0.1, height=0) +
  geom_hline(yintercept=0)+
  ggtitle("After spray gsw - Baseline gsw")

ggplot(
  filter(dat_wide, pop == "SWB"), aes(x=treatment_ecotype, y=change_after_spray_after_rinse, group=treatment_ecotype)) + geom_boxplot() + 
  geom_jitter(width=0.1, height=0) +
  geom_hline(yintercept=0)+
  ggtitle("After rinse gsw - After spray gsw")

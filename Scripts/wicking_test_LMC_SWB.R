# Test for differences in wicking in SWB and LMC based on stomatal conductance measurements

# load required packages

require(tidyverse)
require(readxl)
require(ggpubr)
require(emmeans)
require(ggplot2)
require(dplyr)
require(kableExtra)

# read in stomatal conductance data from 9/5/25
setwd("~/Documents/GitHub/Leaf-surface-traits-2024/")
gsw <- read_excel("./Data/stomatal_conductance_data/Wicking test_M Plunkert_2025_09_05_18_36_05_1_corrected.xlsx")
colnames(gsw) <- gsw[1,]

gsw <- gsw[3:nrow(gsw),c("Time", "remark", "Geno_Rep", "Treatment", "Timepoint", "gsw")]

gsw$Geno_Rep <- as.numeric(gsw$Geno_Rep)
gsw$gsw <- as.numeric(gsw$gsw)

gsw$Timepoint <- factor(gsw$Timepoint, ordered("baseline", "after_spray", "after_rinse"))
gsw$Timepoint <- as.factor(gsw$Timepoint)
gsw$Timepoint <- factor(gsw$Timepoint, ordered=TRUE, levels=c("baseline", "after_spray", "after_rinse"))

gsw$Timepoint <- factor(gsw$Timepoint, levels = c("baseline", "after_spray", "after_rinse"))


# add column to indicate leaf side. Always measured abaxial, then adaxial, but didn't
# indicate which leaf side at time of measurement bc LI-600 doesn't have enough labels
gsw$leaf_side <- rep("abaxial", nrow(gsw))

for (i in 2:nrow(gsw)){
  if (identical(gsw[(i-1),3:5], gsw[i,3:5])){
    gsw$leaf_side[i] <- "adaxial"
  }
}

# indicate which accession
gsw <- mutate(gsw, accession = case_when(
  Geno_Rep >= 10 ~ "LMC",
  Geno_Rep <10 ~"SWB",
  .default = NA_character_
))

# pivot wider so that all 3 timepoints are on the same line

wider_gsw <- gsw[, -c(1, 2)] %>% pivot_wider(
  id_cols = c(Geno_Rep, Treatment, leaf_side, accession),
  names_from = Timepoint,
  values_from = gsw,
  names_prefix = "gsw_"
)

# FOR NOW just filtering out rows with duplicates, but come back and deal with this!
wider_gsw <- wider_gsw %>% filter(lapply(wider_gsw$gsw_after_spray, length) == 1)
wider_gsw <- wider_gsw %>% filter(lapply(wider_gsw$gsw_after_spray, length) == 1)


gsw$id <- paste(gsw$Treatment, gsw$Geno_Rep, gsw$leaf_side, sep="_")

lmc_over_time_ad <- gsw %>% filter(accession == "LMC" & leaf_side=="adaxial") %>% 
  ggplot(aes(x=Timepoint, y=gsw, colour=Treatment))+
  geom_line(aes(group=id)) + ylim(c(0,1.1)) +
  ggtitle("LMC Adaxial")

swb_over_time_ad <- gsw %>% filter(accession == "SWB" & leaf_side=="adaxial") %>% 
  ggplot(aes(x=Timepoint, y=gsw, colour=Treatment))+
  geom_line(aes(group=id)) + ylim(c(0,1.1)) +
  ggtitle("SWB Adaxial")

lmc_over_time_ab <- gsw %>% filter(accession == "LMC" & leaf_side=="abaxial") %>% 
  ggplot(aes(x=Timepoint, y=gsw, colour=Treatment))+
  geom_line(aes(group=id)) + ylim(c(0,1.1)) +
  ggtitle("LMC Abaxial")

swb_over_time_ab <- gsw %>% filter(accession == "SWB" & leaf_side=="abaxial") %>% 
  ggplot(aes(x=Timepoint, y=gsw, colour=Treatment))+
  geom_line(aes(group=id)) + ylim(c(0,1.1)) +
  ggtitle("SWB Abaxial")
ggarrange(lmc_over_time_ad, swb_over_time_ad, lmc_over_time_ab, swb_over_time_ab )

# Calculate differences between consecutive timepoints
wider_gsw$diff_after_spray_baseline <- wider_gsw$gsw_after_spray - wider_gsw$gsw_baseline

wider_gsw$diff_rinse_after_spray <- wider_gsw$gsw_after_rinse - wider_gsw$gsw_after_spray

wider_gsw$diff_rinse_baseline <- wider_gsw$gsw_after_rinse - wider_gsw$gsw_baseline

# How does gsw change between baseline and after spray for salt vs. water treatment?
ggplot(
  wider_gsw,  aes(x=Treatment, y=diff_after_spray_baseline, group=Treatment)) + geom_boxplot() + 
  geom_jitter(width=0.1, height=0) +
  geom_hline(yintercept=0)+
  ggtitle("Change in Stomatal Conductance After Spray")


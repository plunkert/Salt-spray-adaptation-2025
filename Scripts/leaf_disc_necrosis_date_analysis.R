
library(readxl)
library(dplyr)
library(ggplot2)
library(emmeans)

dat <- read_excel("~/Documents/Github/Leaf-surface-traits-2024/Data/Leaf_disc_date_data.xlsx")
View(dat)
colnames(dat) <- c("leaf_id", "est_start", "est_full", "Notes_1", "Notes_2", "disc_num" )

# # counting days since 1900 as the date. Subtracted 45756 from given number as day 1 was April 11th, 2025.
# Now they're coming out negative? Changing to day 1, day 2, etc.
dat$est_full <- as.numeric(dat$est_full)
dat$est_full <- dat$est_full - 44295
dat$est_start <- as.numeric(dat$est_start)
dat$est_start <- dat$est_start - 44295

dat$pop <- substr(dat$leaf_id, 1, 3)
dat <- separate(dat, col = leaf_id, sep = " ", into = c("pop", "treatment", "plate", "plate_rep"))

#removes extra plate column
dat <- dat[,-3]

# add ecotype column
dat$ecotype <- as.factor(case_when(
  dat$pop %in% c('SWB', 'OPB', 'HEC', 'PGR', 'BHE') ~ "coastal",
  dat$pop %in% c('SWC', 'LMC', 'TOR', 'OAE', 'RGR') ~ "inland"))
View(dat)
hist(dat$est_start)

# Lots of rows omitted due to missing data, mainly algae growth preventing 
# scoring of necrosis and blocking tissue from salt exposure. How many of each accession did
# we actually score?
dat_nona <- filter(dat, !is.na(est_start))
table(dat_nona$pop, dat_nona$treatment) # 6-10 disks of each accession on each treatment.

# indicate combinations of ecotype and treatment for easy plotting
dat$eco_trt <- paste(dat$ecotype, str_to_lower(dat$treatment), sep=" ")

lab <- as.data.frame(rbind(label=c("a","b","c","d"),
                           y=c(13,13,13,13),
                           )
  
dat %>% ggplot() +
  aes(x = eco_trt, fill = eco_trt, y = est_start,) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment and Ecotype') +
  scale_y_continuous(
    name = 'Days to start of necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon', 'cyan3', 'salmon')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )+
  annotate("text", x = 1, y = 13, label = "a", size=9)+
  annotate("text", x = 2, y = 13, label = "b", size=9)+
  annotate("text", x = 3, y = 13, label = "c", size=9)+
  annotate("text", x = 4, y = 13, label = "d", size=9)

dat %>% ggplot() +
  aes(x = eco_trt, fill = eco_trt, y = est_full,) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment and Ecotype') +
  scale_y_continuous(
    name = 'Days to full necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('cyan3', 'salmon', 'cyan3', 'salmon')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )+
  annotate("text", x = 1, y = 17, label = "a", size=9)+
  annotate("text", x = 2, y = 17, label = "b", size=9)+
  annotate("text", x = 3, y = 17, label = "a", size=9)+
  annotate("text", x = 4, y = 17, label = "b", size=9)


# test for interaction between ecotype and treatment for start of necrosis
m_tol <- lm(data=dat, est_start ~ ecotype * treatment)
summary(m_tol)
m_tol_emm <- emmeans(m_tol, ~ ecotype*treatment)
contrast(m_tol_emm, method="tukey", adjust='bh')

# test for interaction between ecotype and treatment for full necrosis
m_tol <- lm(data=dat, est_full ~ ecotype * treatment)
summary(m_tol)
m_tol_emm <- emmeans(m_tol, ~ ecotype*treatment)
contrast(m_tol_emm, method="tukey", adjust='bh')

# box plot for costal populations vs inland populations comparing control and salt
dat %>% ggplot() +
  aes(x = ecotype, fill = ecotype, y = est_start) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til start of necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
  )




# # Create a data frame directly from the data in the PDF
# # The data appears to be space-separated
# data <- data.frame(
#   pop = c("BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", 
#           "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE",
#           "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC",
#           "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC",
#           "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB",
#           "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB",
#           "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR",
#           "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR",
#           "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB",
#           "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB"),
#   treatment = c(rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
#                 rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
#                 rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
#                 rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
#                 rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5)),
#   est_full = c(14, 14, NA, NA, 12, 8, 9, 12, NA, NA,
#                12, 10, NA, NA, 13, 11, 10, 12, 10, 12,
#                11, 12, NA, NA, 19, 16, 10, 14, 9, 11,
#                11, 14, NA, NA, 14, 11, 12, 11, 14, 11,
#                11, 15, NA, NA, 15, 9, 8, NA, 10, 9,
#                14, 12, NA, NA, 14, 8, 9, 11, 13, 13,
#                10, 14, NA, NA, 10, 9, 7, 9, 7, 10,
#                10, 14, NA, NA, 10, 9, 7, 10, 9, 11,
#                10, 12, NA, NA, 13, 7, 7, 11, 9, 16,
#                14, 14, NA, NA, 12, 10, 8, 12, 9, 15),
#   ecotype = rep("coastal", 100)
# )
# 
# # Alternative approach without using pipes
# coastal_data <- subset(data, ecotype == "coastal" & !is.na(est_full))

# Create box plots comparing Control vs Salt for coastal ecotype
# Using est_full (full necrosis) as the y-value

# Create basic boxplot using base R (no pipes required)
boxplot(est_full ~ treatment, data = coastal_data,
        main = "Time to Full Necrosis: Control vs Salt (Coastal Ecotype)",
        xlab = "Treatment",
        ylab = "Days to Full Necrosis",
        col = c("lightgreen", "lightblue"))

# If ggplot2 is available, we can use it for a more advanced plot
if (require(ggplot2)) {
  # Create a ggplot boxplot
  p <- ggplot(coastal_data, aes(x = treatment, y = est_full, fill = treatment)) +
    geom_boxplot() +
    labs(title = "Time to Full Necrosis: Control vs Salt (Coastal Ecotype)",
         x = "Treatment",
         y = "Days to Full Necrosis") +
    scale_fill_manual(values = c("Control" = "lightgreen", "Salt" = "lightblue")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p)
  
  # Add individual data points with jitter
  p2 <- ggplot(coastal_data, aes(x = treatment, y = est_full, fill = treatment)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(title = "Time to Full Necrosis: Control vs Salt (Coastal Ecotype)",
         subtitle = "Individual data points shown with jitter",
         x = "Treatment",
         y = "Days to Full Necrosis") +
    scale_fill_manual(values = c("Control" = "lightgreen", "Salt" = "lightblue")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p2)
} else {
  # If ggplot2 isn't available, we'll use a stripchart to show individual points
  stripchart(est_full ~ treatment, data = coastal_data,
             vertical = TRUE, method = "jitter",
             pch = 19, col = "darkgray", add = TRUE)
}

# Perform statistical analysis of the difference
# t-test to compare means
t_test_result <- t.test(est_full ~ treatment, data = coastal_data)
print(t_test_result)

# Calculate summary statistics for each group
# Using base R instead of dplyr
control_data <- subset(coastal_data, treatment == "Control")
salt_data <- subset(coastal_data, treatment == "Salt")

summary_stats <- data.frame(
  treatment = c("Control", "Salt"),
  mean = c(mean(control_data$est_full), mean(salt_data$est_full)),
  median = c(median(control_data$est_full), median(salt_data$est_full)),
  min = c(min(control_data$est_full), min(salt_data$est_full)),
  max = c(max(control_data$est_full), max(salt_data$est_full)),
  sd = c(sd(control_data$est_full), sd(salt_data$est_full)),
  n = c(nrow(control_data), nrow(salt_data))
)

print(summary_stats)

if (!require(dplyr)) install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")

# Load the libraries
library(ggplot2)

# Create a data frame for the inland populations
data_inland <- data.frame(
  pop = c("LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", 
          "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC",
          "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE",
          "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE",
          "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR",
          "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR",
          "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC",
          "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC",
          "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR",
          "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR"),
  treatment = c(rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
                rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
                rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
                rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
                rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5)),
  est_full = c(13, 15, NA, NA, 11, 9, 9, 9, 9, 9,
               16, 10, NA, NA, 13, 9, 9, 10, 10, NA,
               12, 12, NA, NA, 14, 10, 10, 10, 10, 9,
               11, 14, NA, NA, 11, 9, 10, 9, 10, 9,
               15, 13, NA, NA, 14, 10, 10, 9, 12, 9,
               15, 16, NA, NA, NA, 9, 9, 8, 10, 11,
               10, 13, NA, NA, 10, 8, 10, 10, 10, 7,
               9, 9, NA, NA, 10, 9, 9, 10, 9, 6,
               16, 14, NA, NA, NA, 10, 17, 10, 12, 14,
               15, 17, NA, NA, 15, 15, 14, NA, 16, 12),
  ecotype = rep("inland", 100)
)

# Filter data to include only inland ecotype and remove NA values in est_full
inland_data <- subset(data_inland, !is.na(est_full))

# Create basic boxplot using base R (no pipes required)
boxplot(est_full ~ treatment, data = inland_data,
        main = "Time to Full Necrosis: Control vs Salt (Inland Ecotype)",
        xlab = "Treatment",
        ylab = "Days to Full Necrosis",
        col = c("lightgreen", "lightblue"))

# If ggplot2 is available, we can use it for a more advanced plot
if (require(ggplot2)) {
  # Create a ggplot boxplot
  p <- ggplot(inland_data, aes(x = treatment, y = est_full, fill = treatment)) +
    geom_boxplot() +
    labs(title = "Time to Full Necrosis: Control vs Salt (Inland Ecotype)",
         x = "Treatment",
         y = "Days to Full Necrosis") +
    scale_fill_manual(values = c("Control" = "lightgreen", "Salt" = "lightblue")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p)
  
  # Add individual data points with jitter
  p2 <- ggplot(inland_data, aes(x = treatment, y = est_full, fill = treatment)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(title = "Time to Full Necrosis: Control vs Salt (Inland Ecotype)",
         subtitle = "Individual data points shown with jitter",
         x = "Treatment",
         y = "Days to Full Necrosis") +
    scale_fill_manual(values = c("Control" = "lightgreen", "Salt" = "lightblue")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p2)
} else {
  # If ggplot2 isn't available, we'll use a stripchart to show individual points
  stripchart(est_full ~ treatment, data = inland_data,
             vertical = TRUE, method = "jitter",
             pch = 19, col = "darkgray", add = TRUE)
}

# Perform statistical analysis of the difference
# t-test to compare means
t_test_result <- t.test(est_full ~ treatment, data = inland_data)
print(t_test_result)

# Calculate summary statistics for each group
# Using base R instead of dplyr
control_data <- subset(inland_data, treatment == "Control")
salt_data <- subset(inland_data, treatment == "Salt")

summary_stats <- data.frame(
  treatment = c("Control", "Salt"),
  mean = c(mean(control_data$est_full), mean(salt_data$est_full)),
  median = c(median(control_data$est_full), median(salt_data$est_full)),
  min = c(min(control_data$est_full), min(salt_data$est_full)),
  max = c(max(control_data$est_full), max(salt_data$est_full)),
  sd = c(sd(control_data$est_full), sd(salt_data$est_full)),
  n = c(nrow(control_data), nrow(salt_data))
)

if (!require(dplyr)) install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")

# Load the libraries
library(ggplot2)

# Create a data frame for the inland populations with est_start values
data_inland <- data.frame(
  pop = c("LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", 
          "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC", "LMC",
          "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE",
          "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE", "OAE",
          "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR",
          "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR", "RGR",
          "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC",
          "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC", "SWC",
          "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR",
          "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR", "TOR"),
  treatment = c(rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
                rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
                rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
                rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
                rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5)),
  est_start = c(9, 10, NA, NA, 8, 5, 6, 7, 5, 5,
                12, 9, NA, NA, 9, 5, 6, 5, 6, NA,
                10, 9, NA, NA, 11, 7, 7, 6, 7, 6,
                9, 11, NA, NA, 9, 6, 7, 6, 7, 6,
                10, 10, NA, NA, 10, 5, 7, 7, 7, 7,
                10, 9, NA, NA, NA, 6, 5, 5, 8, 7,
                8, 9, NA, NA, 8, 6, 6, 6, 6, 5,
                7, 7, NA, NA, 8, 6, 7, 6, 6, 4,
                13, 11, NA, NA, NA, 7, 9, 6, 7, 8,
                13, 10, NA, NA, 9, 7, 8, 3, 8, 6),
  ecotype = rep("inland", 100)
)

# Filter data to include only inland ecotype and remove NA values in est_start
inland_data <- subset(data_inland, !is.na(est_start))

# Create basic boxplot using base R
boxplot(est_start ~ treatment, data = inland_data,
        main = "Time to Start of Necrosis: Control vs Salt (Inland Ecotype)",
        xlab = "Treatment",
        ylab = "Days to Start of Necrosis",
        col = c("lightgreen", "lightblue"))

# If ggplot2 is available, we can use it for a more advanced plot
if (require(ggplot2)) {
  # Create a ggplot boxplot
  p <- ggplot(inland_data, aes(x = treatment, y = est_start, fill = treatment)) +
    geom_boxplot() +
    labs(title = "Time to Start of Necrosis: Control vs Salt (Inland Ecotype)",
         x = "Treatment",
         y = "Days to Start of Necrosis") +
    scale_fill_manual(values = c("Control" = "lightgreen", "Salt" = "lightblue")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p)
  
  # Add individual data points with jitter
  p2 <- ggplot(inland_data, aes(x = treatment, y = est_start, fill = treatment)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(title = "Time to Start of Necrosis: Control vs Salt (Inland Ecotype)",
         subtitle = "Individual data points shown with jitter",
         x = "Treatment",
         y = "Days to Start of Necrosis") +
    scale_fill_manual(values = c("Control" = "lightgreen", "Salt" = "lightblue")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p2)
} else {
  # If ggplot2 isn't available, we'll use a stripchart to show individual points
  stripchart(est_start ~ treatment, data = inland_data,
             vertical = TRUE, method = "jitter",
             pch = 19, col = "darkgray", add = TRUE)
}



# Create a data frame for the coastal populations with est_start values
data_inland <- data.frame(
  pop = c("BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", 
          "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE", "BHE",
          "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC",
          "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC", "HEC",
          "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB",
          "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB", "OPB",
          "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR",
          "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR", "PGR",
          "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB",
          "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB", "SWB"),
  treatment = c(rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
                rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
                rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
                rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5),
                rep("Control", 5), rep("Salt", 5), rep("Control", 5), rep("Salt", 5)),
  est_full = c(14, 14, NA, NA, 12, 8, 9, 12, NA, NA,
               12, 10, NA, NA, 13, 11, 10, 12, 10, 12,
               11, 12, NA, NA, 19, 16, 10, 14, 9, 11,
               11, 14, NA, NA, 14, 11, 12, 11, 14, 11,
               11, 15, NA, NA, 15, 9, 8, NA, 10, 9,
               14, 12, NA, NA, 14, 8, 9, 11, 13, 13,
               10, 14, NA, NA, 10, 9, 7, 9, 7, 10,
               10, 14, NA, NA, 10, 9, 7, 10, 9, 11,
               10, 12, NA, NA, 13, 7, 7, 11, 9, 16,
               14, 14, NA, NA, 12, 10, 8, 12, 9, 15),
  ecotype = rep("coastal", 100)
)
# Alternative approach without using pipes
coastal_data <- subset(data, ecotype == "coastal" & !is.na(est_start))

# Create box plots comparing Control vs Salt for coastal ecotype
# Using est_start (start of necrosis) as the y-value

# Create basic boxplot using base R (no pipes required)
boxplot(est_start ~ treatment, data = coastal_data,
        main = "Time to Start of Necrosis: Control vs Salt (Coastal Ecotype)",
        xlab = "Treatment",
        ylab = "Days to Start of Necrosis",
        col = c("lightgreen", "lightblue"))

# If ggplot2 is available, we can use it for a more advanced plot
if (require(ggplot2)) {
  # Create a ggplot boxplot
  p <- ggplot(coastal_data, aes(x = treatment, y = est_start, fill = treatment)) +
    geom_boxplot() +
    labs(title = "Time to Start of Necrosis: Control vs Salt (Coastal Ecotype)",
         x = "Treatment",
         y = "Days to Startof Necrosis") +
    scale_fill_manual(values = c("Control" = "lightgreen", "Salt" = "lightblue")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p)
  
  # Add individual data points with jitter
  p2 <- ggplot(coastal_data, aes(x = treatment, y = est_start, fill = treatment)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(title = "Time to Start of Necrosis: Control vs Salt (Coastal Ecotype)",
         subtitle = "Individual data points shown with jitter",
         x = "Treatment",
         y = "Days to Start of Necrosis") +
    scale_fill_manual(values = c("Control" = "lightgreen", "Salt" = "lightblue")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p2)

}

# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)

#BHE plots
bhe_data <- filter(dat, pop=="BHE")

bhe_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_start) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til start of necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))

bhe_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_full) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til full necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))

#HEC plots

hec_data <- filter(dat, pop=="HEC")

hec_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_start) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til start of necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))

hec_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_full) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til full necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))


# OPB plots

opb_data <- filter(dat, pop=="OPB")

opb_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_start) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til start of necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))

opb_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_full) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til full necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))


#PGR plots

pgr_data <- filter(dat, pop=="PGR")

pgr_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_start) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til start of necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))

pgr_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_full) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til full necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))



#SWB plots

swb_data <- filter(dat, pop=="PGR")

swb_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_start) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til start of necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))

swb_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_full) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til full necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))


#LMC plots

lmc_data <- filter(dat, pop=="PGR")

lmc_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_start) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til start of necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))

lmc_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_full) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til full necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))



#OAE plots

oae_data <- filter(dat, pop=="PGR")

oae_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_start) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til start of necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))

oae_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_full) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til full necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))


#RGR plots

rgr_data <- filter(dat, pop=="PGR")

rgr_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_start) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til start of necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))

rgr_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_full) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til full necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))


#SWC plots

swc_data <- filter(dat, pop=="PGR")

swc_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_start) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til start of necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))

swc_data %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_full) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til full necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))



#TOR plates
# subset a dataframe
tor <- filter(dat, pop=="TOR")

tor %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_start) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til start of necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))

tor %>% ggplot() +
  aes(x = treatment, fill = treatment, y = est_full) +
  geom_boxplot(outliers = F) +
  geom_jitter(position=position_jitter(0.1)) +
  # Labels
  scale_x_discrete(name = 'Treatment') +
  scale_y_continuous(
    name = 'Days til full necrosis',
  ) +
  # Style
  scale_fill_manual(values = c('#514663', '#CACF85')) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size=12),
    axis.title = element_text(size=14))
library(ggplot2)   

View(tor)

m_nest <- aov(data=dat, est_start ~ treatment * ecotype/pop)
summary(m_nest)
coefficients(m_nest)

# Residual diagnostics to see if data meet assumptions
res.vals <- resid(m_nest)
pred.vals <- fitted(m_nest) # Fitted values
# Is there a pattern in the residuals?
plot(pred.vals, res.vals, main = "Residuals vs. pred.vals values",
     las = 1, xlab = "Predicted values", ylab = "Residuals", pch = 19)
abline(h = 0)

# Make qqnorm plot and see if points are along diagonal
qqnorm(res.vals, pch = 19)
qqline(res.vals, col = 'red')








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

# make treatment lowercase
dat$treatment <- str_to_lower(dat$treatment)

# indicate combinations of ecotype and treatment for easy plotting
dat$eco_trt <- paste(dat$ecotype, dat$treatment, sep=" ")

lab <- as.data.frame(rbind(label=c("a","b","c","d"),
                           y=c(13,13,13,13)
                           ))
  
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

m_nest_start <- aov(data=dat, est_start ~ treatment * ecotype/pop)
summary(m_nest_start)
coefficients(m_nest_start)

m_nest_full <- aov(data=dat, est_full ~ treatment * ecotype/pop)
summary(m_nest_full)
coefficients(m_nest_full)

# Residual diagnostics to see if data meet assumptions
res.vals <- resid(m_nest_start)
pred.vals <- fitted(m_nest_start) # Fitted values
# Is there a pattern in the residuals?
plot(pred.vals, res.vals, main = "Residuals vs. pred.vals values",
     las = 1, xlab = "Predicted values", ylab = "Residuals", pch = 19)
abline(h = 0)

# Make qqnorm plot and see if points are along diagonal
qqnorm(res.vals, pch = 19)
qqline(res.vals, col = 'red')

# Plot LSM times to start of necrosis

shapes <- rep(c(21, 22, 23, 24, 25, 25, 21, 23, 22, 24), 2)

lsms <- emmip(m_nest_start, pop ~ ecotype*treatment,CIs=TRUE, plotit=FALSE)

lsms$color <- ifelse(lsms$ecotype == "coastal", '#514663', '#cacf85')

start_plot <- emmip(m_nest_start, pop ~ ecotype*treatment,CIs=TRUE, col = lsms$color,
                dotarg = list(shape = shapes, cex = 5, col="black",
                              fill = lsms$color), 
                linearg = list(linetype="solid", col="black"), type = "response", nesting.order=TRUE, plotit = T, dodge = 0.4) +
  ylab('Days to start of necrosis') + xlab("Media and Ecotype") +
  theme(axis.text = element_text(size = 12))+
  scale_x_discrete(limits = c("coastal control", "coastal salt", "inland control", "inland salt"))


full_plot <- emmip(m_nest_full, pop ~ ecotype*treatment,CIs=TRUE, col = lsms$color,
                    dotarg = list(shape = shapes, cex = 5, col="black",
                                  fill = lsms$color), 
                    linearg = list(linetype="solid", col="black"), type = "response", nesting.order=TRUE, plotit = T, dodge = 0.4) +
  ylab('Days to full necrosis') + xlab("Media and Ecotype") +
  theme(axis.text = element_text(size = 12))+
  scale_x_discrete(limits = c("coastal control", "coastal salt", "inland control", "inland salt"))



ggarrange(succulence_plot)
ggsave(ggarrange(succulence_plot, start_plot, 
                 nrow=1, ncol=2), 
       filename = "tolerance_traits_fig.svg", 
       path = "./Results/Figures/SVGs_for_MS/",
       device="svg", width = 10, height = 3.5, units = "in")

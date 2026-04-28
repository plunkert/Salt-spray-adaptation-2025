# This script analyzes necrosis data scored visually from plates that were supplemented with
# 100 mM NaCl (salt) or not (control), with 5 coastal and 5 inland accessions. 


library(readxl)
library(dplyr)
library(ggplot2)
library(emmeans)
library(glmmTMB)

dat <- read_excel("~/Documents/Github/Salt-spray-adaptation-2025/Data/Leaf_disc_date_data.xlsx")
View(dat)
colnames(dat) <- c("leaf_id", "est_start", "est_full", "Notes_1", "Notes_2", "disc_num" )

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

# indicate combinations of ecotype and treatment for easier plotting
dat$eco_trt <- paste(dat$ecotype, dat$treatment, sep=" ")

# indicate which pair each accession belongs to
dat$pair <- case_when(dat$pop == "OPB" | dat$pop == "RGR" ~ "OPB/RGR",
                      dat$pop == "HEC" | dat$pop == "SWC" ~ "HEC/SWC",
                      dat$pop == "BHE" | dat$pop == "OAE" ~ "BHE/OAE",
                      dat$pop == "SWB" | dat$pop == "LMC" ~ "SWB/LMC",
                      dat$pop == "PGR" | dat$pop == "TOR" ~ "PGR/TOR",
                           .default=NA_character_) %>% as.factor()


# test for interaction between ecotype and treatment for start of necrosis

out_start <- glmmTMB(data=dat, est_start ~ ecotype * treatment + (1|pair/pop))
summary(out_start)

# test for interaction between ecotype and treatment for full necrosis
out_full <- glmmTMB(data=dat, est_full ~ ecotype * treatment + (1|pair/pop))
summary(out_full)

# Residual diagnostics to see if data meet assumptions
res.vals <- resid(out_start)
pred.vals <- fitted(out_start) # Fitted values
# Is there a pattern in the residuals?
plot(pred.vals, res.vals, main = "Residuals vs. pred.vals values",
     las = 1, xlab = "Predicted values", ylab = "Residuals", pch = 19)
abline(h = 0)

# Make qqnorm plot and see if points are along diagonal
qqnorm(res.vals, pch = 19)
qqline(res.vals, col = 'red')

# Plot LSM times to start of necrosis for ecotype and treatment combos
start_necrosis_plot <- emmeans(out_start, specs=c("ecotype", "treatment")) %>% as.data.frame() %>% ggplot() +
  aes(x=treatment, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Media Treatment",y="Days to start of necrosis")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position="none")

full_necrosis_plot <- emmeans(out_start, specs=c("ecotype", "treatment")) %>% as.data.frame() %>% ggplot() +
  aes(x=treatment, y=emmean, fill = ecotype, col=ecotype, shape=ecotype, ymax=upper.CL, ymin=lower.CL) +
  scale_fill_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  scale_color_manual(values=c('#514663','#cacf85', '#514663','#cacf85'))+
  geom_pointrange(position = position_dodge(width = .45), size=1.2) + 
  labs(x="Media Treatment",y="Days to full necrosis")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), legend.position=c(0.75, 0.9))


# Cheating a little to make multipanel figures; need to have succulence figure from 
# leaf_surface_trait_analysis_clean.R run to make this work
tolerance_fig <- plot_grid(succ_ecotype_plot, start_necrosis_plot, full_necrosis_plot, ncol = 3, labels = "AUTO", label_size=18, align="hv")

save_plot("./Results/Figures/tolerance_main_text_fig_lsm.svg", plot=tolerance_fig, base_width = 9, base_height = 5)

# Make a function that takes a model and outputs a CSV of fixed effects, std error, and their significance
glmmtmbTable <- function(model, title){
  summary(model)
  # make vector of effects and what they mean
  effect <- c("Intercept(coastal water)", "Ecotype(inland)", "Treatment(salt)", "Treatment(salt):Ecotype(inland)")
  tbl <- as.data.frame(cbind(effect, coef(summary(model))$cond[,c(1:4)]))
  
  colnames(tbl) <- c("Effect", "Estimate", "SE", "z-value", "p-value")
  csv <- tbl %>% mutate(Estimate = round(as.numeric(Estimate), 4),
                        SE = round(as.numeric(SE), 4),
                        `p-value` = case_when(as.numeric(`p-value`) < 0.0001 ~ "<0.0001",
                                              .default = as.character(round(as.numeric(`p-value`), 4))))
  write.csv(csv, file = paste("./Results/Tables/tables_CSV_format/", title, "_glmmTMB_table.csv", sep=""), row.names=FALSE)
}

glmmtmbTable(out_start, "Necrosis Start")
glmmtmbTable(out_full, "Necrosis Full")


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

ggsave(start_plot, 
       filename = "start_necrosis_plates_coastal_inland.svg", 
       path = "./Results/Figures/SVGs_for_MS/",
       device="svg", width = 6, height = 6, units = "in")

ggsave(start_plot, 
       filename = "full_necrosis_plates_coastal_inland.svg", 
       path = "./Results/Figures/SVGs_for_MS/",
       device="svg", width = 6, height = 6, units = "in")


# Make tables reporting nested ANOVA results

# Make a function that takes a nested ANOVA and outputs a formatted table and a CSV
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

anovaTable(m_nest_start, "Days to Start of Necrosis")

anovaTable(m_nest_full, "Days to Full Necrosis")

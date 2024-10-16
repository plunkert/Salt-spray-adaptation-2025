# This script checks whether it's necessary to score stomata on three images per peel
# by looking at the variance within peels.

# Load in data
require(tidyverse)
require(readxl)

# Data wrangling
stomata <- read_csv('./Leaf-surface-traits-2024/Data/stomatal_density_data_10092024.csv') %>%
  # Remove missing data
  filter(!is.na(stomata_count))
# remove unnecessary columns (these will be necessary later but not right now!)
stomata <- stomata[,c(1,2)]

#Create column with unique ID for each peel (so remove view replicate from file name)
stomata$peel <- as.factor(substring(stomata$file_name, 1, nchar(stomata$file_name)-2))

# What's the standard deviation of each group?
stdevs <- cbind(peel=levels(stomata$peel), sd=rep(NA, length(levels(stomata$peel))))
for(i in 1:length(levels(stomata$peel))){
  temp <- filter(stomata, peel == levels(stomata$peel[i]))
  stdevs[i,2] <- sd(temp$stomata_count)
}

stomata_sd <- stomata %>% group_by(peel) %>% summarise(sd = sd(.data[['stomata_count']], na.rm=TRUE))

hist(stomata_sd$sd)

# hm a lot of those are quite high


# Are the three stomata images for each peel correlated with one another?

stomata <- separate(stomata,
    col = file_name, 
    sep = '_',
    into = c('pop.code', 'replicate', 'leaf_side', 'view_rep')
  ) 


wider_stomata <- stomata[,-c(1:3)] %>% 
  pivot_wider(names_from = view_rep,
              values_from = stomata_count,
)
colnames(wider_stomata) <- c("peel", "view_1", "view_2", "view_3")

# remove peels that don't have all 3 values
wider_stomata <- na.omit(wider_stomata)

par(mfrow=c(1,3))
plot(wider_stomata$view_1 ~ wider_stomata$view_2)
abline(a=0, b=1)
plot(wider_stomata$view_1 ~ wider_stomata$view_3)
abline(a=0, b=1)
plot(wider_stomata$view_2 ~ wider_stomata$view_3)
abline(a=0, b=1)
dev.off()
cor(wider_stomata$view_1, wider_stomata$view_2)

cor(wider_stomata$view_2, wider_stomata$view_3)

cor(wider_stomata$view_1, wider_stomata$view_3)


# Add mean
wider_stomata$mean <- (wider_stomata$view_1 + wider_stomata$view_2 + wider_stomata$view_3)/3




# soil mositure figure for Alix

library(tidyverse)
library(here)
library(cowplot)
theme_set(theme_cowplot())

# Description of dataset
# measurements of height of individuals (IndTag) in years 2013, 2014, 2015 and 2017 across species, Site and Treatment

# Importing data
conway <- read.csv(here("data/benchmarking/stand/Conway/Conway_Exclosure.csv"))
summary(conway)
head(conway)

# selecting just important variables for height
conwayHeight <- conway %>%
  select(Site, SiteTreatment, Treatment,
         Species, TAG, IndTag, Height2013, Height2014,
         Height2015, Height2017) %>%
  mutate(Height2013 = as.numeric(Height2013),
         Height2014 = as.numeric(Height2014),
         Height2015 = as.numeric(Height2015),
         Height2017 = as.numeric(Height2017))

# NOTE 
# Sometimes height values are NA
# often means couldn't find tag in a subsequent year
# Task: filter out individuals with NAs that persist across 2014, 2015 and 2017

# which values are NA?
naTags <- conwayHeight %>%
  filter(is.na(Height2014) & is.na(Height2015) & is.na(Height2017)) %>%
  select(IndTag)

conwayHeight = conwayHeight %>% # taking out trees that were na in 2014 - 2017
  filter(!IndTag %in% c("BF76EBS4", "BF82CPB6", 
                     "BF84ETA9", "BF82CTA15",
                     "BF72CTA2"))

test <- conwayHeight %>%
  group_by(IndTag) %>%
  mutate(growth2013_2014 = ifelse(!is.na(Height2014), # if 2014 is present
                                 Height2014 - Height2013, NA), # use 2014 - 2013
         growth2014_2015 = ifelse(!is.na(Height2015) & !is.na(Height2014), # if 2014 is present AND 2015 is present 
                                 Height2015 - Height2014, NA), # use 2015 - 2014
         growth2015_2017 = ifelse(!is.na(Height2015) & !is.na(Height2017), # if 2015 is present AND 2017 is present
                                 Height2017 - Height2015, NA), # use 2017 - 2015
         growth2013_2015 = ifelse(is.na(Height2014) & !is.na(Height2015), # if 2014 is not present but 2015 is
                                 Height2015 - Height2013, NA), # use 2015 - 2013
         growth2013_2017 = ifelse(is.na(Height2014) & is.na(Height2015) & !is.na(Height2017), # if 2014 and 2015 are missing but 2017 is present
                                 Height2017 - Height2013, NA), # use 2017 - 2013
         growth2014_2017 = ifelse(is.na(2015) & !is.na(Height2014) & !is.na(Height2017), # if 2015 is missing but 2017 and 2014 are present
                                  Height2017 - Height2014, NA)) # use 2017 - 2014
test_pivot <- test %>%
  pivot_longer(cols = c(growth2013_2014, growth2014_2015, growth2015_2017,
                        growth2013_2015, growth2013_2017, growth2014_2017),
               names_to = "Timeframe", values_to = "Growth") %>%
  select(Site, Treatment, Timeframe, Species, Growth)
growthRate_piv <- test_pivot %>%
  mutate(Years = ifelse(Timeframe == "growth2013_2014", 1, 
                        ifelse(Timeframe == "growth2014_2015", 1,
                               ifelse(Timeframe == "growth2015_2017", 2,
                                      ifelse(Timeframe == "growth2013_2015", 2,
                                             ifelse(Timeframe == "growth2013_2017", 4,
                                                    ifelse(Timeframe == "growth2014_2017", 3, NA)))))))
growthRate <- growthRate_piv %>%
  mutate(rate = Growth / Years) %>%
  ungroup() %>%
  select(Site, Treatment, Species, rate) %>%
  drop_na(rate) %>%
  mutate(Species = ifelse(Species == "aspen", "Aspen",
                          ifelse(Species == "birch", "Birch", "Spruce")))

BF76 <- growthRate %>%
  filter(Site == "BF76") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
    geom_point(position = position_jitterdodge(dodge.width = 0.8), 
               alpha=0.2, size = 1) +
    geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(y = "Growth Rate (cm/year)", x ="",
       title = "Site BF76",
       subtitle = "Av. soil moisture = 8.85%") +
  ylim(-50, 100) + theme(legend.position = "none")
BF76         

BF86 <- growthRate %>%
  filter(Site == "BF86") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha = 0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "Av. soil moisture = 25.5%",
       title = "BF86",
       x = "", y = "")  + theme(legend.position = "none") +
  ylim(-50, 100)        
BF86

BF72 <- growthRate %>%
  filter(Site == "BF72") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "Av. soil moisture = 29.75%",
       title = "BF72",
       x = "", y = "")  +
  ylim(-50, 100) + theme(legend.position = "none")
BF72

BF79 <- growthRate %>%
  filter(Site == "BF79") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "Av. soil moisture = 43.3%",
       title = "BF79",
       x = "", y = "")   +
  ylim(-50, 100) + theme(legend.position = "none")
BF79

BF84 <- growthRate %>%
  filter(Site == "BF84") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "Av. soil moisture = 47.1%",
       title = "BF84",
       x = "", y = "")   +
  ylim(-50, 100)
BF84

plot <- plot_grid(BF76, BF86, BF72, BF79, BF84,
          rel_widths = c(0.85, 0.85, 0.85, 0.85, 1.1),
          ncol = 5)
png('test.png', width = 1800, height = 300)
print(plot)
dev.off()
# save width height
# 1600 by 400
         
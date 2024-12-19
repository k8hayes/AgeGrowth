# soil mositure figure for Alix

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())

# Description of dataset
# measurements of height of individuals (IndTag) in years 2013, 2014, 2015 and 2017 across species, Site and Treatment

# Importing data
conway <- read.csv("data/Conway_Exclosure.csv")
summary(conway)
head(conway)

# selecting just important variables for height
conwayHeight <- conway %>%
  filter(Site != "BF82") %>%
  select(Site, SiteTreatment, Treatment,
         Species, TAG, IndTag, Height2013, Height2014,
         Height2015, Height2017) %>%
  mutate(Height2013 = as.numeric(Height2013),
         Height2014 = as.numeric(Height2014),
         Height2015 = as.numeric(Height2015),
         Height2017 = as.numeric(Height2017))

# What is the mean canopy height per site at year 2013?
maxHeight = read.csv("data/ConwayMaxHeight.csv")

# NOTE 
# Sometimes height values are NA
# often means couldn't find tag in a subsequent year
# Task: filter out individuals with NAs that persist across 2014, 2015 and 2017

# which values are NA?
naTags <- conwayHeight %>%
  filter(is.na(Height2014) & is.na(Height2015) & is.na(Height2017)) %>%
  select(IndTag)

conwayHeight = conwayHeight %>% # taking out trees that were na in 2014 - 2017
  filter(!IndTag %in% c("BF76EBS4", 
                     "BF84ETA9",
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
                          ifelse(Species == "birch", "Birch", "Black Spruce")))


rm(test, test_pivot, naTags, conway, growthRate_piv)


growthRate = growthRate %>%
  mutate(Label = ifelse(Site == "BF72" & Species == "Aspen", # Site BF72
                        "Av. Initial Height = 200cm", 
                 ifelse(Site == "BF72" & Species == "Birch",
                        "Av. Initial Height = 182cm", 
                 ifelse(Site == "BF72" & Species == "Black Spruce",
                         "Av. Initial Height = 46cm",
                          ifelse(Site == "BF76" & Species == "Aspen", # Site BF76
                                 "Av. Initial Height = 234cm", 
                          ifelse(Site == "BF76" & Species == "Birch",
                                 "Av. Initial Height = 330cm", 
                           ifelse(Site == "BF76" & Species == "Black Spruce",
                                  "Av. Initial Height = 69cm",
                                   ifelse(Site == "BF79" & Species == "Aspen", # Site BF79
                                          "Av. Initial Height = 116cm", 
                                   ifelse(Site == "BF79" & Species == "Birch", 
                                          "Av. Initial Height = 175cm", 
                                   ifelse(Site == "BF79" & Species == "Black Spruce",
                                          "Av. Initial Height = 56cm",
                                          ifelse(Site == "BF84" & Species == "Aspen", # Site BF84
                                                 "Av. Initial Height = 100cm", 
                                          ifelse(Site == "BF84" & Species == "Birch", 
                                                 "Av. Initial Height = 135cm", 
                                          ifelse(Site == "BF84" & Species == "Black Spruce",
                                                  "Av. Initial Height = 54cm",
                                                  ifelse(Site == "BF86" & Species == "Aspen", # Site BF86
                                                          "Av. Initial Height = 122cm", 
                                                  ifelse(Site == "BF86" & Species == "Birch", 
                                                         "Av. Initial Height = 251cm", "Av. Initial Height = 74cm")))))))))))))))


# Plot Birch ######################

BF72 = growthRate %>%
  filter(Site == "BF72") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, color = Species)) + 
  facet_wrap(~Species+Label) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(y = "Growth Rate (cm/year)", x ="",
       title = "Site BF72") +
  ylim(-50, 100) + theme(legend.position = "none", 
                         axis.title.x=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank()) + 
  scale_color_manual(values = c("black", "black", "black"))

BF76 = growthRate %>%
  filter(Site == "BF76") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, color = Species)) + 
  facet_wrap(~Species+Label) + 
  geom_boxplot(outlier.shape = NA) +
    geom_point(position = position_jitterdodge(dodge.width = 0.8), 
               alpha=0.2, size = 1) +
    geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(y = "Growth Rate (cm/year)", x ="",
       title = "Site BF76") +
  ylim(-50, 100) + theme(legend.position = "none") + 
  scale_color_manual(values = c("black", "black", "black"))

BF79 = growthRate %>%
  filter(Site == "BF79") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, color = Species)) + 
  facet_wrap(~Species+Label) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(y = "Growth Rate (cm/year)", x ="",
       title = "Site BF79") +
  ylim(-50, 100) + theme(legend.position = "none",
                         axis.title.x=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank()) + 
  scale_color_manual(values = c("black", "black", "black"))

BF84 = growthRate %>%
  filter(Site == "BF84") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, color = Species)) + 
  facet_wrap(~Species+Label) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(y = "Growth Rate (cm/year)", x ="",
       title = "Site BF84") +
  ylim(-50, 100) + theme(legend.position = "none",axis.title.x=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank()) + 
  scale_color_manual(values = c("black", "black", "black"))

BF86 = growthRate %>%
  filter(Site == "BF86") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, color = Species)) + 
  facet_wrap(~Species+Label) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(y = "Growth Rate (cm/year)", x ="",
       title = "Site BF86") +
  ylim(-50, 100) + theme(legend.position = "none",
                         axis.title.x=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank()) + 
  scale_color_manual(values = c("black", "black", "black"))

plot <- plot_grid(BF84, BF79, BF72, BF86, BF76,
                        nrow = 5)
ggsave2(plot = plot,
        filename = "growthRate.png",
        path = "figures/growthRate/",
        width = 12, height = 20, units = "in")

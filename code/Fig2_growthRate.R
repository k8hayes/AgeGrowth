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
  select(Site, SiteTreatment, Treatment,
         Species, TAG, IndTag, Height2013, Height2014,
         Height2015, Height2017) %>%
  mutate(Height2013 = as.numeric(Height2013),
         Height2014 = as.numeric(Height2014),
         Height2015 = as.numeric(Height2015),
         Height2017 = as.numeric(Height2017))

# What is the mean canopy height per site at year 2013?
conwayHeight %>%
  group_by(Site) %>%
  summarise(mean = mean(Height2013))


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

# Plot Birch ######################

BF76_birch <- growthRate %>%
  filter(Site == "BF76") %>%
  filter(Species == "Birch") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, color = Species)) + 
    geom_point(position = position_jitterdodge(dodge.width = 0.8), 
               alpha=0.2, size = 1) +
    geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(y = "Growth Rate (cm/year)", x ="",
       title = "Site BF76",
       subtitle = "Average starting canopy height = 108 cm") +
  ylim(-50, 100) + theme(legend.position = "none") + 
  scale_color_manual(values = "black")
BF76_birch         

BF86_birch <- growthRate %>%
  filter(Site == "BF86") %>%
  filter(Species == "Birch") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha = 0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "Average starting canopy height = 108 cm",
       title = "Site BF86",
       x = "", y = "Growth Rate (cm/year)")  + theme(legend.position = "none") +
  ylim(-50, 100)        + 
  scale_color_manual(values = "black")
BF86_birch

BF72_birch <- growthRate %>%
  filter(Site == "BF72") %>%
  filter(Species == "Birch") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "Average starting canopy height = 108 cm",
       title = "Site BF72",
       x = "", y = "Growth Rate (cm/year)")  +
  ylim(-50, 100) + theme(legend.position = "none") + 
  scale_color_manual(values = "black")
BF72_birch

BF79_birch <- growthRate %>%
  filter(Site == "BF79") %>%
  filter(Species == "Birch") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "Average starting canopy height = 108 cm",
       title = "Site BF79",
       x = "", y = "Growth Rate (cm/year)")   +
  ylim(-50, 100) + theme(legend.position = "none") + 
  scale_color_manual(values = "black")
BF79_birch

BF84_birch <- growthRate %>%
  filter(Site == "BF84") %>%
  filter(Species == "Birch") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "Average starting canopy height = 108 cm",
       title = "Site BF84",
       x = "", y = "Growth Rate (cm/year)")   +
  ylim(-50, 100) + theme(legend.position = "none") + 
  scale_color_manual(values = "black")
BF84_birch

plot_birch <- plot_grid(BF84_birch, BF79_birch, BF72_birch, BF86_birch, BF76_birch,
          nrow = 5)
plot_birch

ggsave2(plot = plot_birch,
        filename = "birch.png",
        path = "figures/growthRate/",
        width = 4, height = 18, units = "in")

# Plot aspen #########################

BF76_aspen <- growthRate %>%
  filter(Site == "BF76") %>%
  filter(Species == "Aspen") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, color = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(y = "", x =" ",
       title = " ",
       subtitle = "") +
  ylim(-50, 100) + theme(legend.position = "none") + 
  scale_color_manual(values = "black")
BF76_aspen        

BF86_aspen <- growthRate %>%
  filter(Site == "BF86") %>%
  filter(Species == "Aspen") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha = 0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "",
       title = " ",
       x = "", y = " ")  + theme(legend.position = "none") +
  ylim(-50, 100)        + 
  scale_color_manual(values = "black")
BF86_aspen

BF72_aspen <- growthRate %>%
  filter(Site == "BF72") %>%
  filter(Species == "Aspen") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "",
       title = " ",
       x = "", y = "")  +
  ylim(-50, 100) + theme(legend.position = "none") + 
  scale_color_manual(values = "black")
BF72_aspen

BF79_aspen <- growthRate %>%
  filter(Site == "BF79") %>%
  filter(Species == "Aspen") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "",
       title = " ",
       x = "", y = "")   +
  ylim(-50, 100) + theme(legend.position = "none") + 
  scale_color_manual(values = "black")
BF79_aspen

BF84_aspen <- growthRate %>%
  filter(Site == "BF84") %>%
  filter(Species == "Aspen") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = " ",
       title = " ",
       x = "", y = " ")   +
  ylim(-50, 100) + theme(legend.position = "none") + 
  scale_color_manual(values = "black")
BF84_aspen

BF82_aspen <- growthRate %>%
  filter(Site == "BF82") %>%
  filter(Species == "Aspen") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "",
       title = " ",
       x = "", y = "")   +
  ylim(-50, 100) + theme(legend.position = "none") + 
  scale_color_manual(values = "black")
BF82_aspen

plot_aspen <- plot_grid(BF84_aspen, BF79_aspen, BF72_aspen, BF86_aspen, BF76_aspen,
                        nrow = 5)
plot_aspen

ggsave2(plot = plot_aspen,
        filename = "aspen.png",
        path = "figures/growthRate/",
        width = 4, height = 18, units = "in")   

# Plot spruce #########################

BF76_spruce <- growthRate %>%
  filter(Site == "BF76") %>%
  filter(Species == "Spruce") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, color = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(y = "", x =" ",
       title = " ",
       subtitle = "") +
  ylim(-50, 100) + theme(legend.position = "none") + 
  scale_color_manual(values = "black")
BF76_spruce        

BF86_spruce <- growthRate %>%
  filter(Site == "BF86") %>%
  filter(Species == "Spruce") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha = 0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "",
       title = " ",
       x = "", y = " ")  + theme(legend.position = "none") +
  ylim(-50, 100)        + 
  scale_color_manual(values = "black")
BF86_spruce

BF72_spruce <- growthRate %>%
  filter(Site == "BF72") %>%
  filter(Species == "Spruce") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "",
       title = " ",
       x = "", y = "")  +
  ylim(-50, 100) + theme(legend.position = "none") + 
  scale_color_manual(values = "black")
BF72_spruce

BF79_spruce <- growthRate %>%
  filter(Site == "BF79") %>%
  filter(Species == "Spruce") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "",
       title = " ",
       x = "", y = "")   +
  ylim(-50, 100) + theme(legend.position = "none") + 
  scale_color_manual(values = "black")
BF79_spruce

BF84_spruce <- growthRate %>%
  filter(Site == "BF84") %>%
  filter(Species == "Spruce") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = " ",
       title = " ",
       x = "", y = " ")   +
  ylim(-50, 100) + theme(legend.position = "none") + 
  scale_color_manual(values = "black")
BF84_spruce

BF82_spruce <- growthRate %>%
  filter(Site == "BF82") %>%
  filter(Species == "Spruce") %>%
  ggplot(aes(x = as.factor(Treatment), y = rate, col = Species)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.8), 
             alpha=0.2, size = 1) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(subtitle = "",
       title = " ",
       x = "", y = "")   +
  ylim(-50, 100) + theme(legend.position = "none") + 
  scale_color_manual(values = "black")
BF82_spruce

plot_spruce <- plot_grid(BF84_spruce, BF79_spruce, BF72_spruce, BF86_spruce, BF76_spruce,
                        nrow = 5)
plot_spruce

ggsave2(plot = plot_spruce,
        filename = "spruce.png",
        path = "figures/growthRate/",
        width = 4, height = 18, units = "in")         

# plot grid

all <- plot_grid(plot_birch, plot_aspen, plot_spruce,
          ncol = 3)

all

ggsave2(plot = all,
        filename = "all.png",
        path = "figures/growthRate/",
        width = 12, height = 18, units = "in")

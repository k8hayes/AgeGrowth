model <- read.csv("/Users/katherinehayes/Google Drive/Projects/NSF Postdoc/Manuscripts/ConwayAgeGrowth/data/model output/model_data.csv")

model_old <- read.csv("/Users/katherinehayes/Google Drive/Projects/NSF Postdoc/Manuscripts/ConwayAgeGrowth/data/model output/model_data_old.csv")


test_model = model %>%
  filter(Scenario == "No Browsing") %>%
  mutate(treat = "filter")

test_old = model_old  %>%
  filter(Scenario == "No Browsing") %>%
  mutate(treat = "no filter")

test <- full_join(test_model, test_old)

test %>%
  filter(variable == "height") %>%
  filter(species == "Pima") %>%
  ggplot(aes(x = TSF, y = mean, col = treat)) + geom_point()

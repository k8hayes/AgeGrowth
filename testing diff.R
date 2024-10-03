model <- read.csv("/Users/katherinehayes/Google Drive/Projects/NSF Postdoc/Manuscripts/ConwayAgeGrowth/data/model_data.csv")

prefire <- read.csv("/Users/katherinehayes/Google Drive/Projects/NSF Postdoc/Manuscripts/ConwayAgeGrowth/data/prefire.csv")



test_model = model %>%
  filter(Scenario == "No Browsing") %>%
  mutate(treat = "No filter")

test_pre = prefire %>%
  filter(Scenario == "No Browsing") %>%
  mutate(treat = "filter")

test <- full_join(test_model, test_pre )

test %>%
  filter(variable == "height") %>%
  filter(species == "Bene") %>%
  ggplot(aes(x = TSF, y = mean, col = treat)) + geom_point()

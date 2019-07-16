library(tidyverse)
library(reshape2)

# State presidential election results, 1964-2016
state_pres <- read_csv("electoral-deficit/state_presidential_results.csv") %>%
  
  ## Votes to two-party percentages
  mutate(two_party = democrat + republican,
         dem_pct = democrat / two_party,
         rep_pct = republican / two_party,
         margin = dem_pct - rep_pct) %>%
  
  ## Compute lagged margin and change in margin
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(lag_margin = lag(margin),
         margin_change = margin - lag_margin) %>%
  
  ## Select state, year, margin
  dplyr::select(state, year, margin, margin_change) %>%
  ungroup() %>%
  na.omit()

# Correlations in swings
state_corr <- state_pres %>%
  dplyr::select(state, year, margin) %>%
  spread(state, margin) %>%
  dplyr::select(-year) %>%
  cor(method = "pearson")

# Plot the heatmap
state_corr.melt <- melt(state_corr)

ggplot(state_corr.melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 1) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", name = "Correlation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title = "Correlation between state-level presidential election results", subtitle = "Two-party margins, 1964 - 2016")

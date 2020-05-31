library(caret)
library(Hmisc)
library(randomForest)
library(tidyverse)
library(tidycensus)
library(xgboost)

zip_density <- read_csv("CCES/data/zip_code_popdens_2010.csv") %>%
  dplyr::select(zip, zip_density = density)

cces_2016 <- read_csv("CCES/data/2016/cces_2016_abridged.csv") %>%
  mutate(clinton_vote = grepl("Clinton", pres_vote_2016),
         age = 2016 - birthyr,
         age_group = case_when(age <= 29 ~ "18-29",
                               age %in% 30:44 ~ "30-44",
                               age %in% 45:64 ~ "45-64",
                               age >= 65 ~ "65+")) %>%
  left_join(zip_density, by = "zip")

cces_2016_filtered <- cces_2016 %>%
  filter(grepl("Clinton|Trump", pres_vote_2016)) %>%
  mutate(pres_vote_2016 = factor(pres_vote_2016)) %>%
  dplyr::select(id, pres_vote_2016, weight, age, race, educ, gender, faminc, marstat, sexuality, church_attendance, unionhh, zip_density,
                faminc, partyreg) %>%
  na.omit() %>% 
  mutate_if(is.character, as.factor)

# Random forest
set.seed(2020)
pres_vote_rf <- randomForest(pres_vote_2016 ~ age + race + educ + gender + faminc + marstat + sexuality + church_attendance + unionhh + 
                               + zip_density + partyreg, data = cces_2016_filtered, mtry = 2, ntrees = 200)
pres_vote_rf

# XGBoost
cces_2016_matrix <- model.matrix(~ 0 + ., data = cces_2016_filtered %>% dplyr::select(-id, -pres_vote_2016, -weight))
cces_2016_dmatrix <- xgb.DMatrix(data = cces_2016_matrix, label = cces_2016_filtered$pres_vote_2016 == "Hillary Clinton (D)")

set.seed(2020)
xgb_params_list <- list(objective = "binary:logistic",
                        eta = 0.2, 
                        max_depth = 4,
                        nthread = 10,
                        alpha = 0.2)

cces_xgb_cv <- xgb.cv(params = xgb_params_list,
                      data = cces_2016_dmatrix,
                      nrounds = 1000,
                      nfold = 10,
                      early_stopping_rounds = 20)

cces_xgb <- xgboost(data = cces_2016_dmatrix, params = xgb_params_list, nrounds = 52, weight = cces_2016_filtered$weight)

# Evaluating XGBoost performance
cces_2016_preds <- cces_2016_filtered %>%
  mutate(rf_pred = predict(pres_vote_rf, type = "prob")[, "Hillary Clinton (D)"],
         xgb_pred = predict(cces_xgb, newdata = cces_2016_dmatrix)) %>%
  dplyr::select(id, weight, pres_vote_2016, rf_pred, xgb_pred)

# Calibration
cces_2016_preds %>%
  mutate(rf_bin = cut(rf_pred, breaks = quantile(rf_pred, probs = (0:10)/10))) %>%
  group_by(rf_bin) %>%
  summarise(n = n(),
            pred_prob = wtd.mean(rf_pred, weight),
            empirical_prob = wtd.mean(pres_vote_2016 == "Hillary Clinton (D)", weight)) %>%
  na.omit()

cces_2016_preds %>%
  mutate(xgb_bin = cut(xgb_pred, breaks = quantile(xgb_pred, probs = (0:10)/10))) %>%
  group_by(xgb_bin) %>%
  summarise(n = n(),
            pred_prob = wtd.mean(xgb_pred, weight),
            empirical_prob = wtd.mean(pres_vote_2016 == "Hillary Clinton (D)", weight)) %>%
  na.omit()

# XGBoost is better calibrated, so let's use those
cces_2016_with_preds <- cces_2016 %>% 
  left_join(cces_2016_preds, by = c("id", "weight", "pres_vote_2016")) %>%
  mutate(lean = case_when(xgb_pred < 0.2 ~ "Likely Trump",
                          xgb_pred >= 0.2 & xgb_pred < 0.35 ~ "Leans Trump",
                          xgb_pred >= 0.35 & xgb_pred <= 0.65 ~ "Swing voters",
                          xgb_pred > 0.65 & xgb_pred <= 0.8 ~ "Leans Clinton",
                          xgb_pred > 0.8 ~ "Likely Clinton"),
         lean = ordered(lean, levels = c("Likely Clinton", "Leans Clinton", "Swing voters", "Leans Trump", "Likely Trump"))) %>%
  filter(!is.na(lean)) %>%
  mutate(race = case_when(race %in% c("Asian", "Black", "Hispanic", "White") ~ race,
                          !(race %in% c("Asian", "Black", "Hispanic", "White")) ~ "Mixed / Other"),
         generation = case_when(age <= 35 ~ "Millennials/Generation Z (18-35)",
                                age %in% 36:51 ~ "Generation X (36-51)",
                                age %in% 52:70 ~ "ok boomers (52-70)",
                                age %in% 71:88 ~ "Silent Generation (71-88)",
                                age >= 89 ~ "Greatest Generation (89+)") %>%
           ordered(levels = c("Millennials/Generation Z (18-35)", "Generation X (36-51)", "ok boomers (52-70)", "Silent Generation (71-88)",
                              "Greatest Generation (89+)"))) %>%
  left_join(cces_2016_full %>% dplyr::select(id, newsint, know_house_maj, know_sen_maj, internethome, social_read_watch, pastday_tv,
                                             pastday_newspaper, pastday_blog, ideology_trump, ideology_clinton, ideology_democrats,
                                             ideology_republicans), by = "id") %>%
  mutate(newsint = ordered(newsint, levels = c("Don't know", "Hardly at all", "Only now and then", "Some of the time", "Most of the time")))

cces_2016_with_preds %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean) %>%
  summarise(n = n(),
            pct = sum(weight) / mean(wt_sum))

# Cutting the data a bunch of different ways
## By gender
cces_2016_with_preds %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, gender) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  spread(lean, pct, fill = 0)

cces_2016_with_preds %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, Gender = gender) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  bind_rows(cces_2016_with_preds %>%
              mutate(wt_sum = sum(weight)) %>%
              group_by(Gender = gender) %>%
              summarise(pct = sum(weight) / mean(wt_sum)) %>%
              mutate(lean = "All voters")) %>%
  ungroup() %>%
  mutate(lean = ordered(lean, levels = c("All voters", "Swing voters", "Likely Trump", "Leans Trump", "Leans Clinton", "Likely Clinton"))) %>%
  group_by(lean) %>%
  arrange(lean, desc(Gender)) %>%
  mutate(lag_pct = lag(pct),
         lag_pct = ifelse(is.na(lag_pct), 0, lag_pct),
         label_height = 0.5 * pct + cumsum(lag_pct)) %>%
  ggplot(aes(x = lean, fill = Gender)) +
  geom_col(aes(y = pct)) +
  geom_text(aes(y = label_height, label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Breakdown of voter lean segment by gender", x = "Voter lean", y = "Cumulative percentage of voter segment") +
  coord_flip()

## Race
cces_2016_with_preds %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, race) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  spread(lean, pct, fill = 0)

cces_2016_with_preds %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, Race = race) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  bind_rows(cces_2016_with_preds %>%
              mutate(wt_sum = sum(weight)) %>%
              group_by(Race = race) %>%
              summarise(pct = sum(weight) / mean(wt_sum)) %>%
              mutate(lean = "All voters")) %>%
  ungroup() %>%
  mutate(lean = ordered(lean, levels = c("All voters", "Swing voters", "Likely Trump", "Leans Trump", "Leans Clinton", "Likely Clinton"))) %>%
  group_by(lean) %>%
  arrange(lean, desc(Race)) %>%
  mutate(lag_pct = lag(pct),
         lag_pct = ifelse(is.na(lag_pct), 0, lag_pct),
         label_height = 0.5 * pct + cumsum(lag_pct)) %>%
  ggplot(aes(x = lean, fill = Race)) +
  geom_col(aes(y = pct)) +
  geom_text(aes(y = label_height, label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Breakdown of voter lean segment by race", x = "Voter lean", y = "Cumulative percentage of voter segment") +
  coord_flip()

## Age  
cces_2016_with_preds %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, age_group) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  spread(lean, pct, fill = 0)

cces_2016_with_preds %>%
  filter(age <= 88) %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, Generation = generation) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  bind_rows(cces_2016_with_preds %>%
              filter(age <= 88) %>%
              mutate(wt_sum = sum(weight)) %>%
              group_by(Generation = generation) %>%
              summarise(pct = sum(weight) / mean(wt_sum)) %>%
              mutate(lean = "All voters")) %>%
  ungroup() %>%
  mutate(lean = ordered(lean, levels = c("All voters", "Swing voters", "Likely Trump", "Leans Trump", "Leans Clinton", "Likely Clinton"))) %>%
  group_by(lean) %>%
  arrange(lean, desc(Generation)) %>%
  mutate(lag_pct = lag(pct),
         lag_pct = ifelse(is.na(lag_pct), 0, lag_pct),
         label_height = 0.5 * pct + cumsum(lag_pct)) %>%
  ggplot(aes(x = lean, fill = Generation)) +
  geom_col(aes(y = pct)) +
  geom_text(aes(y = label_height, label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Breakdown of voter lean segment by generation", x = "Voter lean", y = "Cumulative percentage of voter segment") +
  coord_flip()

## Education
cces_2016_with_preds %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, educ = ordered(educ, levels = c("No HS", "High school graduate", "Some college", "2-year", "4-year", "Post-grad"))) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  spread(lean, pct, fill = 0)

cces_2016_with_preds %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, Education = ordered(educ, levels = c("No HS", "High school graduate", "Some college", "2-year", "4-year", "Post-grad"))) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  bind_rows(cces_2016_with_preds %>%
              mutate(wt_sum = sum(weight)) %>%
              group_by(Education = ordered(educ, levels = c("No HS", "High school graduate", "Some college", "2-year", "4-year", "Post-grad"))) %>%
              summarise(pct = sum(weight) / mean(wt_sum)) %>%
              mutate(lean = "All voters")) %>%
  ungroup() %>%
  mutate(lean = ordered(lean, levels = c("All voters", "Swing voters", "Likely Trump", "Leans Trump", "Leans Clinton", "Likely Clinton"))) %>%
  group_by(lean) %>%
  arrange(lean, desc(Education)) %>%
  mutate(lag_pct = lag(pct),
         lag_pct = ifelse(is.na(lag_pct), 0, lag_pct),
         label_height = 0.5 * pct + cumsum(lag_pct)) %>%
  ggplot(aes(x = lean, fill = Education)) +
  geom_col(aes(y = pct)) +
  geom_text(aes(y = label_height, label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Breakdown of voter lean segment by educational attainment", x = "Voter lean", y = "Cumulative percentage of voter segment") +
  coord_flip()

## White/college
cces_2016_with_preds %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, `4-year degree` = ifelse(educ %in% c("4-year", "Post-grad"), "Yes", "No"), White = ifelse(race == "White", "Yes", "No")) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  spread(lean, pct, fill = 0)

cces_2016_with_preds %>%
  mutate(`Race/education` = case_when(race == "White" & educ %in% c("4-year", "Post-grad") ~ "White/college",
                                      race == "White" & !(educ %in% c("4-year", "Post-grad")) ~ "White/no college",
                                      race != "White" & educ %in% c("4-year", "Post-grad") ~ "Nonwhite/college",
                                      race != "White" & !(educ %in% c("4-year", "Post-grad")) ~ "Nonwhite/no college")) %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, `Race/education`) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  bind_rows(cces_2016_with_preds %>%
              mutate(`Race/education` = case_when(race == "White" & educ %in% c("4-year", "Post-grad") ~ "White/college",
                                                  race == "White" & !(educ %in% c("4-year", "Post-grad")) ~ "White/no college",
                                                  race != "White" & educ %in% c("4-year", "Post-grad") ~ "Nonwhite/college",
                                                  race != "White" & !(educ %in% c("4-year", "Post-grad")) ~ "Nonwhite/no college"),
                     wt_sum = sum(weight)) %>%
              group_by(`Race/education`) %>%
              summarise(pct = sum(weight) / mean(wt_sum)) %>%
              mutate(lean = "All voters")) %>%
  ungroup() %>%
  mutate(lean = ordered(lean, levels = c("All voters", "Swing voters", "Likely Trump", "Leans Trump", "Leans Clinton", "Likely Clinton"))) %>%
  group_by(lean) %>%
  arrange(lean, desc(`Race/education`)) %>%
  mutate(lag_pct = lag(pct),
         lag_pct = ifelse(is.na(lag_pct), 0, lag_pct),
         label_height = 0.5 * pct + cumsum(lag_pct)) %>%
  ggplot(aes(x = lean, fill = `Race/education`)) +
  geom_col(aes(y = pct)) +
  geom_text(aes(y = label_height, label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Breakdown of voter lean segment by race and college attainment", x = "Voter lean", y = "Cumulative percentage of voter segment") +
  coord_flip()

## Religious attendance
cces_2016_with_preds %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, religiosity = ordered(church_attendance, levels = c("Don't know", "Never", "Seldom", "A few times a year", "Once or twice a month", 
                                                                     "Once a week", "More than once a week"))) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  spread(lean, pct, fill = 0)

cces_2016_with_preds %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, Religiosity = ordered(church_attendance, levels = c("Don't know", "Never", "Seldom", "A few times a year", "Once or twice a month", 
                                                                     "Once a week", "More than once a week"))) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  bind_rows(cces_2016_with_preds %>%
              mutate(wt_sum = sum(weight)) %>%
              group_by(Religiosity = ordered(church_attendance, 
                                             levels = c("Don't know", "Never", "Seldom", "A few times a year", "Once or twice a month", 
                                                        "Once a week", "More than once a week"))) %>%
              summarise(pct = sum(weight) / mean(wt_sum)) %>%
              mutate(lean = "All voters")) %>%
  ungroup() %>%
  mutate(lean = ordered(lean, levels = c("All voters", "Swing voters", "Likely Trump", "Leans Trump", "Leans Clinton", "Likely Clinton"))) %>%
  group_by(lean) %>%
  arrange(lean, desc(Religiosity)) %>%
  mutate(lag_pct = lag(pct),
         lag_pct = ifelse(is.na(lag_pct), 0, lag_pct),
         label_height = 0.5 * pct + cumsum(lag_pct)) %>%
  ggplot(aes(x = lean, fill = Religiosity)) +
  geom_col(aes(y = pct)) +
  geom_text(aes(y = label_height, label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Breakdown of voter lean segment by frequency of religious service attendance", x = "Voter lean", 
       y = "Cumulative percentage of voter segment") +
  coord_flip()

## ZIP code population density
cces_2016_with_preds %>%
  bind_rows(cces_2016_with_preds %>% mutate(lean = "All voters")) %>%
  mutate(lean = ordered(lean, levels = c("Likely Clinton", "Leans Clinton", "Leans Trump", "Likely Trump", "All voters", "Swing voters"))) %>%
  ggplot(aes(x = zip_density, y = ..density.., fill = lean)) +
  facet_wrap(~lean, nrow = 6) +
  geom_histogram(binwidth = 0.05, show.legend = FALSE) +
  scale_fill_manual(name = "Voter lean", values = c("#000066", "#6666FF", "#FF6666", "#660000", "gray1", "#660066")) +
  scale_x_log10(labels = scales::comma_format(accuracy = 1)) +
  labs(title = "Distribution of population density by voter lean", x = "ZIP code population density (log scale, persons per square mile)",
       y = "Probability density")

## News interest
cces_2016_with_preds %>%
  filter(!is.na(newsint)) %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, `News interest` = newsint) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  bind_rows(cces_2016_with_preds %>%
              filter(!is.na(newsint)) %>%
              mutate(wt_sum = sum(weight)) %>%
              group_by(`News interest` = newsint) %>%
              summarise(pct = sum(weight) / mean(wt_sum)) %>%
              mutate(lean = "All voters")) %>%
  ungroup() %>%
  mutate(lean = ordered(lean, levels = c("All voters", "Swing voters", "Likely Trump", "Leans Trump", "Leans Clinton", "Likely Clinton"))) %>%
  group_by(lean) %>%
  arrange(lean, desc(`News interest`)) %>%
  mutate(lag_pct = lag(pct),
         lag_pct = ifelse(is.na(lag_pct), 0, lag_pct),
         label_height = 0.5 * pct + cumsum(lag_pct)) %>%
  filter(!is.na(`News interest`)) %>%
  ggplot(aes(x = lean, fill = `News interest`)) +
  geom_col(aes(y = pct)) +
  geom_text(aes(y = label_height, label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Breakdown of voter lean segment by news interest", x = "Voter lean", y = "Cumulative percentage of voter segment") +
  coord_flip()

## News consumption
cces_2016_with_preds %>%
  filter(!is.na(pastday_tv), !is.na(pastday_newspaper)) %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, `TV` = pastday_tv, `Newspaper` = pastday_newspaper) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  bind_rows(cces_2016_with_preds %>%
              mutate(wt_sum = sum(weight)) %>%
              group_by(`TV` = pastday_tv, `Newspaper` = pastday_newspaper) %>%
              summarise(pct = sum(weight) / mean(wt_sum)) %>%
              mutate(lean = "All voters")) %>%
  ungroup() %>%
  mutate(lean = ordered(lean, levels = c("All voters", "Swing voters", "Likely Trump", "Leans Trump", "Leans Clinton", "Likely Clinton")),
         `Past day news consumption` = case_when(TV & Newspaper ~ "Both",
                                                 !TV & Newspaper ~ "Newspaper only",
                                                 TV & !Newspaper ~ "TV only",
                                                 !TV & !Newspaper ~ "Neither")) %>%
  group_by(lean) %>%
  arrange(lean, desc(`Past day news consumption`)) %>%
  mutate(lag_pct = lag(pct),
         lag_pct = ifelse(is.na(lag_pct), 0, lag_pct),
         label_height = 0.5 * pct + cumsum(lag_pct)) %>%
  filter(!is.na(`Past day news consumption`)) %>%
  ggplot(aes(x = lean, fill = `Past day news consumption`)) +
  geom_col(aes(y = pct)) +
  geom_text(aes(y = label_height, label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Breakdown of voter lean segment by daily news consumption", x = "Voter lean", y = "Cumulative percentage of voter segment") +
  coord_flip()

## Ideology
cces_2016_with_preds %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, Ideology = ordered(ideology, levels = c("Not sure", "Very conservative", "Conservative", "Moderate", "Liberal",
                                                         "Very liberal"))) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  spread(lean, pct, fill = 0)

cces_2016_with_preds %>%
  filter(!is.na(ideology)) %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, Ideology = ordered(ideology, levels = c("Not sure", "Very conservative", "Conservative", "Moderate", "Liberal",
                                                         "Very liberal"))) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  bind_rows(cces_2016_with_preds %>%
              filter(!is.na(ideology)) %>%
              mutate(wt_sum = sum(weight)) %>%
              group_by(Ideology = ordered(ideology, levels = c("Not sure", "Very conservative", "Conservative", "Moderate", "Liberal",
                                                               "Very liberal"))) %>%
              summarise(pct = sum(weight) / mean(wt_sum)) %>%
              mutate(lean = "All voters")) %>%
  ungroup() %>%
  mutate(lean = ordered(lean, levels = c("All voters", "Swing voters", "Likely Trump", "Leans Trump", "Leans Clinton", "Likely Clinton"))) %>%
  group_by(lean) %>%
  arrange(lean, desc(Ideology)) %>%
  mutate(lag_pct = lag(pct),
         lag_pct = ifelse(is.na(lag_pct), 0, lag_pct),
         label_height = 0.5 * pct + cumsum(lag_pct)) %>%
  ggplot(aes(x = lean, fill = Ideology)) +
  geom_col(aes(y = pct)) +
  geom_text(aes(y = label_height, label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = "Ideology", values = c("#888888", "#660000", "#FF6666", "#660066", "#6666FF", "#000066")) +
  theme(legend.position = "bottom") +
  labs(title = "Breakdown of voter lean segment by political ideology", x = "Voter lean", y = "Cumulative percentage of voter segment") +
  coord_flip()

## Party ID (3-point)
cces_2016_with_preds %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, Party = ordered(pid3, levels = c("Other", "Republican", "Independent", "Democrat"))) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  spread(lean, pct, fill = 0)

cces_2016_with_preds %>%
  filter(!is.na(pid3)) %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, Party = ordered(pid3, levels = c("Other", "Republican", "Independent", "Democrat"))) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  bind_rows(cces_2016_with_preds %>%
              filter(!is.na(pid3)) %>%
              mutate(wt_sum = sum(weight)) %>%
              group_by(Party = ordered(pid3, levels = c("Other", "Republican", "Independent", "Democrat"))) %>%
              summarise(pct = sum(weight) / mean(wt_sum)) %>%
              mutate(lean = "All voters")) %>%
  ungroup() %>%
  mutate(lean = ordered(lean, levels = c("All voters", "Swing voters", "Likely Trump", "Leans Trump", "Leans Clinton", "Likely Clinton"))) %>%
  group_by(lean) %>%
  arrange(lean, desc(Party)) %>%
  mutate(lag_pct = lag(pct),
         lag_pct = ifelse(is.na(lag_pct), 0, lag_pct),
         label_height = 0.5 * pct + cumsum(lag_pct)) %>%
  ggplot(aes(x = lean, fill = Party)) +
  geom_col(aes(y = pct), alpha = 0.8) +
  geom_text(aes(y = label_height, label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = "Party", values = c("#888888", "red", "#880088", "blue")) +
  theme(legend.position = "bottom") +
  labs(title = "Breakdown of voter lean segment by party identification", x = "Voter lean", y = "Cumulative percentage of voter segment") +
  coord_flip()

## Party ID (7-point)
cces_2016_with_preds %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, Party = ordered(pid7, levels = c("Not sure", "Strong Republican", "Not very strong Republican", "Lean Republican",
                                                  "Independent", "Lean Democrat", "Not very strong Democrat", "Strong Democrat"))) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  spread(lean, pct, fill = 0)

cces_2016_with_preds %>%
  filter(!is.na(pid7)) %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, Party = ordered(pid7, levels = c("Not sure", "Strong Republican", "Not very strong Republican", "Lean Republican",
                                                  "Independent", "Lean Democrat", "Not very strong Democrat", "Strong Democrat"))) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  bind_rows(cces_2016_with_preds %>%
              filter(!is.na(pid7)) %>%
              mutate(wt_sum = sum(weight)) %>%
              group_by(Party = ordered(pid7, levels = c("Not sure", "Strong Republican", "Not very strong Republican", "Lean Republican",
                                                        "Independent", "Lean Democrat", "Not very strong Democrat", "Strong Democrat"))) %>%
              summarise(pct = sum(weight) / mean(wt_sum)) %>%
              mutate(lean = "All voters")) %>%
  ungroup() %>%
  mutate(lean = ordered(lean, levels = c("All voters", "Swing voters", "Likely Trump", "Leans Trump", "Leans Clinton", "Likely Clinton"))) %>%
  group_by(lean) %>%
  arrange(lean, desc(Party)) %>%
  mutate(lag_pct = lag(pct),
         lag_pct = ifelse(is.na(lag_pct), 0, lag_pct),
         label_height = 0.5 * pct + cumsum(lag_pct)) %>%
  ggplot(aes(x = lean, fill = Party)) +
  geom_col(aes(y = pct), alpha = 0.8) +
  geom_text(aes(y = label_height, label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = "Party lean", values = c("#888888", "#880000", "#FF3333", "#FF8888", "#990099", "#8888FF", "#3333FF", "#000088")) +
  theme(legend.position = "bottom") +
  labs(title = "Breakdown of voter lean segment by party identification", x = "Voter lean", y = "Cumulative percentage of voter segment") +
  coord_flip()

## Perception of others' ideologies
### Democratic Party
cces_2016_with_preds %>%
  filter(!is.na(ideology_democrats)) %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, Ideology = ordered(ideology_democrats, 
                                    levels = c("Not sure", "Very conservative", "Conservative", "Somewhat conservative", "Middle of the road", 
                                               "Somewhat liberal", "Liberal", "Very liberal"))) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  bind_rows(cces_2016_with_preds %>%
              filter(!is.na(ideology_democrats)) %>%
              mutate(wt_sum = sum(weight)) %>%
              group_by(Ideology = ordered(ideology_democrats, 
                                                    levels = c("Not sure", "Very conservative", "Conservative", "Somewhat conservative", 
                                                               "Middle of the road", "Somewhat liberal", "Liberal", "Very liberal"))) %>%
              summarise(pct = sum(weight) / mean(wt_sum)) %>%
              mutate(lean = "All voters")) %>%
  ungroup() %>%
  mutate(lean = ordered(lean, levels = c("All voters", "Swing voters", "Likely Trump", "Leans Trump", "Leans Clinton", "Likely Clinton"))) %>%
  group_by(lean) %>%
  arrange(lean, desc(Ideology)) %>%
  mutate(lag_pct = lag(pct),
         lag_pct = ifelse(is.na(lag_pct), 0, lag_pct),
         label_height = 0.5 * pct + cumsum(lag_pct)) %>%
  ggplot(aes(x = lean, fill = Ideology)) +
  geom_col(aes(y = pct), alpha = 0.8) +
  geom_text(aes(y = label_height, label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = "Perceived ideology", values = c("#888888", "#880000", "#FF3333", "#FF8888", "#990099", "#8888FF", "#3333FF", "#000088")) +
  theme(legend.position = "bottom") +
  labs(title = "Perceived ideology of the Democratic Party", x = "Voter lean", 
       y = "Cumulative percentage of voter segment") +
  coord_flip()

### Republican Party
cces_2016_with_preds %>%
  filter(!is.na(ideology_republicans)) %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, Ideology = ordered(ideology_republicans, 
                                    levels = c("Not sure", "Very conservative", "Conservative", "Somewhat conservative", "Middle of the road", 
                                               "Somewhat liberal", "Liberal", "Very liberal"))) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  bind_rows(cces_2016_with_preds %>%
              filter(!is.na(ideology_republicans)) %>%
              mutate(wt_sum = sum(weight)) %>%
              group_by(Ideology = ordered(ideology_republicans, 
                                          levels = c("Not sure", "Very conservative", "Conservative", "Somewhat conservative", 
                                                     "Middle of the road", "Somewhat liberal", "Liberal", "Very liberal"))) %>%
              summarise(pct = sum(weight) / mean(wt_sum)) %>%
              mutate(lean = "All voters")) %>%
  ungroup() %>%
  mutate(lean = ordered(lean, levels = c("All voters", "Swing voters", "Likely Trump", "Leans Trump", "Leans Clinton", "Likely Clinton"))) %>%
  group_by(lean) %>%
  arrange(lean, desc(Ideology)) %>%
  mutate(lag_pct = lag(pct),
         lag_pct = ifelse(is.na(lag_pct), 0, lag_pct),
         label_height = 0.5 * pct + cumsum(lag_pct)) %>%
  ggplot(aes(x = lean, fill = Ideology)) +
  geom_col(aes(y = pct), alpha = 0.8) +
  geom_text(aes(y = label_height, label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = "Perceived ideology", values = c("#888888", "#880000", "#FF3333", "#FF8888", "#990099", "#8888FF", "#3333FF", "#000088")) +
  theme(legend.position = "bottom") +
  labs(title = "Perceived ideology of the Republican Party", x = "Voter lean", y = "Cumulative percentage of voter segment") +
  coord_flip()

### Hillary Clinton
cces_2016_with_preds %>%
  filter(!is.na(ideology_clinton)) %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, Ideology = ordered(ideology_clinton, 
                                    levels = c("Not sure", "Very conservative", "Conservative", "Somewhat conservative", "Middle of the road", 
                                               "Somewhat liberal", "Liberal", "Very liberal"))) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  bind_rows(cces_2016_with_preds %>%
              filter(!is.na(ideology_clinton)) %>%
              mutate(wt_sum = sum(weight)) %>%
              group_by(Ideology = ordered(ideology_clinton, 
                                          levels = c("Not sure", "Very conservative", "Conservative", "Somewhat conservative", 
                                                     "Middle of the road", "Somewhat liberal", "Liberal", "Very liberal"))) %>%
              summarise(pct = sum(weight) / mean(wt_sum)) %>%
              mutate(lean = "All voters")) %>%
  ungroup() %>%
  mutate(lean = ordered(lean, levels = c("All voters", "Swing voters", "Likely Trump", "Leans Trump", "Leans Clinton", "Likely Clinton"))) %>%
  group_by(lean) %>%
  arrange(lean, desc(Ideology)) %>%
  mutate(lag_pct = lag(pct),
         lag_pct = ifelse(is.na(lag_pct), 0, lag_pct),
         label_height = 0.5 * pct + cumsum(lag_pct)) %>%
  ggplot(aes(x = lean, fill = Ideology)) +
  geom_col(aes(y = pct), alpha = 0.8) +
  geom_text(aes(y = label_height, label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = "Perceived ideology", values = c("#888888", "#880000", "#FF3333", "#FF8888", "#990099", "#8888FF", "#3333FF", "#000088")) +
  theme(legend.position = "bottom") +
  labs(title = "Perceived ideology of Hillary Clinton", x = "Voter lean", 
       y = "Cumulative percentage of voter segment") +
  coord_flip()


### Donald Trump
cces_2016_with_preds %>%
  filter(!is.na(ideology_trump)) %>%
  group_by(lean) %>%
  mutate(wt_sum = sum(weight)) %>%
  group_by(lean, Ideology = ordered(ideology_trump, 
                                    levels = c("Not sure", "Very conservative", "Conservative", "Somewhat conservative", "Middle of the road", 
                                               "Somewhat liberal", "Liberal", "Very liberal"))) %>%
  summarise(pct = sum(weight) / mean(wt_sum)) %>%
  bind_rows(cces_2016_with_preds %>%
              filter(!is.na(ideology_trump)) %>%
              mutate(wt_sum = sum(weight)) %>%
              group_by(Ideology = ordered(ideology_trump, 
                                          levels = c("Not sure", "Very conservative", "Conservative", "Somewhat conservative", 
                                                     "Middle of the road", "Somewhat liberal", "Liberal", "Very liberal"))) %>%
              summarise(pct = sum(weight) / mean(wt_sum)) %>%
              mutate(lean = "All voters")) %>%
  ungroup() %>%
  mutate(lean = ordered(lean, levels = c("All voters", "Swing voters", "Likely Trump", "Leans Trump", "Leans Clinton", "Likely Clinton"))) %>%
  group_by(lean) %>%
  arrange(lean, desc(Ideology)) %>%
  mutate(lag_pct = lag(pct),
         lag_pct = ifelse(is.na(lag_pct), 0, lag_pct),
         label_height = 0.5 * pct + cumsum(lag_pct)) %>%
  ggplot(aes(x = lean, fill = Ideology)) +
  geom_col(aes(y = pct), alpha = 0.8) +
  geom_text(aes(y = label_height, label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = "Perceived ideology", values = c("#888888", "#880000", "#FF3333", "#FF8888", "#990099", "#8888FF", "#3333FF", "#000088")) +
  theme(legend.position = "bottom") +
  labs(title = "Perceived ideology of Donald Trump", x = "Voter lean", 
       y = "Cumulative percentage of voter segment") +
  coord_flip()

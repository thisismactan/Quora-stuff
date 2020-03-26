library(tidyverse)

cases <- read_csv("COVID-19/covid19_cases.csv") %>%
  mutate(t = as.numeric(date - first_case_date),
         log_cases = log(cases))

# Linear regression models
case_model_exp <- lm(log_cases ~ t, data = cases %>% filter(date >= as.Date("2020-02-26")))
case_model_power <- lm(log_cases ~ log(t), data = cases %>% filter(date >= as.Date("2020-02-26")))

summary(case_model_exp)
summary(case_model_power)

# Plots
## Log scale
ggplot(cases, aes(x = date, y = cases)) +
  geom_vline(xintercept = as.Date("2020-02-26")) +
  geom_text(data = tibble(x = as.Date("2020-03-05"), y = 10000*sqrt(10), label = "First community transmission"),
            aes(x = x, y = y, label = label), size = 3) +
  geom_point() +
  geom_line() +
  scale_y_continuous(label = scales::comma, trans = "log10") +
  labs(title = "COVID-19 cases since first confirmed case", x = "", y = "Cases (log scale)", subtitle = "United States")

ggplot(cases, aes(x = t, y = cases)) +
  geom_vline(xintercept = 35) +
  geom_text(data = tibble(x = 21, y = 10000*sqrt(10), label = "First community transmission"),
            aes(x = x, y = y, label = label), size = 3) +
  geom_point() +
  geom_line() +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(label = scales::comma, trans = "log10") +
  labs(title = "COVID-19 cases since first confirmed case", x = "Days since first case (log scale)", y = "Cases (log scale)", 
       subtitle = "United States")
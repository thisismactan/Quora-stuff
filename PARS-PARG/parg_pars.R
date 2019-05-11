library(lme4)
library(tidyverse)

# PARG
parg_data <- read_csv("PARS-PARG/parg_data.csv") %>%
  mutate(net_of_rec = 100*net/(approve + disapprove),
         quarter = round((quarter - as.Date("2017-01-01"))/90))

parg_model <- lm(net~lean+quarter, data = parg_data)
summary(parg_model)

parg_model.int <- lm(net~lean*party, data = parg_data)
summary(parg_model.int)

parg_model <- lm(net_of_rec~lean, data = parg_data)
summary(parg_model)

parg_model.int <- lm(net_of_rec~lean*party, data = parg_data)
summary(parg_model.int)

ggplot(parg_data, aes(x = lean, y = net, col = party)) +
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.7) +
  scale_colour_manual(name = "Party", values = c("D" = "blue", "R" = "red", "I" = "green4"), 
                      labels = c("D" = "Democratic", "R" = "Republican", "I" = "Independent")) +
  labs(title = "Net approval rating of governors vs. partisan lean", subtitle = "Morning Consult, January 2017 to April 2019",
       x = "State partisan lean toward governor's party", y = "Net approval rating")

ggplot(parg_data, aes(x = lean, y = net, col = party)) +
  facet_wrap(~quarter) +
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.7) +
  scale_colour_manual(name = "Party", values = c("D" = "blue", "R" = "red", "I" = "green4"), 
                      labels = c("D" = "Democratic", "R" = "Republican", "I" = "Independent")) +
  labs(title = "Net approval rating of governors vs. partisan lean", subtitle = "By quarter",
       x = "State partisan lean toward governor's party", y = "Net approval rating")

# Random intercepts for governor
parg_model.re <- lmer(net~lean+(1|governor/state), data = parg_data)
summary(parg_model.re)

ranefs_parg <- ranef(parg_model.re)

# PARS
pars_data <- read_csv("PARS-PARG/pars_data.csv") %>%
  mutate(net_of_rec = 100*net/(approve + disapprove),
         quarter = round((quarter - as.Date("2017-01-01"))/90) %>% as.numeric())

pars_model <- lm(net~lean, data = pars_data)
summary(pars_model)

ggplot(pars_data, aes(x = lean, y = net, col = party)) +
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.7) +
  scale_colour_manual(name = "Party", values = c("D" = "blue", "R" = "red", "I" = "green4"), 
                      labels = c("D" = "Democratic", "R" = "Republican", "I" = "Independent")) +
  labs(title = "Net approval rating of senators vs. partisan lean", subtitle = "Morning Consult, 115th and 116th Congresses",
       x = "State partisan lean toward senator's party", y = "Net approval rating")

ggplot(pars_data, aes(x = lean, y = net, col = party)) +
  facet_wrap(~quarter) +
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.7) +
  scale_colour_manual(name = "Party", values = c("D" = "blue", "R" = "red", "I" = "green4"), 
                      labels = c("D" = "Democratic", "R" = "Republican", "I" = "Independent")) +
  labs(title = "Net approval rating of senators vs. partisan lean", subtitle = "By quarter",
       x = "State partisan lean toward senator's party", y = "Net approval rating")

pars_model.re <- lmer(net~lean+quarter+I(quarter^2)+(1|senator), data = pars_data)
summary(pars_model.re)

ranefs_pars <- ranef(pars_model.re)
ranefs_senators <- ranefs_pars$senator
ranefs_senators %>%
  mutate(senator = rownames(ranefs_pars$senator)) %>%
  arrange(desc(`(Intercept)`)) %>%
  left_join(pars_data %>% group_by(senator, party, state) %>% summarise(pars_538 = mean(pars_538, na.rm = TRUE))) %>%
  mutate(PARS = round(`(Intercept)`, 1)) -> pars_re
  dplyr::select(Senator = senator, Party = party, State = state, `StatSheet PARS` = PARS, `538 PARS` = pars_538) 

## Fixed effects (most recent time period)
pars_data_116th <- pars_data %>% filter(quarter == 9)
pars_model.fe <- lm(net~lean, data = pars_data_116th)
residuals(pars_model.fe)

data.frame(Senator = pars_data_116th$senator, 
           Party = pars_data_116th$party, 
           State = pars_data_116th$state,
           PARS_STATSHEET = round(residuals(pars_model.fe), 1),
           pars_538 = pars_data_116th$pars_538) %>%
  arrange(desc(PARS_STATSHEET)) -> pars_fe

cor.test(pars_fe$PARS_STATSHEET, pars_fe$pars_538, method = "spearman")
cor.test(pars_re$PARS, pars_re$pars_538, method = "spearman")

pars_combined <- pars_re %>%
  merge(pars_fe %>% dplyr::select(Senator, PARS_STATSHEET), by.x = "senator", by.y = "Senator", all.x = TRUE)

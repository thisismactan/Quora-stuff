library(tidyverse)
library(class)

# Read CCES 2016
cces_2016 <- read_csv("CCES/data/2016/cces_2016_abridged.csv")

# Subset to major-party voters
cces_majorparty_voters <- cces_2016 %>%
  filter(grepl("Clinton|Trump", pres_vote_2016))

# Complete cases on selected issues
majorparty_issues <- cces_majorparty_voters %>%
  dplyr::select(pres_vote_2016, weight, imm_amnesty, imm_moresecurity, imm_dream, imm_deport, abortion_always, abortion_20wkban, 
                abortion_rapeincesthealth, abortion_noinsurance, abortion_hyde, abortion_never, env_co2reg, env_35mpg, env_renewablereq, 
                env_cleanacts, crime_nomindrugsentences, crime_bodycameras, crime_morepolice, crime_longersentences, gay_marriage, 
                guns_backgroundchecks, guns_prohibitregistry, guns_assaultban, guns_concealed, support_tpp, support_nclbrepeal, 
                support_infrastructure, support_iransanctions, support_capitatedmedicare, support_acarepeal, support_12minwage, mil_helpun) %>%
  na.omit()

# Cast the issue positions to numeric
majorparty_issue_data <- majorparty_issues %>%
  dplyr::select(-pres_vote_2016, -weight) %>%
  mutate_all(as.numeric)

# Perform PCA and grab rotation matrix, principal component scores
majorparty_issues_pca <- prcomp(~., data = majorparty_issue_data)
issues_pca_rotation <- majorparty_issues_pca$rotation
train_data_pcs <- as.data.frame(majorparty_issues_pca$x) %>%
  mutate(clinton_vote = majorparty_issues$pres_vote_2016 == "Hillary Clinton (D)",
         weight = majorparty_issues$weight)

# Exact features chosen using AIC
vote_model <- glm(clinton_vote ~ PC1 + PC3 + PC4 + PC5 + PC7 + PC8 + PC9 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC21 + PC23 + PC26 + 
                    PC30, data = train_data_pcs, family = binomial, weights = weight)

summary(vote_model)

# Rotate issue positions for Johnson voters
cces_johnson_voters <- cces_2016 %>%
  filter(grepl("Johnson", pres_vote_2016))

johnson_issues <- cces_johnson_voters %>%
  dplyr::select(imm_amnesty, imm_moresecurity, imm_dream, imm_deport, abortion_always, abortion_20wkban, abortion_rapeincesthealth,
                abortion_noinsurance, abortion_hyde, abortion_never, env_co2reg, env_35mpg, env_renewablereq, env_cleanacts,
                crime_nomindrugsentences, crime_bodycameras, crime_morepolice, crime_longersentences, gay_marriage, guns_backgroundchecks, 
                guns_prohibitregistry, guns_assaultban, guns_concealed, support_tpp, support_nclbrepeal, support_infrastructure,
                support_iransanctions, support_capitatedmedicare, support_acarepeal, support_12minwage, mil_helpun) %>%
  na.omit() %>%
  mutate_all(as.numeric) %>%
  as.matrix()

# Johnson PC scores
johnson_pcs <- as.data.frame(johnson_issues %*% issues_pca_rotation) %>%
  mutate(clinton_prob = predict(vote_model, newdata = ., type = "response"))

mean(johnson_pcs$clinton_prob)

library(tidyverse)
library(class)

# Read CCES 2016
cces_2016 <- read_csv("CCES/data/2016/cces_2016_abridged.csv")

# Subset to major-party voters
cces_majorparty_voters <- cces_2016

# Complete cases on selected issues
cces_issues <- cces_2016 %>% 
  dplyr::select(pres_vote_2016, weight, imm_amnesty, imm_moresecurity, imm_dream, imm_deport, abortion_always, abortion_20wkban, 
                abortion_rapeincesthealth, abortion_noinsurance, abortion_hyde, abortion_never, env_co2reg, env_35mpg, env_renewablereq, 
                env_cleanacts, crime_nomindrugsentences, crime_bodycameras, crime_morepolice, crime_longersentences, gay_marriage, 
                guns_backgroundchecks, guns_prohibitregistry, guns_assaultban, guns_concealed, support_tpp, support_nclbrepeal, 
                support_infrastructure, support_iransanctions, support_capitatedmedicare, support_acarepeal, support_12minwage, mil_helpun) %>%
  na.omit()

majorparty_issues <- cces_issues %>%
  filter(grepl("Clinton|Trump", pres_vote_2016))

# Cast the issue positions to numeric
issue_data <- cces_issues %>%
  dplyr::select(-pres_vote_2016, -weight) %>%
  mutate_all(as.numeric)

# Perform PCA and grab rotation matrix, principal component scores
issues_pca <- prcomp(~., data = issue_data)
issues_pca_rotation <- issues_pca$rotation
data_pcs <- as.data.frame(issues_pca$x) %>%
  mutate(vote = cces_issues$pres_vote_2016,
         weight = cces_issues$weight)

# Exact features chosen using AIC
vote_model <- glm(I(vote == "Hillary Clinton (D)") ~ PC1 + PC3 + PC4 + PC5 + PC8 + PC9 + PC11 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 +
                    PC20 + PC21 + PC22 + PC23 + PC24 + PC26 + PC30,
                  data = data_pcs %>% filter(grepl("Clinton|Trump", vote)), family = binomial, weights = weight)

summary(vote_model)

# Rotate issue positions for Johnson voters
cces_johnson_voters <- cces_2016 %>%
  filter(grepl("Johnson", pres_vote_2016))

johnson_issues <- cces_issues %>%
  filter(pres_vote_2016 == "Gary Johnson (L)") %>%
  dplyr::select(-pres_vote_2016, -weight) %>%
  mutate_all(as.numeric) %>%
  as.matrix()

# Johnson PC scores
johnson_pcs <- as.data.frame(johnson_issues %*% issues_pca_rotation) %>%
  mutate(clinton_prob = predict(vote_model, newdata = ., type = "response"))

mean(johnson_pcs$clinton_prob)

# Johnson, Clinton, and Trump voters
all_voters <- train_data_pcs %>%
  filter(grepl("Clinton|Trump|Johnson", vote)) %>%
  mutate(alpha = ifelse(vote == "Gary Johnson (L)", 1, 0.0001))

# In the PC1-PC2 plane
all_voters %>%
  ggplot(aes(x = -PC1, y = PC2, col = vote)) +
  geom_point(aes(alpha = alpha), size = 1) +
  scale_colour_manual(name = "Candidate", values = c("Hillary Clinton (D)" = "blue", "Donald Trump (R)" = "red", "Gary Johnson (L)" = "gold2")) +
  theme_dark() +
  guides(alpha = FALSE) +
  labs(title = "CCES 2016 voters in issue principal components space", subtitle = "First and second principal components", 
       x = "PC1 (left-right partisanship)", y = "PC2 (libertarian lean)")

# In the PC1-PC3 plane
all_voters %>%
  ggplot(aes(x = -PC1, y = PC3, col = candidate)) +
  geom_point(aes(alpha = alpha), size = 1) +
  scale_colour_manual(name = "Candidate", values = c("Clinton (D)" = "blue", "Trump (R)" = "red", "Johnson (L)" = "gold2")) +
  theme_dark() +
  guides(alpha = FALSE) +
  labs(title = "CCES 2016 voters in issue principal components space", subtitle = "First and second principal components", 
       x = "PC1 (left-right partisanship)", y = "PC2 (libertarian lean)")

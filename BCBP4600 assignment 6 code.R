library(tidyverse)
library(dplyr)
health <- read.csv("Health_conditions_among_children_under_age_18__by_selected_characteristics__United_States.csv")
unique(health$PANEL)
health <- health %>%
  mutate(PANEL_short = case_when(
    grepl("Current asthma", PANEL) ~ "Asthma",
    grepl("ADHD", PANEL) ~ "ADHD",
    grepl("Asthma attack", PANEL) ~ "Asthma Attack",
    grepl("Serious emotional", PANEL) ~ "Emotional Difficulties",
    grepl("Food allergy", PANEL) ~ "Food Allergy",
    grepl("Skin allergy", PANEL) ~ "Skin Allergy",
    grepl("Hay fever", PANEL) ~ "Hay Fever",
    grepl("Ear infections", PANEL) ~ "Ear Infections",
    TRUE ~ PANEL))
ggplot(health, aes(x = PANEL_short, y = STUB_NAME_NUM)) + geom_boxplot()
summary(health)
ggplot(health, aes(x = YEAR, fill = as.factor(PANEL_short))) +
  geom_bar(position = "stack") +
  labs(title = "Child Health vs Year",
       x = "PANEL",
       y = "Count") +
  theme_minimal()
# the points is average assign for each time interval
unique(health$YEAR)
health <- health %>%
  mutate(Century = ifelse(as.numeric(substring(YEAR, 1, 4)) < 2000, 
                          "20th Century", "21st Century"))
ggplot(health, aes(x = Century, fill = as.factor(PANEL_short))) +
  geom_bar(position = "stack") +
  labs(title = "Child Health vs Century",
       x = "Cenrury",
       y = "Panel Count") +
  theme_minimal()
# seems like more modern datapoints

glimpse(health)
health2 <- health %>% select(PANEL_short, STUB_NAME, STUB_LABEL, 
                             YEAR, Century, AGE, PANEL_NUM,
                             STUB_NAME_NUM, YEAR_NUM)
unique(health2$AGE)
# the number column in original data for age are difficult 
#to analyze. So mutate new column for it
age_mapping <- c("Under 18 years" = 1, "10-17 years" = 2, "0-4 years" = 3, 
                 "5-17 years" = 4, "5-9 years" = 5)
health2 <- health2 %>%
  mutate(AGE_num = age_mapping[AGE])

health3 <- health2 %>%
  filter(AGE != "Under 18 years")
ggplot(health3, aes(x = PANEL_short, fill = as.factor(AGE))) +
  geom_bar(position = "stack") +
  labs(title = "Child Health vs Age",
       x = "PANEL",
       y = "Count") +
  theme_minimal()
# Asthma, Asthma Attack, Ear Infections, Food Allergy, Skin Allergy, 
#and Hay Fever seem is occur for all age interval.

library(corrplot)
cor.mat <- cor(health2[,7:10])
corrplot(cor.mat, type = "lower")
summary(cor.mat)
library(ggplot2)
library(GGally)
ggpairs(health2[,7:10])
library(FactoMineR)
library(factoextra)
library(FactoInvestigate)
health.pca <- PCA(health2[,7:10], scale = T, graph = F)
health.pca$eig
plot(health.pca, label = "none")
fviz_eig(health.pca)
fviz_pca_var(health.pca, axes = c(1,2), repel = T)
Investigate(health.pca)

# Model
shapiro.test(health2$STUB_NAME_NUM)
qqnorm(health2$STUB_NAME_NUM)
qqline(health2$STUB_NAME_NUM)
# not normal distribution, but as we have more than 2000 sample, 
#could use Central Limit Theorem may help to ensure that the 
#distribution of the parameter estimates is approximately normal, 
#even if the underlying data is not perfectly normal.
model1 <- lm(PANEL_NUM ~ STUB_NAME_NUM, data = health2)
summary(model1)
#seem like single X-value is not work
model2 <- lm(PANEL_NUM ~ STUB_NAME_NUM + AGE_num, data = health2)
summary(model2)
model3 <- lm(PANEL_NUM ~ STUB_NAME_NUM + YEAR_NUM + AGE_num, 
             data = health2)
summary(model3)
plot(model3$fitted.values, model3$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red", lty = 2)
qqnorm(model3$residuals, main = "Normal Q-Q Plot")
qqline(model3$residuals)

library(randomForest)
library(randomForestExplainer)
library(randomForestSRC)
rf_model <- randomForest(PANEL_NUM ~ STUB_NAME_NUM + YEAR_NUM + AGE_num, 
                         data = health2)
print(rf_model)
print(importance(rf_model))
varImpPlot(rf_model)

# Analyze
library(car)
vif(model3)
# seems that multicollinearity is not a significant issue
library(caret)
train_predictions <- predict(rf_model, health2)
train_rmse <- sqrt(mean((health2$PANEL_NUM - train_predictions)^2))
cat("Training RMSE:", train_rmse, "\n")
#  Lower RMSE values indicate better model performance.
# Cross validation
set.seed(123)
cv_results <- train(
  PANEL_NUM ~ STUB_NAME_NUM + YEAR_NUM + AGE_num,
  data = health2,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5))
cv_rmse <- cv_results$results$RMSE[1]
cat("Cross-Validation RMSE:", cv_rmse, "\n")
# performed 5-fold cross-validation for the Random Forest model
# Lower RMSE values indicate better model fit.

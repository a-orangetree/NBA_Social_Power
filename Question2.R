library(tidyverse)
library(modelr)
library(boot)
library(leaps)
library(gridExtra)
library(boot)
library(leaps)

#####################################################
# Question 2: Can we predict PPG based on other performance statistics?
######################################################

# Load processed dataset
players_with_stats_salary <- read_csv("data/processed/players_with_stats_salary.csv")

# Set consistent seed
set.seed(1)

# Explore which predictors are statistically significant
explore_ppg <- players_with_stats_salary %>%
  select(-PLAYER) %>%
  select(-X1) %>%
  select(-Rk) %>%
  select(-POSITION.x) %>%
  select(-TEAM.x) %>%
  lm(POINTS ~., data = .)

summary(explore_ppg)

# Looks like the predictors most correlated with points per game are: FT, 3PA, FG 2P
# Let's make a linear model to graph this
# Make train and test sets
train_set_points <- players_with_stats_salary %>%
  sample_frac(0.75, replace=FALSE)

test_set_points <- players_with_stats_salary %>%
  setdiff(train_set_points)

# This model is too good - probably some collinearity
ppg_model <- lm(POINTS ~ FT + `3P` + FG + `2P`, data=train_set_points)

# Add predicted salary to test set
test_set_points <- add_predictions(test_set_points, ppg_model)

# Plots of predicted vs. actual for points based on those stats
points_predicted_plot <- ggplot() +
  geom_point(data = test_set_points, aes(x = seq(1:dim(test_set_points)[1]), y = POINTS), color="red") +
  geom_point(data = test_set_points, aes(x = seq(1:dim(test_set_points)[1]), y = pred), color="blue") +
  geom_smooth(data = test_set_points, aes(x = seq(1:dim(test_set_points)[1]), y = pred)) +
  xlab("Player Index") + ylab("Points") + ylim(0, range(test_set_points$pred)[2]) +
  ggtitle("Points Predicted by other Performance Stats") + theme(plot.title = element_text(hjust = 0.5))

# points_predicted_plot
coef(ppg_model)

# Plot actual vs. predicted points
actual_v_pred_points <- ggplot(data = test_set_points, aes(x = POINTS, y = pred)) +
  geom_point() + geom_smooth() +
  xlab("Actual PPG") + ylab("Predicted PPG") + 
  ggtitle("Actual vs Predicted Points Per Game") + theme(plot.title = element_text(hjust = 0.5))
actual_v_pred_points

# Make sure that points isn't just a summary of those predictors
test_set_points <- test_set_points %>%
  mutate(summer = 1*FT+3*`3P`+2*`2P`)

mini_table <- tibble(original_points = (test_set_points$POINTS)[1:10])
mini_table <- mini_table %>%
  mutate(pred_points = round((test_set_points$pred)[1:10], 3),
         sum_col = (test_set_points$summer)[1:10])

mini_table
# Not quite, but close

# What if we try just using FG to predict PPG?
ppg_model_fg <- lm(POINTS ~ FG, data=train_set_points)

# Add predicted salary to test set
test_set_points_fg <- add_predictions(test_set_points, ppg_model_fg)

# Plots of predicted vs. actual for points based on those stats
points_predicted_plot_fg <- ggplot() +
  geom_point(data = test_set_points_fg, aes(x = seq(1:dim(test_set_points_fg)[1]), y = POINTS), color="red") +
  geom_point(data = test_set_points_fg, aes(x = seq(1:dim(test_set_points_fg)[1]), y = pred), color="blue") +
  geom_smooth(data = test_set_points_fg, aes(x = seq(1:dim(test_set_points_fg)[1]), y = pred)) +
  xlab("Player Index") + ylab("Points") + ylim(0, range(test_set_points_fg$pred)[2]) +
  ggtitle("Points Predicted by other Performance Stats") + theme(plot.title = element_text(hjust = 0.5))

# Plot actual vs. predicted points
actual_v_pred_points_fg <- ggplot(data = test_set_points_fg, aes(x = POINTS, y = pred)) +
  geom_point() + geom_smooth() +
  xlab("Actual PPG") + ylab("Predicted PPG") + 
  ggtitle("Actual vs Predicted Points Per Game - Field Goals as Only Predictor") + theme(plot.title = element_text(hjust = 0.5))
actual_v_pred_points_fg

# ----------- Trying out subset selection
# Select variables with best subsets - problems because of high collinearity
regfit_first <- regsubsets(POINTS~., select(players_with_stats_salary, -PLAYER, -X1, -Rk), nvmax=35, method="forward")
regfit_summary <- summary(regfit_first)


# Create tibble which contains data from results object
points_subset_data <- tibble(num_pred = 1:36
                             ,rss = regfit_summary$rss
                             ,rsquared = regfit_summary$rsq
                             ,adj_rsquared = regfit_summary$adjr2
                             ,cp = regfit_summary$cp
                             ,bic = regfit_summary$bic)

# RSS
p1 <- points_subset_data %>% 
  ggplot(aes(num_pred, rss)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(points_subset_data$rss)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('RSS')

# ADJ R-SQUARED
p2 <- points_subset_data %>% 
  ggplot(aes(num_pred, adj_rsquared)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.max(points_subset_data$adj_rsquared)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('Adj R-squared')

# CP
p3 <- points_subset_data %>% 
  ggplot(aes(num_pred, cp)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(points_subset_data$cp)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('Cp')

# BIC
p4 <- points_subset_data %>% 
  ggplot(aes(num_pred, bic)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(points_subset_data$bic)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('BIC')

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

# Results are really all over the place... but we do see that the number of predictors seems to converge around 3-4,
# and on the ones we were already using above.
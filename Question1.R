library(tidyverse)
library(modelr)
library(boot)
library(leaps)
library(gridExtra)

# PS = Performance Statistics
att_val_elo <- read_csv('raw_data/nba_2017_att_val_elo.csv') # Attendance, valuation, and ELO
br <- read_csv('raw_data/nba_2017_br.csv') # 486 x 30 (PS)
endorsements <- read_csv('raw_data/nba_2017_endorsements.csv')
players_with_salary <- read_csv('raw_data/nba_2017_nba_players_with_salary.csv') # 342 x 39 (PS)
pie <- read_csv('raw_data/nba_2017_pie.csv') # Player impoct estimation
players_with_stats_combined <- read_csv('raw_data/nba_2017_players_stats_combined.csv') # 446 x 38 (PS)
players_with_salary_wiki_twitter <- read_csv('raw_data/nba_2017_players_with_salary_wiki_twitter.csv') # 239 x 42 (PS)
real_plus_minus <- read_csv('raw_data/nba_2017_real_plus_minus.csv') # 468 x 8 (Subset of PS?)
salary <- read_csv('raw_data/nba_2017_salary.csv') %>% # 449 x 5
  mutate(SALARY2 = (SALARY / 1000000)) 
team_valuations <- read_csv('raw_data/nba_2017_team_valuations.csv')
player_twitter <- read_csv('raw_data/nba_2017_twitter_players.csv') # 329 x 3
team_name_crosswalk <- read_csv('raw_data/team_name_crosswalk.csv') #Doesn't map 100% from Short to Long

####################################################
# Question 1: Can we predict salary from performance statistics?
###################################################

# Used the below to determine that RPM should be removed. Could
# also remove ORPM and DRPM
# Matrix::rankMatrix(pracma::rref(cor(stats_salary_data)))

# Remove qualitative data
# Notes:
# 1. Removed RPM because it was highly correlated with ORPM and DRPM
# and thus was causing an error
# 2. Only displays observations which the salary is less than the 90th percentile
stats_salary_data <- players_with_stats_salary %>% 
  # Because each statistic below indicates a different model has the 
  # smallest test error, this line creates a smaller dataset for comparison  
  select(-PLAYER, -X1, -POSITION.x, -TEAM.x, -RPM, -Rk, -`FG%`, -`3P%`, -`2P%`, -`eFG%`,
         -`FT%`, -TRB, -ORPM, -DRPM, -WINS_RPM, -PLAYER, -X1, -MP) %>%
  # this is the original select
  # select(-PLAYER, -X1, -POSITION.x, -TEAM.x, -RPM) %>% 
  filter(SALARY <= quantile(salary$SALARY, .95))

# Create variable to hold the number of predictors
num_predictors <- dim(stats_salary_data)[2] - 1

# Perform best-subsets 
stats_salary_model <- regsubsets(SALARY ~ .
                                 ,data = stats_salary_data
                                 ,nvmax = num_predictors)

stats_salary_summary <- summary(stats_salary_model)

# Create tibble which contains data from results object
stats_salary_results <- tibble(num_pred = 1:num_predictors
                               ,rss = stats_salary_summary$rss
                               ,rsquared = stats_salary_summary$rsq
                               ,adj_rsquared = stats_salary_summary$adjr2
                               ,cp = stats_salary_summary$cp
                               ,bic = stats_salary_summary$bic)

# RSS
plot1_q1 <- stats_salary_results %>% 
  ggplot(aes(num_pred, rss)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(stats_salary_results$rss)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('RSS')

# ADJ R-SQUARED
plot2_q1 <- stats_salary_results %>% 
  ggplot(aes(num_pred, adj_rsquared)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.max(stats_salary_results$adj_rsquared)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('Adj R-squared')

# CP
plot3_q1 <- stats_salary_results %>% 
  ggplot(aes(num_pred, cp)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(stats_salary_results$cp)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('Cp')

# BIC
plot4_q1 <- stats_salary_results %>% 
  ggplot(aes(num_pred, bic)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(stats_salary_results$bic)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('BIC')

# Each of the measures comes up with vastly different number of 
# predictors... 
grid.arrange(plot1_q1, plot2_q1, plot3_q1, plot4_q1, nrow = 2, ncol = 2)

# Selecting a model. Display coefficients
coef(stats_salary_model, 6)

# Create training and test data sets
training_data_q1 <- stats_salary_data %>% sample_frac(.8)
test_data_q1 <- setdiff(stats_salary_data, training_data_q1)

# Fit model using the coefficients above and add predictions to test data
salary_model1 <- lm(SALARY ~ AGE + `2P` + `2PA` + ORB + MPG + W, data = training_data_q1)
test_data_q1 <- add_predictions(test_data_q1, salary_model1, var = 'pred_lm')

salary_model2 <- glm(SALARY ~ AGE + `2P` + `2PA` + ORB + MPG, data = training_data_q1, family = Gamma(link = "log"))
# test_data_q1 <- add_predictions(test_data_q1, salary_model2, var = 'pred_glm', type = 'response')

test_data_q1 <- mutate(test_data_q1, pred_glm = predict(salary_model2, test_data_q1, type = 'response'))

# ggplot() +
#   geom_point(data = test_data_q1, aes(x = seq(1:dim(test_data_q1)[1]), y = SALARY)) +
#   geom_point(data = test_data_q1, aes(x = seq(1:dim(test_data_q1)[1]), y = pred_lm), color=
#                "green") +
#   geom_smooth(data = test_data_q1, aes(x = seq(1:dim(test_data_q1)[1]), y = pred_lm), color=
#                 "green")
# 
# ggplot() +
#   geom_point(data = test_data_q1, aes(x = seq(1:dim(test_data_q1)[1]), y = SALARY)) +
#   geom_point(data = test_data_q1, aes(x = seq(1:dim(test_data_q1)[1]), y = pred_glm)
#              , color = "blue") +
#   geom_smooth(data = test_data_q1, aes(x = seq(1:dim(test_data_q1)[1]), y = pred_glm)
#               , color = "blue")

# Actual vs Predicted for linear model
ggplot() +
  geom_point(data = test_data_q1, aes(x = SALARY, y = pred_lm)) +
  geom_smooth(data = test_data_q1, aes(x = SALARY, y = pred_lm))

# Actual vs Predicted for Gamma model
ggplot() +
  geom_point(data = test_data_q1, aes(x = SALARY, y = pred_glm)) +
    geom_smooth(data = test_data_q1, aes(x = SALARY, y = pred_glm))

# Calculate root mean squared error
rmse(salary_model1, test_data_q1)
rmse(salary_model2, test_data_q1)

# Exploring plots of residuals / fitted / leverage / Q-Q 

########### Do we need the below? (Does the same thing, but keep training and 
########### test data sets inline.)
##########################################################################
# # Add columns to players
# players_with_stats_salary <- players_with_stats_salary %>% 
#   mutate(training = sample(c(TRUE, FALSE)
#         , nrow(players_with_stats_salary), prob = c(.8, .2),rep = TRUE)
#            ,testing = !training)
# 
# # Create linear model
# salary_model1 <- lm(SALARY ~ AGE + POINTS, filter(players_with_stats_salary, training == TRUE))
# summary(salary_model1)
# 
# # Why doesn't this work? ******************************************************************
# # autoplot(salary_model1) 
# 
# # Add predictions back to data
# players_with_stats_salary <-players_with_stats_salary %>% 
#   add_predictions(salary_model1)
# 
# # Since the rmse() funciton isn't working, calcluate RMSE manually
# players_with_stats_salary <-  players_with_stats_salary %>%
#   mutate(actual_pred_diff = SALARY - pred
#          ,actual_pred_diff_sq = actual_pred_diff^2)
# 
# sqrt(mean(players_with_stats_salary$actual_pred_diff_sq))
##########################################################################

########### K-Fold Cross Validation

stats_salary10fold <- stats_salary_data %>% 
  crossv_kfold(10, id = 'fold') %>% 
  mutate(train = map(train, as_tibble)) %>% 
  mutate(lm_model = map(train, ~ lm(SALARY ~ AGE + `2P` + `2PA` + ORB + MPG + W, data = .))
         ,glm_model = map(train
                          , ~ glm(SALARY ~ AGE + `2P` + `2PA` + ORB + MPG, data = .
                                  , family = Gamma(link = "log"))))

stats_salary10fold %>% 
  mutate(rmse = map2_dbl(stats_salary10fold$lm_model, stats_salary10fold$test, rmse)) %>%
  summarise(mean_rmse = mean(rmse))

stats_salary10fold %>% 
  mutate(rmse = map2_dbl(stats_salary10fold$glm_model, stats_salary10fold$test, rmse)) %>%
  summarise(mean_rmse = mean(rmse))

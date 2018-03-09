library(tidyverse)
library(modelr)
library(boot)
library(leaps)
library(gridExtra)
library(gam)

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


set.seed(1)

dim(players_with_stats_combined)
dim(salary)

# Adding salary to performance data
players_with_stats_salary <- inner_join(players_with_stats_combined, salary
                                        , by = c('PLAYER' =  'NAME'))
players_with_stats_salary <- select(players_with_stats_salary, -POSITION.y, -TEAM.y, -SALARY2)

players_with_stats_salary <- drop_na(players_with_stats_salary)
head(players_with_stats_salary)
dim(players_with_stats_salary)

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
coef(stats_salary_model, 10)

# Create training and test data sets
training_data_q1 <- stats_salary_data %>% sample_frac(.8)
test_data_q1 <- setdiff(stats_salary_data, training_data_q1)

# Fit model using the coefficients above and add predictions to test data
salary_model4 <- lm(SALARY ~ AGE + `2PA` + MPG + W, data = training_data_q1)
salary_model5 <- lm(SALARY ~ AGE + `2P` + `2PA` + MPG + W, data = training_data_q1)
salary_model6 <- lm(SALARY ~ AGE + `2P` + `2PA` + ORB + POINTS + W, data = training_data_q1)
salary_model7 <- lm(SALARY ~ AGE + `2P` + `2PA` + ORB + POINTS + W + TOV, data = training_data_q1)
salary_model8 <- lm(SALARY ~ AGE + `2P` + `2PA` + ORB + POINTS + W + TOV + STL, data = training_data_q1)
salary_model9 <- lm(SALARY ~ AGE + FGA + `2P` + `3PA` + ORB + POINTS + W + TOV + MPG, data = training_data_q1)
salary_model10 <- lm(SALARY ~ AGE + FGA + `3PA` + `2P` + `2PA` + ORB + TOV + POINTS + MPG + W, data = training_data_q1)

test_data_q1 <- add_predictions(test_data_q1, salary_model4, var = 'pred_lm4')
test_data_q1 <- add_predictions(test_data_q1, salary_model5, var = 'pred_lm5')
test_data_q1 <- add_predictions(test_data_q1, salary_model6, var = 'pred_lm6')
test_data_q1 <- add_predictions(test_data_q1, salary_model7, var = 'pred_lm7')
test_data_q1 <- add_predictions(test_data_q1, salary_model8, var = 'pred_lm8')
test_data_q1 <- add_predictions(test_data_q1, salary_model9, var = 'pred_lm9')
test_data_q1 <- add_predictions(test_data_q1, salary_model10, var = 'pred_lm10')

salary_model_gamma <- glm(SALARY ~ AGE + `2P` + `2PA` + ORB + MPG, data = training_data_q1, family = Gamma(link = "log"))
test_data_q1 <- mutate(test_data_q1, pred_glm = predict(salary_model_gamma, test_data_q1, type = 'response'))

# Attempting to further limit the data. This is not feasible if we attempt to add predictors
# back to the test data (since then we have a different length vector which we are attempting to 
# to the vector). Just calculate the rmse and add to the rmse table below 
# test_data_q1 <- mutate(test_data_q1, pred_glm2 = predict(salary_model_gamma
#                                                          , test_data_q1[test_data_q1$SALARY < 10000000]
#                                                          , type = 'response'))

# Original plots - Overlaying two sets of points
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
plot_lm4 <- ggplot() +
  geom_point(data = test_data_q1, aes(x = SALARY, y = pred_lm4)) +
  geom_smooth(data = test_data_q1, aes(x = SALARY, y = pred_lm4), method = 'lm')

plot_lm5 <- ggplot() +
  geom_point(data = test_data_q1, aes(x = SALARY, y = pred_lm5)) +
  geom_smooth(data = test_data_q1, aes(x = SALARY, y = pred_lm5), method = 'lm')

plot_lm6 <- ggplot() +
  geom_point(data = test_data_q1, aes(x = SALARY, y = pred_lm6)) +
  geom_smooth(data = test_data_q1, aes(x = SALARY, y = pred_lm6), method = 'lm')

plot_lm7 <- ggplot() +
  geom_point(data = test_data_q1, aes(x = SALARY, y = pred_lm7)) +
  geom_smooth(data = test_data_q1, aes(x = SALARY, y = pred_lm7), method = 'lm')

plot_lm8 <- ggplot() +
  geom_point(data = test_data_q1, aes(x = SALARY, y = pred_lm8)) +
  geom_smooth(data = test_data_q1, aes(x = SALARY, y = pred_lm8), method = 'lm')

plot_lm9 <- ggplot() +
  geom_point(data = test_data_q1, aes(x = SALARY, y = pred_lm9)) +
  geom_smooth(data = test_data_q1, aes(x = SALARY, y = pred_lm9), method = 'lm')

plot_lm10 <- ggplot() +
  geom_point(data = test_data_q1, aes(x = SALARY, y = pred_lm10)) +
  geom_smooth(data = test_data_q1, aes(x = SALARY, y = pred_lm10), method = 'lm')

# Actual vs Predicted for Gamma model
plot_glm <- ggplot() +
  geom_point(data = test_data_q1, aes(x = SALARY, y = pred_glm)) +
    geom_smooth(data = test_data_q1, aes(x = SALARY, y = pred_glm), method = 'lm', color = "red")

grid.arrange(plot_lm4, plot_lm5, plot_lm6, plot_lm7, plot_lm8, plot_lm9, plot_lm10, plot_glm, nrow = 4, ncol = 2)

# Calculate root mean squared error
rmse(salary_model4, test_data_q1)
rmse(salary_model5, test_data_q1)
rmse(salary_model6, test_data_q1)
rmse(salary_model7, test_data_q1)
rmse(salary_model8, test_data_q1)
rmse(salary_model9, test_data_q1)
rmse(salary_model10, test_data_q1)
rmse(salary_model_gamma, test_data_q1)


#########################################################################
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
#############################################################################

salary_model10 <- lm(SALARY ~ AGE + FGA + `3PA` + `2P` + `2PA` + ORB + TOV + POINTS + MPG + W, data = training_data_q1)

stats_salary10fold <- stats_salary_data %>% 
  crossv_kfold(5, id = 'fold') %>%  #*************************** Look at 5-10
  mutate(train = map(train, as_tibble)) %>% 
  mutate(lm_model4 = map(train, ~ lm(SALARY ~ AGE + `2PA` + MPG + W, data = .))
         ,lm_model5 = map(train, ~ lm(SALARY ~ AGE + `2P` + `2PA` + MPG + W, data = .))
         ,lm_model6 = map(train, ~ lm(SALARY ~ AGE + `2P` + `2PA` + ORB + POINTS + W, data = .))
         ,lm_model7 = map(train, ~ lm(SALARY ~ AGE + `2P` + `2PA` + ORB + POINTS + W + TOV, data = .))
         ,lm_model8 = map(train, ~ lm(SALARY ~ AGE + `2P` + `2PA` + ORB + POINTS + W + TOV + STL, data = .))
         ,lm_model9 = map(train, ~ lm(SALARY ~ AGE + FGA + `2P` + `3PA` + ORB + POINTS + W + TOV + MPG, data = .))
         ,lm_model10 = map(train, ~ lm(SALARY ~ AGE + FGA + `3PA` + `2P` + `2PA` + ORB + TOV + POINTS + MPG + W, data = .))
         ,glm_model = map(train
                          , ~ glm(SALARY ~ AGE + `2P` + `2PA` + ORB + MPG, data = .
                                  , family = Gamma(link = "log"))))

rmse_lm4 <- stats_salary10fold %>% 
  mutate(rmse = map2_dbl(stats_salary10fold$lm_model4, stats_salary10fold$test, rmse)) %>%
  summarise(mean_rmse = mean(rmse))

rmse_lm5 <- stats_salary10fold %>% 
  mutate(rmse = map2_dbl(stats_salary10fold$lm_model5, stats_salary10fold$test, rmse)) %>%
  summarise(mean_rmse = mean(rmse))

rmse_lm6 <- stats_salary10fold %>% 
  mutate(rmse = map2_dbl(stats_salary10fold$lm_model6, stats_salary10fold$test, rmse)) %>%
  summarise(mean_rmse = mean(rmse))

rmse_lm7 <- stats_salary10fold %>% 
  mutate(rmse = map2_dbl(stats_salary10fold$lm_model7, stats_salary10fold$test, rmse)) %>%
  summarise(mean_rmse = mean(rmse))

rmse_lm8 <- stats_salary10fold %>% 
  mutate(rmse = map2_dbl(stats_salary10fold$lm_model8, stats_salary10fold$test, rmse)) %>%
  summarise(mean_rmse = mean(rmse))

rmse_lm9 <- stats_salary10fold %>% 
  mutate(rmse = map2_dbl(stats_salary10fold$lm_model9, stats_salary10fold$test, rmse)) %>%
  summarise(mean_rmse = mean(rmse))

rmse_lm10 <- stats_salary10fold %>% 
  mutate(rmse = map2_dbl(stats_salary10fold$lm_model10, stats_salary10fold$test, rmse)) %>%
  summarise(mean_rmse = mean(rmse))

rmse_glm <- stats_salary10fold %>% 
  mutate(rmse = map2_dbl(stats_salary10fold$glm_model, stats_salary10fold$test, rmse)) %>%
  summarise(mean_rmse = mean(rmse))

rmse_summary <- tibble(lm4 = rmse_lm4$mean_rmse
                       ,lm5 = rmse_lm5$mean_rmse
                       ,lm6 = rmse_lm6$mean_rmse
                       ,lm7 = rmse_lm7$mean_rmse
                       ,lm8 = rmse_lm8$mean_rmse
                       ,lm9 = rmse_lm9$mean_rmse
                       ,lm10 = rmse_lm10$mean_rmse
                       ,glm = rmse_glm$mean_rmse)

rmse_summary

####################################
# Exploratory graphs to observe if non-linear patterns exist 
####################################


ggplot() +
  geom_point(data = stats_salary_data, aes(x = AGE, y = SALARY)) +
  geom_smooth(data = stats_salary_data, aes(x = AGE, y = SALARY), method = 'lm')

ggplot() +
  geom_point(data = stats_salary_data, aes(x = `2P`, y = SALARY)) +
  geom_smooth(data = stats_salary_data, aes(x = `2P`, y = SALARY), method = 'lm')

ggplot() +
  geom_point(data = stats_salary_data, aes(x = `2PA`, y = SALARY)) +
  geom_smooth(data = stats_salary_data, aes(x = `2PA`, y = SALARY), method = 'lm')

ggplot() +
  geom_point(data = stats_salary_data, aes(x = ORB, y = SALARY)) +
  geom_smooth(data = stats_salary_data, aes(x = ORB, y = SALARY), method = 'lm')

ggplot() +
  geom_point(data = stats_salary_data, aes(x = POINTS, y = SALARY)) +
  geom_smooth(data = stats_salary_data, aes(x = POINTS, y = SALARY), method = 'lm')

ggplot() +
  geom_point(data = stats_salary_data, aes(x = W, y = SALARY)) +
  geom_smooth(data = stats_salary_data, aes(x = W, y = SALARY), method = 'lm')


##########################################################
# Attempting to fit non-linear graphs
##########################################################

#ORB + POINTS + W + TOV + STL

# Attempting local regression
local_reg_model1 <- loess(SALARY ~ AGE + MPG + W, data = training_data_q1)
local_reg_model2 <- loess(SALARY ~ AGE + MPG + W + POINTS, data = training_data_q1)

test_data_q1 <- mutate(test_data_q1, pred_local1 = predict(local_reg_model1, test_data_q1, type = 'response'))
test_data_q1 <- mutate(test_data_q1, pred_local2 = predict(local_reg_model2, test_data_q1, type = 'response'))

ggplot() +
  geom_point(data = test_data_q1, aes(x = SALARY, y = pred_local1)) +
  geom_smooth(data = test_data_q1, aes(x = SALARY, y = pred_local1))

ggplot() +
  geom_point(data = test_data_q1, aes(x = SALARY, y = pred_local2)) +
  geom_smooth(data = test_data_q1, aes(x = SALARY, y = pred_local2))

rmse_summary <- rmse_summary %>% 
  mutate(loc1 = rmse(local_reg_model1, test_data_q1)
          ,loc2 = rmse(local_reg_model2, test_data_q1))

rmse_summary

stop('Below is not useful')

# Not needed. The above graphs show these will not be helpful.
gam_age <- gam(SALARY ~ ns(AGE, 2) + `2PA` + MPG + W, data = training_data_q1)
plot(gam_age)

gam_2PA <- gam(SALARY ~ AGE + ns(`2PA`, 2) + MPG + W, data = training_data_q1)
plot(gam_2PA)

gam_MPG <- gam(SALARY ~ AGE + `2PA` + ns(MPG, 2) + W, data = training_data_q1)
plot(gam_MPG)

gam_W <- gam(SALARY ~ AGE + `2PA` + MPG + ns(W, 2), data = training_data_q1)
plot(gam_W)



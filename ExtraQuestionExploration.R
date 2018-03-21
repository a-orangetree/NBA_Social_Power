library(tidyverse)
library(modelr)
library(boot)
library(leaps)
library(gridExtra)

# FILES TO LOAD FROM NEW PROCESSED SUBDIR
# ALSO GET THE SALARY_VALUATIONS_2
players_with_stats_combined <- read_csv('raw_data/nba_2017_players_stats_combined.csv') # 446 x 38 (PS)
salary <- read_csv('raw_data/nba_2017_salary.csv') %>% # 449 x 5
  mutate(SALARY2 = (SALARY / 1000000)) 
team_valuations <- read_csv('raw_data/nba_2017_team_valuations.csv')


###########################################################
# Extra Question Exploration: Can we predict team valuations from individual salaries?
###########################################################

salary$TEAM <- str_replace(salary$TEAM, 'LA Clippers', 'Los Angeles Clippers')

# Same as the above but with extra fields
salary_valuations_by_team2 <- left_join(salary, players_with_stats_combined, by = c('NAME' = 'PLAYER')) %>% 
  group_by(TEAM.x) %>% 
  summarise(total_salary = sum(SALARY2, na.rm = TRUE)
            ,median_salary = median(SALARY2, na.rm = TRUE)
            ,mean_salary = mean(SALARY2, na.rm = TRUE)
            ,low_salary = min(SALARY2, na.rm = TRUE)
            ,high_salary = max(SALARY2, na.rm = TRUE)
            ,avg_age = median(AGE, na.rm = TRUE)
            ,avg_FG = median(FG, na.rm = TRUE)
            ,avg_FGA = median(FGA, na.rm = TRUE)
            ,avg_3P = median(`3P`, na.rm = TRUE)
            ,avg_3PA = median(`3PA`, na.rm = TRUE)
            ,avg_2P = median(`2P`, na.rm = TRUE)
            ,avg_2PA = median(`2PA`, na.rm = TRUE)
            ,avg_FT = median(FT, na.rm = TRUE)
            ,avg_FTA = median(FTA, na.rm = TRUE)
            ,avg_ORB = median(ORB, na.rm = TRUE)
            ,avg_DRB = median(DRB, na.rm = TRUE)
            ,avg_AST = median(AST, na.rm = TRUE)
            ,avg_STL = median(STL, na.rm = TRUE)
            ,avg_BLK = median(BLK, na.rm = TRUE)
            ,avg_TOV = median(TOV, na.rm = TRUE)
            ,avg_GP = median(GP, na.rm = TRUE)
            ,avg_MPG = median(MPG, na.rm = TRUE)
            ,avg_PACE = median(PACE, na.rm = TRUE)
            ,avg_W = median(W, na.rm = TRUE)) %>% 
  right_join(team_valuations, by = c('TEAM.x' = 'TEAM')) %>% 
  mutate(TEAM.x = factor(TEAM.x))

# cor(salary_valuations_by_team2)

# Remove qualitative columns and data which has a high correlation
salary_valuations_by_team2 <- salary_valuations_by_team2 %>% 
  # original select... commented out because we may be facing the curse
  # of dimensionality
  select(-TEAM.x, -median_salary, -avg_FGA, -avg_3PA, -avg_2PA, -avg_FTA)
# select(VALUE_MILLIONS, total_salary, mean_salary, low_salary, high_salary)

# Create variable to hold the number of predictors
num_predictors <- dim(salary_valuations_by_team2)[2] - 1

# Perform best-subsets 
val_from_salary_model <- regsubsets(VALUE_MILLIONS ~ .
                                    ,data = salary_valuations_by_team2
                                    ,nvmax = num_predictors
                                    ,method = 'forward')

val_from_salary_summary <- summary(val_from_salary_model)

# Create tibble which contains data from results object
val_from_salary_results <- tibble(num_pred = 1:num_predictors
                                  ,rss = val_from_salary_summary$rss
                                  ,rsquared = val_from_salary_summary$rsq
                                  ,adj_rsquared = val_from_salary_summary$adjr2
                                  ,cp = val_from_salary_summary$cp
                                  ,bic = val_from_salary_summary$bic)

# RSS
plot1 <- val_from_salary_results %>% 
  ggplot(aes(num_pred, rss)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(val_from_salary_results$rss)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('RSS')

# ADJ R-SQUARED
plot2 <- val_from_salary_results %>% 
  ggplot(aes(num_pred, adj_rsquared)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.max(val_from_salary_results$adj_rsquared)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('Adj R-squared')

# CP
plot3 <- val_from_salary_results %>% 
  ggplot(aes(num_pred, cp)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(val_from_salary_results$cp)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('Cp')

# BIC
plot4 <- val_from_salary_results %>% 
  ggplot(aes(num_pred, bic)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(val_from_salary_results$bic)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('BIC')

# Each of the measures comes up with vastly different number of 
# predictors... 
grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)

coef(stats_salary_model, 1)

# Take predictors from salary prediction and break out valuation based on
# each individual predictor (create individual plots). If there is no
# pattern, we may be able to conclude that the driver of valuation is
# is not contained in our set of predictors (i.e. in this dataset)

ggplot(data = salary_valuations_by_team2, aes(x = avg_age, y = VALUE_MILLIONS)) +
  geom_point() +
  geom_smooth(color = 'brown')

ggplot(data = salary_valuations_by_team2, aes(x = avg_2P, y = VALUE_MILLIONS)) +
  geom_point() +
  geom_smooth(color = 'brown')

ggplot(data = salary_valuations_by_team2, aes(x = avg_ORB, y = VALUE_MILLIONS)) +
  geom_point() +
  geom_smooth(color = 'brown')

ggplot(data = salary_valuations_by_team2, aes(x = avg_MPG, y = VALUE_MILLIONS)) +
  geom_point() +
  geom_smooth(color = 'brown')

ggplot(data = salary_valuations_by_team2, aes(x = avg_W, y = VALUE_MILLIONS)) +
  geom_point() +
  geom_smooth(color = 'brown')

# Don't go higher than 2 degrees, if that

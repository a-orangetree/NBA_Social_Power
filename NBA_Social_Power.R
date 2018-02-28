library(tidyverse)
library(modelr)
library(boot)
library(leaps)
library(gridExtra)

########################################################
# Import all files [for the moment? Not sure if we need all]
########################################################

# Redundant data files
#nba_2016_2017 <- read_csv('raw_data/nba_2016_2017_100.csv') # Not useful
#attendance <- read_csv('raw_data/nba_2017_attendance.csv')
#att_val <- read_csv('raw_data/nba_2017_att_val.csv')
#att_val_elo_with_cluster <- read_csv('raw_data/nba_2017_att_val_elo_with_cluster.csv')
#elo <- read_csv('raw_data/nba_2017_elo.csv')
#player_wikipedia <- read_csv('raw_data/nba_2017_player_wikipedia.csv') # bad data: only contains Russell Westbrook

att_val_elo <- read_csv('raw_data/nba_2017_att_val_elo.csv')
br <- read_csv('raw_data/nba_2017_br.csv')
endorsements <- read_csv('raw_data/nba_2017_endorsements.csv')
players_with_salary <- read_csv('raw_data/nba_2017_nba_players_with_salary.csv')
pie <- read_csv('raw_data/nba_2017_pie.csv') #player impoct estimation
players_with_stats_combined <- read_csv('raw_data/nba_2017_players_stats_combined.csv')
players_with_salary_wiki_twitter <- read_csv('raw_data/nba_2017_players_with_salary_wiki_twitter.csv')
real_plus_minus <- read_csv('raw_data/nba_2017_real_plus_minus.csv')
salary <- read_csv('raw_data/nba_2017_salary.csv') %>% 
  mutate(SALARY2 = (SALARY / 1000000))
team_valuations <- read_csv('raw_data/nba_2017_team_valuations.csv')
player_twitter <- read_csv('raw_data/nba_2017_twitter_players.csv')
team_name_crosswalk <- read_csv('raw_data/team_name_crosswalk.csv') #Doesn't map 100% from Short to Long


# TODO:
# 1. The EDA is somewhat a mess. Need to make it more into a story...
# 2. Need to add x and y labels for each plot
# 3. Can we get team name on the X axis for plots?
# 4. Start social media analysis


################################################
# Combine / create data sets
################################################


# Creates a dataframe with both salary and valuation by team
salary_valuations_by_team <- salary %>% 
  group_by(TEAM) %>% 
  summarise(total_salary = sum(SALARY2)
            ,median_salary = median(SALARY2)
            ,mean_salary = mean(SALARY2)
            ,low_salary = min(SALARY2)
            ,high_salary = max(SALARY2)) %>% 
  inner_join(team_valuations, by = 'TEAM') %>% 
  mutate(val_salary_ratio = VALUE_MILLIONS / total_salary)

# Aggregates social media statistics at a team level
# Removes players which played for more than one team
team_twitter_wiki <- drop_na(players_with_salary_wiki_twitter) %>% 
  group_by(TEAM) %>% 
  summarise(total_pageviews = sum(PAGEVIEWS)
            ,total_twitter_favorite = sum(TWITTER_FAVORITE_COUNT)
            ,total_twitter_retweet = sum(TWITTER_RETWEET_COUNT)) %>% 
  filter(!str_detect(TEAM, '/'))


##############################################
# Exploratory analysis begins here
##############################################

# pie %>% select(-PLAYER, -TEAM) %>% cor()
# pie %>% select(-PLAYER, -TEAM) %>% pairs()


# Made the x monetary value due to difficult of fitting team names
# Not a good graph
ggplot(team_valuations, aes(x = VALUE_MILLIONS
                            ,y = TEAM
                            ,size = VALUE_MILLIONS
                            ,color = VALUE_MILLIONS)) +
  geom_point() +
  labs(x = 'Team Value', y = 'Team Name')

# Same as the above but in tabular format
# team_valuations %>% 
#   arrange(desc(VALUE_MILLIONS))

# Shows attendance and team value are correlated
ggplot(att_val_elo, aes(x = VALUE_MILLIONS
                            ,y = AVG
                            ,size = VALUE_MILLIONS
                            ,color = VALUE_MILLIONS)) + 
  geom_smooth(color = 'brown') +
  geom_point() +
  labs(y = 'Attendance', x = 'Team Value')

# To be exact...
# cor(att_val_elo$VALUE_MILLIONS, att_val_elo$AVG)

# Compares team values between conferences... nothing significant
att_val_elo %>% 
  group_by(CONF) %>% 
  ggplot(aes(CONF, VALUE_MILLIONS)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, color = 'brown') +
  labs(x = 'Conference', 'Team Value')

# Display histogram of salaries... not surprisingly, right skewed
salary %>% 
  ggplot(aes(SALARY2, labels = TRUE)) +
  geom_histogram(binwidth = 2.5) +
  stat_bin(geom="text", aes(label=..count..), binwidth = 2.5) +
  labs(x = 'Salary in Millions', y = 'Number of Players')

# Find a quantiles
quantile(salary$SALARY, seq(0, 1, .1))

# Display salaries by position 
# Removed rows that only display a position a 'F' or 'G'
salary %>% 
  filter(POSITION == c('C', 'PF', 'PG', 'SF', 'SG')) %>% 
  ggplot(aes(POSITION, SALARY)) +
  geom_boxplot() +
  labs(x = 'Salary', y = 'Posiiton')

# Which were the players that were not included in the above
# Totals 13 players
# salary %>%
#   filter(POSITION == c('F', 'G'))

# Displays total salary by team
salary_valuations_by_team %>% 
  ggplot(aes(total_salary, TEAM, size = total_salary, color = total_salary)) +
  geom_point() +
  labs(x = 'Total Salary', y = 'Team')

# Displays median salary by team
salary_valuations_by_team %>% 
  ggplot(aes(median_salary, TEAM, size = median_salary, color = median_salary)) +
  geom_point() +
  labs(x = 'Median Salary', y = 'Team')

# Displays both valuations and salaries 
salary_valuations_by_team %>% 
  ggplot() +
  geom_point(aes(val_salary_ratio, TEAM, color = val_salary_ratio, size = val_salary_ratio)) +
  labs(x = 'Ratio of team value to salary', y = 'Team')

## Social Media quantiles
# use players with gt 1000 views
quantile(players_with_salary_wiki_twitter$PAGEVIEWS, seq(0, 1, .1)) 

# use players with gt 100 favorite
quantile(drop_na(players_with_salary_wiki_twitter)$TWITTER_FAVORITE_COUNT, seq(0, 1, .1))

# use players with gt 150 retweet
quantile(drop_na(players_with_salary_wiki_twitter)$TWITTER_RETWEET_COUNT, seq(0, 1, .1))

# Simple plot of social media stats by team
# Why don't the colors correspond to the label?*****************************************************
ggplot(team_twitter_wiki) + 
  geom_point(aes(total_pageviews, TEAM, color = 'red')) +
  geom_point(aes(total_twitter_favorite, TEAM, color = 'blue')) +
  geom_point(aes(total_twitter_retweet, TEAM, color = 'black'))


##########################################################
# Why does players_with_stats_combined have 40 less observations than br?
##########################################################


# dim(players_with_stats_combined)
# dim(br)

# # But, shows no players missing???
# players1 <- unique(players_with_stats_combined$PLAYER)
# players2 <- unique(br$Player)

# Not sure why these players are missing yet from the players_with_stats_combined
# table. 
# (missing_players <- setdiff(players2, players1))
# filter(br, Player %in% missing_players)


#######################################
# Below seeks correlations between individual performance statistics and salary
######################################


dim(players_with_stats_combined)
dim(salary)

# Adding salary to performance data
players_with_stats_salary <- inner_join(players_with_stats_combined, salary, by = c('PLAYER' =  'NAME'))
players_with_stats_salary <- select(players_with_stats_salary, -POSITION.y, -TEAM.y, -SALARY2)

players_with_stats_salary <- drop_na(players_with_stats_salary)
head(players_with_stats_salary)
dim(players_with_stats_salary)

# Correlation between Age and Salary. Took out who are either very young or very old.
# Not sure if meaningful...
players_with_stats_salary %>% 
  filter(AGE >= 20, AGE <= 35) %>% 
  ggplot(aes(x = AGE, y = SALARY)) +
    geom_point() +
    geom_smooth()

# To be precise...
cor(filter(players_with_stats_salary, AGE >= 20, AGE <= 35)$AGE
    ,filter(players_with_stats_salary, AGE >= 20, AGE <= 35)$SALARY)

# Checking if there's a significant decrease if we include all players
cor(players_with_stats_salary$AGE, players_with_stats_salary$SALARY)

# Average age of each team / Perhaps compare this later to average salary/valuation
# do not use players_with_stats_combined; it contains combination team names (e.g. CLE/ATL)
br %>%
  group_by(Tm) %>% 
  summarise(avg_age = mean(Age)) %>% 
  ggplot(aes(avg_age, Tm, size = avg_age, color = avg_age)) + 
  geom_point()

# Some correlation between Player Effectiveness and Salary
ggplot(players_with_stats_salary, aes(x = PIE, y = SALARY)) +
  geom_point() +
  geom_smooth()

# To be precise...
cor(players_with_stats_salary$PIE, players_with_stats_salary$SALARY)


####################################################
# Question 1: Can we predict salary from performance statistics?
###################################################


# Remove qualitative data
# Notes:
# 1. Removed RPM because it was highly correlated with ORPM and DRPM
# and thus was causing an error
# 2. Only displays observations which the salary is less than the 90th percentile
stats_salary_data <- players_with_stats_salary %>% 
  select(-PLAYER, -X1, -POSITION.x, -TEAM.x, -RPM) %>% 
  filter(SALARY <= quantile(salary$SALARY, .9))

# Used the below to determine that RPM should be removed. Could
# also remove ORPM and DRPM
# Matrix::rankMatrix(pracma::rref(cor(stats_salary_data)))

num_predictors <- dim(stats_salary_data)[2] - 1

stats_salary_model <- regsubsets(SALARY ~ .
                                 ,data = stats_salary_data
                                 ,nvmax = num_predictors)

(stats_salary_summary <- summary(stats_salary_model))

# Create tibble which contains data from results object
stats_salary_results <- tibble(num_pred = 1:num_predictors
                              ,rss = stats_salary_summary$rss
                              ,rsquared = stats_salary_summary$rsq
                              ,adj_rsquared = stats_salary_summary$adjr2
                              ,cp = stats_salary_summary$cp
                              ,bic = stats_salary_summary$bic)

# RSS
plot1 <- stats_salary_results %>% 
  ggplot(aes(num_pred, rss)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(stats_salary_results$rss)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('RSS')

# ADJ R-SQUARED
plot2 <- stats_salary_results %>% 
  ggplot(aes(num_pred, adj_rsquared)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.max(stats_salary_results$adj_rsquared)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('Adj R-squared')

# CP
plot3 <- stats_salary_results %>% 
  ggplot(aes(num_pred, cp)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(stats_salary_results$cp)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('Cp')

# BIC
plot4 <- stats_salary_results %>% 
  ggplot(aes(num_pred, bic)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(stats_salary_results$bic)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('BIC')

# Each of the measures comes up with vastly different number of 
# predictors... 
grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)
# Should the best model selections be so far apart for each estimate?

# Selecting a model. Display coefficients
coef(stats_salary_model, 4)


###########################################################
# Question2: Can we predict team valuations from individual salaries?
###########################################################


# Add columns to plays
players_with_stats_salary <- players_with_stats_salary %>% 
  mutate(training = sample(c(TRUE, FALSE), nrow(players_with_stats_salary), prob = c(.9, .1),rep = TRUE)
           ,testing = !training)

# Create linear model
salary_model1 <- lm(SALARY ~ AGE + POINTS, filter(players_with_stats_salary, training == TRUE))
summary(salary_model1)

# Why doesn't this work? ********************************************
autoplot(salary_model1) 

# Add predictions back to data
players_with_stats_salary <-players_with_stats_salary %>% 
  add_predictions(salary_model1)


# Since the rmse() funciton isn't working, calcluate RMSE manually
players_with_stats_salary <-  players_with_stats_salary %>%
  mutate(actual_pred_diff = SALARY - pred
         ,actual_pred_diff_sq = actual_pred_diff^2)

sqrt(mean(players_with_stats_salary$actual_pred_diff_sq))

#####################################################
# Question 3/4: Predict salary based on social media stats or vice versa
######################################################

#####################################################
# Question 5: Can we predict points from other performance statistics?
#####################################################

####################################################
# Appendix/Garbage/Foolin' around 
###################################################

stop('Everything below this point is garbage')

# Any correlation between salary and games won? ...Meh. 
# ...Wait, does W mean games won? Variable does not behave as expected
# where players on the same team have different W values.
ggplot(players_with_stats_salary, aes(x = W, y = SALARY)) + 
  geom_point() +
  geom_smooth()

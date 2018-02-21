library(tidyverse)
library(modelr)
library(boot)
library(leaps)
library(gridExtra)

########################################################
# Import all files [for the moment? Not sure if we need all]
########################################################

#nba_2016_2017 <- read_csv('raw_data/nba_2016_2017_100.csv') # Not useful
#attendance <- read_csv('raw_data/nba_2017_attendance.csv') # Redundant
#att_val <- read_csv('raw_data/nba_2017_att_val.csv') # Redundant
att_val_elo <- read_csv('raw_data/nba_2017_att_val_elo.csv')# Redundant
#att_val_elo_with_cluster <- read_csv('raw_data/nba_2017_att_val_elo_with_cluster.csv')
br <- read_csv('raw_data/nba_2017_br.csv')
#elo <- read_csv('raw_data/nba_2017_elo.csv') # Redundant
endorsements <- read_csv('raw_data/nba_2017_endorsements.csv')
players_with_salary <- read_csv('raw_data/nba_2017_nba_players_with_salary.csv')
pie <- read_csv('raw_data/nba_2017_pie.csv') #player impoct estimation
players_with_stats_combined <- read_csv('raw_data/nba_2017_players_stats_combined.csv')
players_with_salary_wiki_twitter <- read_csv('raw_data/nba_2017_players_with_salary_wiki_twitter.csv')
player_wikipedia <- read_csv('raw_data/nba_2017_player_wikipedia.csv')
real_plus_minus <- read_csv('raw_data/nba_2017_real_plus_minus.csv')
salary <- read_csv('raw_data/nba_2017_salary.csv')
team_valuations <- read_csv('raw_data/nba_2017_team_valuations.csv')
twitter_players <- read_csv('raw_data/nba_2017_twitter_players.csv')


##############################################
# Exploratory analysis begins here
##############################################

# Made the x monetary value due to difficult of fitting team names
# Not a good graph
ggplot(team_valuations, aes(x = VALUE_MILLIONS
                            ,y = TEAM
                            ,size = VALUE_MILLIONS
                            ,color = VALUE_MILLIONS)) +
  geom_point() +
  labs(x = 'Team Value')

# Same as the above but in tabular format
team_valuations %>% 
  arrange(desc(VALUE_MILLIONS))

# Shows attendance and team value are correlated
ggplot(att_val_elo, aes(x = VALUE_MILLIONS
                            ,y = AVG
                            ,size = VALUE_MILLIONS
                            ,color = VALUE_MILLIONS)) + 
  geom_smooth(color = 'brown') +
  geom_point() +
  labs(y = 'Attendance', x = 'Team Value')

# To be exact...
cor(att_val_elo$VALUE_MILLIONS, att_val_elo$AVG)

# Compares team values between conferences... nothing significant
att_val_elo %>% 
  group_by(CONF) %>% 
  ggplot(aes(CONF, VALUE_MILLIONS)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, color = 'brown') +
  labs(x = 'Conference', 'Team Value')

# Displays total Salary by team
# TODO: We should overlap this with the graph of team valuations *****************************
salary %>% 
  group_by(TEAM) %>% 
  summarise(totalSalary = sum(SALARY)) %>% 
  ggplot(aes(totalSalary, TEAM, size = totalSalary, color = totalSalary)) + geom_point()

##########################################################
# Why does players_with_stats_combined have 40 less observations than br?
##########################################################

dim(players_with_stats_combined)
dim(br)

# But, shows no players missing???
players1 <- unique(players_with_stats_combined$PLAYER)
players2 <- unique(br$Player)

# Not sure why these players are missing yet from the players_with_stats_combined
# table. 
(missing_players <- setdiff(players2, players1))
filter(br, Player %in% missing_players)

#######################################
# Below seeks correlations between individual performance statistics and salary
######################################

dim(players_with_stats_combined)
dim(salary)

# Adding salary to performance data
players_with_stats_salary <- merge(players_with_stats_combined, salary, by.x = 'PLAYER', by.y =  'NAME')
players_with_stats_salary <- select(players_with_stats_salary, -POSITION.y, -TEAM.y)

players_with_stats_salary <- drop_na(players_with_stats_salary)
head(players_with_stats_salary)
dim(players_with_stats_salary)

### Age v Salary
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


### PIE v Salary
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
stats_salary_data <- players_with_stats_salary %>% 
  select(-PLAYER, -X1, -POSITION.x, -TEAM.x)

num_predictors <- dim(stats_salary_data)[2] - 2 #why does this need to be two instead of one ***************

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

# Selecting a model with two predictors. Display coefficients
coef(stats_salary_model, 2)

###########################################################
# Can we predict team valuations from individual salaries?
###########################################################

# Uses Validation Set Don't think this is working appropriately*************************************

# Add columns to plays
players_with_stats_salary <- players_with_stats_salary %>% 
  mutate(training = sample(c(TRUE, FALSE), nrow(players_with_stats_salary), rep = TRUE)
           ,testing = !training)

# Create linear model
salary_model1 <- lm(SALARY ~ AGE + POINTS, filter(players_with_stats_salary, training == TRUE))

# Add predictions back to data (should I only be adding predictions to the test?)**************************
players_with_stats_salary <-players_with_stats_salary %>% 
  add_predictions(salary_model1)

# Calculate Test MSE Not Working******************************************
rmse(filter(players_with_stats_salary, testing == TRUE)$SALARY
     ,filter(players_with_stats_salary, testing == TRUE)$pred)

####################################################
# Appendix/Garbage/Foolin' around 
###################################################

stop('Everything below this point is garbage')

# Quick diagnostic plots...
ggplot(twitter_players, aes(TWITTER_FAVORITE_COUNT)) + geom_histogram()
ggplot(player_wikipedia, aes(pageviews)) + geom_histogram()

# Any correlation between salary and games won? ...Meh. 
# ...Wait, does W mean games W? Variable does not behave as expected
# where players on the same team have different W values.
ggplot(players_with_stats_salary, aes(x = W, y = SALARY)) + 
  geom_point() +
  geom_smooth()

# Are any numerical statistics correlated with anything else?
pie %>% select(-PLAYER, -TEAM) %>% 
  pairs()

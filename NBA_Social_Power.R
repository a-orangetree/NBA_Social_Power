library(tidyverse)
library(modelr)
library(boot)
library(leaps)
library(gridExtra)

########################################################
# Import files
########################################################

# Redundant data files
#nba_2016_2017 <- read_csv('raw_data/nba_2016_2017_100.csv') # Not useful
#attendance <- read_csv('raw_data/nba_2017_attendance.csv')
#att_val <- read_csv('raw_data/nba_2017_att_val.csv')
#att_val_elo_with_cluster <- read_csv('raw_data/nba_2017_att_val_elo_with_cluster.csv')
#elo <- read_csv('raw_data/nba_2017_elo.csv')
#player_wikipedia <- read_csv('raw_data/nba_2017_player_wikipedia.csv') # bad data: only contains Russell Westbrook

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

# TODO:
# - Need to figure out the differences between the player data sets - are we using the correct one?

################################################
# Combine / create data sets
################################################

# dim(players_with_stats_combined)
# dim(team_valuations)
# dim(salary)
# dim(salary_valuations_by_team)

salary$TEAM <- str_replace(salary$TEAM, 'LA Clippers', 'Los Angeles Clippers')

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
            #,`avg_3P%` = avg_3P / avg_3PA
            ,avg_2P = median(`2P`, na.rm = TRUE)
            ,avg_2PA = median(`2PA`, na.rm = TRUE)
            #,`avg_2P%` = avg_2P / avg_2PA
            ,avg_FT = median(FT, na.rm = TRUE)
            ,avg_FTA = median(FTA, na.rm = TRUE)
            #,`avg_FT%` = avg_FT / avg_FTA
            ,avg_ORB = median(ORB, na.rm = TRUE)
            ,avg_DRB = median(DRB, na.rm = TRUE)
            #,avg_TRB = median(TRB, na.rm = TRUE)
            ,avg_AST = median(AST, na.rm = TRUE)
            ,avg_STL = median(STL, na.rm = TRUE)
            ,avg_BLK = median(BLK, na.rm = TRUE)
            ,avg_TOV = median(TOV, na.rm = TRUE)
            #,avg_PF = median(PF, na.rm = TRUE)
            #,avg_POINTS = median(POINTS, na.rm = TRUE)
            ,avg_GP = median(GP, na.rm = TRUE)
            ,avg_MPG = median(MPG, na.rm = TRUE)
            #,avg_ORPM = median(ORPM, na.rm = TRUE)
            #,avg_DRPM = median(DRPM, na.rm = TRUE)
            #,avg_RPM = median(RPM, na.rm = TRUE)
            #,avg_WINS_RPM = median(WINS_RPM, na.rm = TRUE)
            #,avg_PIE = median(PIE, na.rm = TRUE)
            ,avg_PACE = median(PACE, na.rm = TRUE)
            ,avg_W = median(W, na.rm = TRUE)) %>% 
  right_join(team_valuations, by = c('TEAM.x' = 'TEAM')) %>% 
  mutate(TEAM.x = factor(TEAM.x))

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
  filter(POSITION %in% c('C', 'PF', 'PG', 'SF', 'SG')) %>% 
  ggplot(aes(POSITION, SALARY)) +
  geom_boxplot() +
  labs(x = 'Salary', y = 'Posiiton')


(avg_salary <- salary %>% 
  filter(POSITION %in% c('C', 'PF', 'PG', 'SF', 'SG')) %>% 
  group_by(POSITION) %>% 
  summarise(avg_salary = median(SALARY) / 1000000))

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
ggplot(team_twitter_wiki) + 
  geom_point(aes(total_pageviews, TEAM, color = 'red')) +
  geom_point(aes(total_twitter_favorite, TEAM, color = 'blue')) +
  geom_point(aes(total_twitter_retweet, TEAM, color = 'black'))


#############################################################
# Differences why are there differences between data sets regarding 
# the players which they contain?
#############################################################


dim(br)
br_players <- length(unique(br$Player)) #486

dim(players_with_salary) # Has duplicates
players_with_salary_players <- length(unique(players_with_salary$PLAYER)) #335

dim(players_with_stats_combined)
players_with_stats_combined_players <- length(unique(players_with_stats_combined$PLAYER)) # 446

dim(players_with_salary_wiki_twitter) # Has one duplicate
players_with_salary_wiki_twitter_players <- length(unique(players_with_salary_wiki_twitter$PLAYER)) # 238


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
players_with_stats_salary <- inner_join(players_with_stats_combined, salary
                                        , by = c('PLAYER' =  'NAME'))
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


# Plots player efficiency by position
plotA <- ggplot() +
  geom_boxplot(data = filter(players_with_stats_salary
                             , POSITION.x %in% c('C', 'PF', 'PG', 'SF', 'SG'))
               , aes(POSITION.x, PIE))

# Plots median salary by position
plotB <- ggplot() +
  geom_point(data = avg_salary, aes(POSITION, avg_salary))

grid.arrange(plotA, plotB, nrow = 1, ncol = 2)


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
  filter(SALARY <= quantile(salary$SALARY, .9))

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

# Selecting a model. Display coefficients
coef(stats_salary_model, 6)

# Create training and test data sets
training_data <- stats_salary_data %>% sample_frac(.8)
test_data <- setdiff(stats_salary_data, training_data)

# Fit model using the coefficients above and add predictions to test data
salary_model <- lm(SALARY ~ AGE + `2P` + `2PA` + ORB + MPG + W, data = training_data)
test_data <- add_predictions(test_data, salary_model)

# Calculate root mean squared error
rmse(salary_model, test_data)

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

# What other models should we try? *************************************************


###########################################################
# Question2: Can we predict team valuations from individual salaries?
###########################################################

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


#####################################################
# Question 3/4: Predict salary based on social media stats or vice versa
######################################################

# First let's predict salary based on a few social media stats

# New tibble that only has player name, salary, wiki stats, twitter stats
players_wiki_twitter <- select(players_with_salary_wiki_twitter, PLAYER)
players_wiki_twitter <- players_wiki_twitter %>%
  mutate(index = players_with_salary_wiki_twitter$X1,
         salary = players_with_salary_wiki_twitter$SALARY_MILLIONS,
         wiki_views = players_with_salary_wiki_twitter$PAGEVIEWS,
         twitter_fav = players_with_salary_wiki_twitter$TWITTER_FAVORITE_COUNT,
         twitter_RT = players_with_salary_wiki_twitter$TWITTER_RETWEET_COUNT)

# drop rows with missing social media stats
players_wiki_twitter <- drop_na(players_wiki_twitter)

# Explore which predictors are statistically significant
exploratory_mod <- players_wiki_twitter %>%
  select(-PLAYER) %>%
  select(-index) %>%
  lm(salary ~., data = .)

summary(exploratory_mod)
# Observe that wikipedia views, twitter favorites, and twitter retweets are
# all statistically significant - twitter RT seems to be a bit less significant than the others

# VALIDATION SET APPROACH
# First make train and test sets
train_set <- players_wiki_twitter %>%
  sample_frac(0.75, replace=FALSE)

test_set <- players_wiki_twitter %>%
  setdiff(train_set)

# Make linear regression model
# Let's try just using wikipedia page views alone first
glm_train <- lm(salary ~ wiki_views, data=train_set)

# Add predicted salary 
test_set <- add_predictions(test_set, glm_train)
test_set <- test_set %>% 
  mutate(salary_from_wiki = pred) %>%
  select(-pred)

# Calculate root mean squared error 
rmse(glm_train, test_set)

# Quick plot of predicted vs. actual values shows us that using only Wiki is not useful
# these plots are NOT USEFUL - just looking at the predicted vs. actual
wiki_only_plot <- ggplot() +
  geom_point(data = test_set, aes(x = seq(1:dim(test_set)[1]), y = salary)) +
  geom_point(data = test_set, aes(x = seq(1:dim(test_set)[1]), y = salary_from_wiki), color=
                                    "red") +
  geom_smooth(data = test_set, aes(x = seq(1:dim(test_set)[1]), y = salary_from_wiki), color=
                 "red") +
  xlab("Player Index") + ylab("Salary") + 
  ggtitle("Salary Predicted by Wiki Stats")

#````````````````````````````````````````````````````````````

# Try model using just twitter stats, no page-view stats
twitter_only_model <- lm(salary ~ twitter_fav + twitter_RT, data=train_set)

# Add predicted salary 
test_set <- add_predictions(test_set, twitter_only_model)
test_set <- test_set %>% 
  mutate(salary_from_twitter = pred) %>%
  select(-pred)

# Calculate root mean squared error 
rmse(twitter_only_model, test_set)

# Quick plot of predicted vs. actual values shows us that using only Wiki is not useful
# these plots are NOT USEFUL - just looking at the predicted vs. actual
twitter_only_plot <- ggplot() +
  geom_point(data = test_set, aes(x = seq(1:dim(test_set)[1]), y = salary)) +
  geom_point(data = test_set, aes(x = seq(1:dim(test_set)[1]), y = salary_from_twitter), color=
               "green") +
  geom_smooth(data = test_set, aes(x = seq(1:dim(test_set)[1]), y = salary_from_twitter), color=
                 "green") +
  xlab("Player Index") + ylab("Salary") +  ylim(0, range(test_set$salary_from_wiki)[2]) +
  ggtitle("Salary Predicted by Twitter Stats")

# This is still really bad...

#````````````````````````````````````````````````````````````````````

# Those models are bad - let's use all three predictors next!
# Make linear regression model
# Let's try just using wikipedia page views alone first
salary_vs_media_model <- lm(salary ~ wiki_views + twitter_fav + twitter_RT, data=train_set)

# Add predicted salary 
test_set <- add_predictions(test_set, salary_vs_media_model)
test_set <- test_set %>% 
  mutate(salary_from_media = pred) %>%
  select(-pred)

# Calculate root mean squared error 
rmse(salary_vs_media_model, test_set)

# Plots of predicted vs. actual with twitter and wikipedia
media_salary_plot <- ggplot() +
  geom_point(data = test_set, aes(x = seq(1:dim(test_set)[1]), y = salary)) +
  geom_point(data = test_set, aes(x = seq(1:dim(test_set)[1]), y = salary_from_media), color="blue") +
  geom_smooth(data = test_set, aes(x = seq(1:dim(test_set)[1]), y = salary_from_media)) +
  xlab("Player Index") + ylab("Salary") + ylim(0, range(test_set$salary_from_wiki)[2]) +
  ggtitle("Salary Predicted by Media Stats")

# Plot side-by-side
grid.arrange(wiki_only_plot, twitter_only_plot, media_salary_plot, nrow = 1, ncol = 3)
range(players_wiki_twitter$salary)

# RMSE is the lowest using all predictors, but still pretty bad overall. 
# TODO: make more useful plots - do more exploration to see if we can make a better
# predictor of salary based on social media and wikipedia stats, also explore if we can predict 
# social media stats based on salary (other way around).


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
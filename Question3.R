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

#####################################################
# Question 3/4: Predict salary based on social media stats or vice versa
######################################################

# Adding salary to performance data
players_with_stats_salary <- inner_join(players_with_stats_combined, salary
                                        , by = c('PLAYER' =  'NAME'))
players_with_stats_salary <- select(players_with_stats_salary, -POSITION.y, -TEAM.y, -SALARY2)
players_with_stats_salary <- drop_na(players_with_stats_salary)

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
# glm_preds <- predict(glm_train, test_set, type="response")
# final_preds <- predict(final_mod, newdata=list(age = age_grid), se=TRUE)
# test_set <- mutate(test_set, glm_preds = glm_preds)

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

wiki_only_plot

# actual vs. predicted
# SHOW THIS TO SHOW HOW IT DIDN'T WORK WELL
wiki_actual_v_pred <- ggplot(data = test_set, aes(x = salary, y = salary_from_wiki)) +
  geom_point() + geom_smooth() +
  xlab("Actual Salary") + ylab("Predicted Salary") + 
  ggtitle("Actual vs Predicted - Wikipedia Stats")
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
twitter_only_plot

twitter_actual_v_pred <- ggplot(data = test_set, aes(x = salary, y = salary_from_twitter)) +
  geom_point() + geom_smooth() +
  xlab("Actual Salary") + ylab("Predicted Salary") + 
  ggtitle("Actual vs Predicted - Twitter Combined Stats")
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

all_social_actual_v_pred <- ggplot(data = test_set, aes(x = salary, y = salary_from_media)) +
  geom_point() + geom_smooth() +
  xlab("Actual Salary") + ylab("Predicted Salary") + 
  ggtitle("Actual vs Predicted - Twitter and Wiki Stats")

# Plot side-by-side
grid.arrange(wiki_only_plot, twitter_only_plot, media_salary_plot, nrow = 1, ncol = 3)
grid.arrange(wiki_actual_v_pred, twitter_actual_v_pred, all_social_actual_v_pred, nrow=1, ncol=3)
range(players_wiki_twitter$salary)
media_max_range <- range(test_set$salary_from_media)

# RMSE is the lowest using all predictors, but still pretty bad overall. 
# TODO - make the range of those graphs all the same - hard to compare right now

# EXPLORING A LITTLE Q4
# Can we predict PPG based on performance statistics?

# Points by Position (already put this in EDA)
players_with_stats_salary %>%
  filter(POSITION.x %in% c('C', 'PF', 'PG', 'SF', 'SG')) %>% 
  ggplot(aes(POSITION.x, POINTS)) +
  geom_boxplot() +
  labs(x = 'Position', y = 'Av. PPG') + 
  ggtitle("Points By Position")


# also let's graph points by position
# Explore which predictors are statistically significant
explore_ppg <- players_with_stats_salary %>%
  select(-PLAYER) %>%
  select(-X1) %>%
  select(-Rk) %>%
  select(-POSITION.x) %>%
  select(-TEAM.x) %>%
  lm(POINTS ~., data = .)

summary(explore_ppg)

# Looks like the predictors most correlated with points per game are:
# FT, 3PA, FG 2P
# Let's make a linear model to graph this
# Make train and test sets
train_set_points <- players_with_stats_salary %>%
  sample_frac(0.75, replace=FALSE)

test_set_points <- players_with_stats_salary %>%
  setdiff(train_set_points)

# THIS MODEL IS TOO GOOD!!!!!!
ppg_model <- lm(POINTS ~ FT + `3P` + FG + `2P`, data=train_set_points)

rmse(ppg_model, test_set_points)
# Add predicted salary to test set
test_set_points <- add_predictions(test_set_points, ppg_model)

# Plots of predicted vs. actual for points based on those stats
points_predicted_plot <- ggplot() +
  geom_point(data = test_set_points, aes(x = seq(1:dim(test_set_points)[1]), y = POINTS), color="red") +
  geom_point(data = test_set_points, aes(x = seq(1:dim(test_set_points)[1]), y = pred), color="blue") +
  geom_smooth(data = test_set_points, aes(x = seq(1:dim(test_set_points)[1]), y = pred)) +
  xlab("Player Index") + ylab("Points") + ylim(0, range(test_set_points$pred)[2]) +
  ggtitle("Points Predicted by other Performance Stats")

points_predicted_plot
coef(ppg_model)

# Plot actual vs. predicted points
actual_v_pred_points <- ggplot(data = test_set_points, aes(x = POINTS, y = pred)) +
  geom_point() + geom_smooth() +
  xlab("Actual PPG") + ylab("Predicted PPG") + 
  ggtitle("Actual vs Predicted Points Per Game")
actual_v_pred_points


# Make sure that points isn't just a summary of those predictors
players_with_stats_test <- players_with_stats_salary %>%
  mutate(sum_4stats = FT+`3P`+`2P`)


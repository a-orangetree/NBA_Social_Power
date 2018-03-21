library(tidyverse)
library(modelr)
library(boot)
library(leaps)
library(gridExtra)
library(boot)
library(leaps)


#####################################################
# Question 2: Can we predict salary based on social media stats?
######################################################
# Below we explore simple linear models for predicting salary based on wikipedia pageviews,
# twitter RTs/favorites, and all 3 combined. Results were not good and the sample size is pretty small,
# and intuitively there isn't a ton of correlation here.

# Load processed data files
players_with_stats_salary <- read_csv('data/processed/players_with_stats_salary.csv')
players_wiki_twitter <- read_csv('data/processed/players_wiki_twitter.csv')


# First let's predict salary based on a few social media stats
# Explore which predictors are statistically significant using simple linear model
exploratory_mod <- players_wiki_twitter %>%
  select(-PLAYER) %>%
  select(-index) %>%
  lm(salary ~., data = .)

summary(exploratory_mod)
# Observe that wikipedia views, twitter favorites, and twitter retweets are
# all statistically significant - twitter RT seems to be a bit less significant than the others

# VALIDATION SET APPROACH ---------------------------------
# First make train and test sets
train_set <- players_wiki_twitter %>%
  sample_frac(0.75, replace=FALSE)

test_set <- players_wiki_twitter %>%
  setdiff(train_set)

#---------------WIKI PAGEVIEWS ONLY-----------------------------

# Make linear regression model
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

# wiki_only_plot

# actual vs. predicted
# SHOW THIS TO SHOW HOW IT DIDN'T WORK WELL
wiki_actual_v_pred <- ggplot(data = test_set, aes(x = salary, y = salary_from_wiki)) +
  geom_point() + geom_smooth() +
  xlab("Actual Salary") + ylab("Predicted Salary") + 
  ggtitle("Actual vs Predicted - Wikipedia Stats")

#----------------------TWITTER STATS ALONE---------------------------

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
# twitter_only_plot

twitter_actual_v_pred <- ggplot(data = test_set, aes(x = salary, y = salary_from_twitter)) +
  geom_point() + geom_smooth() +
  xlab("Actual Salary") + ylab("Predicted Salary") + 
  ggtitle("Actual vs Predicted - Twitter Combined Stats")


#----------------------COMBINED MEDIA STATS---------------------------

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

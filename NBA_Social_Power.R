library(tidyverse)

# Import all files [for the moment? Not sure if we need all]
#nba_2016_2017 <- read_csv('raw_data/nba_2016_2017_100.csv') # Not useful
#attendance <- read_csv('raw_data/nba_2017_attendance.csv') # Redundant
#att_val <- read_csv('raw_data/nba_2017_att_val.csv') # Redundant
#att_val_elo <- read_csv('raw_data/nba_2017_att_val_elo.csv')# Redundant
att_val_elo_with_cluster <- read_csv('raw_data/nba_2017_att_val_elo_with_cluster.csv')
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
  geom_point()

# Shows attendance and value are not strongly correlated
ggplot(att_val_elo, aes(x = VALUE_MILLIONS
                            ,y = AVG
                            ,size = VALUE_MILLIONS
                            ,color = VALUE_MILLIONS)) + 
  geom_point() +
  geom_smooth(color = 'brown')

# To be exact...
cor(att_val_elo$VALUE_MILLIONS, att_val_elo$AVG)

# Compares team values between conferences... nothing significant
att_val_elo %>% 
  group_by('CONF') %>% 
  ggplot(aes(CONF, VALUE_MILLIONS)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, color = 'brown')

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

players_with_stats_salary <- merge(players_with_stats_combined, salary, by.x = 'PLAYER', by.y =  'NAME')
dim(players_with_stats_salary)
glimpse(players_with_stats_salary)

# Little if any correlation between Age and Salary
ggplot(players_with_stats_salary, aes(x = AGE, y = SALARY)) +
  geom_point() +
  geom_smooth()

cor(players_with_stats_salary$AGE, players_with_stats_salary$SALARY)

# Some correlation between Player Effectiveness and Salary
ggplot(players_with_stats_salary, aes(x = PIE, y = SALARY)) +
  geom_point() +
  geom_smooth()

cor(players_with_stats_salary$PIE, players_with_stats_salary$SALARY)

####################################################
# Model to predict salary from individual performance statistics
###################################################


####################################################
# Appendix/Garbage/Foolin' around 
###################################################

# Why doesn't the below break out by team????
x <- br %>% 
  group_by('Tm') %>% 
  summarise(avg_age = median(Age, na.rm = TRUE))

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

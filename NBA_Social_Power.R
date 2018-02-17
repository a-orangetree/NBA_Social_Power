library(tidyverse)

# Initial Questions: 
# 1. Does salary or performance correlate with social media usage?
# 2. Does salary correlate with general performance or a specific 
#    performance metric?

# Import all files FOR THE MOMENT. Not sure if we need all.
#nba_2016_2017 <- read_csv('raw_data/nba_2016_2017_100.csv')
#attendance <- read_csv('raw_data/nba_2017_attendance.csv')
#att_val <- read_csv('raw_data/nba_2017_att_val.csv')
att_val_elo <- read_csv('raw_data/nba_2017_att_val_elo.csv')
#att_val_elo_with_cluster <- read_csv('raw_data/nba_2017_att_val_elo_with_cluster.csv')
br <- read_csv('raw_data/nba_2017_br.csv')
#elo <- read_csv('raw_data/nba_2017_elo.csv')
endorsements <- read_csv('raw_data/nba_2017_endorsements.csv')
players_with_salary <- read_csv('raw_data/nba_2017_nba_players_with_salary.csv')
pie <- read_csv('raw_data/nba_2017_pie.csv')
players_with_stats_combined <- read_csv('raw_data/nba_2017_players_stats_combined.csv')
players_with_salary_wiki_twitter <- read_csv('raw_data/nba_2017_players_with_salary_wiki_twitter.csv')
player_wikipedia <- read_csv('raw_data/nba_2017_player_wikipedia.csv')
real_plus_minus <- read_csv('raw_data/nba_2017_real_plus_minus.csv')
salary <- read_csv('raw_data/nba_2017_salary.csv')
team_valuations <- read_csv('raw_data/nba_2017_team_valuations.csv')
twitter_players <- read_csv('raw_data/nba_2017_twitter_players.csv')

################ Exploratory analysis begins here #########################################

# Mostly garbage...

# Made the x monetary value due to difficult of fitting team names
# Not a good graph
ggplot(team_valuations, aes(x = VALUE_MILLIONS
                            ,y = TEAM
                            ,size = VALUE_MILLIONS
                            ,color = VALUE_MILLIONS)) + geom_point()

# Quick diagnostic plots...
ggplot(twitter_players, aes(TWITTER_FAVORITE_COUNT)) + geom_histogram()
ggplot(player_wikipedia, aes(pageviews)) + geom_histogram()

# Any correlation between salary and games won? ...Meh.
dim(players_with_stats_combined)
dim(salary)

players_with_stats_salary <- merge(players_with_stats_combined, salary, by.x = 'PLAYER', by.y =  'NAME')
dim(players_with_stats_salary)

ggplot(players_with_stats_salary, aes(x = W, y = SALARY)) + 
  geom_point() +
  geom_smooth()

library(tidyverse)
library(gridExtra)

########################################################
# Import files
########################################################

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


# ################################################
# # Combine / create data sets
# ################################################
 
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

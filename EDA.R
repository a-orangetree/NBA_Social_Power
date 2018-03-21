library(tidyverse)
library(gridExtra)

########################################################
# First load all raw, unprocessed files
########################################################

# PS = Performance Statistics
att_val_elo <- read_csv('data/unprocessed/nba_2017_att_val_elo.csv') # Attendance, valuation, and ELO
br <- read_csv('data/unprocessed/nba_2017_br.csv') # 486 x 30 (PS)
endorsements <- read_csv('data/unprocessed/nba_2017_endorsements.csv')
players_with_salary <- read_csv('data/unprocessed/nba_2017_nba_players_with_salary.csv') # 342 x 39 (PS)
pie <- read_csv('data/unprocessed/nba_2017_pie.csv') # Player impoct estimation
players_with_stats_combined <- read_csv('data/unprocessed/nba_2017_players_stats_combined.csv') # 446 x 38 (PS)
players_with_salary_wiki_twitter <- read_csv('data/unprocessed/nba_2017_players_with_salary_wiki_twitter.csv') # 239 x 42 (PS)
real_plus_minus <- read_csv('data/unprocessed/nba_2017_real_plus_minus.csv') # 468 x 8 (Subset of PS?)
salary <- read_csv('data/unprocessed/nba_2017_salary.csv') %>% # 449 x 5
  mutate(SALARY2 = (SALARY / 1000000)) 
team_valuations <- read_csv('data/unprocessed/nba_2017_team_valuations.csv')
player_twitter <- read_csv('data/unprocessed/nba_2017_twitter_players.csv') # 329 x 3
team_name_crosswalk <- read_csv('data/unprocessed/team_name_crosswalk.csv') #Doesn't map 100% from Short to Long

# ################################################
# Clean data and aggregate fields we're going to be using in the questions into a few main dataframes
# Write out CSVs for the datasets we are mainly using (to access in the Questionx.R files)
# ################################################
 
# First write out some clean versions of the main data files
salary$TEAM <- str_replace(salary$TEAM, 'LA Clippers', 'Los Angeles Clippers')
salary <- drop_na(salary)
write_csv(salary, file.path('data/processed', "salary.csv"))

players_with_stats_combined <- drop_na(players_with_stats_combined)
write_csv(players_with_stats_combined, file.path('data/processed', "players_with_stats_combined.csv"))

players_with_salary_wiki_twitter <- drop_na(players_with_salary_wiki_twitter)
write_csv(players_with_salary_wiki_twitter, file.path('data/processed', "players_with_salary_wiki_twitter.csv"))


# Creates a dataframe with both salary statistics and valuation by team
salary_valuations_by_team <- drop_na(salary) %>%
  group_by(TEAM) %>%
  summarise(total_salary = sum(SALARY2)
            ,median_salary = median(SALARY2)
            ,mean_salary = mean(SALARY2)
            ,low_salary = min(SALARY2)
            ,high_salary = max(SALARY2)) %>%
  inner_join(team_valuations, by = 'TEAM') %>%
  mutate(val_salary_ratio = VALUE_MILLIONS / total_salary)

write_csv(salary_valuations_by_team, file.path('data/processed', "salary_valuations_by_team.csv"))

# Aggregates social media statistics at a team level
# Removes players which played for more than one team
team_twitter_wiki <- drop_na(players_with_salary_wiki_twitter) %>%
  group_by(TEAM) %>%
  summarise(total_pageviews = sum(PAGEVIEWS)
            ,total_twitter_favorite = sum(TWITTER_FAVORITE_COUNT)
            ,total_twitter_retweet = sum(TWITTER_RETWEET_COUNT)) %>%
  filter(!str_detect(TEAM, '/'))

write_csv(team_twitter_wiki, file.path('data/processed', "team_twitter_wiki.csv"))


# Adding salary to performance data
players_with_stats_salary <- inner_join(players_with_stats_combined, salary
                                        , by = c('PLAYER' =  'NAME'))
players_with_stats_salary <- select(players_with_stats_salary, -POSITION.y, -TEAM.y, -SALARY2)

players_with_stats_salary <- drop_na(players_with_stats_salary)

write_csv(players_with_stats_salary, file.path('data/processed', "players_with_stats_salary.csv"))


# Same as the above but with extra fields about salary stats
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

# write out
write_csv(salary_valuations_by_team2, file.path('data/processed', "salary_valuations_by_team2.csv"))


# New tibble that only has player name, salary, wiki stats, twitter stats (Q3)
players_wiki_twitter <- select(players_with_salary_wiki_twitter, PLAYER)
players_wiki_twitter <- players_wiki_twitter %>%
  mutate(index = players_with_salary_wiki_twitter$X1,
         salary = players_with_salary_wiki_twitter$SALARY_MILLIONS,
         wiki_views = players_with_salary_wiki_twitter$PAGEVIEWS,
         twitter_fav = players_with_salary_wiki_twitter$TWITTER_FAVORITE_COUNT,
         twitter_RT = players_with_salary_wiki_twitter$TWITTER_RETWEET_COUNT)

# drop rows with missing social media stats
players_wiki_twitter <- drop_na(players_wiki_twitter)

# write out
write_csv(players_wiki_twitter, file.path('data/processed', "players_wiki_twitter.csv"))


##############################################
# Exploratory analysis begins here
##############################################

# Made the x monetary value due to difficult of fitting team names
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

# Points by Position (already put this in EDA)
players_with_stats_salary %>%
  filter(POSITION.x %in% c('C', 'PF', 'PG', 'SF', 'SG')) %>% 
  ggplot(aes(POSITION.x, POINTS)) +
  geom_boxplot() +
  labs(x = 'Position', y = 'Av. PPG') + 
  ggtitle("Points By Position")

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

team_social_media <- ggplot(team_twitter_wiki) + 
  geom_point(aes(total_pageviews, TEAM), color = 'red') +
  geom_point(aes(total_twitter_favorite, TEAM), color = 'blue') +
  geom_point(aes(total_twitter_retweet, TEAM), color = 'black') +
  labs(x = 'Social Media Statistics', y = 'Team') + 
  ggtitle("Social Media Stats per Team")

team_social_media
cols <- c(total_pageviews = "red", total_twitter_favorite = "blue", total_twitter_retweet = "black")

team_social_media + scale_colour_manual(
  values = c("red", "blue", "black")
)


#############################################################
# Exploring why are there differences between data sets regarding 
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


#######################################
# Below seeks correlations between individual performance statistics and salary
######################################

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

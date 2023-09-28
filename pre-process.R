##### Half-time #####

#Setting the working directory
setwd("/Users/quinne/Desktop/all_code/dissertation_dataset")
getwd()

library(dplyr)
library(tidyr)
library(ggplot2)
library(skimr)
library(corrplot)
library(viridis)  
library(RColorBrewer)
library(gridExtra)
library(naniar)
library(visdat)
library(mice)
library(vcd)
library(lattice)
library(factoextra)
library(FactoMineR)
library(tidyverse)
library(cluster)
library(pROC)

##### Import Data and Make Suitable for Analysis #####
# Import Data
df <- read.csv("~/Desktop/all_code/dissertation_dataset/double_dataframe.csv")
df_bat <- df[df$toss_decision == "bat", ]
head(df_bat)

###Drop columns that don't make sense for half-time model
df_bat <- df_bat[, -(which(names(df) %in% c("toss_decision", 
                                            "choose_to_field", 
                                            "forced_to_field")))]
### New variable: 'days_from_start'
### converting time variables like months and days to the first day from the start of each season
df_bat$year <- df_bat$season
df_bat$date <- as.Date(with(df_bat, paste(year, month, day, sep="-")), "%Y-%m-%d")

get_start_date <- function(season, gender) {
  if (season == 2021 && gender == "female") return(as.Date("2021-07-21"))
  if (season == 2021 && gender == "male") return(as.Date("2021-07-22"))
  if (season == 2022 && gender == "female") return(as.Date("2022-08-11"))
  if (season == 2022 && gender == "male") return(as.Date("2022-08-03"))
}

df_bat$days_from_start <- mapply(function(season, gender, date) {
  start_date <- get_start_date(season, gender)
  as.numeric(difftime(date, start_date, units = "days"))
}, df_bat$season, df_bat$gender, df_bat$date)

df_bat$year <- NULL
df_bat$month <- NULL
df_bat$day <- NULL
df_bat$date <- NULL

# Overview of data structure
skim(df_bat) 
dim(df_bat) #123 22
names(df_bat)
str(df_bat)
summary(df_bat)

class(df_bat[2,20])
is.factor(df_bat$Result)

# Change to correct type
df_bat$Result <- as.factor(df_bat$Result)
table(df_bat$Result) 

# Binary categorical variable: group1
df_bat$gender = as.factor(df_bat$gender)
df_bat$home_advantage = as.factor(df_bat$home_advantage)

df_bat$last_match_result <- trimws(df_bat$last_match_result)
df_bat$last_match_result <- dplyr::na_if(df_bat$last_match_result, "") #na:8
df_bat$last_match_result = as.factor(df_bat$last_match_result)
#levels(df_bat$last_match_result) <- c(0,1)
table(df_bat$last_match_result) 

df_bat$choose_to_bat = as.factor(df_bat$choose_to_bat)
df_bat$forced_to_bat = as.factor(df_bat$forced_to_bat)

# Multicategorical variable: group2
df_bat$team = as.factor(df_bat$team)
df_bat$opponent = as.factor(df_bat$opponent)
df_bat$toss_winner = as.factor(df_bat$toss_winner)
df_bat$winner = as.factor(df_bat$winner)

df_bat$venue = as.factor(df_bat$venue)
df_bat$city = as.factor(df_bat$city)

str(df_bat)
skim(df_bat)  #123 22


##### EDA #####
### Missingness ###
image(is.na(df_bat))
colSums(is.na(df_bat))
gg_miss_var(df_bat,show_pct = TRUE) #avg_win_rate, avg_wickets_out, avg_score
vis_dat(df_bat)
vis_miss(df_bat)
dev.off()
options(width = 100) 
md.pattern(df_bat)

#Check for missing values:
sum(is.na(df_bat)) 
#32 missing values- find where missing values are:
which(is.na(df_bat))
#Check by columns:
colMissingData <- sapply(df_bat, anyNA)


#Dealing with missing values：8/123 almost 6.5% of bat data
df_bat <- na.omit(df_bat) 
#2021 the first four matches no history performance variables
dim(df_bat) #115 20
str(df_bat)
table(is.na(df_bat))

### EDA: Variables Visualisation ###
color_palette <- colorRampPalette(c("lightblue", "darkblue"))

### Target Variable ###
result_bp = barplot(table(df_bat$Result), col = color_palette(2) , ylim = c(0,100),
                    main = "Number of Observations in 'Result' Factor Levels", cex.main = 1.5,
                    names.arg = c( "lose", "win"),
                    xlab = "Result",ylab = "Observation Count", cex.lab = 1.5, cex.names = 2.0)
text(result_bp, c(table(df_bat$Result)), table(df_bat$Result), cex=1.5, pos=3)

result_freq <- table(df_bat$Result)
lbls <- paste(names(result_freq), "\n", round(result_freq/sum(result_freq)*100, 1), "%")
pie(result_freq, main = "Pie Chart of Result", col =color_palette(2), labels = lbls)
#55% lose vs 45% win 
#balance

### Categorical Variables ###
###Group1: binary categorical variables

# last match result
last_result_freq <- table(df_bat$last_match_result)
lbls <- paste(names(last_result_freq), "\n", round(last_result_freq/sum(last_result_freq)*100, 1), "%")
pie(last_result_freq, main = "Pie Chart of Last Match Result", col =color_palette(2), labels = lbls)
#balance

# Gender
gender_freq <- table(df_bat$gender)
lbls <- paste(names(gender_freq), "\n", round(gender_freq/sum(gender_freq)*100, 1), "%")
pie(gender_freq, main = "Pie Chart of Gender", col = color_palette(2), labels = lbls)
#balance

# Season
season_freq <- table(df_bat$season)
lbls <- paste(names(season_freq), "\n", round(season_freq/sum(season_freq)*100, 1), "%")
pie(season_freq, main = "Pie Chart of Season", col = color_palette(2), labels = lbls)
#balance

# Home Advantage
home_advantage_freq <- table(df_bat$home_advantage)
lbls <- paste(names(home_advantage_freq), "\n", round(home_advantage_freq/sum(home_advantage_freq)*100, 1), "%")
pie(home_advantage_freq, main = "Pie Chart of Home Advantage", col = color_palette(2), labels = lbls)
#no 61% yes 39% 
#not balance, describes the data structure

# Choose to bat
choose_to_bat_freq <- table(df_bat$choose_to_bat)
lbls <- paste(names(choose_to_bat_freq), "\n", round(choose_to_bat_freq/sum(choose_to_bat_freq)*100, 1), "%")
pie(choose_to_bat_freq, main = "Pie Chart of choose_to_bat", col = color_palette(2), labels = lbls)
#no 72% yes 28%
#not balance, describes the data structure

#Forced to bat
forced_to_bat_freq <- table(df_bat$forced_to_bat)
lbls <- paste(names(forced_to_bat_freq), "\n", round(forced_to_bat_freq/sum(forced_to_bat_freq)*100, 1), "%")
pie(forced_to_bat_freq, main = "Pie Chart of forced_to_bat", col = color_palette(2), labels = lbls)
#yes 72%, no 28%
#not balance, describes the data structure


### Does choose_to_bat & forced_to_bat completely opposite?
df_bat$choose_to_bat_numeric <- ifelse(df_bat$choose_to_bat == "yes", 1, 0)
df_bat$forced_to_bat_numeric <- ifelse(df_bat$forced_to_bat == "yes", 1, 0)

correlation_test <- cor.test(df_bat$choose_to_bat_numeric, df_bat$forced_to_bat_numeric)
print(correlation_test) #-1
#Two variables are completely opposite
#only one needs to be kept to avoid multicollinearity

#drop meaningless variables
df_bat <- df_bat[, -which(names(df_bat) %in% c("choose_to_bat_numeric", "forced_to_bat_numeric", "forced_to_bat"))]

binary_vars <- c("last_match_result", "season", "gender", "home_advantage", "choose_to_bat")
plots <- list()
for(var in binary_vars){
  p <- ggplot(df_bat, aes_string(var)) + 
    geom_bar(fill=color_palette(2)) +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal()
  plots[[var]] <- p
}
do.call("grid.arrange", c(plots, ncol = 2))

###imbalance variables with respect to target
doubledecker(Result ~ home_advantage, data=df_bat, gp =gpar(fill=color_palette(2)))
doubledecker(Result ~ choose_to_bat, data=df_bat, gp =gpar(fill=color_palette(2)))

###last_match_result& home_advantage:p<0.05
doubledecker(last_match_result ~ home_advantage, data=df_bat,gp =gpar(fill=color_palette(2)))
doubledecker(Result ~ home_advantage+last_match_result, data=df_bat, gp =gpar(fill=color_palette(2)))
#last result no + no home advantage
#more likely to lose in this match


###Group2: Multi-categorical Variables
#Team, Opponent, Winner, Toss winner
team_colors <- brewer.pal(8, "Set2")
plot_team<-ggplot(data=df_bat, aes(x=team, fill=team)) + 
  geom_bar() + 
  scale_fill_manual(values=team_colors) +
  labs(title="Barplot of Team") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  coord_cartesian(ylim=c(0, 25))
plot_opponent<-ggplot(data=df_bat, aes(x=opponent, fill=opponent)) + 
  geom_bar() + 
  scale_fill_manual(values=team_colors) +
  labs(title="Barplot of Opponent") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  coord_cartesian(ylim=c(0, 25))
plot_winner<-ggplot(data=df_bat, aes(x=winner, fill=winner)) + 
  geom_bar() + 
  scale_fill_manual(values=team_colors) +
  labs(title="Barplot of Winner") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  coord_cartesian(ylim=c(0, 25))
plot_toss_winner<-ggplot(data=df_bat, aes(x=toss_winner, fill=toss_winner)) + 
  geom_bar() + 
  scale_fill_manual(values=team_colors) +
  labs(title="Barplot of Toss Winner") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  coord_cartesian(ylim=c(0, 25))

grobs <- list(ggplotGrob(plot_team), ggplotGrob(plot_opponent),
              ggplotGrob(plot_winner),ggplotGrob(plot_toss_winner))
grid.arrange(grobs = grobs, ncol = 2)

multi_vars <- c("winner", "toss_winner", "team", "opponent")
for(var in multi_vars){
  p <- ggplot(df_bat, aes_string(var)) + 
    geom_bar(fill=team_colors) +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal() +
    coord_flip()  
  print(p)
}

###'team''opponent''winner''toss_winner' respect to target
doubledecker(Result ~ team, data=df_bat, gp =gpar(fill=color_palette(2)))
doubledecker(Result ~ opponent, data=df_bat, gp =gpar(fill=color_palette(2)))
doubledecker(Result ~ toss_winner, data=df_bat, gp =gpar(fill=color_palette(2)))
doubledecker(Result ~ winner, data=df_bat, gp =gpar(fill=color_palette(2)))


##### Is Toss Winner also the Winner? #####
df_bat$toss_equals_winner <- ifelse(df_bat$toss_winner == df_bat$winner, "Yes", "No")
toss_winner_table <- table(df_bat$toss_equals_winner)

pie(toss_winner_table, col = c("lightblue", "darkblue"), 
    main = "Is Toss Winner also the Match Winner?",
    labels = paste(names(toss_winner_table), "\n", toss_winner_table, " matches"))

df_bat$toss_equals_winner <- NULL

### gender & toss winner & winner ###
df_men <- subset(df_bat, gender == "male")
df_women <- subset(df_bat, gender == "female")

df_men$toss_equals_winner <- ifelse(df_men$toss_winner == df_men$winner, "Yes", "No")
df_women$toss_equals_winner <- ifelse(df_women$toss_winner == df_women$winner, "Yes", "No")

toss_winner_table_men <- table(df_men$toss_equals_winner)
toss_winner_table_women <- table(df_women$toss_equals_winner)

prop_men <- prop.table(toss_winner_table_men)
prop_women <- prop.table(toss_winner_table_women)

ci_men <- sqrt(prop_men * (1 - prop_men) / sum(toss_winner_table_men)) * qnorm(0.975)
ci_women <- sqrt(prop_women * (1 - prop_women) / sum(toss_winner_table_women)) * qnorm(0.975)

lower_limit_men <- prop_men - ci_men
upper_limit_men <- prop_men + ci_men

lower_limit_women <- prop_women - ci_women
upper_limit_women <- prop_women + ci_women

bar_mid <- barplot(cbind(prop_men, prop_women), beside = TRUE, ylim = c(0, 1),
                   main = "Proportion of Matches where Toss Winner is also Match Winner",
                   col = c("lightblue", "lightpink"), names.arg = c("Men", "Women"),
                   ylab = "Proportion", xlab = "Gender", legend.text = c("Yes", "No"))

abline(h = lower_limit_men[1], col = "lightblue", lty = 2)  
abline(h = upper_limit_men[1], col = "lightblue", lty = 2)  
abline(h = lower_limit_women[1], col = "lightpink", lty = 2)  
abline(h = upper_limit_women[1], col = "lightpink", lty = 2)  

df_men$toss_equals_winner <- NULL
df_women$toss_equals_winner <- NULL


### home_advantage & toss outcome & gender ###
men_home_won_toss <- subset(df_bat, gender == "male" & home_advantage == 'yes' & team == toss_winner)
men_home_lost_toss <- subset(df_bat, gender == "male" & home_advantage == 'yes' & team != toss_winner)
men_away_won_toss <- subset(df_bat, gender == "male" & home_advantage == 'no' & team == toss_winner)
men_away_lost_toss <- subset(df_bat, gender == "male" & home_advantage == 'no' & team != toss_winner)
women_home_won_toss <- subset(df_bat, gender == "female" & home_advantage == 'yes' & team == toss_winner)
women_home_lost_toss <- subset(df_bat, gender == "female" & home_advantage == 'yes' & team != toss_winner)
women_away_won_toss <- subset(df_bat, gender == "female" & home_advantage == 'no' & team == toss_winner)
women_away_lost_toss <- subset(df_bat, gender == "female" & home_advantage == 'no' & team != toss_winner)

men_home_won_toss_win_percent <- mean(men_home_won_toss$Result == "win")
men_home_lost_toss_win_percent <- mean(men_home_lost_toss$Result == "win")
men_away_won_toss_win_percent <- mean(men_away_won_toss$Result == "win")
men_away_lost_toss_win_percent <- mean(men_away_lost_toss$Result == "win")

women_home_won_toss_win_percent <- mean(women_home_won_toss$Result == "win")
women_home_lost_toss_win_percent <- mean(women_home_lost_toss$Result == "win")
women_away_won_toss_win_percent <- mean(women_away_won_toss$Result == "win")
women_away_lost_toss_win_percent <- mean(women_away_lost_toss$Result == "win")


colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00")
bar_mid <- barplot(c(men_home_won_toss_win_percent, men_home_lost_toss_win_percent, 
                     men_away_won_toss_win_percent, men_away_lost_toss_win_percent, 
                     women_home_won_toss_win_percent, women_home_lost_toss_win_percent, 
                     women_away_won_toss_win_percent, women_away_lost_toss_win_percent), 
                   beside=TRUE, ylim = c(0, 1),
                   main = "Winning Percentage by Toss Outcome and Home Advantage",
                   col = colors, 
                   names.arg = c("Men Home Won", "Men Home Lost", "Men Away Won", "Men Away Lost", "Women Home Won", "Women Home Lost", "Women Away Won", "Women Away Lost"), 
                   ylab="Winning Percentage", xlab="Groups")
abline(h=0.5, lty=2, lwd=2)
legend("topright", legend=c("Home Won Toss", "Home Lost Toss", 
                            "Away Won Toss", "Away Lost Toss"), 
       fill=colors)
dev.off()





##### Is Toss Winner also the Opponent? #####
df_bat$toss_equals_opponent <- ifelse(df_bat$toss_winner == df_bat$opponent, "Yes", "No")
toss_opponent_table <- table(df_bat$toss_equals_opponent)

pie(toss_opponent_table, col = c("lightblue", "darkblue"), 
    main = "Is Toss Winner also the Opponent?",
    labels = paste(names(toss_opponent_table), "\n", toss_opponent_table, " matches"))

df_bat$toss_equals_opponent <- NULL
#opponent& toss winner: highly correlated


##### How to win Finals??  #####
participation_count <- df_bat %>%
  select(team, opponent) %>%
  unlist() %>%
  table() %>%
  as.data.frame() %>%
  rename_with(.cols = everything(), .fn = ~ c("team_name", "participation_count"))
win_count <- df_bat %>%
  count(winner) %>%
  rename(team_name = winner, win_count = n)
team_stats <- left_join(participation_count, win_count, by = "team_name") %>%
  replace_na(list(win_count = 0)) %>%
  mutate(loss_count = participation_count - win_count) %>%
  pivot_longer(cols = c(win_count, loss_count), names_to = "result", values_to = "count")

ggplot(team_stats, aes(x = team_name, y = count, fill = result)) +
  geom_bar(stat = "identity") +
  labs(title = "Participation and Wins by Team",
       x = "Team",
       y = "Count",
       fill = "Result") +
  scale_fill_manual(values = c("lightblue", "darkblue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#City, Venue
city_colors <- brewer.pal(7, "Set2")
names(city_colors) <- unique(df_bat$city) 

df_bat$venue_city <- paste(df_bat$venue, df_bat$city)
df_bat$venue_city = as.factor(df_bat$venue_city)
table(df_bat$venue_city)
plot_venue <- ggplot(data = df_bat, aes(x = venue_city, fill = city)) + 
  geom_bar() + 
  scale_fill_manual(values = city_colors) +
  labs(title = "Barplot of Venue/City") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  coord_cartesian(ylim = c(0, 20))

table(df_bat$city)
plot_city <- ggplot(data = df_bat, aes(x = city, fill = city)) + 
  geom_bar() + 
  scale_fill_manual(values = city_colors) +
  labs(title = "Barplot of City") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  coord_cartesian(ylim = c(0, 35))

grobs <- list(ggplotGrob(plot_venue), ggplotGrob(plot_city))
grid.arrange(grobs = grobs, ncol = 2)

###compare with city and venue
cross_table <- table(df_bat$city, df_bat$venue)
print(cross_table)
cross_df <- as.data.frame.table(cross_table)
ggplot(cross_df, aes(x=Var1, y=Var2, fill=Freq)) + 
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "City", y = "Venue", fill = "Frequency", 
       title = "Cross Table of 'City' and 'Venue'") 
#These two variables are highly correlated

##### strategy: toss winner& choose to bat #####
df_bat$team_won_toss_and_batting <- ifelse((df_bat$toss_winner == df_bat$team & df_bat$choose_to_bat == 'yes') | 
                                             (df_bat$toss_winner == df_bat$opponent & df_bat$choose_to_bat == 'no'), 
                                           'yes', 'no')
df_bat$team_won_toss_and_batting
#The toss winner always chooses to bat. 
#If the toss winner is the TEAM, then they choose to bat first, 'choose_to_bat'=yes
#and if the toss winner is the OPPONENT, they choose to bat later. 'choose_to_bat'=no
#This is a common strategy because the batting team has a greater advantage.


###Because a new variable has been added that includes city and venue information, 
#keep venue_city and drop 'city' and 'venue'
df_bat <- df_bat[, -(which(names(df_bat) %in% c("venue", 
                                            "city", 
                                            "team_won_toss_and_batting",
                                            'toss_and_match_winner')))]


### Numerical Variables ###
###Group3: numeric varaibles
# Numerical Variable subset
numeric_columns = unlist(lapply(df_bat, is.numeric))
numeric_variables = df_bat[ ,numeric_columns]
names(numeric_variables)
#"match_number"    "season"          "team_score"      "team_wickets"    "win_by_runs"    
#"win_by_wickets"  "avg_win_rate"    "avg_score"       "avg_wickets_out" "days_from_start"

# corrplot
cor_matrix = cor(numeric_variables, method = "pearson")
corrplot(cor_matrix, method="circle", type = "upper")
corrplot(cor_matrix, method="circle", type = "upper", addCoef.col = "black")
##### days from start & match number: 0.95 #####

color_mapping = ifelse(df_bat$Result == "win", "darkblue", "lightblue")
pairs(numeric_variables, main = "Scatter Plot Matrix for Numeric Variables",
      method = "pearson", 
      hist.col = "cornflowerblue",
      density = T, col = color_mapping,
      ellipses = F,
      pch = 16)

### days from start & match number: 0.95  ###
xyplot(days_from_start ~ match_number | Result, data = df_bat,pch=16,
       xlab = "match number",
       ylab = "days from start in every season",
       main = "Relationship between days from start and match number by Result")

### Frequency of Halftime Scores ###
ggplot(df_bat, aes(x = team_score)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "cornflowerblue") +
  geom_density(alpha = 0.5, fill = "blue") +
  labs(x = "Team Score",
       y = "Frequency",
       title = "Frequency of Team Scores") +
  theme_minimal()

### Frequency of Halftime Wickets ###
ggplot(df_bat, aes(x = team_wickets)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "cornflowerblue") +
  geom_density(color = "red") +
  labs(title = "Distribution of Team Wickets",
       x = "Team Wickets",
       y = "Density") +
  xlim(c(2, 10)) +
  theme_minimal()

### Frequency of Team Score for Female ###
df_female <- subset(df_bat, gender == "female")
df_win <- subset(df_female, Result == "win")
df_lose <- subset(df_female, Result == "lose")
min_win_score <- min(df_win$team_score)
guarantee_win_score <- max(df_lose$team_score) 

hist_range <- range(c(df_win$team_score, df_lose$team_score))
hist(df_win$team_score, col=rgb(1,0,0,0.5), xlim=hist_range, 
     main="Frequency of Team Scores for Female", xlab="Team Score", ylab="Frequency")
hist(df_lose$team_score, col=rgb(0,0,1,0.5), add=TRUE)
legend("topright", legend=c("Win", "Lose"), fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
abline(v=min_win_score, lty=2, col="black", lwd=2)
abline(v=guarantee_win_score, lty=2, col="black", lwd=2)
mtext(side = 1, text = "min_win_score", at = min_win_score, line = 1, col = "red")
mtext(side = 1, text = "guarantee_win_score", at = guarantee_win_score, line = 1, col = "red")


### Frequency of Team Score for Male ###
df_male <- subset(df_bat, gender == "male")
df_win <- subset(df_male, Result == "win")
df_lose <- subset(df_male, Result == "lose")
min_win_score <- min(df_win$team_score)
guarantee_win_score <- max(df_lose$team_score) 

hist_range <- range(c(df_win$team_score, df_lose$team_score))
hist(df_win$team_score, col=rgb(1,0,0,0.5), xlim=hist_range, 
     main="Frequency of Team Scores for Male", xlab="Team Score", ylab="Frequency")
hist(df_lose$team_score, col=rgb(0,0,1,0.5), add=TRUE)
legend("topright", legend=c("Win", "Lose"), fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
abline(v=min_win_score, lty=2, col="black", lwd=2)
abline(v=guarantee_win_score, lty=2, col="black", lwd=2)
mtext(side = 1, text = "min_win_score", at = min_win_score, line = 1, col = "red")
mtext(side = 1, text = "guarantee_win_score", at = guarantee_win_score, line = 1, col = "red")



##### Look-ahead Bias ####
#The information contained in the data is a direct result of winning a cricket match.

#But here the halftime model is trained based on past historical information 
#(which has already happened), 
#so there is no need to consider look-ahead bias

order <- c('Result', "match_number", 'team','opponent', 
               'gender','season',
               "team_score","team_wickets",
               'winner','toss_winner', 'home_advantage',
               'venue_city', 'choose_to_bat',
               "avg_score","avg_win_rate", "avg_wickets_out", "last_match_result")

df_bat <- df_bat[, order]
str(df_bat)
names(df_bat)
write.csv(df_bat,"halftime_dataframe_R.csv", row.names = F)


##### Encoding #####
levels(df_bat$Result) <- c(0, 1)
typeof(df_bat$Result)
table(df_bat$Result) 

levels(df_bat$last_match_result) <- c(0, 1)
levels(df_bat$gender) <- c(0, 1)
levels(df_bat$home_advantage) <- c(0, 1)
levels(df_bat$choose_to_bat) <- c(0, 1)

### label encoding ###
levels(df_bat$team)
levels(df_bat$venue_city)

teams <- sort(unique(c(df_bat$team, df_bat$opponent, df_bat$winner, df_bat$toss_winner)))

df_bat$team_code <- match(df_bat$team, teams)
df_bat$opponent_code <- match(df_bat$opponent, teams)
df_bat$winner_code <- match(df_bat$winner, teams)
df_bat$toss_winner_code <- match(df_bat$toss_winner, teams)

team_to_city <- c(
  "Birmingham Phoenix" = "Edgbaston, Birmingham Birmingham",
  "London Spirit" = "Lord's, London London", 
  "Manchester Originals" = "Old Trafford, Manchester Manchester", 
  "Northern Superchargers" = "Headingley, Leeds Leeds", 
  "Oval Invincibles" = "Kennington Oval, London London",
  "Southern Brave" = "The Rose Bowl, Southampton Southampton", 
  "Trent Rockets" = "Trent Bridge, Nottingham Nottingham", 
  "Welsh Fire" = "Sophia Gardens, Cardiff Cardiff"
)

df_bat$venue_city_code <- ifelse(df_bat$venue_city == team_to_city[df_bat$team],
                                 df_bat$team_code, df_bat$opponent_code)


###Drop columns
df_bat <- subset(df_bat, select = -c(team, opponent, winner, toss_winner, venue_city))

# drop 'days_from_start'
new_order <- c('Result',
               "match_number", 'team_code','opponent_code', 
               'gender','season',
               "team_score","team_wickets",
               'winner_code', 'toss_winner_code',
               'home_advantage','venue_city_code','choose_to_bat',
               "avg_score","avg_win_rate", "avg_wickets_out", "last_match_result")

df_bat <- df_bat[, new_order]
str(df_bat)
names(df_bat)

write.csv(df_bat,"halftime_encode_R.csv", row.names = F)

# Preservation level mapping
teams <- sort(unique(c(df_bat$team, df_bat$opponent, df_bat$winner, df_bat$toss_winner)))
team_to_city <- c(
  "Birmingham Phoenix" = "Edgbaston, Birmingham Birmingham",
  "London Spirit" = "Lord's, London London", 
  "Manchester Originals" = "Old Trafford, Manchester Manchester", 
  "Northern Superchargers" = "Headingley, Leeds Leeds", 
  "Oval Invincibles" = "Kennington Oval, London London",
  "Southern Brave" = "The Rose Bowl, Southampton Southampton", 
  "Trent Rockets" = "Trent Bridge, Nottingham Nottingham", 
  "Welsh Fire" = "Sophia Gardens, Cardiff Cardiff"
)

saveRDS(list(teams = teams, team_to_city = team_to_city), "level_mappings.rds")

### End EDA & halftime features ###
#Descriptive statistics 
summary(df_bat)

df_bat$Result <- as.numeric(df_bat$Result) -1
df_bat$gender <- as.numeric(df_bat$gender) -1
df_bat$home_advantage <- as.numeric(df_bat$home_advantage) -1
df_bat$choose_to_bat <- as.numeric(df_bat$choose_to_bat) -1
df_bat$last_match_result <- as.numeric(df_bat$last_match_result) -1


#Break into win and lose classes to examine variance of both classes
Windf_bat <- df_bat[df_bat$Result==1,]
Losedf_bat <- df_bat[df_bat$Result==0,]

#Covariance
(cov(Windf_bat))
(cov(Losedf_bat))

#Export
write.csv(cov(Windf_bat),"cov_Win_R.csv", row.names = F) 
write.csv(cov(Losedf_bat),"cov_Lose_R.csv", row.names = F)

cor_matrix <- cor(df_bat)
print(cor_matrix)

##Correlation and covariance matrices of df_bat
(corbat<-cor(df_bat))
(covbat<-cov(df_bat))

write.csv(corbat,"df_bat_Correlation_R.csv", row.names = F) 
write.csv(covbat,"df_bat_Covariance_R.csv", row.names = F)

##### PCA & Clustering #####
# using PCA removes this redundancy, thus reducing the dimensionality of the data
d2 <- df_bat[, !(names(df_bat) %in% c('Result'))] #16 variables

#Standardised the dataset
apply(d2, 2, sd)
apply(d2, 2, mean) #Needs to be standardised
#They are vastly different, so there is need for scaling.

pr.out<-prcomp(d2,scale=TRUE)
summary(pr.out)

pr.out$center #Means and standard deviations of variables before standardisation.
pr.out$scale #Means and standard deviations of variables after standardisation.

#eigenvalues
pr.out$sdev^2
#PC loadings  vector
pr.out$rotation
#PC score
pr.out$x

#The Standard deviation of the principal components is the square root of the corresponding eigenvalues.
sd_all <- summary(pr.out)$importance[1,] #Standard deviation
sd_all^2/sum(sd_all^2) #pr.out$sdev

#scree plot
fviz_screeplot(pr.out, addlabels = TRUE) 
#The rule of thumb recommends retaining the component that explains 80-90% of the variability in the original data set

pr.out$sdev #Standard deviation of each principal component
pr.var=pr.out$sdev^2
pr.var  #Variance explained by each principal component
pve=pr.var/sum(pr.var)
pve 

par(mfrow=c(1,2))
plot(pve,xlab='Principal Component',ylab='Proportion of Variance Explained',type='b',
     main='Proportion of variance explained')
plot(cumsum(pve),xlab='Principal Component',
     ylab='Cumulative Proportion of Variance Explained',type='b',
     main='Cummulative proportion of variance explained')
dev.off()
plot(summary(pr.out)$importance[2,], 
     type="b", xlab="PCs", ylab="Variability explained")
#The summary shows that 8 components are enough to keep 80.53% of the variability in data. 
#Also choosing 8 principal components seems reasonable based on the so-called elbow method.

#biplot
fviz_pca_biplot(pr.out,addlabels = TRUE)

scores <- pr.out$x[, 1:8]
loadings <- pr.out$rotation[, 1:8]

#Correlation between variables and PCs
var <- get_pca_var(pr.out)
var$cor  #Correlation between variables and PC
cor(d2,pr.out$x[,1:8]) #Variable Correlation Chart
fviz_pca_var(pr.out) 
fviz_pca_var(pr.out, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "blue", "red",
                               "green", "purple", "orange", "pink"),
             repel = TRUE)

#Cos2 between variables and PCs
var$cos2 #quality of representation (var$cor)^2
cos2 <- (cor(d2, pr.out$x[,1:8]))^2
cos2
fviz_cos2(pr.out,choice='var',axes=1) 
fviz_cos2(pr.out,choice='var',axes=2)


##Contribution between variables and PCs
var$contrib 
#any variable whose height is up to 6.25 (100/16) and 
#above is considered to have contributed significantly to the component.

fviz_contrib(pr.out, choice = "var", axes = 1, top = 10) #Variable contribution to PC1
#opponent_code, toss_winner_code, venue_city_code, winner_code
fviz_contrib(pr.out, choice = "var", axes = 2, top = 10) #Variable contribution to PC2
#gender, team_score, team_code, avg_score, avg_wickets_out, avg_win_rate


### Using the first two principal components to visualise the effect of dimensionality reduction and true labelling #####
pca_scores <- as.data.frame(pr.out$x[, 1:2])
pca_scores$Result <- df_bat$Result
ggplot(pca_scores, aes(x = PC1, y = PC2, color = as.factor(Result))) +
  geom_point() +
  labs(x = "Principal Component 1", y = "Principal Component 2", color = "Result") +
  theme_minimal() +
  ggtitle("PCA Plot with True Labels")

pca_scores <- as.data.frame(pr.out$x)
pca_scores$Result <- as.factor(df_bat$Result)
fviz_pca_biplot(pr.out, 
                label = "var", 
                habillage = pca_scores$Result, 
                palette = c("#00AFBB", "#FC4E07"), 
                addEllipses = TRUE) 


##### Cluster analysis  #####

#1.k-means clustering
df.scaled <- scale(d2)

#Choose the best k in the k-means algorithm, using the sum of the sums of squares within the clusters
fviz_nbclust(df.scaled, kmeans, method = "wss")+
  geom_vline( xintercept  =  5 , linetype  =  2 )
#total within-cluster sum of square (wss): 9
fviz_nbclust(df.scaled,kmeans, method = "silhouette")+
  labs(title = "K-means")

# but we have true label: Result
# Compute k-means with k = 2
set.seed(123)
k2 <- kmeans(df.scaled, 2, nstart = 25)
k2
names(k2)
#Try 25 different random initial cluster assignments and choose the best result corresponding to the one with the least variation within the clusters
#between_SS / total_SS = 12.4%，Percentage of variance explained by the mean of the clusters
k2$centers #2 centroids 
k2$iter #1 iteration only
k2$size #62 53
k2$withinss #865.6874 732.1852
k2$tot.withinss #1597.873
k2$totss #1824
k2$betweenss #226.1275
#Percentage of variance explained by the cluster means = (between_ss/total_ss)*100
# k2$betweenss=k2$totss -k2$tot.withinss
# 12.4 %= (k2$totss-k2$tot.withinss)/k2$totss= k2$betweenss/k2$totss

cols=c('red','darkgreen')
plot(df.scaled,col=cols[k2$cluster],main='K-means clustering with 2 clusters',xlab='',ylab='')
points(k2$centers,pch=19,cex=2,col=cols) #k=2 is not good enough, there is overlap

#Find the mean of 16 variables based on the newly created cluster = k2$centers #2 centroids under 16 variables (16 dimensions)
aggregate(df.scaled, by=list(cluster=k2$cluster), mean)

# Visualise kmeans clustering
fviz_cluster(k2, df.scaled, ellipse.type = "norm")

# Evaluate the clustering result:k-means
table(k2$cluster, df_bat$Result) 
#53.91%


#2.Hierarchical clustering
## Find the best number of clusters using the elbow method
fviz_nbclust(df.scaled, FUNcluster = function(x, k) { list(cluster = cutree(hclust(dist(x)), k = k)) }, 
             method = "silhouette") +labs(title = "Hierarchical")
#fviz_nbclust(df.scaled, FUNcluster = hcut, method = "silhouette")+ labs(title = "Hierarchical")
#k_optimal=2

# Compute distances and hierarchical clustering
dd <- dist(df.scaled, method = "euclidean")
hc <- hclust(dd, method = "complete")
hc
hcut<-cutree(hc,k=2)
hcut
table(hcut) #cluster1:108,cluster2:7
rownames(df.scaled)[hcut == 1]

#We can visualise the object by using a dendrogram. 
#This will enable us to determine the point to cut the tree, 
#resulting in the number of clusters.
dendros <- as.dendrogram(hc)
plot(dendros, main = "Combination data - Complete linkage",
     ylab = "Height")
abline(h=0.5, lty = 2, col="red")
abline(h=1, lty = 2, col="blue")
Hs <- hc$height[(length(hc$height)-4):length(hc$height)]
abline(h=Hs, col=3, lty=2)

fviz_cluster(list(data=df.scaled, cluster=cutree(hc, 2)), ellipse.type = "norm")
fviz_dend(hc, k = 2, # Cut in two groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB"),
          color_labels_by_k = TRUE, # color labels by groups
          ggtheme = theme_gray() # Change theme
)

hcut <- cutree(hc, k = 2)
table(hcut) #Check the number of observations in each of the two clusters
aggregate(d2, by=list(cluster=hcut), mean) 
#Can be used to calculate the mean of each variable by clusters using raw data

hc_silhouette_score <- mean(silhouette(hcut, dist(df.scaled))[, "sil_width"])
hc_silhouette_score #0.1035079

m <- c("average","single","complete")
names(m) <- c("average","single","complete")
# function to compute coefficient
ac <- function(x){
  agnes(df.scaled, method = x)$ac
}
map_dbl(m,ac) #complete: 0.6652113 

library(igraph)
fviz_dend(hc, k = 2, k_colors = "jco", type = "phylogenic", repel = TRUE)

# Evaluate the clustering result
table(hcut, df_bat$Result) #59.13%

#The two types of clustering done after normalising the continuous variables of the dataset 
#by dimensionality reduction and merging them with the categorical variables 
#did not work well and did not yield much information.






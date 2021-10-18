View(IPL_Matches_2008_2020)
view(IPL_Ball_by_Ball_2008_2020)
#summary
summary(IPL_Matches_2008_2020)
summary(IPL_Ball_by_Ball_2008_2020)
#column names
colnames(IPL_Matches_2008_2020)
names(IPL_Ball_by_Ball_2008_2020)
#loading libraries
library(tidyverse) ## wrangle the data
library(lubridate) ## wrangle the data attributes 
install.packages("ggplot2")
library(ggplot2) ## data visualization
library(tidyr) ## tidy data 
library(dplyr)
library(forecast)
#changing to a variable
delivaries<-IPL_Ball_by_Ball_2008_2020
matches<-IPL_Matches_2008_2020
View(delivaries)
View(matches)
#head of both delivaries and the matches
head(delivaries)
head(matches)
#knowing the class of the datasets
class(matches)
class(delivaries)
#structure of the datasets
str(matches)
#counting no. of matches
count(matches)
#Which team has won most number of matches?
names(matches)
#creating a another data frame with different combination
matches%>%group_by(winner)%>%summarize(wins=n(),.groups = 'drop')%>%
#plotting a graph using ggplot library
ggplot(aes(x=wins, y=winner, fill=winner)) + geom_col(position="dodge") + 
  labs(x="Number of Wins", y="Team", title = "Number  of Matches by Team")

#creating a another data frame with toss winner and by city combination
matches%>%group_by(city)%>%summarize(t_wins=n(),.groups = 'drop')%>%
  ggplot(aes(x=t_wins,y=city,fill=t_wins))+geom_col(position = "dodge")+
  labs(x="Number of toss Wins", y="city", title = "Number  of toss winners by city")
#Who has got number of man of the match awards
matches %>% 
  group_by(player_of_match) %>%
  summarize(awards = n(),.groups = 'drop')%>%
  ggplot(aes(x=awards,y=player_of_match,fill=awards))+geom_col(position = "dodge")+
  labs(x="Number of awards", y="Players", title = "Number  of awards won by players")
install.packages("plotly")
library(plotly)
ggp <- matches %>% 
  group_by(player_of_match) %>%
  summarize(awards = n(),.groups = 'drop')%>%ggplot(aes(x=awards,y=player_of_match,fill=awards)) + 
  geom_bar(stat="identity",fill="Red") +
  coord_flip() +
  ylab("Players") + xlab("Number of awards")


"check in the viewer(using ggplotly library)
consolidated bar plot we can use the zoomin as well as pan option with this
ggplotly library"
ggp %>% ggplotly

#Top 10 players got the Man of the match awards
matches %>% 
  group_by(player_of_match) %>%
  summarize(awards = n()) %>%
  top_n(10)
#Plotting the Top 10 players man of the match
visual1<-matches %>% 
  group_by(player_of_match) %>%
  summarize(awards = n()) %>% 
  top_n(10) %>%
  ggplot(aes(x = player_of_match, y=awards, fill= player_of_match)) + geom_col(position="dodge") +
  labs(x="Player_of_match", y = "Awards" , title = "Top 10 Player Man of the Match")

#visual viewer with all dimensions
visual1%>%ggplotly


matches$day <- format(as.Date(matches$date), "%d")
matches$month <- format(as.Date(matches$date), "%m")
matches$year <- format(as.Date(matches$date), "%Y")
season_count <- length(unique(matches$year))
season_count
#runs count by the  winners
runs <- matches %>% filter(result == "runs") %>%
  select('winner', 'result')
runs
count(runs)
#wickets count by the winners
wickets <- matches %>% filter(result == "wickets") %>%
  select('winner', 'result')

wickets
count(wickets)

#Matches won by runs : 364 matches
#Matches won by wickets : 435 matches

#Which Season has most number of matches

season<-matches%>%group_by(year)%>%summarize(most_no.of_matches=n())%>%ggplot(aes(x = year, y=most_no.of_matches, fill= most_no.of_matches)) + geom_col(position="dodge") +
  labs(x="Season",y="Number of Matches", title ="Season wise number of matches")
season
"2013 has most no.of matches and 2009 has less no.of matches"

#match wins by top 5 cities
matches %>% 
  filter(result != 'No result') %>%
  group_by(winner,city) %>%
  summarize(wins = n(),.groups='drop') %>%
  arrange(city)

#moving average 
ts<-ts(delivaries$batsman_runs,end=c(2022,2),frequency=4)
ts
plot(decompose(ts))
library(forecast)
plot(ts)
summary(ts)
opar<-par(no.readonly = TRUE)
par(mfrow=c(2,3))
ylim<-c(min(ts),max(ts))
plot(ts, main="raw time series")
plot(ma(ts,3),main="simple moving average(k=3)",ylim=ylim)
plot(ma(ts,7),main="simple moving average(k=7)",ylim=ylim)
plot(ma(ts,15),main="simple moving average(k=7)",ylim=ylim)

k<-log(ts)
k
decompose(k)
g1<-stl(k,s.window = "periodic")
g1
plot(g1)
diff(k)
ndiffs(k)
acf_test<-acf(k,lag.max = 40,plot = TRUE)
acf_test
library(tseries)
adf.test(k)
#the values are stationary
fit_ets<-ets(k,model = "MMM")
fit_ets
plot(forecast(fit_ets,5))
fit_ets_additive<-ets(k,model = "AAA")
fit_ets_additive
plot(forecast(fit_ets_additive,5))
fit_ets_mam<-ets(k,model = "MAM")
fit_ets_mam
fit_ets_ann<-ets(k,model = "ANN")
fit_ets_ann
plot(forecast(fit_ets_ann,5))
#ANN is the best fit model of all the above 
accuracy(fit_ets_ann)

library(readr)
library(tidyverse)

setwd('~/Betting Project')
Recruiting.Data <- read_csv("Lagged Recruiting Data.csv")

Recruiting.Data$Team = gsub('UCF', 'Central Florida', Recruiting.Data$Team, fixed = T)
Recruiting.Data$Team = gsub('Ole Miss', 'Mississippi', Recruiting.Data$Team, fixed = T)
Recruiting.Data$Team = gsub('FIU', 'Florida Intl', Recruiting.Data$Team, fixed = T)
Recruiting.Data$Team = gsub('Miami (OH)', 'Miama Ohio', Recruiting.Data$Team, fixed = T)

Recruiting.Data$Team = gsub('Appalachian State',
                            'Appalachian St',
                            Recruiting.Data$Team,
                            fixed = T)
Recruiting.Data$Team = gsub('Miami', 'Miami Florida', Recruiting.Data$Team, fixed = T)

Recruiting.Data$Team = gsub('Miama Ohio', 'Miami Ohio', Recruiting.Data$Team, fixed = T)


Recruiting.Data$Team = gsub('Brigham Young', 'BYU', Recruiting.Data$Team, fixed = T)

Recruiting.Data$Team = gsub('Mississippi State', 'Mississippi St', Recruiting.Data$Team, fixed = T)

Recruiting.Data$Team = gsub('Lafayette', 'UL Lafayette', Recruiting.Data$Team, fixed = T)
Recruiting.Data$Team = gsub('Mississippi State', 'Mississippi St', Recruiting.Data$Team, fixed = T)
Recruiting.Data$Team = gsub('Louisiana-Monroe', 'UL Monroe', Recruiting.Data$Team, fixed = T)
Recruiting.Data$Team = gsub('Kent State', 'Kent', Recruiting.Data$Team, fixed = T)

Recruiting.Data$Team = gsub('Middle Tennessee State', 'Middle Tenn St', Recruiting.Data$Team, fixed = T)

Recruiting.Data$Team = gsub('USF', 'South Florida', Recruiting.Data$Team, fixed = T)

Recruiting.Data$Team = gsub('Charlotte', 'Charlotte U', Recruiting.Data$Team, fixed = T)

Recruiting.Data$Team = gsub('South Carolina State', 'South Carolina St', Recruiting.Data$Team, fixed = T)

Recruiting.Data$Team = gsub('Gardner-Webb', 'Gardner Webb', Recruiting.Data$Team, fixed = T)


Recruiting.Data$Team = gsub('Southeastern Louisiana', 'SE Louisiana', Recruiting.Data$Team, fixed = T)

Recruiting.Data$Team = gsub('Northwest Missouri State', 'NW Missouri State', Recruiting.Data$Team, fixed = T)
Recruiting.Data$Team = gsub('McNeese State', 'McNeese St', Recruiting.Data$Team, fixed = T)
Recruiting.Data$Team = gsub('Gardner-Webb', 'Gardner Webb', Recruiting.Data$Team, fixed = T)
Recruiting.Data$Team = gsub('Gardner-Webb', 'Gardner Webb', Recruiting.Data$Team, fixed = T)





home.recruiting = Recruiting.Data
colnames(home.recruiting)[which(colnames(home.recruiting) == 'Team')] = 'Home Team'
colnames(home.recruiting)[3:length(colnames(home.recruiting))] =
  paste('Home', 'Recruit', colnames(home.recruiting)[3:length(colnames(home.recruiting))],
        sep = '.')


visitor.recruiting = Recruiting.Data
colnames(visitor.recruiting)[which(colnames(visitor.recruiting) == 'Team')] = 'Visitor Team'
colnames(visitor.recruiting)[3:length(colnames(visitor.recruiting))] =
  paste('Visitor', 'Recruit', colnames(visitor.recruiting)[3:length(colnames(visitor.recruiting))],
        sep = '.')

GameResults <- read_csv("GameResults.csv")
GameResults$Season = as.numeric(word(
  GameResults$Season,
  start = 1,
  end = 1,
  sep  = fixed("-")
))



GameResults = left_join(GameResults, home.recruiting)

GameResults = left_join(GameResults, visitor.recruiting)




GameResults = GameResults[!duplicated(GameResults),]

lagged.stats = read_csv(file = 'Lagged.Averages.csv')


lagged.stats.2 = lagged.stats[,which(colnames(lagged.stats) == 'Season'):ncol(lagged.stats)]

lagged.stats = cbind(lagged.stats$School, lagged.stats$Date,
                       lagged.stats.2)

colnames(lagged.stats)[1:2] = c('School', 'Date')


home.lagged = lagged.stats
colnames(home.lagged)[which(colnames(home.lagged) == 'School')] = 'Home Team'
colnames(home.lagged)[4:length(colnames(home.lagged))] =
  paste('Home', colnames(home.lagged)[4:length(colnames(home.lagged))],
        sep = '.')


visitor.lagged = lagged.stats
colnames(visitor.lagged)[which(colnames(visitor.lagged) == 'School')] = 'Visitor Team'
colnames(visitor.lagged)[4:length(colnames(visitor.lagged))] =
  paste('Visitor', colnames(visitor.lagged)[4:length(colnames(visitor.lagged))],
        sep = '.')

GameResults = left_join(GameResults, home.lagged)

GameResults = left_join(GameResults, visitor.lagged)




GameResults = GameResults[!duplicated(GameResults),]

write.csv(GameResults, row.names = F, 'GameResults-Recruiting-LaggedStats.csv')





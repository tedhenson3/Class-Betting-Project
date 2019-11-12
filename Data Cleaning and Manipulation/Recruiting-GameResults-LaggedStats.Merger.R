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

GameResults[is.na(GameResults)] = 0




GameResults = GameResults[!duplicated(GameResults),]

lagged.stats = read_csv(file = 'Lagged.Averages.csv')


lagged.stats$`School` = gsub('North Carolina State',
                          'NC State',
                          lagged.stats$`School`)

lagged.stats$`Opponent` = gsub('North Carolina State',
                            'NC State',
                            lagged.stats$`Opponent`)


lagged.stats$`School` = gsub('Pitt',
                          'Pittsburgh',
                          lagged.stats$`School`)


lagged.stats$`School` = gsub('Miami (FL)',
                             'Miami Florida',
                             lagged.stats$`School`,
                             fixed = T)


lagged.stats$`Opponent` = gsub('Miami (FL)',
                             'Miami Florida',
                             lagged.stats$`Opponent`,
                             fixed = T)

lagged.stats$`Opponent` = gsub('Pittsburghsburgh',
                            'Pittsburgh',
                            lagged.stats$`Opponent`)



lagged.stats$`School` = gsub('Florida International',
                          'Florida Intl',
                          lagged.stats$`School`)

lagged.stats$`Opponent` = gsub('Florida International',
                            'Florida Intl',
                            lagged.stats$`Opponent`)



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

conferences = read_csv('Conferences.csv')

conferences$Team = gsub('North Carolina State', 'NC State',
                        conferences$Team)


conferences$Team = gsub('UCF', 'Central Florida',
                        conferences$Team)


conferences$Team = gsub('Appalachian State', 'Appalachian St',
                        conferences$Team)

conferences$Team = gsub('Middle Tennessee', 'Middle Tenn St',
                        conferences$Team)


conferences$Team = gsub('Miami (FL)', 'Miami Florida',
                        fixed = T,
                        conferences$Team)



conferences$Team = gsub('Miami (OH)', 'Miami Ohio',
                        fixed = T,
                        conferences$Team)



conferences$Team = gsub('Mississippi State', 'Mississippi St',
                        conferences$Team)


conferences$Team = gsub('Ole Miss', 'Mississippi',
                        conferences$Team)





conferences$Team = gsub('Brigham Young', 'BYU',
                        conferences$Team)
conferences$Team = gsub('Kent State', 'Kent',
                        conferences$Team)
conferences$Team = gsub('Louisiana-Lafayette', 'UL Lafayette',
                        conferences$Team)
conferences$Team = gsub('Louisiana-Monroe', 'UL Monroe',
                        conferences$Team)

conferences$Team = gsub('Florida International', 'Florida Intl',
                        conferences$Team)


conferences$Team = gsub('Charlotte', 'Charlotte U',
                        conferences$Team)




home.conferences = conferences
colnames(home.conferences)[1] = 'Home Team'
colnames(home.conferences)[2:ncol(home.conferences)] = paste('Home',
                                                             colnames(home.conferences)[2:ncol(home.conferences)],
                                                             sep = '.')


visitor.conferences = conferences
colnames(visitor.conferences)[1] = 'Visitor Team'

colnames(visitor.conferences)[2:ncol(visitor.conferences)] = paste('Visitor',
                                                             colnames(visitor.conferences)[2:ncol(visitor.conferences)],
                                                             sep = '.')


GameResults = left_join(GameResults, home.conferences)
GameResults = left_join(GameResults, visitor.conferences)


missing = unique(c(GameResults$`Home Team`[which(is.na(
  GameResults$Home.Conference))]), GameResults$`Visitor Team`[which(is.na(
    GameResults$Visitor.Conference))])

missing = unique(missing)


GameResults$Home.Conference = ifelse(is.na(GameResults$Home.Conference),
                                     'FCS',
                                     GameResults$Home.Conference)


GameResults$Visitor.Conference = ifelse(is.na(GameResults$Visitor.Conference),
                                     'FCS',
                                     GameResults$Visitor.Conference)


GameResults$Visitor.Division = ifelse(GameResults$Visitor.Conference == 'FCS',
                                     'FCS',
                                     GameResults$Visitor.Division)



GameResults$Home.Division = ifelse(GameResults$Home.Conference == 'FCS',
                                      'FCS',
                                      GameResults$Home.Division)

GameResults$is.conf.game = ifelse(GameResults$Home.Conference == GameResults$Visitor.Conference,
                                1, 0)

GameResults$is.conf.div.game = ifelse(GameResults$Home.Conference == 
                                        GameResults$Visitor.Conference & 
                                        GameResults$Home.Division == GameResults$Visitor.Division,
                                  1, 0)




write.csv(GameResults, row.names = F, 'GameResults-Recruiting-LaggedStats.csv')





  
  
  library(readr)
  library(tidyverse)
  
  setwd('~/Betting Project')
  Recruiting.Data <- read_csv("Lagged Recruiting Data.csv")
  
  Recruiting.Data$Team = gsub('UCF', 'Central Florida', Recruiting.Data$Team, fixed = T)
  Recruiting.Data$Team = gsub('Ole Miss', 'Mississippi', Recruiting.Data$Team, fixed = T)
  Recruiting.Data$Team = gsub('Miami (OH)', 'Miama Ohio', Recruiting.Data$Team, fixed = T)
  
  Recruiting.Data$Team = gsub('Appalachian State',
                              'Appalachian St',
                              Recruiting.Data$Team,
                              fixed = T)
  
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
  
  
  Recruiting.Data = Recruiting.Data %>% arrange(Team,
                                                desc(Season))
  
  Recruiting.Data = Recruiting.Data %>% group_by(Team) %>% filter(Season == 2019)
  
  
  
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
  
  Predictions = read_csv('Lagged.Averages(Predictions).csv')
  
  
  
  
  Predictions = left_join(Predictions, home.recruiting)
  
  Predictions = left_join(Predictions, visitor.recruiting)
  
  Predictions[is.na(Predictions)] = 0
  
  
  
  conferences = read_csv('Conferences.csv')
  
  conferences$Team = gsub('North Carolina State', 'NC State',
                          conferences$Team)
  
  
  conferences$Team = gsub('UCF', 'Central Florida',
                          conferences$Team)
  
  
  conferences$Team = gsub('Appalachian State', 'Appalachian St',
                          conferences$Team)
  
  conferences$Team = gsub('Middle Tennessee', 'Middle Tenn St',
                          conferences$Team)
  
  
  conferences$Team = gsub('Miami (FL)', 'Miami',
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
  
  
  
  conferences$Team = gsub('Charlotte', 'Charlotte U',
                          conferences$Team)
  
  conferences$Team = gsub('Florida International', 'FIU',
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
  
  
  Predictions = left_join(Predictions, home.conferences)
  Predictions = left_join(Predictions, visitor.conferences)
  
  
  missing = unique(c(Predictions$`Home Team`[which(is.na(
    Predictions$Home.Conference))]), Predictions$`Visitor Team`[which(is.na(
      Predictions$Visitor.Conference))])
  
  missing = unique(missing)
  
  
  Predictions$Home.Conference = ifelse(is.na(Predictions$Home.Conference),
                                       'FCS',
                                       Predictions$Home.Conference)
  
  
  Predictions$Visitor.Conference = ifelse(is.na(Predictions$Visitor.Conference),
                                       'FCS',
                                       Predictions$Visitor.Conference)
  
  
  Predictions$Visitor.Division = ifelse(Predictions$Visitor.Conference == 'FCS',
                                       'FCS',
                                       Predictions$Visitor.Division)
  
  
  
  Predictions$Home.Division = ifelse(Predictions$Home.Conference == 'FCS',
                                        'FCS',
                                        Predictions$Home.Division)
  
  Predictions$is.conf.game = ifelse(Predictions$Home.Conference == Predictions$Visitor.Conference,
                                  1, 0)
  
  Predictions$is.conf.div.game = ifelse(Predictions$Home.Conference == 
                                          Predictions$Visitor.Conference & 
                                          Predictions$Home.Division == Predictions$Visitor.Division,
                                    1, 0)
  


write.csv(Predictions, row.names = F, 'test.csv')





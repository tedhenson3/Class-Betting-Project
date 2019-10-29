library(readr)
library(tidyverse)
library(pracma)

setwd('~/Betting Project')
GameStats <- read.csv('GameStats.csv')
GameStats = GameStats[,2:ncol(GameStats)]

GameStats$Year  = as.numeric(word(
  GameStats$Date,
  start = 3,
  end = 3,
  sep  = fixed("/")
))


GameStats$Month  = as.numeric(word(
  GameStats$Date,
  start = 1,
  end = 1,
  sep  = fixed("/")
))


GameStats$Day  = as.numeric(word(
  GameStats$Date,
  start = 2,
  end = 2,
  sep  = fixed("/")
))



GameStats  = GameStats %>% arrange(School, Year, Month, Day)




y = rep(0, nrow(GameStats))

for(i in 1:nrow(GameStats)){
  
  g = GameStats$G.[i]
  
  if(g == 1){
    
    
  
  y[i] = GameStats$Year[i]
  

  
  }
  
  else{
    
  y[i] = y[i-1]  
    
  }
  
  
  
  
  
}

GameStats$Season = y


freq.table = GameStats %>% group_by(Opponent,
                                    Season) %>% summarise(freq = n())

GameStats = left_join(GameStats,
                      freq.table) %>% dplyr::filter(freq >= 4) %>% dplyr::select(-freq)



GameStats  = GameStats %>% arrange(Opponent, Year, Month, Day)



library(zoo)
GameStats.def = GameStats %>% dplyr::group_by(Opponent,
                                      Season) %>%
  mutate(ypa.def.roll.avg = lag(rollapplyr((PassYds + RushYds)/(PassAtt + RushAtt), 15, mean, partial = TRUE),
                            n = 1),
         tos.def.roll.avg = lag(rollapplyr(TotalTO, 15, mean, partial = TRUE),
                                n = 1),
         pts.def.roll.avg = lag(rollapplyr(c((RushTD + PassTD)*6  + XPM + 3*FGM), 15, mean, partial = TRUE),
                                n = 1))

GameStats.def = GameStats.def %>% dplyr::select(Opponent,
                                                ypa.def.roll.avg, 
                                                Date,
                                                tos.def.roll.avg,
                                                pts.def.roll.avg,
                                                Season)
                                                

colnames(GameStats.def)[which(colnames(GameStats.def) == 'Opponent')] = 'School'




GameStats  = GameStats %>% arrange(School, Year, Month, Day)



library(zoo)
GameStats.off = GameStats %>% dplyr::group_by(School,
                                              Season) %>%
  mutate(ypa.off.roll.avg = lag(rollapplyr((PassYds + RushYds)/(PassAtt + RushAtt), 15, mean, partial = TRUE),
                                n = 1),
         tos.off.roll.avg = lag(rollapplyr(TotalTO, 15, mean, partial = TRUE),
                                n = 1),
         pts.off.roll.avg = lag(rollapplyr(c((RushTD + PassTD)*6  + XPM + 3*FGM), 15, mean, partial = TRUE),
                                n = 1))

GameStats.off = GameStats.off %>% dplyr::select(School,
                                                ypa.off.roll.avg, 
                                                tos.off.roll.avg,
                                                pts.off.roll.avg,
                                                
                                                Date,
                                                Season)



GameStats = left_join(GameStats,
                      GameStats.off)

GameStats = left_join(GameStats,
                      GameStats.def)

GameStats = GameStats %>% dplyr::select(School,
                                        Opponent,
                                        Date,
                                        Season,
                                        tos.off.roll.avg,
                                        tos.def.roll.avg,
                                        pts.off.roll.avg,
                                        pts.def.roll.avg,
                                        ypa.off.roll.avg,
                                        ypa.def.roll.avg,
                                        G.
                                        )




write.csv(GameStats, row.names = F, 'Lagged.Averages.csv')
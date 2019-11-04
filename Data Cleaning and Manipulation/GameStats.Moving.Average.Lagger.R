library(readr)
library(tidyverse)
library(pracma)

setwd('~/Betting Project')
GameStats <- read.csv('GameStats.csv')
GameStats = GameStats[,2:ncol(GameStats)]


Predictions = read_csv('Predictions.csv')



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

GameStats$X.1 = ifelse(GameStats$X.1  == 'W', 1, 0)


library(zoo)
GameStats.def = GameStats %>% dplyr::group_by(Opponent,
                                      Season) %>%
  mutate(ypa.def.roll.avg = lag(rollapplyr((PassYds + RushYds)/(PassAtt + RushAtt), 15, mean, partial = TRUE),
                            n = 1),
         comp.pct.def.roll.avg = lag(rollapplyr(PassPct, 15, mean, partial = TRUE),
                                     n = 1),
         int.rate.def.roll.avg = lag(rollapplyr(Int / PassAtt, 15, mean, partial = TRUE),
                                     n = 1),
         rush.ypa.def.roll.avg = lag(rollapplyr(RushAvg, 15, mean, partial = TRUE),
                                     n = 1),
         pass.ypa.def.roll.avg = lag(rollapplyr(PassYds / PassAtt, 15, mean, partial = TRUE),
                                     n = 1),
         tos.rate.def.roll.avg = lag(rollapplyr(TotalTO / (PassAtt + RushAtt), 15, mean, partial = TRUE),
                                n = 1),
         rush.tds.def.roll.avg = lag(rollapplyr(RushTD , 15, mean, partial = TRUE),
                                     n = 1),
         pass.tds.def.roll.avg = lag(rollapplyr(PassTD , 15, mean, partial = TRUE),
                                     n = 1),
         tds.rate.def.roll.avg = lag(rollapplyr((PassTD + RushTD) / (PassAtt + RushAtt) , 15, mean, partial = TRUE),
                                     n = 1),
         tos.def.roll.avg = lag(rollapplyr(TotalTO, 15, mean, partial = TRUE),
                                n = 1),
         pts.def.roll.avg = lag(rollapplyr(c((RushTD + PassTD)*6  + XPM + 3*FGM) , 15, mean, partial = TRUE),
                                n = 1),
         pts.rate.def.roll.avg = lag(rollapplyr(c((RushTD + PassTD)*6  + XPM + 3*FGM) / (PassAtt + RushAtt), 15, mean, partial = TRUE),
                                n = 1))

GameStats.def = GameStats.def %>% dplyr::select(Opponent,
                                                ypa.def.roll.avg, 
                                                comp.pct.def.roll.avg,
                                                int.rate.def.roll.avg,
                                                rush.ypa.def.roll.avg, 
                                                pass.ypa.def.roll.avg, 
                                                rush.tds.def.roll.avg,
                                                pass.tds.def.roll.avg,
                                                tds.rate.def.roll.avg,
                                                Date,
                                                tos.rate.def.roll.avg,
                                                tos.def.roll.avg,
                                                pts.rate.def.roll.avg,
                                                pts.def.roll.avg,
                                                Season)
                                                

colnames(GameStats.def)[which(colnames(GameStats.def) == 'Opponent')] = 'School'




GameStats  = GameStats %>% arrange(School, Year, Month, Day)



library(zoo)
GameStats.off = GameStats %>% dplyr::group_by(School,
                                              Season) %>%
  mutate(ypa.off.roll.avg = lag(rollapplyr((PassYds + RushYds)/(PassAtt + RushAtt), 15, mean, partial = TRUE),
                                n = 1),
         comp.pct.off.roll.avg = lag(rollapplyr(PassPct, 15, mean, partial = TRUE),
                                n = 1),
         rush.ypa.off.roll.avg = lag(rollapplyr(RushAvg, 15, mean, partial = TRUE),
                                n = 1),
         int.rate.off.roll.avg = lag(rollapplyr(Int / PassAtt, 15, mean, partial = TRUE),
                                     n = 1),
         pass.ypa.off.roll.avg = lag(rollapplyr(PassYds / PassAtt, 15, mean, partial = TRUE),
                                     n = 1),
         tos.rate.off.roll.avg = lag(rollapplyr(TotalTO / (PassAtt + RushAtt), 15, mean, partial = TRUE),
                                n = 1),
         tos.off.roll.avg = lag(rollapplyr(TotalTO, 15, mean, partial = TRUE),
                                     n = 1),
         rush.tds.off.roll.avg = lag(rollapplyr(RushTD , 15, mean, partial = TRUE),
                                     n = 1),
         pass.tds.off.roll.avg = lag(rollapplyr(PassTD , 15, mean, partial = TRUE),
                                n = 1),
         tds.rate.off.roll.avg = lag(rollapplyr((PassTD + RushTD) / (PassAtt + RushAtt) , 15, mean, partial = TRUE),
                                     n = 1),
         pts.off.roll.avg = lag(rollapplyr(c((RushTD + PassTD)*6  + XPM + 3*FGM) , 15, mean, partial = TRUE),
                                     n = 1),
         win.pct.roll = lag(rollapplyr(X.1 , 15, mean, partial = TRUE),
                            n = 1),
         pts.rate.off.roll.avg = lag(rollapplyr(c((RushTD + PassTD)*6  + XPM + 3*FGM) /(PassAtt + RushAtt) , 15, mean, partial = TRUE),
                                n = 1))

GameStats.off = GameStats.off %>% dplyr::select(School,
                                                win.pct.roll,
                                                ypa.off.roll.avg, 
                                                tds.rate.off.roll.avg,
                                                rush.tds.off.roll.avg,
                                                pass.tds.off.roll.avg,
                                                int.rate.off.roll.avg,
                                                tos.off.roll.avg,
                                                comp.pct.off.roll.avg,
                                                rush.ypa.off.roll.avg, 
                                                pass.ypa.off.roll.avg,
                                                tos.rate.off.roll.avg,
                                                pts.rate.off.roll.avg,
                                                tds.rate.off.roll.avg,
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
                                        G.,
                                        win.pct.roll,
                                      
                                        
                                        #off variables
                                        ypa.off.roll.avg, 
                                        rush.tds.off.roll.avg,
                                        pass.tds.off.roll.avg,
                                        int.rate.off.roll.avg,
                                        tos.off.roll.avg,
                                        comp.pct.off.roll.avg,
                                        rush.ypa.off.roll.avg, 
                                        pass.ypa.off.roll.avg,
                                        tos.rate.off.roll.avg,
                                        pts.rate.off.roll.avg,
                                        pts.off.roll.avg,
                                        tds.rate.off.roll.avg,
                                        
                                        
                                        #def variables
                                        ypa.def.roll.avg, 
                                        comp.pct.def.roll.avg,
                                        int.rate.def.roll.avg,
                                        rush.ypa.def.roll.avg, 
                                        pass.ypa.def.roll.avg, 
                                        rush.tds.def.roll.avg,
                                        pass.tds.def.roll.avg,
                                        tos.rate.def.roll.avg,
                                        tos.def.roll.avg,
                                        pts.rate.def.roll.avg,
                                        pts.def.roll.avg,
                                        tds.rate.def.roll.avg
                                        )




write.csv(GameStats, row.names = F, 'Lagged.Averages.csv')
library(readr)
library(tidyverse)
library(pracma)

setwd('~/Betting Project')
GameStats <- read.csv('GameStats.csv')
GameStats = GameStats[,2:ncol(GameStats)]



GameStats$`School` = gsub('North Carolina State',
                               'NC State',
                               GameStats$`School`)

GameStats$`Opponent` = gsub('North Carolina State',
                                     'NC State',
                                     GameStats$`Opponent`)


GameStats$`School` = gsub('Pitt',
                               'Pittsburgh',
                               GameStats$`School`)

GameStats$`Opponent` = gsub('Pittsburghsburgh',
                                     'Pittsburgh',
                                     GameStats$`Opponent`)


GameStats$`School` = gsub('Miami (FL)',
                               'Miami',
                               GameStats$`School`,
                               fixed = T)

GameStats$`Opponent` = gsub('Miami (FL)',
                                     'Miami',
                                     GameStats$`Opponent`,
                                     fixed = T)


GameStats$`School` = gsub('Florida International',
                               'FIU',
                               GameStats$`School`)

GameStats$`Opponent` = gsub('Florida International',
                                     'FIU',
                                     GameStats$`Opponent`)






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
  
  if(g < 10){
    
    
  
  y[i] = GameStats$Year[i]
  

  
  }
  
  else{
    
  y[i] = y[i-1]  
    
  }
  
  
  
  
  
}

GameStats$Season = y


# freq.table = GameStats %>% group_by(Opponent,
#                                     Season) %>% summarise(freq = n())
# 
# GameStats = left_join(GameStats,
#                       freq.table) %>% dplyr::filter(freq >= 2) %>% dplyr::select(-freq)



GameStats  = GameStats %>% arrange(Opponent, Year, Month, Day)

GameStats$X.1 = ifelse(GameStats$X.1  == 'W', 1, 0)


library(zoo)
GameStats.def = GameStats %>% dplyr::group_by(Opponent,
                                      Season) %>%
  mutate(ypa.def.roll.avg = rollapplyr((PassYds + RushYds)/(PassAtt + RushAtt), 15, mean, partial = TRUE),
         comp.pct.def.roll.avg = rollapplyr(PassPct, 15, mean, partial = TRUE),

         int.rate.def.roll.avg = rollapplyr(Int / PassAtt, 15, mean, partial = TRUE),

         rush.ypa.def.roll.avg = rollapplyr(RushAvg, 15, mean, partial = TRUE),
         pass.ypa.def.roll.avg = rollapplyr(PassYds / PassAtt, 15, mean, partial = TRUE),
                                     
         tos.rate.def.roll.avg = rollapplyr(TotalTO / (PassAtt + RushAtt), 15, mean, partial = TRUE),
         rush.tds.def.roll.avg = rollapplyr(RushTD , 15, mean, partial = TRUE),
         pass.tds.def.roll.avg = rollapplyr(PassTD , 15, mean, partial = TRUE),
         tds.rate.def.roll.avg = rollapplyr((PassTD + RushTD) / (PassAtt + RushAtt) , 15, mean, partial = TRUE),
         tos.def.roll.avg = rollapplyr(TotalTO, 15, mean, partial = TRUE),
         pts.def.roll.avg = rollapplyr(c((RushTD + PassTD)*6  + XPM + 3*FGM) , 15, mean, partial = TRUE),
         pts.rate.def.roll.avg = rollapplyr(c((RushTD + PassTD)*6  + XPM + 3*FGM) / (PassAtt + RushAtt), 15, mean, 
                                            partial = TRUE))

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
                                                G.,
                                                Season)
                                                





GameStats  = GameStats %>% arrange(School, Year, Month, Day)



library(zoo)
GameStats.off = GameStats %>% dplyr::group_by(School,
                                              Season) %>%
  mutate(ypa.off.roll.avg = rollapplyr((PassYds + RushYds)/(PassAtt + RushAtt), 15, mean, partial = TRUE),
         comp.pct.off.roll.avg = rollapplyr(PassPct, 15, mean, partial = TRUE),

         rush.ypa.off.roll.avg = rollapplyr(RushAvg, 15, mean, partial = TRUE),
         int.rate.off.roll.avg = rollapplyr(Int / PassAtt, 15, mean, partial = TRUE),
         pass.ypa.off.roll.avg = rollapplyr(PassYds / PassAtt, 15, mean, partial = TRUE),
         tos.rate.off.roll.avg = rollapplyr(TotalTO / (PassAtt + RushAtt), 15, mean, partial = TRUE),
         tos.off.roll.avg = rollapplyr(TotalTO, 15, mean, partial = TRUE),
         rush.tds.off.roll.avg = rollapplyr(RushTD , 15, mean, partial = TRUE),
         pass.tds.off.roll.avg = rollapplyr(PassTD , 15, mean, partial = TRUE),
         tds.rate.off.roll.avg = rollapplyr((PassTD + RushTD) / (PassAtt + RushAtt) , 15, mean, partial = TRUE),
         pts.off.roll.avg = rollapplyr(c((RushTD + PassTD)*6  + XPM + 3*FGM) , 15, mean, partial = TRUE),
         win.pct.roll = rollapplyr(X.1 , 15, mean, partial = TRUE),
         pts.rate.off.roll.avg = rollapplyr(c((RushTD + PassTD)*6  + XPM + 3*FGM) /
                                              (PassAtt + RushAtt) , 15, 
                                            mean, partial = TRUE))

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
                                                G.,
                                                Season)

GameStats.off = GameStats.off %>% arrange(School, desc(Season), desc(G.))


GameStats.off = GameStats.off %>%  dplyr::group_by(School) %>% filter(row_number() == 1)




GameStats.def = GameStats.def %>% arrange(Opponent, desc(Season), desc(G.))

GameStats.def = GameStats.def %>%  dplyr::group_by(Opponent) %>% filter(row_number() == 1)

colnames(GameStats.def)[which(colnames(GameStats.def) == 'Opponent')] = 'Team'


colnames(GameStats.off)[which(colnames(GameStats.off) == 'School')] = 'Team'


GameStats.def = GameStats.def %>% ungroup() %>% dplyr::select(-Date,
                                                -G.,
                                                -Season)

GameStats.off = GameStats.off %>% ungroup() %>% dplyr::select(-Date,
                                                -G.,
                                                -Season)

GameStats.off.home = GameStats.off
colnames(GameStats.off.home)[1]= 'Home Team'

colnames(GameStats.off.home)[2:ncol(GameStats.off.home)] = 
  paste('Home', colnames(GameStats.off.home)[2:ncol(GameStats.off.home)],
                                     sep = '.')


GameStats.def.home = GameStats.def
colnames(GameStats.def.home)[1]= 'Home Team'

colnames(GameStats.def.home)[2:ncol(GameStats.def.home)] = 
  paste('Home', colnames(GameStats.def.home)[2:ncol(GameStats.def.home)],
        sep = '.')



#visitor

GameStats.off.visitor = GameStats.off
colnames(GameStats.off.visitor)[1]= 'Visitor Team'

colnames(GameStats.off.visitor)[2:ncol(GameStats.off.visitor)] = 
  paste('Visitor', colnames(GameStats.off.visitor)[2:ncol(GameStats.off.visitor)],
        sep = '.')


GameStats.def.visitor = GameStats.def
colnames(GameStats.def.visitor)[1]= 'Visitor Team'

colnames(GameStats.def.visitor)[2:ncol(GameStats.def.visitor)] = 
  paste('Visitor', colnames(GameStats.def.visitor)[2:ncol(GameStats.def.visitor)],
        sep = '.')



predictions = read_csv('Predictions.csv')

predictions = left_join(predictions, GameStats.off.home)

predictions = left_join(predictions, GameStats.off.visitor)

predictions = left_join(predictions, GameStats.def.home)

predictions = left_join(predictions, GameStats.def.visitor)


write.csv(predictions, row.names = F, 'Lagged.Averages(Predictions).csv')
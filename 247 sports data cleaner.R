library(readr)
library(tidyverse)
setwd('~/Betting Project')

all.seasons <- data.frame(matrix(ncol = 11, nrow = 0))

colnames(all.seasons) <- c("Previous.National.Ranking", 
                          "Final.National.Ranking", 
                          "Team", 
                          "Team.2", 
                          "Num.Commits", 
                          "Average.Rating", 
                          "Num.5.Stars", 
                          "Num.4.Stars",
                          "Num.3.Stars", 
                          "Total.Points",
                          'Season')


for(season in 2011:2019){
  
  

file = paste("247 Team Rankings", ' ', season, '.csv', sep = '')

data = read_csv(file)


data = rbind(1, data)
colnames(data) = c('Values')

data = data$Values

indices <- seq(from = 1, to = length(data), by = 10)




clean.data <- data.frame(matrix(ncol = 10, nrow = 0))


for(i in 1:c(length(indices)-2)){
  
  start <- indices[i]
  
  
  end <- indices[i+1] - 1
  
  if(is.na(end)){
    
    end = 10
    start = 1
  }
  
  if(end == 0){
    end = 10
    start = 10
    
  }
  
  
  #print(data)
  row <- data[start:end]
  
  
  clean.data[i,] <- row
  
  
}


colnames(clean.data) <- c("Previous.National.Ranking", 
                          "Final.National.Ranking", 
                          "Team", 
                          "Team.2", 
                          "Num.Commits", 
                          "Average.Rating", 
                          "Num.5.Stars", 
                          "Num.4.Stars",
                          "Num.3.Stars", 
                          "Total.Points")

clean.data$Season = season

#clean.data$Season = i



all.seasons = rbind(clean.data,all.seasons)

all.seasons = as.data.frame(all.seasons)



all.seasons$Final.National.Ranking = as.numeric(all.seasons$Final.National.Ranking)
all.seasons$Previous.National.Ranking = as.numeric(all.seasons$Previous.National.Ranking)
all.seasons$Average.Rating  = as.numeric(all.seasons$Average.Rating)
all.seasons$Num.5.Stars  = as.numeric(all.seasons$Num.5.Stars)
all.seasons$Num.4.Stars  = as.numeric(all.seasons$Num.4.Stars)
all.seasons$Num.3.Stars  = as.numeric(all.seasons$Num.3.Stars)
all.seasons$Total.Points  = as.numeric(all.seasons$Total.Points)


all.seasons = all.seasons %>% dplyr::filter(Total.Points != 0)

all.seasons = all.seasons %>% dplyr::filter(Team != '0')



}
all.seasons$Num.Commits = gsub('Commits', '', all.seasons$Num.Commits)

all.seasons$Num.Commits = as.numeric(all.seasons$Num.Commits)

all.seasons = all.seasons %>% dplyr::select(-Team.2)





write.csv(all.seasons, '247 Team Rankings 2011-2019.csv', row.names = F)









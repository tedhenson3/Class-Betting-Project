library(readr)
library(tidyverse)
setwd('~/Betting Project')

recruiting = read_csv('247 Team Rankings 2011-2019.csv')


recruiting = recruiting %>% dplyr::arrange(Team, Season)


##RS means Redshirt

recruiting = recruiting %>% group_by(Team) %>% mutate(
  Points.Sophomore= lag(Total.Points, n = 1),
  Points.Junior = lag(Total.Points, n = 2),
  Points.Senior = lag(Total.Points, n = 3),
  Points.RS.Senior = lag(Total.Points, 4),
)


colnames(recruiting)[which(colnames(recruiting) == 'Total.Points')] = 'Points.Freshman'


recruiting = recruiting %>% group_by(Team) %>% mutate(
  Mean.Rating.Sophomore = lag(Average.Rating, n = 1),
  Mean.Rating.Junior = lag(Average.Rating, n = 2),
  Mean.Rating.Senior = lag(Average.Rating, n = 3),
  Mean.Rating.RS.Senior = lag(Average.Rating, 4),
)


colnames(recruiting)[which(colnames(recruiting) == 'Average.Rating')] = 'Mean.Rating.Freshman'


recruiting = recruiting %>% group_by(Team) %>% mutate(
  Num.Sophomore = lag(Num.Commits, n = 1),
  Num.Junior= lag(Num.Commits, n = 2),
  Num.Senior = lag(Num.Commits, n = 3),
  Num.RS.Senior = lag(Num.Commits, 4),
)


colnames(recruiting)[which(colnames(recruiting) == 'Num.Commits')] = 'Num.Freshman'

recruiting = recruiting %>% dplyr::select(-c(Num.5.Stars,
                                             Num.4.Stars,
                                             Num.3.Stars,
                                             Previous.National.Ranking,
                                             Final.National.Ranking))


recruiting = recruiting %>% dplyr::select(Team,
                                          Season,
                                          Points.Freshman,
                                          Points.Sophomore,
                                          Points.Junior,
                                          Points.Senior,
                                          Points.RS.Senior,
                                          Mean.Rating.Freshman,
                                          Mean.Rating.Sophomore,
                                          Mean.Rating.Junior,
                                          Mean.Rating.Senior,
                                          Mean.Rating.RS.Senior,
                                          Num.Freshman,
                                          Num.Sophomore,
                                          Num.Junior,
                                          Num.Senior,
                                          Num.RS.Senior
                                          )


recruiting = recruiting %>% dplyr::filter(Season >= 2015)

recruiting[is.na(recruiting)] = 0

write.csv(recruiting, row.names = F, 'Lagged Recruiting Data.csv')
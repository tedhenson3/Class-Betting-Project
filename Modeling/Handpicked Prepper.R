
setwd('~/Betting Project')

library(readr)
library(tidyverse)
library(caret)
library(caretEnsemble)

data = read_csv('test.Handpicked.csv')
data$`Season Type` = 'Regular'
data$`Location` = 'Visitor'

# 
data$Month  = as.numeric(word(
  data$Date,
  start = 1,
  end = 1,
  sep  = fixed("/")
))

data = data %>% dplyr::filter(Month != 8 & Month != 9) %>% dplyr::select(-Month)




data = data[,6:ncol(data)]





data$Home.Conf.Div = paste(data$Home.Conference, data$Home.Division,
                           sep = ' ')

data$Visitor.Conf.Div = paste(data$Visitor.Conference, data$Visitor.Division,
                              sep = ' ')


data = data %>% dplyr::select(-Season,
                              -Visitor.Division,
                              -Home.Division,
                              -Home.Conf.Div,
                              -Visitor.Conf.Div)

data = data %>% dplyr::select(`Season Type`,
                              Location,
                              Home.Conference,
                              Visitor.Conference,
                              
                              is.conf.game,
                              is.conf.div.game,
                              everything())

data[,1:6] = data[,1:6] %>% map(as.factor)




# data$home.is.power.five = as.factor(ifelse(data$Home.Conference %in%
#                                    c('Atlantic Coast',
#                                      'Pac-12',
#                                      'Big Ten',
#                                      'Southeaastern',
#                                      'Big 12'),
#                                  1, 0))
# 
# data$visitor.is.power.five = as.factor(ifelse(data$Visitor.Conference %in%
#                                    c('Atlantic Coast',
#                                      'Pac-12',
#                                      'Big Ten',
#                                      'Southeaastern',
#                                      'Big 12'),
#                                  1, 0))



recruit.data = data[,which(grepl('Recruit', colnames(data)))]

roll.data = data[,which(grepl('.roll', colnames(data)))]


dif.roll = data.frame(matrix(ncol = 25, nrow = nrow(data)))
colnames(dif.roll) = gsub('Home', 'Dif', colnames(roll.data)[1:25])

for(i in 1:25){
  
  dif.roll[,i] = roll.data[,i] - roll.data[,c(i+25)]
  
}

data = data[,-c(which(grepl('.roll', colnames(data))))]


dif.talent = data.frame(matrix(ncol = 15, nrow = nrow(data)))
colnames(dif.talent) = gsub('Home', 'Dif', colnames(recruit.data)[1:15])

for(i in 1:15){
  
  dif.talent[,i] = recruit.data[,i] - recruit.data[,c(i+15)]
  
}

data = data[,-c(which(grepl('Recruit', colnames(data))))]

data = cbind(data, dif.talent, dif.roll)



results.data = na.omit(data)

test = results.data

test$Spread = predict(stack.glm.my.spread, test)


neural = predict(win.loss$avNNet, test)

dart = predict(win.loss$xgbDART, test)

logit = predict(win.loss$glmnet, test)





test$Total = predict(stack.glm.my.total, test)



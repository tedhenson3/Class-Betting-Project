
setwd('~/Betting Project')

library(readr)
library(tidyverse)
library(caret)
library(caretEnsemble)

data = read_csv('GameResults-Recruiting-LaggedStats.csv')

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
                              -Home.G.,
                              -Visitor.G.,
                              -Home.Conf.Div,
                              -Visitor.Conf.Div)

data = data %>% dplyr::select(Result,
                              `Season Type`,
                              Location,
                              Home.Conference,
                              Visitor.Conference,
                              
                              is.conf.game,
                              is.conf.div.game,
                              everything())

data[,1:7] = data[,1:7] %>% map(as.factor)




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


spread = results.data$Spread
total = results.data$Total

results.data = results.data %>% dplyr::select(-Spread,
                                              -Total)


spread.data = cbind(spread, results.data)



spread.data = spread.data %>% dplyr::select(-Result)



#classes were not represented unless it was an even split
smp_size <- floor(0.9 * nrow(spread.data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(spread.data)), size = smp_size)



train <- spread.data

library(bartMachine)
options(java.parameters = "-Xmx5g")

algorithmList <- c('bartMachine', 
                   'avNNet',
                   'mlp',
                   'rf')


algorithmList <- c('pls',
                   'earth',
                   'lasso', 
                   'ridge')





fitControl <- trainControl(
  method = 'boot',                   # k-fold cross validation
  number = 1000,  
  savePredictions = 'final'
) 




spread.models <- train(form = spread ~ ., 
                           data=train,  
                           trControl=fitControl, 
                           methodList='rf')



results <- resamples(spread.models)
summary(results)



set.seed(433)
# creat another resampling method
stackControl <- trainControl(
  method = 'boot',                   # k-fold cross validation
  number = 50,  
  
) 



# this is a linear regression on the predictions from the other four, very effective
stack.glm.spread <- caretStack(spread.models, 
                                     method = "glm", 
                                     trControl=stackControl) 




save.image("~/Betting Project/Spread Simple Boot Models (Post September).RData")




# 
# stack.err = test$spread - stack.pred
# lasso.err = test$spread - lasso.pred
# ridge.err = test$spread - ridge.pred
# pcr.err = test$spread - pcr.pred
# earth.err = test$spread - earth.pred
# 
# 
# rmse.stacked <- sqrt(mean(stack.err^2))
# rmse.ridge <- sqrt(mean(ridge.err^2))
# rmse.earth <- sqrt(mean(earth.err^2))
# rmse.pcr = sqrt(mean(pcr.err^2))
# rmse.lasso = sqrt(mean(lasso.err^2))
# 
# 
# 
# spread.rmse.matrix = matrix(nrow = 5, ncol = 2)
# spread.rmse.matrix[1,1] = 'earth'
# spread.rmse.matrix[2,1] = 'Ridge'
# spread.rmse.matrix[3,1] = 'Lasso'
# spread.rmse.matrix[4,1] = 'rf'
# spread.rmse.matrix[5,1] = 'Stacked'
# 
# spread.rmse.matrix[1,2] = rmse.earth
# spread.rmse.matrix[2,2] = rmse.ridge
# spread.rmse.matrix[3,2] = rmse.lasso
# spread.rmse.matrix[4,2] = rmse.rf
# spread.rmse.matrix[5,2] = rmse.stacked
# 
# spread.rmse.matrix = as.data.frame(spread.rmse.matrix)
# colnames(spread.rmse.matrix)[1] = 'Model Type'
# colnames(spread.rmse.matrix)[2] = 'Out of Sample RMSE'
# print(spread.rmse.matrix)



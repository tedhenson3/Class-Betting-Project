
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



library(caretEnsemble)
options(java.parameters = "-Xmx2500m")

algorithmList <- c('avNNet',

  'xgbDART',
                   'glmnet',
                   'bartMachine')



#classes were not represented unless it was an even split
smp_size <- floor(0.9 * nrow(results.data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(results.data)), size = smp_size)



train <- results.data

#test = test

fitControl <- trainControl(
  method = 'boot',                   # k-fold cross validation
  number =  75,  
  savePredictions = 'final',
  classProbs = T
) 

win.loss <- caretList(form = Result ~ ., data=train,  
                      trControl=fitControl, 
                      
                      methodList=algorithmList) 

results <- resamples(win.loss)
summary(results)


# 
stackControl <- trainControl(
  method = 'boot',                   # k-fold cross validation
  number = 50,

  savePredictions = 'final'      # saves predictions for optimal tuning parameter
)

stack.glm.win.loss <- caretStack(win.loss,
                               method = "glm",
                               trControl=stackControl)


test = read_csv('In Progress Predictions.3.csv')

stack.pred = predict(stack.glm.win.loss, test)
# 
test$Result = stack.pred

test = test %>% dplyr::select(Date,
                              `Home Team`,
                              `Visitor Team`,
                              Spread,
                              Result,
                              Total)

write.csv(test, 'In Progress Predictions.4.csv',
          row.names = F)



save.image("~/Betting Project/Advanced Class Models (booted).RData")







# svmLinearWeights2.rate = mean(svmLinearWeights2.pred == test$Result)
# lda.rate = mean(lda.pred == test$Result)
# rf.rate = mean(rf.pred == test$Result)
# logit.rate = mean(logit.pred == test$Result)
# 
# rate.matrix = matrix(data = c('svmLinearWeights2', svmLinearWeights2.rate,
#                               'lda', lda.rate,
#                               'rf', rf.rate,
#                               'logit', logit.rate),
#                      byrow = T,
#                      nrow = 4, ncol = 2)
# 
# colnames(rate.matrix) = c('Model Type', 'Accuracy')
# rate.matrix[, 'Accuracy'] = paste(as.numeric(rate.matrix[, 'Accuracy']) * 100, '%', sep = '')



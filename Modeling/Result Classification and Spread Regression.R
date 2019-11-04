setwd('~/Betting Project')

library(readr)
library(tidyverse)

data = read_csv('GameResults-Recruiting-LaggedStats.csv')
# data = data %>% dplyr::select(-Date,
#                               -Season,
#                               -`Home Score`,
#                               -`Visitor Score`)

test = read_csv('test.csv')


test = test %>% dplyr::select(-Spread,
                              -Result,
                              -Total)
# 
# test = test %>% dplyr::select(-Date,
#                               -Season)

test$Location = 'Visitor'

test$`Season Type` = 'Regular'

test = test %>% dplyr::select(`Season Type`,
                              Location,
                              Home.Conference,
                              Visitor.Conference,
                              
                              is.conf.game,
                              is.conf.div.game,
                              everything())

test[,1:6] = test[,1:6] %>% map(as.factor)


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



# recruit.data = data[,which(grepl('Recruit', colnames(data)))]
# 
# dif.talent = data.frame(matrix(ncol = 15, nrow = nrow(data)))
# colnames(dif.talent) = gsub('Home', 'Dif', colnames(recruit.data)[1:15])
# 
# for(i in 1:15){
#   
#   dif.talent[,i] = recruit.data[,i] - recruit.data[,c(i+15)]
#   
# }


# 
# dif.talent$Total.Dif.Recruit.Points = dif.talent$Dif.Recruit.Points.Freshman + 
#                                   dif.talent$Dif.Recruit.Points.Sophomore + 
#                                   dif.talent$Dif.Recruit.Points.Junior + 
#                                   dif.talent$Dif.Recruit.Points.Senior + 
#                                   dif.talent$Dif.Recruit.Points.RS.Senior
# 
# 
# dif.talent$Total.Dif.Mean.Rating  = dif.talent$Dif.Recruit.Mean.Rating.Freshman + 
#   dif.talent$Dif.Recruit.Mean.Rating.Sophomore + 
#   dif.talent$Dif.Recruit.Mean.Rating.Junior + 
#   dif.talent$Dif.Recruit.Mean.Rating.Senior + 
#   dif.talent$Dif.Recruit.Mean.Rating.RS.Senior

# results.data = cbind(result, 
#                      site,
#                      home.g,
#                      visitor.g,
#                     season.type,
#                     spread,
#                     home.conf,
#                     visitor.conf,
#                     
#                      dif.talent,
#                     roll.avgs
#                     )


# data  = data[,-c(which(grepl('Recruit', colnames(data))))]
# 
# data = cbind(data, dif.talent)

results.data = na.omit(data)


spread = results.data$Spread
total = results.data$Total

results.data = results.data %>% dplyr::select(-Spread,
                              -Total)


# 
# 
# spread.data = cbind(data$Result, dif.talent)
# 
# colnames(spread.data)[1] = 'Spread'


# mod = glm(factor(Result) ~ ., data = dif.talent, family = 'binomial')
# 
# summary(mod)


library(caret)

fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 10,  
  savePredictions = 'final',
  classProbs=TRUE
) 





library(caretEnsemble)
options(java.parameters = "-Xmx5g")

algorithmList <- c('svmLinearWeights2',
                   'lda',
                   'qda')



#classes were not represented unless it was an even split
smp_size <- floor(0.9 * nrow(results.data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(results.data)), size = smp_size)



train <- results.data

test = test



win.loss <- caretList(form = Result ~ ., data=train,  
                      trControl=fitControl, 
                      methodList=algorithmList) 
# results <- resamples(win.loss)
# summary(results)


logit <- caretList(form = Result ~ ., data=train,  
                      trControl=fitControl, 
                      methodList='glm', family = 'binomial') 

# 
# stackControl <- trainControl(
#   method = 'cv',                   # k-fold cross validation
#   number = 40,  
#   
#   savePredictions = 'final'      # saves predictions for optimal tuning parameter
# ) 
# 
# stack.glm.win.loss <- caretStack(win.loss, 
#                                method = "glm", 
#                                trControl=stackControl) 



svmLinearWeights2.pred = predict(win.loss$svmLinearWeights2, test)
lda.pred = predict(win.loss$lda, test)
qda.pred = predict(win.loss$qda, test)
logit.pred = predict(logit$glm, test)

test$Result = logit.pred
test = test %>% dplyr::select(Result,
                              everything())


svmLinearWeights2.rate = mean(svmLinearWeights2.pred == test$Result)
lda.rate = mean(lda.pred == test$Result)
qda.rate = mean(qda.pred == test$Result)
logit.rate = mean(logit.pred == test$Result)

rate.matrix = matrix(data = c('svmLinearWeights2', svmLinearWeights2.rate,
                              'lda', lda.rate,
                              'qda', qda.rate,
                              'logit', logit.rate),
                     byrow = T,
                     nrow = 4, ncol = 2)

colnames(rate.matrix) = c('Model Type', 'Accuracy')
rate.matrix[, 'Accuracy'] = paste(as.numeric(rate.matrix[, 'Accuracy']) * 100, '%', sep = '')




spread.data = cbind(spread, results.data)



spread.data = spread.data %>% dplyr::select(-Result)



#classes were not represented unless it was an even split
smp_size <- floor(0.9 * nrow(spread.data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(spread.data)), size = smp_size)



train <- spread.data



algorithmList <- c('lasso', 'earth',
                   'ridge',
                   'rf')



fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 10,  
  savePredictions = 'final'
) 




spread.models <- caretList(form = spread ~ ., 
                           data=train,  
                           trControl=fitControl, 
                           methodList=algorithmList)
results <- resamples(spread.models)
summary(results)


stackControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 10,  
  
  savePredictions = 'final'      # saves predictions for optimal tuning parameter
) 

stack.glm.spread <- caretStack(spread.models, 
                             method = "glm", 
                             trControl=stackControl) 

library(bartMachine)




stack.pred = predict(stack.glm.spread, test)
lasso.pred = predict(spread.models$lasso, test)
ridge.pred = predict(spread.models$ridge, test)
rf.pred = predict(spread.models$rf, test)
earth.pred = predict(spread.models$earth, test)

test$Spread = stack.pred
test = test %>% dplyr::select(Result,
                              Spread,
                              everything())


stack.err = test$spread - stack.pred
lasso.err = test$spread - lasso.pred
ridge.err = test$spread - ridge.pred
rf.err = test$spread - rf.pred
earth.err = test$spread - earth.pred


rmse.stacked <- sqrt(mean(stack.err^2))
rmse.ridge <- sqrt(mean(ridge.err^2))
rmse.earth <- sqrt(mean(earth.err^2))
rmse.rf = sqrt(mean(rf.err^2))
rmse.lasso = sqrt(mean(lasso.err^2))



spread.rmse.matrix = matrix(nrow = 5, ncol = 2)
spread.rmse.matrix[1,1] = 'earth'
spread.rmse.matrix[2,1] = 'Ridge'
spread.rmse.matrix[3,1] = 'Lasso'
spread.rmse.matrix[4,1] = 'rf'
spread.rmse.matrix[5,1] = 'Stacked'

spread.rmse.matrix[1,2] = rmse.earth
spread.rmse.matrix[2,2] = rmse.ridge
spread.rmse.matrix[3,2] = rmse.lasso
spread.rmse.matrix[4,2] = rmse.rf
spread.rmse.matrix[5,2] = rmse.stacked

spread.rmse.matrix = as.data.frame(spread.rmse.matrix)
colnames(spread.rmse.matrix)[1] = 'Model Type'
colnames(spread.rmse.matrix)[2] = 'Out of Sample RMSE'
print(spread.rmse.matrix)




total.data = cbind(total, results.data)



total.data = total.data %>% dplyr::select(-Result)



#classes were not represented unless it was an even split
smp_size <- floor(0.9 * nrow(total.data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(total.data)), size = smp_size)



train <- total.data[train_ind, ]
test <- total.data[-train_ind, ]



algorithmList <- c('lasso',
                   'ridge',
                   'earth',
                   'pls')



total.models <- caretList(form = total ~ ., data=train,  trControl=fitControl, methodList=algorithmList)
results <- resamples(total.models)
summary(results)


stackControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 10,  
  
  savePredictions = 'final'      # saves predictions for optimal tuning parameter
) 

stack.glm.total <- caretStack(total.models, 
                               method = "glm", 
                               trControl=stackControl) 

library(bartMachine)




stack.pred = predict(stack.glm.total, test)
lasso.pred = predict(total.models$lasso, test)
ridge.pred = predict(total.models$ridge, test)
pls.pred = predict(total.models$pls, test)
earth.pred = predict(total.models$earth, test)


test$Total = stack.pred
test = test %>% dplyr::select(Result,
                              Spread,
                              Total,
                              everything())
write.csv(test, 'Current.Predictions.csv')

stack.err = test$total - exp(stack.pred)
lasso.err = test$total - exp(lasso.pred)
ridge.err = test$total - exp(ridge.pred)
pls.err = test$total - exp(pls.pred)
earth.err = test$total - exp(earth.pred)


rmse.stacked <- sqrt(mean(stack.err^2))
rmse.ridge <- sqrt(mean(ridge.err^2))
rmse.earth <- sqrt(mean(earth.err^2))
rmse.pls = sqrt(mean(pls.err^2))
rmse.lasso = sqrt(mean(lasso.err^2))



total.rmse.matrix = matrix(nrow = 5, ncol = 2)
total.rmse.matrix[1,1] = 'earth'
total.rmse.matrix[2,1] = 'Ridge'
total.rmse.matrix[3,1] = 'Lasso'
total.rmse.matrix[4,1] = 'PLS'
total.rmse.matrix[5,1] = 'Stacked'

total.rmse.matrix[1,2] = rmse.earth
total.rmse.matrix[2,2] = rmse.ridge
total.rmse.matrix[3,2] = rmse.lasso
total.rmse.matrix[4,2] = rmse.pls
total.rmse.matrix[5,2] = rmse.stacked

total.rmse.matrix = as.data.frame(total.rmse.matrix)
colnames(total.rmse.matrix)[1] = 'Model Type'
colnames(total.rmse.matrix)[2] = 'Out of Sample RMSE'
print(total.rmse.matrix)

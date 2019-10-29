setwd('~/Betting Project')

library(readr)
library(tidyverse)

data = read_csv('GameResults-Recruiting-LaggedStats.csv')




site = data$Location
spread = data$Spread


season.type = data$`Season Type`

home.g = data$Home.G.

visitor.g = data$Visitor.G.


result  = data$Result
result = ifelse(result == 'W', 1, 0)


data = data[,which(colnames(data) == 'Result'):ncol(data)]


roll.avgs = data[,which(grepl('.roll.avg', colnames(data)))]

dif.talent = data.frame(matrix(ncol = 15, nrow = nrow(data)))
colnames(dif.talent) = gsub('Home', 'Dif', colnames(data)[2:16])

for(i in 2:16){
  
  dif.talent[,i-1] = data[,i] - data[,c(i+15)]
  
}


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

results.data = cbind(result, 
                     site,
                     home.g,
                     visitor.g,
                    season.type,
                    spread,
                     dif.talent,
                    roll.avgs
                    )


results.data = na.omit(results.data)

spread = results.data$spread
results.data = results.data %>% dplyr::select(-spread)

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
  number = 5,  
  savePredictions = 'final',
) 





library(caretEnsemble)
options(java.parameters = "-Xmx5g")

algorithmList <- c('qda',
                   'rf',
                   'nnet',
                   'multinom')



#classes were not represented unless it was an even split
smp_size <- floor(0.9 * nrow(results.data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(results.data)), size = smp_size)


results.data$result = as.factor(results.data$result)

train <- results.data[train_ind, ]
test <- results.data[-train_ind, ]



win.loss <- caretList(form = result ~ ., data=train,  trControl=fitControl, methodList=algorithmList) 
results <- resamples(win.loss)
summary(results)





qda.predictions = predict(win.loss$qda, test)
rf.predictions = predict(win.loss$rf, test)
nnet.predictions = predict(win.loss$nnet, test)
multinom.predictions = predict(win.loss$multinom, test)


qda.rate = mean(qda.predictions == test$result)
rf.rate = mean(rf.predictions == test$result)
nnet.rate = mean(nnet.predictions == test$result)
multinom.rate = mean(multinom.predictions == test$result)

rate.matrix = matrix(data = c('qda', qda.rate,
                              'rf', rf.rate,
                              'nnet', nnet.rate,
                              'multinom', multinom.rate),
                     byrow = T,
                     nrow = 4, ncol = 2)
rate.matrix




spread.data = cbind(spread, results.data)



spread.data = spread.data %>% dplyr::select(-result)



#classes were not represented unless it was an even split
smp_size <- floor(0.9 * nrow(spread.data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(spread.data)), size = smp_size)


spread.data$result = as.factor(spread.data$result)

train <- spread.data[train_ind, ]
test <- spread.data[-train_ind, ]



algorithmList <- c('lasso',
                   'ridge',
                   'rf',
                   'nnet')



spread.models <- caretList(form = Spread ~ ., data=train,  trControl=fitControl, methodList=algorithmList)
results <- resamples(spread.models)
summary(results)





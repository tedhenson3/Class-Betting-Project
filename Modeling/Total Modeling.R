
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


total.roll = data.frame(matrix(ncol = 25, nrow = nrow(data)))
colnames(total.roll) = gsub('Home', 'total', colnames(roll.data)[1:25])

for(i in 1:25){
  
  total.roll[,i] = roll.data[,i] + roll.data[,c(i+25)]
  
}

data = data[,-c(which(grepl('.roll', colnames(data))))]


total.talent = data.frame(matrix(ncol = 15, nrow = nrow(data)))
colnames(total.talent) = gsub('Home', 'total', colnames(recruit.data)[1:15])


#now doing sum
for(i in 1:15){
  
  total.talent[,i] = recruit.data[,i] + recruit.data[,c(i+15)]
  
}

data = data[,-c(which(grepl('Recruit', colnames(data))))]

data = cbind(data, total.talent, total.roll)



Total.data = na.omit(data)



Total.data = Total.data %>% dplyr::select(-Spread,
                                              -Result)

train = Total.data



library(caretEnsemble)
options(java.parameters = "-Xmx5g")




library(caret)

fitControl <- trainControl(
  method = 'boot',                   # k-fold cross validation
  number = 1000,  
  savePredictions = 'final'
) 






algorithmList <- c('lasso',
                   'ridge',
                   'earth',
                   'pls')



Total.models <- caretList(form = Total ~ ., data=train,  trControl=fitControl, methodList=algorithmList)

results <- resamples(Total.models)
summary(results)

save.image("~/Betting Project/Total Simple Boot Models (Post September).RData")


stackControl <- trainControl(
  method = 'boot',                   # k-fold cross validation
  number = 50,  
  
  savePredictions = 'final'      # saves predictions for optimal tuning parameter
) 

stack.glm.Total <- caretStack(Total.models, 
                              method = "glm", 
                              trControl=stackControl) 


save.image("~/Betting Project/Total Simple Boot Models (Post September).RData")



stack.pred = predict(stack.glm.Total, test)



test = read_csv('In Progress Predictions.2.csv')


test$Total = stack.pred


write.csv(test, 'In Progress Predictions.3.csv',
          row.names = F)


# 
# stack.err = test$Total - exp(stack.pred)
# lasso.err = test$Total - exp(lasso.pred)
# ridge.err = test$Total - exp(ridge.pred)
# pls.err = test$Total - exp(pls.pred)
# earth.err = test$Total - exp(earth.pred)
# 
# 
# rmse.stacked <- sqrt(mean(stack.err^2))
# rmse.ridge <- sqrt(mean(ridge.err^2))
# rmse.earth <- sqrt(mean(earth.err^2))
# rmse.pls = sqrt(mean(pls.err^2))
# rmse.lasso = sqrt(mean(lasso.err^2))
# 
# 
# 
# Total.rmse.matrix = matrix(nrow = 5, ncol = 2)
# Total.rmse.matrix[1,1] = 'earth'
# Total.rmse.matrix[2,1] = 'Ridge'
# Total.rmse.matrix[3,1] = 'Lasso'
# Total.rmse.matrix[4,1] = 'pls'
# Total.rmse.matrix[5,1] = 'Stacked'
# 
# Total.rmse.matrix[1,2] = rmse.earth
# Total.rmse.matrix[2,2] = rmse.ridge
# Total.rmse.matrix[3,2] = rmse.lasso
# Total.rmse.matrix[4,2] = rmse.pls
# Total.rmse.matrix[5,2] = rmse.stacked
# 
# Total.rmse.matrix = as.data.frame(Total.rmse.matrix)
# colnames(Total.rmse.matrix)[1] = 'Model Type'
# colnames(Total.rmse.matrix)[2] = 'Out of Sample RMSE'
# print(Total.rmse.matrix)



setwd('~/Betting Project')

library(readr)
#performance
results <- resamples(spread.models)
summary(results)


results <- resamples(win.loss)
summary(results)


results <- resamples(Total.models)
summary(results)

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



recruit.test = test[,which(grepl('Recruit', colnames(test)))]

roll.test = test[,which(grepl('.roll', colnames(test)))]


dif.roll = data.frame(matrix(ncol = 25, nrow = nrow(test)))
colnames(dif.roll) = gsub('Home', 'Dif', colnames(roll.test)[1:25])

for(i in 1:25){
  
  dif.roll[,i] = roll.test[,i] - roll.test[,c(i+25)]
  
}

test = test[,-c(which(grepl('.roll', colnames(test))))]


dif.talent = data.frame(matrix(ncol = 15, nrow = nrow(test)))
colnames(dif.talent) = gsub('Home', 'Dif', colnames(recruit.test)[1:15])

for(i in 1:15){
  
  dif.talent[,i] = recruit.test[,i] - recruit.test[,c(i+15)]
  
}

test = test[,-c(which(grepl('Recruit', colnames(test))))]

test = cbind(test, dif.talent, dif.roll)



svmLinearWeights2.pred = predict(win.loss$svmLinearWeights2, 
                                 test)
lda.pred = predict(win.loss$lda, test)
rf.pred = predict(win.loss$rf, test)
logit.pred = predict(logit$glm, test)

test$Result = svmLinearWeights2.pred
test = test %>% dplyr::select(Result,
                              everything())



stack.pred = predict(stack.glm.spread, test)
lasso.pred = predict(spread.models$lasso, test)
ridge.pred = predict(spread.models$ridge, test)
rf.pred = predict(spread.models$rf, test)
earth.pred = predict(spread.models$earth, test)

test$Spread = stack.pred
write.csv(test, 'In Progress Predictions.csv', row.names = F)
test = test %>% dplyr::select(Spread,
                              everything())


stack.pred = predict(stack.glm.Total, test)
lasso.pred = predict(Total.models$lasso, test)
ridge.pred = predict(Total.models$ridge, test)
rf.pred = predict(Total.models$rf, test)
earth.pred = predict(Total.models$earth, test)


test$Result = stack.pred
test = test %>% dplyr::select(Result,
                              Spread,
                              `Home Team`,
                              `Visitor Team`,
                              everything())
# write.csv(test, 'In Progress Predictions.csv',
#           row.names = F)

save.image("~/Betting Project/All Models (Post September).RData")



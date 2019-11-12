

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


total.roll = data.frame(matrix(ncol = 25, nrow = nrow(test)))
colnames(total.roll) = gsub('Home', 'total', colnames(roll.test)[1:25])

for(i in 1:25){
  
  total.roll[,i] = roll.test[,i] + roll.test[,c(i+25)]
  
}

test = test[,-c(which(grepl('.roll', colnames(test))))]


total.talent = data.frame(matrix(ncol = 15, nrow = nrow(test)))
colnames(total.talent) = gsub('Home', 'total', colnames(recruit.test)[1:15])

for(i in 1:15){
  
  total.talent[,i] = recruit.test[,i] + recruit.test[,c(i+15)]
  
}

test = test[,-c(which(grepl('Recruit', colnames(test))))]

test = cbind(test, total.talent, total.roll)


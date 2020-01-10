rm(list = ls())

wine = read.csv('mlwr/redwines.csv')
str(wine)
summary(wine)

1599 * 0.75
wine_train = wine[1:1200, ]
wine_test = wine[1201:1599, ]

wine_rpart = rpart(quality ~ ., wine_train)
wine_rpart
summary(wine_rpart)
rpart.plot(wine_rpart)
rpart.plot(wine_rpart, fallen.leaves = T, digits = 3)

wine_predict = predict(wine_rpart, wine_test)
head(wine_predict, 10)
summary(wine_predict)
summary(wine_test$quality)

# MAE(Mean Absolute Error): 평균 절대 오차
# 오차(실제값 - 예측값)들의 절대값의 평균
MAE = function(actual, predict) {
  return(mean(abs(actual - predict)))
}

cor(wine_predict, wine_test$quality) # 0.5986281 높을수록 좋음
MAE(wine_predict, wine_test$quality) # 0.5384269 낮을수로 좋음



## 성능 향상
wine_cubist = cubist(wine_train[-12], wine_train$quality)
summary(wine_cubist)

wine_predict_cubist = predict(wine_cubist, wine_test)
summary(wine_predict_cubist)
summary(wine_predict)
summary(wine_test$quality)

cor(wine_predict_cubist, wine_test$quality) # 0.6684898
MAE(wine_predict_cubist, wine_test$quality) # 0.4533078





rm(list = ls())
# RegressionTree와 Model Tree

# 1. 데이터 준비
wine = read.csv('mlwr/whitewines.csv')

str(wine)
  # 4,898 obs. , 12 variables

head(wine, 10)
tail(wine, 10)
summary(wine)

# 종속 변수(quality)의 분포
hist(wine$quality)


# regression tree를 사용하기 위한 패키지
# rpart: recursive partioning
install.packages("rpart")
library(rpart)

# 모델 학습
# 학습 데이터 세트(75%)/ 테스트 데이터 세트(25%)
4898 * 0.75
wine_train = wine[1:3674, ]
wine_test = wine[3675:4898, ]

# 학습 데이터를 rpart 패키지를 사용해서 학습시킴
wine_rpart = rpart(formula = quality ~ ., data = wine_train)
wine_rpart

summary(wine_rpart)

# rpart(회귀 트리) 결과를 시각적으로 보여주는 패키지
install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(x = wine_rpart, digits = 3)
rpart.plot(x = wine_rpart, digits = 4, fallen.leaves = F)


# 모델 평가 - regression tree가 테스트 데이터를 얼마나 잘 설명하는가
wine_predict = predict(wine_rpart, wine_test)
summary(wine_predict) # 예측 quality의 기술통계량
summary(wine_test$quality) # 실제 quality의 기술통계량


# 모델 성능 평가 1)
# 상관 계수(correlation coefficient): -1 <= 0 <= 1
cor(wine_predict, wine_test$quality) # 0.5354775


# 모델 성능 평가 2)
# MAE(Mean Absolute Error): 평균 절대 오차
# 오차(실제값 - 예측값)들의 절대값의 평균
MAE = function(actual, predict) {
  return(mean(abs(actual - predict)))
}

# 함수 테스트
MAE(actual = c(1, 2, 3), predict = c(1.1, 1.9, 3.0))

MAE(wine_test$quality, wine_predict) # 0.6



# 5. 모델 성능 향상
# 모델 트리(Model Tree):
# Regression Tree(분류) + Regression modeling(회귀 모델 적용)
# 교재: RWeka 패키지의 M5P 함수 사용
# Cubist 패키지: 규칙 학습 기반 분류 + M5P 알고리즘 회귀 모델 적용
install.packages("Cubist")
library(Cubist)

# cubist(x = 훈련 데이터, y = 훈련 데이터의 결과)
# 훈련 데이터에는 레이블(클레스), 즉 종속 변수가 포함되면 안됨
# 훈련 데이터에는 분류에 필요한 독립 변수들만 포함시킴
wine_cubist = cubist(x = wine_train[-12], y = wine_train$quality)

wine_cubist
summary(wine_cubist)

wine_predict_cubist = predict(wine_cubist, wine_test)

# 기술 통계량
summary(wine_predict_cubist)
summary(wine_test$quality)

# 상관 계수
cor(wine_predict_cubist, wine_test$quality) # 0.640193

# MAE: 평균 절대값 오차
MAE(wine_predict_cubist, wine_test$quality) # 0.5379868

# Artificial Neural Network(인공 신경망)
rm(list = ls())


# f(x) = 2x + 1
curve(expr = 2 * x + 1, from = -5, to = 5)
# sigmoid  함수 : f(x) = 1 / (1 + exp(-x))
curve(expr = 1 / (1 + exp(-x)), from = -10, to = 10)
# hypobolic tangent : f(x) = tanh(x)
curve(expr = tanh(x), from = -5, to = 5)



# 콘크리트의 강도 예측

# 1. 데이터 준비
concrete = read.csv('mlwr/concrete.csv')
str(concrete)
head(concrete, 10)
summary(concrete)

# 정규화(Normalization) : 실제값을 -> 0 ~ 1 
# 표준화(Standardization) : z-score 표준화(평균, 표준편차)

normalization = function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


concrete_norm = as.data.frame(lapply(concrete, normalization))
# lapply 는 리스트(list)를 리턴하는 함수이다. 그래서 as.data.frame 으로 데이터 프레임으로 변경
summary(concrete_norm)


# 신경망 알고리즘 적용하기 위한 패키지 : neuralnet
# 오차 역전파(backpropagation)를 사용해서 신경망을 훈련시키는 알고리즘
install.packages("neuralnet")
library(neuralnet)

# 3. 모델 생성, 학습
# 학습 데이터 세트(75%) /  테스트 데이터 세트(25%)
1030 * 0.75
concrete_train = concrete_norm[1:773, ]
concrete_test = concrete_norm[774:1030, ]

summary(concrete_train)
summary(concrete_test)


# 신경망 모델 생성
set.seed(12345)
concrete_model = neuralnet(formula = strength ~ ., data = concrete_train)

# 생성된 NN을 확인
concrete_model
summary(concrete_model)

plot(concrete_model)

library(neuralnet)
detach("package:dplyr")
search()

# 4. 만들어진 NN을 평가 - 테스트 데이터 세트에서 적용
model_result = neuralnet::compute(concrete_model, concrete_test[-9]) # 결과 제외한 test
head(model_result, 5) # 신경망 모델에 의해서 계산된 strength 예측값
summary(model_result)


predict_result = model_result$net.result

# 예측 결과와 실제 값의 상관 관계 - 상관 계수
cor(predict_result, concrete_test$strength) # 0.8064656


tail(concrete_test$strength, 3)


# 5. 모델 향상
## 노드 2개
model2 = neuralnet(formula = strength ~ .,
                   data = concrete_train,
                   hidden = 2)
plot(model2)

model2_result = compute(model2, concrete_test[-9])
predict_model2 = model2_result$net.result
str(predict_model2)
cor(predict_model2, concrete_test$strength) # 0.9024369

## 노드 5개  
model5 = neuralnet(strength ~ .,
                   concrete_train,
                   hidden = 5)
plot(model5)

model5_result = compute(model5, concrete_test[-9])
predict_model5 = model5_result$net.result
cor(predict_model5, concrete_test$strength) # 0.9280144




# 각 모델에서 예측 결과와 실제 strength 간의 상관 계수를 계산
cor(predict_model2, concrete_test$strength) # 0.9024369
cor(predict_model5, concrete_test$strength) # 0.9280144


# 평균 절대 오차(MAE: Mean Absolute Error) 함수 작성
# 각 모델에 MAE를 계산

MAE = function(x, y) {
  return(mean(abs(x - y)))
}

MAE(predict_model2, concrete_test$strength) # 0.06856742
MAE(predict_model5, concrete_test$strength) # 0.05736873


# 데이터 역 정규화 (정규화 -> 실제값) 함수 작성
# 실제 데이터 프레임(concrete)의 값들과 비교

## 정규화
normalization = function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

## 역정규화
u_normalize = function(norm, x) {
  return(norm * (max(x) - min(x)) + min(x))
}

### 모델2
concrete_u_norm2 = u_normalize(predict_model2, concrete$strength)

head(concrete_u_norm2, 10)
summary(concrete_u_norm2)
summary(concrete$strength)

strength_comp2 = data.frame('concrete_strength' = concrete$strength[c(774:1030)], 'pre_strength_2' = concrete_u_norm2)
str(strength_comp2)
head(strength_comp2, 10)
summary(strength_comp2)

### 모델5
concrete_u_norm5 = u_normalize(predict_model5, concrete$strength)

summary(concrete_u_norm5)
strength_comp5 = data.frame('concrete_strength2' = concrete$strength[c(774:1030)], 'pre_strength_5' = concrete_u_norm5)
str(strength_comp5)
head(strength_comp5, 10)
tail(strength_comp5, 10)
summary(strength_comp5)


## 계층 노드 조정

###
softsum = function(x) {
  log(1 + exp(x))
}
curve(expr = softsum, from = -5, to = 5)


model5_3 = neuralnet(formula = strength ~ .,
                   data = concrete_train,
                   hidden = c(5, 3),
                   act.fct = softsum) # 활성 함수



plot(model5_3)
model5_3_result = compute(model5_3, concrete_test[-9])
predict_model5_3 = model5_3_result$net.result

cor(predict_model5_3, concrete_test$strength) # 0.9354836
MAE(predict_model5_3, concrete_test$strength) # 0.0528646

concrete_u_norm5_3 = u_normalize(predict_model5_3, concrete$strength)
strength_comp5_3 = data.frame('concrete_strength3' = concrete$strength[c(774:1030)], 'pre_strength_5_3' = concrete_u_norm5_3)
summary(strength_comp5_3)



#############
#############
#############

library(dplyr)
head(strength_comp2)
head(strength_comp5)
strength_all = cbind(strength_comp2, strength_comp5, strength_comp5_3)

summary(strength_all)
head(strength_all)
str(strength_all)

strength_all = strength_all %>% 
  select(concrete_strength, pre_strength_2, pre_strength_5, pre_strength_5_3)

summary(strength_all)
head(strength_all)
str(strength_all)


#############
#############
#############

model4_5_4 = neuralnet(strength ~ .,
                       concrete_train,
                       hidden = c(4, 5, 4),
                       act.fct = softsum) # 활성 함수
plot(model4_5_4)
model4_5_4_result = neuralnet::compute(model4_5_4, concrete_test[-9])
predict_model4_5_4 = model4_5_4_result$net.result
head(predict_model4_5_4)

cor(predict_model4_5_4, concrete_test$strength) # 0.9382415
MAE(predict_model4_5_4, concrete_test$strength) # 0.05217814

concrete_u_norm4_5_4 = u_normalize(predict_model4_5_4, concrete$strength)
head(concrete_u_norm4_5_4)

strength_all = cbind(strength_all, pre_strength_4_5_4 = concrete_u_norm_4_5_4)

head(strength_all)

#############
#############
#############

cor_str_2 = cor(predict_model2, concrete_test$strength)
cor_str_5 = cor(predict_model5, concrete_test$strength)
cor_str_5_3 = cor(predict_model5_3, concrete_test$strength)
cor_str_4_5_4 = cor(predict_model4_5_4, concrete_test$strength)

strength_cor = data.frame(cor_str_2,
                          cor_str_5,
                          cor_str_5_3,
                          cor_str_4_5_4)
strength_cor 

#############
#############
#############

MAE_str_2 = MAE(predict_model2, concrete_test$strength)
MAE_str_5 = MAE(predict_model5, concrete_test$strength)
MAE_str_5_3 = MAE(predict_model5_3, concrete_test$strength)
MAE_str_4_5_4 = MAE(predict_model4_5_4, concrete_test$strength)

strength_MAE = data.frame(MAE_str_2,
                          MAE_str_5,
                          MAE_str_5_3,
                          MAE_str_4_5_4)
strength_MAE


#############
#############
#############

head(strength_all)
strength_cor
strength_MAE

#############
#############
#############

actual_predict_df = data.frame('actual' = concrete$strength[c(774:1030)],
                               'predict_2' = concrete_u_norm2,
                               'predict_5' = concrete_u_norm5,
                               'predict_5_3' = concrete_u_norm5_3,
                               'predict_4_5_4' = concrete_u_norm4_5_4)
head(actual_predict_df, 10)

cor(actual_predict_df$actual, actual_predict_df$predict_2)
cor(actual_predict_df$actual, actual_predict_df$predict_5)
cor(actual_predict_df$actual, actual_predict_df$predict_5_3)
cor(actual_predict_df$actual, actual_predict_df$predict_4_5_4)

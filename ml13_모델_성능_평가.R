# 모델 성능 평가

# m = naivebayes(훈련 데이터, ...)
# predict = predict(model, 테스트 데이터, type = '...')

sms_result = read.csv('mlwr/sms_results.csv')
str(sms_result)
head(sms_result, 10)

tail(sms_result, 10)


library(dplyr)

# spam/ham 분류에서 예측 확률이 50% 근처인 경우 - 예측하기 애매한 경우
# 모델이 잠못 예측할 가능성이 크다

sms_result %>% 
  filter(prob_spam > 0.4 & prob_spam <0.6) %>% 
  head(10)


# 실제 값과 예측 값이 다른 경우
sms_result %>% 
  filter(actual_type != predict_type) %>% 
  head(10)
sms_result %>% 
  filter(actual_type != predict_type) %>% 
  tail(10)


# 혼동 행렬(confusion matrix)
table(sms_result$actual_type, sms_result$predict_type)

library(gmodels)
CrossTable(sms_result$actual_type, sms_result$predict_type,
           prop.chisq = F)


# TN + TP
pr_a = 0.865 +0.109
pr_a
# Pr(e): 예상 일치 (expected agreement) 비율
# 독립 사건이라는 가정 아래에서
# P(실제 스팸) * P(예측 스팸) + P(실제 햄) * P(예측 햄)
pr_e =  (0.132) * (0.112) + (0.868) * (0.888)
kappa = (pr_a - pr_e) / (1 - pr_e)
kappa # 0.8787494

# caret 패키지: Classification And REgression Training
install.packages('caret')
library(caret)

confusionMatrix(sms_result$predict_type, sms_result$actual_type,
                positive = 'spam')
  # data = 예측 결과, reference = 실제 결과
  # positive = 관심 클래스

# Pos Pred Value -> 정밀도 : 관심 없는 것 중 맞춘것 
# 


# 민감도
sensitivity(data = sms_result$predict_type,
          reference = sms_result$actual_type,
          positive = 'spam')

# 특이도
specificity(data = sms_result$predict_type,
            reference = sms_result$actual_type,
            negative = 'ham')

# 정밀도
precision(data = sms_result$predict_type,
           reference = sms_result$actual_type,
           relevant = 'spam')
           
# F-척도 = (2 * precision * recall) / (precision + recall)
F_meas(data = sms_result$predict_type,
       reference = sms_result$actual_type,
       relevant = 'spam')
f = (2 * 0.974359 * 0.8306011) / (0.974359 + 0.8306011)
f


# ROC(Receiver Operation Characteristic)
install.packages('pROC')
library(pROC)

sms_roc = roc(response = sms_result$actual_type,
              predictor =  sms_result$prob_spam)
plot(sms_roc)
plot(sms_roc, col = 'blue', lwd = 3)


sms_knn = read.csv('mlwr/sms_results_knn.csv')
head(sms_knn, 10)
sms_knn_roc = roc(response = sms_result$actual_type,
                  predictor = sms_knn$p_spam)

plot(sms_knn_roc, col = 'red', lwd = 3, add = T)


# k-fold CV(Cross Validation, 교차 검증)
caret::createFolds()
rm(list = ls())
# Decision Tree(의사 결정 나무)

# csv 파일 읽어서 데이터 프레임 생성
credit = read.csv('mlwr/credit.csv', encoding = 'UTF-8')

str(credit)
head(credit)

# 대출 상환 능력과 관계가 있을 것 같은 변수들?
# 범주형 변수(특징) -> table(도수분포표)
# 수치형 변수(특징) -> summary(기술통계량)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)

# 채무 불이행(default) 빈도수
table(credit$default)


head(credit$amount, 10)
tail(credit$amount, 10)
head(credit$default, 10)
tail(credit$default, 10)

# random sampling: 정렬 되어 있는 데이터를 무작위 추출
sample(1000, 10)

# 학습 데이터 세트와 테스트 데이터 세트(9:1)를 준비
rows = sample(1000, 900)
train_set = credit[rows, ] # 학습 데이터 세트
test_set = credit[-rows, ]

# 학습/테스트 데이터 세트에서 defaul 비율 확인
prop.table(table(train_set$default))
prop.table(table(test_set$default))


# 의사결정 나무를 사용하기 위한 패키지
install.packages('C50')
library(C50)

# C50::C5.0(train, class, trials = 1, costs = NULL)
# train = 학습 데이터 세트
# class = 학습 데이터의 레이블(분류)
# trials, costs: 옵션 -> 성능 개선
# 학습 데이터 세트에서는 데이터의 레이블(클래스)를 제거해야 함!
credit_model = C5.0(train_set[-17], train_set$default)
credit_model
summary(credit_model)

# 모델 테스트 데디터를 사용해서 평가
# stats::predict(모델, 테스트 데이터세트)
credit_predict = predict(credit_model, test_set[-17])

# 테스트 평가 결과 - 이원 교차표
library(gmodels)
CrossTable(x = test_set$default, y = credit_predict, prop.chisq = F)


# 모델 성능 개선 1) 의사결정 나무의 개수를 변경
credit_boost = C5.0(train_set[-17], train_set$default, trials = 10)
credit_boost
summary(credit_boost)

# AdaBoost를 적용한 모델 성능 평가
credit_predict_boost = predict(credit_boost, test_set[-17])
table(credit_predict_boost)

CrossTable(x = test_set$default, y = credit_predict_boost, prop.chisq = F)


# 모델 성능 개선 방법 2) 비용 행렬(cost matrix) 사용
# 발생할 수 있는 오류에 패널티를 추가
# 비용 행렬: 패널티 값들로 이루어진 행렬
# 비용 행렬의 행/열의 이름

matrix_dimname = list(predict = c('no', 'yes'),
                      actual = c('no', 'yes'))
matrix_dimname

# 비용 행렬
cost_matrix = matrix(data = c(0, 1, 4, 0),
                     nrow = 2,
                     dimnames = matrix_dimname)
cost_matrix

# 모델 훈련에 비용 행렬을 적용
credit_cost = C5.0(train_set[-17], train_set$default, 
                   costs = cost_matrix)
summary(credit_cost)

credit_predict_cost = predict(credit_cost, test_set[-17])
CrossTable(x = test_set$default, y = credit_predict_cost, 
           prop.chisq = F,
           prop.r = F,
           prop.c = F)




# 규칙 학습자(rule learner) 분류기
mushroom = read.csv('mlwr/mushrooms.csv', encoding = 'UTF-8')
str(mushroom)
head(mushroom, 10)
table(mushroom$type)


mushroom$veil_type = NULL
str(mushroom)

# 규칙 분류기 - One Rule 분류기
install.packages('OneR')
library(OneR)

# 모델 훈련
mushroom_1r = OneR(type ~ ., data = mushroom)
mushroom_1r


mushroom_1r_cap = OneR(type ~ cap_shape + cap_surface + cap_color,
                       data = mushroom)
mushroom_1r_cap

summary(mushroom_1r)


# 성능을 개선하기 위해 RIPPER 알고리즘을 사용
install.packages('RWeka')
library(RWeka)

mushroom_ripper = JRip(type ~ ., mushroom)
mushroom_ripper
summary(mushroom_ripper)

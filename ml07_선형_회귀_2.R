# 미국 의료비 데이터
insurance = read.csv('mlwr/insurance.csv')

# 데이터 확인
str(insurance)
## BMI(Body-Mass(체중) Index) = 체중(kg) / [키(m)]^2
summary(insurance)
head(insurance, 5)

# 종속 변수 - expenses(의료비 지출)
boxplot(insurance$expenses)
hist(insurance$expenses) # 오른쪽으로 꼬리가 긴 분포

# 대다수의 사람들의 의료비는 0 ~ 15,000 사이에 분포

# 상관 계수: cor(x, y)
cor(insurance$bmi, insurance$expenses)

# 상관 행렬: 상관 계수들로 만든 행렬
cor(insurance[c('age', 'bmi', 'children', 'expenses')])

pairs(insurance[c('age', 'bmi', 'children', 'expenses')])


install.packages('psych')
library(psych)

pairs.panels(insurance[c('age', 'bmi', 'children', 'expenses')])
pairs.panels(insurance)



# 다중 선형 회귀(multiple linear regression)
# expenses ~ 나머지 모든 변수
ins_model = lm(formula = expenses ~ ., data = insurance)
ins_model
summary(ins_model)

# 선형 회귀 모델을 수정해서 모델 성능 향상
# 나이의 비선형 항을 추가
insurance$age2 = insurance$age^2
head(insurance[c('age', 'age2')], 10)

# 수치형 변수를 이진화 (연속적인 값은 1, 0으로 변환)
# bmi의 값이 30 이상이면 1, 그렇지 않으면 0으로 변환
insurance$bmi30 = ifelse(insurance$bmi >= 30, 1, 0)
head(insurance[c('bmi', 'bmi30')], 10)

# 두 변수 이상의 상호작용을 선형 회귀 모델에 추가
# 흡연+비만
str(insurance)

ins_model2 = lm(formula = expenses ~ age + sex + bmi + children + smoker + region + age2 + bmi30 + smoker*bmi30,
                data = insurance)
summary(ins_model2)

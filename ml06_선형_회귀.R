rm(list = ls())

# 선형 회기(Linear Regression)
# 종속 변수 하나와 독립(설명) 변수들 산의 관계를 수식으로 설명/예측는 방법
# 독립 변수가 한나인 경우 : 단순 , 독립 변수가 여러개인 경우 : 다중
# y = a + bx (a, b 는 상수)
# x : 독립(설명) 변수, y : 종속 변수

heights = read.csv('mlwr/heights.csv')
str(heights)
head(heights, 5)

# 아들의 키가 아버지의 키에 영향을 받는가? (유전)
# 아버지 =  x , 아들 = y
# son: 종속 변수(y축), father: 독립 변수(x축)

# 아버지 키(father), 아들 키(son)의 분포
summary(heights)
hist(heights$father)
hist(heights$son)

boxplot(heights$father)
boxplot(heights$son)

# 산점도 그래프(scatter plot)
plot(heights, col = rgb(0.7, 0.2, 0.2, 0.5))
# rgb(red, green, blue, 불투명도)
abline(h = mean(heights$son))
# abline(): 보조선, h: 수평 보조선
abline(v = mean(heights$father))
# abline(): 보조선, v: 수직 보조선


# lm() 함수: linear regression model(선형 회귀 모델)
lm_heights = lm(formula = son ~ father, data = heights)
lm_heights
# y = a + bx ## y: 종속(아들) , x: 독립(아버지) , a: Intercept(y 절편) , b: 기울기

summary(lm_heights)


# 선형 모델에서 찾은 coefficient(계수)들을 이용해서 선형 모델 그래프를 추가
abline(a = 86.10257, b = 0.51391)


# ggplot2를 이용한 그래프
ggplot(heights, aes(x = father, y = son)) +
  geom_point(color = rgb(0.7, 0.2, 0.2, 0.5)) +
  geom_hline(yintercept = mean(heights$son),
             linetype = 'dashed', color = 'darkblue') +
  geom_vline(xintercept = mean(heights$father),
             linetype = 'dashed', color = 'darkblue') +
  stat_smooth(method = 'lm') +
  theme_bw()


# 선형 회귀 모델식: y = a + bx

# y = a + bx
# a, b 를 결정
# a = y - bx

# a = mean(y) - b * mean(x)
# b = 공분산(x, y) / 분산(x)

m_x = mean(heights$father) # 아버지 키 평균
m_y = mean(heights$son) # 아들 키 평균
cov_xy = cov(heights$father, heights$son) #공분산(covariance)
var_x = var(heights$father) # 분산(variance)
b = cov_xy / var_x
a = m_y - b * m_x

# Pearson's Correlation Coefficient(상관 계수)
cor(heights$father, heights$son)
# -1 <= Corr <= 1
# 피어슨 상관 계수의 절대값이 1에 가까울수록 상관 관계가 높고, 0에 가까울수록 상관 관계가 낮다



# 첼리저호의 사고 조사 데이터
launch = read.csv('mlwr/challenger.csv')
str(launch)
head(launch, 5)
summary(launch)

# 단순 선형 회귀(distress_ct ~ temperature)
plot(x = launch$temperature, y = launch$distress_ct)

lm_launch = lm(formula = distress_ct ~ temperature, data = launch)
summary(lm_launch)
a = lm_launch$coefficients[1] # 선형 모델의 y 절편
a
b = lm_launch$coefficients[2] # 선형 모델의 기울기
b

abline(a = a, b = b, col = 'blue')



# 다중 선형 회귀(multiple linear regression)
# y ~ x1 + x2 + x3 = ...
str(launch)
lm_launch = lm(formula = distress_ct ~ ., data = launch)
summary(lm_launch)


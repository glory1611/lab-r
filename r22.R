# csv 파일에서 데이터 가져오기 및 분석

mydata = read.csv('mlwr/usedcars.csv',
                  stringsAsFactors = F)
# 데이터 프레임의 구조(structure) 확인
str(mydata)

# 데이터 일부 확인
head(mydata)
tail(mydata)

# 기술통계량 확인
summary(mydata)
summary(mydata$price)
summary(mydata[c('price', 'mileage')])

# 최대값과 최솟값 출력
max(mydata$price)
min(mydata$price)
# 범위(range): 최솟갑과 최대값을 출력
range(mydata$price)
# diff = max - min
diff(range(mydata$price))

# IQR(Inter-Quartile Range): 3rd Qu. - 1st Qu.
IQR(mydata$price)
# Quartile: 1/4, 1/2, 3/4 위치의 값들
# 0.25 (25%) 위치: 1사분위(1st quartile)
# 0.5 (50%) 위치: 2사분위(2nd quartile), 중앙값(median)
# 0.75 (75%) 위치: 3사분위(3rd quartile)

# Quantile
quantile(mydata$price)
quantile(mydata$price, probs = c(0.1, 0.9))
quantile(mydata$price, probs = seq(0, 1, 0.25))
quantile(mydata$price, probs = seq(0, 1, 0.1))

# median(): 중앙값(2사분위, 50%위치)
median(mydata$price)
# mean(): 평균
mean(mydata$price)
# 평균, 중앙값, 사분위값들, IQR -> 수치형 데이터의 퍼짐 정도 측정




summary(mydata$price)
boxplot(x = mydata$price)

summary(mydata$mileage)
boxplot(mydata$mileage)


library(ggplot2)

ggplot(mydata, aes(x = '', y = price)) +
  geom_boxplot()

ggplot(mydata, aes(x = '', y = mileage)) +
  geom_boxplot() +
  ylab('주행거리') +
  ggtitle('주행거리 상자 그림') +
  theme_bw()



# 히스토그램(histogram):
# 연속된 수치구간을 일정한 간격으로 나눠서 그 구간에 해당하는 데이터들의 개수(빈도수) 막대로 출력 
hist(mydata$price, breaks = seq(0, 25000, 1000))
hist(mydata$mileage)


ggplot(mydata, aes(x = price)) +
  geom_histogram(bins = 10, color = 'black', fill = 'grey') +
  theme_bw()

ggplot(mydata, aes(x = mileage)) +
  geom_histogram(bins = 8, color = 'black', fill = 'grey') +
  theme_bw()



# 분산(variance), 표준편차(standard deviation)
var(mydata$price)
sd(mydata$price)


# 정규 분포: 평균을 중심으로 양쪽이 대칭인 종 모양의 분포

str(mydata)

# 이산형 데이터(discrete data) 또는 범주형 데이터의 빈도수
table(mydata$year)
table(mydata$model)
table(mydata$color)



# 2개 이상의 변수들 간의 상관 관계
# 중고차의 주행거리(mileage)가 가격(price) 영향
plot(x = mydata$mileage, y = mydata$price)

ggplot(mydata, aes(x = mileage, y = price, color = model)) +
  geom_point() +
  xlab('주행거리(mile)') +
  ylab('가격(USD)') +
  ggtitle('주행거리 vs 가격') +
  theme_bw()




# 이원 교차표(two-way cross table)
# 두개의 명목 변수 간의 관계를 파악

install.packages('gmodels')
library(gmodels)

# mydata 데이터 프레임에 새로운 변수를 추가
mydata$conserve = mydata$color %in% c('Black', 'Gray', 'Silver', 'White')
table(mydata$conserve)
# table(): 빈도수, 도수분포표
# prop.table(): 변수들의 비율을 표로 작성
prop.table(table(mydata$conserve))
prop.table(table(mydata$model))

# CrossTable(x = 행, y = 열)
CrossTable(x = mydata$model, y = mydata$conserve)

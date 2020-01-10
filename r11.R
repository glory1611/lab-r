# ggplot2를 사용한 여러가지 그래프


search()
library(dplyr) # 데이터를 가공/정제하는 패키지
library(ggplot2) # 그래프를 그리기 위한 패키지

# ggplot2::mpg 데이터 프레임
str(mpg)

# 자동차 구동방식(drv)별로 연비 차이?
df_mpg = mpg %>% 
  group_by(drv) %>% 
  summarise(mean_cty = mean(cty))
df_mpg

# 그래프
ggplot(data = df_mpg, aes(x = drv, y = mean_cty)) +
  geom_col() +
  theme_bw()

# x축의 데이터 순서를 y축의 값에 따라서 정렬하면
# 막대 그래프를 크기 순서로 보여줄 수 있음
# reorder(정렬할 데이터, 정렬 기준)
ggplot(df_mpg, aes(x = reorder(drv, mean_cty), y = mean_cty)) +
  geom_col() +
  theme_classic()


# 구동방식 별 고속도로 연비 차이?
df_mpg2 = mpg %>% group_by(drv) %>% summarise(mean_hwy = mean(hwy))s
df_mpg2

ggplot(df_mpg2, aes(x = reorder(drv, mean_hwy), y = mean_hwy)) +
  geom_col() +
  theme_bw() +
  xlab('구동방식') +
  ylab('고속도로 평균 연비')


ggplot(mpg, aes(x = hwy)) +
  geom_bar()

# col 그래프와 bar 그래프 구분
# col 그래프는 요약 표를 만든후 그래프 생성
# bar 그래프는 원자료 이용해서 그래프 생성


# 돗수분포표: table()
table(mpg$class)
ggplot(mpg, aes(x = class)) +
  geom_bar()

ggplot(mpg, aes(x = reorder(class, table(class)[class]))) +
  geom_bar()

# geom_bar(): 변수의 빈돗수를 막대 그래프로 그려주는 함수
# geom_bar()에서 x축에 사용될 수 있는 변수
# 1) 범주형 변수
# 2) 연속전인 변수(숫자) -> 구간을 나워서 구간 안에 포함된 갯수(빈돗수)
#    (예: mpg의 cty, hwy)
# geom_bar()를 사용할 때는 x축 변수만 mapping시키면 됨

# geom_col(): 어떤 변수의 크기를 막대그래프로 그려주는 함수
# 예: 성별에 따른 급여
# geom_col()을 사용할 때는 x축 변수, y축 변수를 모두 mapping시켜야 함.


df = as.data.frame(table(mpg$class))
df
ggplot(df, aes(x = Var1, y = Freq)) +
  geom_col()



# 선 그래프
#ggplot2::economics 데이터 프레임
str(economics)
head(economics)
tail(economics)

# 시간에 따른 실업자수(unemploy)의 변화
ggplot(data = economics, mapping = aes(x = date, y = unemploy)) +
  geom_line()

# 시간에 따른 인구 변화?
str(economics)
ggplot(economics, aes(x = date, y = pop)) +
  geom_line()

# ggplot2::economics 데이터 프레임을 복사
df = as.data.frame(economics)

# df 데이터 프레임에 인구 대비 실업자 비율 변수(unemp_ratio) 추가
df = df %>% mutate(unemp_ratio = (unemploy / pop * 100))
df
ggplot(df, aes(x = date, y = unemp_ration)) +
  geom_line()

# 시간에 따은 개인 저축률(psavert)을 그래프로 작성
ggplot(df, aes(x = date, y = psavert)) +
  geom_line() +
  theme_bw()

# 두개 이상의 선 그래프를 하나의 차트에
# 시간(date)에 따른 실업률과 저축률을 하나의 차트에
ggplot(df, aes(x = date)) + # 공통 데이터/축 설정
  geom_line(aes(y = unemp_ratio), color =  'red') +
  geom_line(aes(y = psavert), color = 'blue') +
  theme_bw()

str(df)

# 실업률과 평균 실업 기간과의 관계가 있을까(?)
ggplot(df, aes(x = date)) +
  geom_line(aes(y = unemp_ratio), color = 'red') +
  geom_line(aes(y = uempmed), color = 'blue')



# boxplot: 기술 통계량들을 한눈에 알아볼 수 있는 그래프
# 최소, 1사분위, 중앙, 3사분위, 최대값

# boxplot$stats의 결과값이 아래 1) ~ 5)
# 1) 아래쪽 수염 : -1.5IQR 값과 이상치가 아닌 데이터의 최솟값 중에서 더 큰 값
# 2) 사각형의 아래쪽 : 데이터들을 정령했을 때 25%에 해당하는 값(1Qu.)
# 3) 사각형 안의 선 : 데이터들을 전령했을 때 50%에 해당하는 값(중앙값)
# 4) 사각형의 위쪽 : 데이터들을 정렬했을 때 75%에 해당하는 값(3Qu.)
# 5) 위쪽 수염 : +1.5IQR 값과 이상치가 아닌 데이터의 최대값 중에서 더 작은 값
# 6) 이상치 : 사각형 +/- 1.5 * IQR을 벗어난 값
# IQR = 3Qu. - 1Qu.

boxplot(mpg$cty)
boxplot(mpg$cty)$stats
summary(mpg$cty)

ggplot(mpg, aes(y = cty)) +
  geom_boxplot() +
  theme_bw()

# cyl별 cty의 boxplot
table(mpg$cyl)

df4 = mpg %>% filter(cyl == 4) #filter(mpg, cyl == 4)
boxplot(df4$cty)

df6 = mpg %>% filter(cyl == 6)
boxplot(df6$cty)


ggplot(mpg, aes(x = as.factor(cyl), y = cty)) +
  geom_boxplot() +
  theme_bw()
# as.factor()으로 cyl를 카테고리화 한다


# drv별 cty의 boxplot
ggplot(mpg, aes(x = drv, y = cty)) +
  geom_boxplot() +
  theme_bw()

table(mpg$drv)


mpg %>% filter(class %in% c('compact', 'subcompact', 'suv')) %>% 
  ggplot(aes(x = class, y = cty)) +
  geom_boxplot()



install.packages('ggthemes')
library(ggthemes)
search()


ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  theme_excel_new()

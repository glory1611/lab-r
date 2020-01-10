# ggplot2 패키지를 사용한 그래프
# grammar of graph
library(ggplot2)
rm(list = ls())
search()

# ggplot2 패키지의 mpg 데이터 프레임 구조 확인
str(mpg)
summary(mpg$displ)
summary(mpg$cty)
boxplot(mpg$cty)$stats
# 자동차 배기량(displ)과 시내주행 연비(cty) 사이의 관계
# 1) 그래프를 그릴 데이터(데이터 프레임), 좌표축, 데이터 설정
g = ggplot(data = mpg, mapping = aes(x = displ, y = cty))
g

# 2) 그래프의 종류 선택
g = g + geom_point()
g

# 3) 옵션 추가
g = g + xlim(3, 6)
g


ggplot(mpg, aes(x = displ, y = cty)) + 
  geom_point() +
  ylim(9, 26)


#
table(mpg$cyl)
ggplot(mpg, aes(x = cyl, y = cty)) +
  geom_point()
# 의미 없는 그래프

ggplot(mpg, aes(x = displ, y = cty, 
                color = as.factor(cyl),
                shape = as.factor(drv))) + 
  geom_point() +
  theme_bw()




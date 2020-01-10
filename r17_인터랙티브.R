rm(list = ls())

install.packages('plotly')

library(dplyr)
library(ggplot2)
library(plotly)
search()

# ggplot2에서 학습용으로 제공하는 mpg 데이터 프레임
str(mpg)

# 배기량(displ), 구동방식(drv)과 고속도로 연비(hwy)와의 관계

g = ggplot(mpg, aes(x = displ, y = hwy, col = drv)) +
  geom_point()

ggplotly(g)




str(diamonds)
# 다이아몬드 도수본포표

g = ggplot(data = diamonds,
           aes(x = cut, fill = clarity)) +
  geom_bar(position = 'dodge')

ggplotly(g)




#####

str(economics)

g = ggplot(economics,
           aes(x = date, y = psavert)) +
  geom_line()

ggplotly(g)



# 시계열 데이터에 대한 기능을
install.packages('dygraphs')

library(xts)

eco_psavert = xts(x = economics$psavert, order.by = economics$date)
str(eco_psavert)
head(eco_psavert)
dygraph(eco_psavert) %>% dyRangeSelector()



economics = ggplot2::economics
economics$unemprt = (economics$unemploy / economics$pop) * 100

head(economics$unemprt)
tail(economics$unemprt)

eco_unemprt = xts(x = economics$unemprt, order.by = economics$date)

str(eco_unemprt)
head(eco_unemprt)

dygraph(eco_unemprt)

# dygraph에서 시계열 그래프 2개 이상 그리려면,
data = cbind(eco_psavert, eco_unemprt)
str(data)
head(data)

dygraph(data)

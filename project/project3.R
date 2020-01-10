rm(list = ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(gmodels)
library(psych)
library(corrplot)
library(caret)
library(ggthemes)
library(neuralnet)
library(rpart)
library(rpart.plot)
library(Cubist)
library(xts)
library(dygraphs)


#########################################################
# 대기 오염 데이터
pm_data_18_1 = read_xlsx('project/data/2018/2018년 1분기.xlsx')
pm_data_18_2 = read_xlsx('project/data/2018/2018년 2분기.xlsx')
pm_data_18_3 = read_xlsx('project/data/2018/2018년 3분기.xlsx')
pm_data_18_4 = read_xlsx('project/data/2018/2018년 4분기.xlsx')

pm_data_19_1 = read_xlsx('project/data/2019/2019년 1월.xlsx')
pm_data_19_2 = read_xlsx('project/data/2019/2019년 2월.xlsx')
pm_data_19_3 = read_xlsx('project/data/2019/2019년 3월.xlsx')
pm_data_19_4 = read_xlsx('project/data/2019/2019년 4월.xlsx')

# 날씨 데이터
weather = read.csv('project/data/2018/2018기상청.csv')
weather_19 = read.csv('project/data/2019/2019기상청.csv')

#########################################################
str(pm_data_18_3)
pm_data_18_3 = pm_data_18_3[-2]
pm_data_18_4 = pm_data_18_4[-2]

pm_data_19_1 = pm_data_19_1[-2]
pm_data_19_2 = pm_data_19_2[-2]
pm_data_19_3 = pm_data_19_3[-2]
pm_data_19_4 = pm_data_19_4[-2]

str(pm_data_18_3)
#########################################################
# 서울 데이터 추출

head(pm_data_18_1, 10)
str(pm_data_18_1)

pm_seoul = rbind(pm_data_18_1, pm_data_18_2)
pm_seoul = rbind(pm_seoul, pm_data_18_3)
pm_seoul = rbind(pm_seoul, pm_data_18_4)
pm_seoul = rbind(pm_seoul, pm_data_19_1)
pm_seoul = rbind(pm_seoul, pm_data_19_2)
pm_seoul = rbind(pm_seoul, pm_data_19_3)
pm_seoul = rbind(pm_seoul, pm_data_19_4)

str(pm_seoul)

######
######

weather_a = rbind(weather, weather_19)
str(weather_a)
#########################################################
pm_jongro = pm_seoul %>% 
  filter(측정소코드 == 111123)

str(pm_jongro)
pm_jongro = as.data.frame(pm_jongro)
#########################################################

pm_jongro = pm_jongro %>% rename('time' = '측정일시',
                                 'region' = '지역',
                                 'loc_id' = '측정소코드',
                                 'loc' = '측정소명',
                                 'address' = '주소')

weather_a = weather_a %>% rename('time' = '일시',
                                 'w_office' = '지점',
                                 'w_temp' = '기온..C.',
                                 'w_rain' = '강수량.mm.',
                                 'w_wspeed' = '풍속.m.s.',
                                 'w_wdir' = '풍향.16방위.',
                                 'w_hum' = '습도...',
                                 'w_vprprssr' = '증기압.hPa.',
                                 'w_athmprsssr' = '현지기압.hPa.')

#########################################################
#########################################################
# 종로구 필터, NA 제거, 주소 

pm_jongro = na.omit(pm_jongro)

pm_jongro = pm_jongro[-11]
str(pm_jongro)

#########################################################
# join을 위한 시간 통일

x = strptime(pm_jongro$time, format = "%Y%m%d%H")

pm_jongro$time = format(x, format = "%Y%m%d%H")
str(pm_jongro)


#####

y = strptime(weather_a$time, format = "%Y-%m-%d %H")

weather_a$time = format(y, format = "%Y%m%d%H")
str(weather_a)



#####
# 강수 NA -> 0
summary(weather_a)
weather_a$w_rain = ifelse(is.na(weather_a$w_rain), 0, weather_a$w_rain)

# NA 제거
weather_a = na.omit(weather_a)
summary(weather_a)

#########################################################
str(pm_jongro)
str(weather_a)

# 데이터 합치기
df = left_join(pm_jongro, weather_a, by = 'time')
head(df, 10)
summary(df)

#########################
# NA 제거
df = na.omit(df)

#########################

time = data.frame(time = df$time)
str(time)
time = as.numeric(paste(time$time))

#########################################################

head(df, 10)

#########################################################

#########################
# region, loc_id, loc, time, w_office 제거
str(df)
df = df[c(-1, -2, -3, -4, -11)]
df_2 = df[c(-1, -2, -3, -11)]

#########################################################
#########################################################
#########################################################
str(df)
summary(df)

# 데이터 정규화

normalize = function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

df_norm = as.data.frame(lapply(df, normalize))
str(df_norm)
summary(df_norm)

# pairs.panels(df_norm)

#########################################################
# 훈련 데이터 만들기

df_norm_t = cbind(time, df_norm)
str(df_norm_t)

#########################################################

df_cor = cor(df_norm_t, method = c("pearson", "kendall", "spearman"))
summary(df_cor)

corrplot(df_cor, method = 'shade', type = 'upper')

#####
time2 = data.frame(time = strptime(df$time, format = "%Y%m%d%H"))
str(time2)

df_2$time = time2$time
str(df_2)

eco_time_PM10 = xts(x = df_2$PM10, order.by = df_2$time)
eco_time_PM25 = xts(x = df_2$PM25, order.by = df_2$time)

eco_time_PM = cbind(eco_time_PM10, eco_time_PM25)

dygraph(eco_time_PM) %>% dyRangeSelector()



#########################################################
d_train = df_norm_t %>% filter(time < 2019010101) %>%  select(-time, -PM25)
d_test = df_norm_t %>% filter(time >= 2019010101) %>%  select(-time, -PM25)

str(d_train)
str(d_test)
#########################################################
#########################################################
#########################################################
# 모델 훈련

# 1
set.seed(1231)
d_model = neuralnet(PM10 ~ ., d_train)
plot(d_model)

# 2
set.seed(1231)
d_model2 = neuralnet(PM10 ~ ., d_train,
                     hidden = 3)
plot(d_model2)

# 3
set.seed(1231)
d_model3 = neuralnet(PM10 ~ ., d_train,
                     hidden = c(2, 4, 2),
                     stepmax = 1e7)
plot(d_model3)

# 9
set.seed(1231)
d_model9 = neuralnet(PM10 ~ ., d_train,
                     hidden = c(10, 5, 3),
                     stepmax = 1e8)

#####

# 4
set.seed(1231)
d_part = rpart(formula = PM10 ~ ., data = d_train)
rpart.plot(x = d_part, digits = 3)

d_cubist = cubist(d_train[-5], d_train$PM10)
d_cubist


#########################################################
# 모델 테스트

# 1
d_result = compute(d_model, d_test)
d_predict = d_result$net.result

# 2
d_result2 = compute(d_model2, d_test)
d_predict2 = d_result2$net.result

# 3
d_result3 = compute(d_model3, d_test)
d_predict3 = d_result3$net.result

# 9
d_result9 = predict(d_model9, d_test)
d_predict9 = d_result9

#####

# 4
d_predict4 = predict(d_part, d_test)
summary(d_predict4)

d_predict4_cubist = predict(d_cubist, d_test[-5])


#########################################################
# 모델 결과

# 1 (1)
cor(d_predict, d_test$PM10) # 0.6790394

# 2 (3)
cor(d_predict2, d_test$PM10) # 0.7702157

# 3 (4, 3)
cor(d_predict3, d_test$PM10) # 0.7581541

# 9
cor(d_predict9, d_test$PM10) # 0.7628366


#####

MAE = function(actual, predict) {
  return(mean(abs(actual - predict)))
}

# 4
cor(d_predict4, d_test$PM10) # 0.6104711
MAE(d_predict4, d_test$PM10) # 0.0671309

cor(d_predict4_cubist, d_test$PM10) # 0.5998596
MAE(d_predict4_cubist, d_test$PM10) # 0.06556541
#########################################################

















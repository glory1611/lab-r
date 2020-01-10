rm(list = ls())
devtools::install_github("dpastoor/PKPDdatasets")
install.packages('e1071')
install.packages('pROC')


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
library(tidyverse)
library(PKPDdatasets)
library(class)
library(C50)
library(e1071)
library(pROC)

#########################################################
# 대기 오염 데이터


pm_data_19_1 = read_xlsx('project/data/2019/2019년 1월.xlsx')
pm_data_19_2 = read_xlsx('project/data/2019/2019년 2월.xlsx')
pm_data_19_3 = read_xlsx('project/data/2019/2019년 3월.xlsx')
pm_data_19_4 = read_xlsx('project/data/2019/2019년 4월.xlsx')

# 날씨 데이터
weather_19 = read.csv('project/data/2019/2019기상청.csv')

#########################################################
str(pm_data_19_1)

pm_data_19_1 = pm_data_19_1[c(-2, -3, -4, -12)]
pm_data_19_2 = pm_data_19_2[c(-2, -3, -4, -12)]
pm_data_19_3 = pm_data_19_3[c(-2, -3, -4, -12)]
pm_data_19_4 = pm_data_19_4[c(-2, -3, -4, -12)]

str(pm_data_19_1)
#########################################################
# 서울 데이터 추출

head(pm_data_19_1, 10)
str(pm_data_19_1)

pm_seoul = rbind(pm_data_19_1, pm_data_19_2, pm_data_19_3, pm_data_19_4)
pm_seoul = rbind(pm_seoul, pm_data_19_3)
pm_seoul = rbind(pm_seoul, pm_data_19_4)

str(pm_seoul)

######

#########################################################
#pm_jongro = pm_seoul %>% 
#  filter(측정소코드 == 111123)

str(pm_seoul)
pm_seoul = as.data.frame(pm_seoul)
#########################################################

pm_seoul = pm_seoul %>% rename('time' = '측정일시',
                                 'region' = '지역',
                                 'loc' = '측정소명',
                               'loc_id' = '측정소코드')

weather_19 = weather_19 %>% rename('time' = '일시',
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

pm_seoul = na.omit(pm_seoul)
str(pm_seoul)

pm_seoul = pm_seoul %>% filter(region %in% c('서울 중구', '서울 용산구', '서울 종로구', '서울 중구 청', '서울 광진구', 
                                             '서울 성동구', '서울 중랑구', '서울 동대문구', '서울 성북구', '서울 도봉구', 
                                             '서울 은평구', '서울 서대문구', '서울 마포구', '서울 강서로', '서울 강서구', 
                                             '서울 구로구', '서울 영등포구', '서울 동작구', '서울 관악구', '서울 강남구', 
                                             '서울 서초구', '서울 송파구', '서울 강동구', '서울 금천구', '서울 강북구', 
                                             '서울 양천구', '서울 노원구'))
str(pm_seoul)
x = format(pm_seoul$time, format = "%Y%m%d%H")
x = as.POSIXct(x, format = "%Y%m%d%H")
pm_seoul$time = x
str(pm_seoul)


#########################################################
# join을 위한 시간 통일

y = as.POSIXct(weather_19$time, format = "%Y-%m-%d %H")
weather_19$time = y

str(weather_19)


#####
# 강수 NA -> 0
summary(weather_19)
weather_19$w_rain = ifelse(is.na(weather_19$w_rain), 0, weather_19$w_rain)

# NA 제거
weather_19 = na.omit(weather_19)
summary(weather_19)

#########################################################
str(pm_seoul)
str(weather_19)

# 데이터 합치기
df = left_join(pm_seoul, weather_19, by = 'time')
head(df, 10)
summary(df)

#########################
# NA 제거
df = na.omit(df)
str(df)
df$PM10_t = ifelse(df$PM10 <= 30, 1,
                   ifelse(df$PM10 <= 80, 2,
                          ifelse(df$PM10 <= 150, 3,
                                 ifelse(df$PM10 > 150, 4, 0))))

df$PM10_t = factor(df$PM10_t,
                   levels = c(1:4),
                   labels = c('good', 'fair', 'poor', 'very poor'))
#########################################################

head(df, 10)

#################

ggplot(df, aes(region, PM10, color = region)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# PM10
ggplot(df, aes(time, PM10, group = region, color = region)) +
  geom_line() +
  facet_wrap(~ region) + 
  theme_bw()

# CO
ggplot(df, aes(time, CO, group = region, color = region)) +
  geom_line() +
  facet_wrap(~ region) +
  theme_bw()

eco_time_PM10 = xts(x = df$PM10, order.by = df$time)
dygraph(eco_time_PM10) %>% dyRangeSelector()

#########################################################

# region, loc_id, loc, time, w_office 제거
str(df)
df_2 = df[c(-1, -2, -3, -4, -11)]
region_time = data.frame(region = df$region, time = df$time)
str(region_time)

#########################################################
#########################################################
#########################################################
str(df_2)
summary(df_2)

# 데이터 정규화

normalize = function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

df_norm = as.data.frame(lapply(df_2[-14], normalize))
str(df_norm)
summary(df_norm)

#####

df_cor = cor(df_norm, method = c("pearson", "kendall", "spearman"))
summary(df_cor)

corrplot(df_cor, method = 'shade', type = 'upper')

#########################################################
# 훈련 데이터 만들기

df_norm_2 = cbind(region_time, df_norm, PM10_t = df_2$PM10_t)
str(df_norm_2)

#########################################################

str(df)

ggplot(df, aes(w_temp, PM10)) + 
  geom_point(color = 'black') +
  stat_smooth(method = lm, se = TRUE, color = "red") +
  theme_bw()


#########################################################
str(df_norm_2)
df_norm_2$time = format(df_norm_2$time, format = "%Y%m%d%H")
df_norm_2$region = as.numeric(df_norm_2$region)

#d_train = df_norm_2 %>% filter(time < 2019020101) %>% select(-region, -time, -w_athmprsssr)
#d_test = df_norm_2 %>% filter(time >= 2019020101) %>% select(-region, -time, -w_athmprsssr)
str(d_train)
str(d_test)

d_train_2 = df_norm_2 %>% filter(time < 2019020101) %>% select(-time, -w_athmprsssr)
d_test_2 = df_norm_2 %>% filter(time >= 2019020101) %>% select(-time, -w_athmprsssr)
str(d_train_2)
str(d_test_2)

d_train_2_data = d_train_2[c(-6, -14)]
d_train_2_label = factor(d_train_2$PM10_t)
d_test_2_data = d_test_2[c(-6, -14)] 
d_test_2_label = factor(d_test_2$PM10_t)

str(d_train_2)
summary(d_train_2)

str(d_train_2_data)
str(d_train_2_label)
head(d_train_2_label, 10)

dim(d_train_2_data)
dim(d_train_2_label)
dim(d_test_2_data)
dim(d_test_2_label)

#########################################################
#########################################################
#########################################################
# 모델 훈련

# 1 NN
set.seed(1231)
d_model = neuralnet(PM10 ~ ., d_train,
                    stepmax = 1e8)
plot(d_model)

#####

# 4 Regression Tree
set.seed(1231)
d_part = rpart(formula = PM10 ~ ., data = d_train)
rpart.plot(x = d_part, digits = 3)

# 5 Cubist
d_cubist = cubist(d_train[-6], d_train$PM10)
d_cubist

#####

# 6 Decision Tree
d_C50 = C5.0(d_train_2_data, d_train_2_label, trials = 10)


#########################################################
# 모델 테스트

# 1
d_result = compute(d_model, d_test) 
d_predict = d_result$net.result


#####

# 4
d_predict4 = predict(d_part, d_test)
summary(d_predict4)

# 5
d_predict4_cubist = predict(d_cubist, d_test[-6])


#####

# 6
d_predict6_C50 = predict(d_C50, d_test_2_data)

# 7
d_predict6_knn = knn(train = d_train_2_data,
                     test = d_test_2_data,
                     cl = d_train_2_label,
                     k = 7)

str(d_train_2_data)
str(d_train_2_label)
dim(d_train_2_data)
summary(d_train_2_label)
#########################################################
# 모델 결과

# 1 (1)
cor(d_predict, d_test$PM10) # 0.8889669


#####

MAE = function(actual, predict) {
  return(mean(abs(actual - predict)))
}

# 4
cor(d_predict4, d_test$PM10) # 0.8501756
MAE(d_predict4, d_test$PM10) # 0.05268023

# 5
cor(d_predict4_cubist, d_test$PM10) # 0.6639973
MAE(d_predict4_cubist, d_test$PM10) # 0.07063454

# 6
CrossTable(d_test_2_label, d_predict6_C50, prop.chisq = F)
table(d_predict6_C50)
table(d_test_2_label)

# 7
CrossTable(d_test_2_label, d_predict6_knn, prop.chisq = F)

#########################################################

#########################################################
# 

# 1 (1)

region_time_test = df_norm_2 %>% filter(time >= 2019020101) %>% select(region, time)
region_time_test$time = as.POSIXct(region_time_test$time, format = "%Y%m%d%H")
region_time_test$region = factor(region_time_test$region,
                                 levels = c(1:27),
                                 labels = c('서울 중구', '서울 용산구', '서울 종로구', '서울 중구 청', '서울 광진구', 
                                            '서울 성동구', '서울 중랑구', '서울 동대문구', '서울 성북구', '서울 도봉구', 
                                            '서울 은평구', '서울 서대문구', '서울 마포구', '서울 강서로', '서울 강서구', 
                                            '서울 구로구', '서울 영등포구', '서울 동작구', '서울 관악구', '서울 강남구', 
                                            '서울 서초구', '서울 송파구', '서울 강동구', '서울 금천구', '서울 강북구', 
                                            '서울 양천구', '서울 노원구'))

str(region_time_test)

df_compare = data.frame(region = region_time_test$region,
                        time = region_time_test$time,
                        test = d_test$PM10, 
                        predict = d_predict)
str(df_compare)

ggplot(df_compare, aes(x = time, group = region)) +
  geom_line(aes(y = df_compare$test), color = 'blue') +
  geom_line(aes(y = df_compare$predict), color = 'red') +
  facet_wrap(~ region) +
  labs(x = '월', y = 'R = 예측, B = PM10') +
  theme_bw()




confusionMatrix(d_test_2_label, d_predict6_knn,
                positive = 'very poor')

df_roc_knn = multiclass.roc(response = df_roc_knn$test,
                            predictor =  df_roc_knn$predict)
plot(sms_roc)

str(d_predict6_knn)
str(d_test_2_label)

df_roc_knn = data.frame(test = d_test_2_label, predict = d_predict6_knn)
str(df_roc_knn)


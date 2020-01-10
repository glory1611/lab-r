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


#########################################################
# 대기 오염 데이터
pm_data_18_1 = read_xlsx('project/data/2018년 1분기.xlsx')
pm_data_18_2 = read_xlsx('project/data/2018년 2분기.xlsx')
pm_data_18_3 = read_xlsx('project/data/2018년 3분기.xlsx')
pm_data_18_4 = read_xlsx('project/data/2018년 4분기.xlsx')

# 날씨 데이터
weather = read.csv('project/data/2018기상청.csv')

#########################################################
str(pm_data_18_3)
pm_data_18_3 = pm_data_18_3[-2]
pm_data_18_4 = pm_data_18_4[-2]
str(pm_data_18_3)
#########################################################
# 서울 데이터 추출

head(pm_data_18_1, 10)
str(pm_data_18_1)
pm_seoul_18_1 = pm_data_18_1 %>% 
  filter(측정소코드 %in% c(111121, 111122, 111123, 111124, 111125, 111131, 111141, 111142, 111143, 111151,
                           111152, 111154, 111161, 111162, 111171, 111181, 111191, 111201, 111202, 111212, 
                           111213, 111221, 111231, 111232, 111241, 111242, 111251, 111261, 111262, 111263,
                           111264, 111273, 111274, 111275, 111281, 111291, 111301, 111311, 111312))
str(pm_seoul_18_1)
pm_seoul_18_2 = pm_data_18_2 %>% 
  filter(측정소코드 %in% c(111121, 111122, 111123, 111124, 111125, 111131, 111141, 111142, 111143, 111151,
                      111152, 111154, 111161, 111162, 111171, 111181, 111191, 111201, 111202, 111212, 
                      111213, 111221, 111231, 111232, 111241, 111242, 111251, 111261, 111262, 111263,
                      111264, 111273, 111274, 111275, 111281, 111291, 111301, 111311, 111312))
pm_seoul_18_3 = pm_data_18_3 %>% 
  filter(측정소코드 %in% c(111121, 111122, 111123, 111124, 111125, 111131, 111141, 111142, 111143, 111151,
                      111152, 111154, 111161, 111162, 111171, 111181, 111191, 111201, 111202, 111212, 
                      111213, 111221, 111231, 111232, 111241, 111242, 111251, 111261, 111262, 111263,
                      111264, 111273, 111274, 111275, 111281, 111291, 111301, 111311, 111312))
pm_seoul_18_4 = pm_data_18_4 %>% 
  filter(측정소코드 %in% c(111121, 111122, 111123, 111124, 111125, 111131, 111141, 111142, 111143, 111151,
                      111152, 111154, 111161, 111162, 111171, 111181, 111191, 111201, 111202, 111212, 
                      111213, 111221, 111231, 111232, 111241, 111242, 111251, 111261, 111262, 111263,
                      111264, 111273, 111274, 111275, 111281, 111291, 111301, 111311, 111312))
pm_seoul_18 = rbind(pm_seoul_18_1, pm_seoul_18_2)
pm_seoul_18 = rbind(pm_seoul_18, pm_seoul_18_3)
pm_seoul_18 = rbind(pm_seoul_18, pm_seoul_18_4)

str(pm_seoul_18)

#########################################################

pm_seoul_18 = pm_seoul_18 %>% rename('time' = '측정일시',
                     'region' = '지역',
                     'loc_id' = '측정소코드',
                     'loc' = '측정소명',
                     'address' = '주소')

weather = weather %>% rename('time' = '일시',
                             'w_office' = '지점',
                             'w_temp' = '기온..C.',
                             'w_rain' = '강수량.mm.',
                             'w_wspeed' = '풍속.m.s.',
                             'w_wdir' = '풍향.16방위.',
                             'w_hum' = '습도...',
                             'w_hPa' = '증기압.hPa.')

#########################################################
# 종로구 필터, NA 제거, 주소 

pm_seoul_18 = na.omit(pm_seoul_18)

pm_jongro = pm_seoul_18%>% 
  filter(loc_id == 111123)

pm_jongro = pm_jongro[-11]
str(pm_jongro)

#########################################################
# join을 위한 시간 통일

x = strptime(pm_jongro$time, format = "%Y%m%d%H")

pm_jongro$time = format(x, format = "%Y%m%d%H")
str(pm_jongro)

#####

y = strptime(weather$time, format = "%Y-%m-%d %H")

weather$time = format(y, format = "%Y%m%d%H")
str(weather)

#####
# 강수 NA -> 0
summary(weather)
weather$w_rain = ifelse(is.na(weather$w_rain), 0, weather$w_rain)

# NA 제거
weather = na.omit(weather)
summary(weather)

#########################################################
# 데이터 합치기
df = left_join(pm_jongro, weather, by = 'time')
head(df, 10)
summary(df)

# NA 제거
df = na.omit(df)

# region, loc_id, loc, time, w_office 제거
str(df)
df = df[c(-1, -2, -3, -4, -11)]

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

pairs.panels(df_norm)

#########################################################
# 훈련 데이터 만들기

trainIndex = createDataPartition(df_norm$PM10, p = 0.8, list = F, times = 1)
d_train = df_norm[trainIndex, ]
d_test = df_norm[-trainIndex, ]

str(d_train)
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
                     hidden = c(4, 3))
plot(d_model3)


#########################################################
# 모델 테스트

# 1
d_result = compute(d_model, d_test)
d_predict = d_result$net.result

# 2
d_result2 = compute(d_model2, d_test)
d_predict2 = d_result2$net.result

# 3
d_result3 = compute(d_model2, d_test)
d_predict3 = d_result3$net.result

#########################################################
# 모델 결과

# 1 (1)
cor(d_predict, d_test$PM10) # 0.8900616

# 2 (3)
cor(d_predict2, d_test$PM10) # 0.8997349

# 3 (4, 3)
cor(d_predict3, d_test$PM10) # 0.9244547

#########################################################




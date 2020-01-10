rm(list = ls())

# Clustering(군집합)

teens = read.csv('mlwr/snsdata.csv')


str(teens)
summary(teens)
head(teens, 5)

table(is.na(teens$gender))
table(teens$gender, useNA = 'ifany')


# female 변수를 데이터 프레임에 추가
# 성별이 F이고 NA가 아니면 1 그렇지 않으면 0을 입력

teens$female = ifelse(teens$gender == 'F' & !is.na(teens$gender), 1, 0)
table(teens$female, useNA = 'ifany')


# nogender 변수를 데이터 프레임에 추가
teens$nogender = ifelse(is.na(teens$gender), 1, 0 )
table(teens$nogender, useNA = 'ifany')


# age 변수
summary(teens$age)
# age의 정상 범위는 13 ~19 라고 가정 -> 이외의 값들은 NA
teens$age = ifelse(teens$age >= 13 & teens$age <= 19,
                   teens$age, NA)
head(teens$age, 10)
summary(teens$age)


# age의 NA 값들을 grage 별 age 의 평균값으로 대체
# dplyr 패키지 이용
library(dplyr)

teens %>% 
  group_by(gradyear) %>% 
  filter(!is.na(age)) %>% 
  summarise(mean(age))

teens %>% 
  group_by(gradyear) %>% 
  summarise(mean(age, na.rm = T))


# 그룹별 평균(또는 임의 함수)를 적용해서 벡터를 리턴하는 함수
# stats::ave(평균을 계산할 벡터, 그룹핑 변수, FUN = mean)

df = data.frame(class = c(1, 1, 1, 2, 2),
                score = c(10, 9, NA, 9, 8))
df
ave(df$score, df$class, FUN = mean)

my_mean = function(x) {
  mean(x, na.rm = T)
}
ave(df$score, df$class, FUN = my_mean)


ave_age = ave(teens$age, teens$gradyear, FUN = my_mean)
head(ave_age, 5)
tail(ave_age, 5)

teens$age = ifelse(is.na(teens$age), ave_age, teens$age)
summary(teens$age)



# 모델 생성
# k-평균 군집합 알고리즘

str(teens)
# 개인 식별 정보(gradyear, gender, age, friends)를 제외하고,
# 오로지 관심사들로만 clustering을 시도

interests = teens[5:40]
str(interests)

normalize = function(x) {
  (x - min(x)) / (max(x) - min(x))
}

norn_interests = as.data.frame(lapply(interests, normalize))
summary(norn_interests)

#####################
#####################
set.seed(2345)

teen_clusters = kmeans(interests, 5)
str(teen_clusters)
str(teen_clusters$cluster)
table(teen_clusters$cluster)

# 모델이 분류한 클러스터가 어떤 특징들을 갖고 있을까?
teens$cluster = teen_clusters$cluster
teens[1:10, c('cluster', 'gender', 'age', 'friends')]

teen_clusters$centers

summary(teen_cluster)


######################
######################
teen_clsut_norm = kmeans(norn_interests, 5)
str(teen_clsut_norm)
teen_clsut_norm$centers
teens$cluster_norm = teen_clsut_norm$cluster
teens[1:10, c('cluster', 'cluster_norm', 'gender', 'age', 'friends')]


library(ggplot2)
norn_interests$cluster = teen_clsut_norm$cluster

ggplot(norn_interests$cluster, aes(x = god, y = church)) +
       geom_point(color = 'cluster')




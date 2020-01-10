# 결측치(Missing Value , NA) 처리

# 결측치(NA)가 있는 데이터 프레임 생성
df = data.frame(gender = c('M', 'F', NA,'M', 'F'),
                score = c(5, 4, 3, NA, 6))
df
# 데이터 프레임의 구조(structure)
str(df)

# 데이터 프레임의 기술 통계량(summary)
summary(df)

# is.na(): NA는 T, NA가 아니면 F를 리턴하는 함수
is.na(df) # 데이터 프레임의 각 원소의 NA여부를 출력
is.na(df$gender)
table(is.na(df))

df %>% filter(!is.na(gender))

df %>%
  group_by(gender) %>% 
  summarise((mean = mean(score)))
  
df %>%
  filter(!is.na(gender) & !is.na(score)) %>% 
  group_by(gender) %>% 
  summarise((mean = mean(score)))

df
mean(df$score, na.rm = T)

df %>%
  group_by(gender) %>% 
  summarise((mean = mean(score, na.rm = T)))

# table(): 돗수분포표
table(is.na(df$gender))
table(is.na(df))

# 결측치를 대체할 때
# 1) 0으로 대체
df$score = ifelse(is.na(df$score), 0, df$score)
df
df %>% group_by(gender) %>% summarise(mean = mean(score))

# 2) 평균값으로 대체
df[4, 2] = NA
df
# NA를 제외한 평균값
avg = mean(df$score, na.rm = T)
avg
df$score = ifelse(is.na(df$score), avg, df$score)
df
df %>% group_by(gender) %>% summarise(mean = mean(score))



# 이상치: 논리적으로 들어갈 수 없는 값. 있어서는 안될 값.
movie_rating = data.frame(rating = c(5, 4, 3, 4, 5, 3, 10))
movie_rating
mean(movie_rating$rating)
# 영화 별점은 1 ~ 5점까지만 가능하다고 한다면, 10점은 있어서는 안될 값.
movie_rating$rating =
  ifelse(movie_rating$rating %in% c(1:5), movie_rating$rating, NA)
movie_rating
mean(movie_rating$rating, na.rm = T)




# outlier
mpg = ggplot2::mpg
mpg
summary(mpg$hwy)
boxplot(mpg$hwy)
boxplot(mpg$hwy)$stats
# stats[1, 1]: 아랫쪽 수염의 위치
# stats[1, 5]: 윗쪽 수염의 위치
# boxplot에서 수염을 벗어난 데이터들을 outlier라고 하고,
# outlier들을 NA로 처리하고 통계 처리

mean(mpg$hwy) # 23.44017 - outlier가 포함된 값

# outlier들을 NA로 처리
mpg$hwy = ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)

# NA의 갯수 확인
table(is.na(mpg$hwy))
mean(mpg$hwy, na.rm = T) # 23.18615 - outliew가 제거된 값


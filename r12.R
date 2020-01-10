# 복지 패널 데이터

rm(list = ls())

# 통계 전용 프로그램 SPSS에서 만들어진 sav 파일을 읽어서
# 데이터 프레임을 생성하려면 foreign 패키지가 필요
install.packages('foreign')

# 분석에 필요한 패키지들을 건색 경로에 로드
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
search()


# SPSS에서 사용하는 sav 파일을 R에서 사용할 수 있는 
# data.frame 타입으로 변환
raw_welfare = read.spss(file = 'data/Koweps_hpc10_2015_beta1.sav', to.data.frame = T)
# read.spss() 사용시 to.data.frame 파라미터를 T로 변경

# 원본 데이터 프레임을 복사
welfare = raw_welfare

# 데이터 프레임 구조 확인
str(welfare)
table(welfare$h10_g3) # 성별
as.data.frame(table(welfare$h10_g4)) # 태어난 연도
table(welfare$h10_g10) # 혼인 상태
table(welfare$h10_reg7)

# 변수 이름들을 분석하기 쉽게 변경
# rename(데이터프레임, 바꿀이름 = 원래 변수 이름)
# h10_g3: 성별 => gender
# h10_g4: 태어난 연도 => birth
# h10_g10 : 혼인 여부 => marriage
# h10_g11 : 종교 => religion
# h10_eco9 : 직종코드 => code_job
# p1002_8aq1 : 월 급여 => income
# h10_reg7 : 7개 권역(지역, 서울/수도권/경남/...) => code_region

welfare = welfare %>% 
  rename(gender = h10_g3,
         birth = h10_g4,
         marriage = h10_g10,
         religion = h10_g11,
         code_job = h10_eco9,
         income = p1002_8aq1,
         code_region = h10_reg7)

# 성별에 따른 월급 차이?
table(welfare$gender) #성별 도수분포표
# 성별 변수에는 이상치가 없다.
# 만약에 이상치가 있는 경우에는 이상치를 NA로 처리
welfare$gender = ifelse(welfare$gender %in% c(1, 2),
                        welfare$gender, NA)

# 성별은 직적 변수(qualitative variable):
# 1, 2라는 숫자(의 크기)가 중요한 것이 아니라, 남/여 구분이 중요
# gender 변수를 factor로 만듦
welfare$gender = factor(welfare$gender,
                        levels = c(1, 2),
                        labels = c('남자', '여자'))
table(welfare$gender)
class(welfare$gender)

ggplot(welfare, aes(x = gender, fill = gender)) +
       geom_bar()
# 막대에 색을 입히기 위해서는 fill 사용


# 월 급여(income)
class(welfare$income)
# income 변수의 데이터 타입은 숫자 -> 양적 변수(quantitive variable)
# income 변수의 요약 정보
summary(welfare$income)
# income 변수의 값이 9999인 경우 할달 급여를 응답하지 않은 경우임
# -> 이상치 처리를 해야함
welfare$income = ifelse(welfare$income > 0 & welfare$income < 9999,
                        welfare$income, NA)
summary(welfare$income)

# 급여의 분포
ggplot(welfare, aes(x = income)) +
  geom_bar(binwidth = 50) +
  xlim(0, 1000)


# 성별 평균 한달 급여
welfare %>% 
  group_by(gender) %>% 
  summarise(income_by_gender = mean(income, na.rm = T))

income_by_gender = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(gender) %>% 
  summarise(mean = mean(income))

ggplot(income_by_gender, aes(x = gender, y = mean, fill = gender)) +
  geom_col()

#
ggplot(welfare, aes(x = gender, y = income)) +
  geom_boxplot()


# 나이(연령)에 따른 급여
# 태어난 연도 도수분포표
table(welfare$birth)
# NA 여부 확인
table(is.na(welfare$birth))

# welfare 데이터 프레임에 age(나이) 변수 추가
welfare$age = 2015 - welfare$birth
table(welfare$age)
summary(welfare$age)

# 연령대 분포(나이 도수분포표를 막대 그래프로)
ggplot(welfare, aes(x = age)) + 
  geom_bar()

income_by_age = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean = mean(income))
income_by_age

ggplot(income_by_age, aes(x = age, y = mean)) +
  geom_line()


# 연령대별 성별 평균 급여 그래프
df_gender_age_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, gender) %>% 
  summarise(mean = mean(income))

summary(df_gender_age_income)
head(df_gender_age_income)
tail(df_gender_age_income)

ggplot(df_gender_age_income, aes(x = age, y = mean, color = gender)) +
  geom_line()


# 연령대별 평균 급여
# 연령대를 30세 미만(young), 60세 미만(middle), 60세 이상(old) 구간으로 나누기
welfare = welfare %>% mutate(ageg = ifelse(age < 30, 'young',
                                           ifelse(age < 60,  'middle', 'old')))

# ageg 변수를 질적 변수(순서형 변수, ordinal)로 만듦.
welfare$ageg = factor(welfare$ageg,
                      levels = c('young', 'middle', 'old'),
                      ordered = T) # factor 순서 지정


welfare$ageg

table(welfare$ageg)

ggplot(welfare, aes(x = ageg)) +
  geom_bar()

# 연령대별 평균 급여 테이블
income_by_ageg = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean = mean(income))

ggplot(income_by_ageg, aes(x = ageg, y = mean, fill = ageg)) +
  geom_col()

ggplot(income_by_ageg, aes(x = ageg, y = mean, fill = ageg)) +
  geom_col() +
  scale_x_discrete(limits = c('young', 'middle', 'old')) # 막대 순서


# 연령대별, 성별 평균 급여
income_by_ageg_gender = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, gender) %>% 
  summarise(mean = mean(income))

income_by_ageg_gender

ggplot(income_by_ageg_gender, aes(x = ageg, y = mean, fill = gender)) +
  geom_col(position = 'dodge') + # 막대 분리
  theme_bw()

# welfare 데이터 프레임에 age_range 변수 추가
# age < 20 이면 'age10',  age < 30 이면 'age20', ...
# age < 80 이면 'age70' 그 이외엥는 'age80'

welfare = welfare %>% 
  mutate(age_range = ifelse(age < 20, 'age10',
                            ifelse(age < 30, 'age20',
                                   ifelse(age < 40, 'age30',
                                          ifelse(age < 50, 'age40',
                                                 ifelse(age < 60, 'age50',
                                                        ifelse(age < 70, 'age60',
                                                               ifelse(age < 80, 'age70', 'age80+'))))))))
table(welfare$age_range)
welfare$age_range

ggplot(welfare, aes(x = age_range)) +
  geom_bar(fill = 'pink') +
  theme_classic()

income_by_age_range = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age_range) %>% 
  summarise(mean = mean(income))

ggplot(income_by_age_range, aes(x = age_range, y = mean)) +
  geom_col(fill = 'skyblue') +
  theme_classic()

income_by_age_range_gender = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age_range, gender) %>% 
  summarise(mean = mean(income))

ggplot(income_by_age_range_gender, aes(x = age_range, y = mean)) +
  geom_col(position = 'dodge')

ggplot(income_by_age_range_gender, aes(x = age_range, y = mean, fill = gender)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(values = c('skyblue', 'pink')) +
  xlab('나이') +
  ylab('평균 월급') +
  theme_classic()



# 직업별 급여의 차이
class(welfare$code_job)
head(welfare$code_job)
tail(welfare$code_job)
summary(welfare$code_job)




##
##
# 
df_jobs = read_excel('data/Koweps_Codebook.xlsx', sheet = 2)
head(df_jobs)  
str(df_jobs)


# welfare, df_jobs 데이터 프레임을 join
welfare = left_join(welfare, df_jobs, by = 'code_job')

welfare %>% 
  select(code_job, job) %>% 
  head(10)  

welfare %>% 
  select(code_job, job) %>% 
  tail(10)
  
table(welfare$code_job)

job_top10 = welfare %>% 
  filter(!is.na(code_job)) %>% 
  group_by(job) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(10)

job_top10

ggplot(job_top10, aes(x = reorder(job, -count), y = count)) +
  geom_col() +
  coord_flip() +
  theme_bw()



# 남자들이 가장 많이 종사하는 직종 10개를 찾아서 그래프
job_male_top10 = welfare %>%
  filter(!is.na(job) & gender == '남자') %>% 
  group_by(gender, job) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(10)

ggplot(job_male_top10, aes(x = reorder(job, -count), y = count, fill = gender)) +
  geom_col(fill = 'skyblue') +
  coord_flip() +
  xlab('직종') +
  ylab('인원') +
  theme_classic()

# 여자들이 가장 많이 종사하는 직종 10개를 찾아서 그래프
job_female_top10 = welfare %>%
  filter(!is.na(job) & gender == '여자') %>% 
  group_by(gender, job) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(10)

ggplot(job_female_top10, aes(x = reorder(job, -count), y = count, fill = gender)) +
  geom_col(fill = 'pink') +
  coord_flip() +
  xlab('직종') +
  ylab('인원') +
  theme_classic()

######

# 데이터 가공을 해왔던 welfare 데이터 프레임을
# R에서 사용하는 데이터 파일 형식(rda)으로 저장
save(welfare, file = 'data/welfare.rda')

df = load('data/welfare.rda')

load('data/welfare.rda')
str(welfare)

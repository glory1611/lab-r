# 복지 패널 데이터 분석

search()

# 저장해둔 R 데이터 파일을 로드 -> 변수 자동 생성
load('data/welfare.rda')

# 데이터 프레임 확인
str(welfare)

table(is.na(welfare$job))

# 직업별 평균 소득 분석
income_by_job = welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

income_by_job

# 평균 소득 상위 10개 직종

top_10 = income_by_job %>% 
  arrange(-mean_income) %>% 
  head(10)

top_10

ggplot(top_10, aes(x = reorder(job, -mean_income), y = mean_income), fill = mean_income) +
  geom_col(fill = 'skyblue') + 
  coord_flip() +
  xlab('상위 10개 직종') +
  ylab('평균 급여') +
  theme_classic()


# 평균 소득 하위 10개 직종
bottom_10 = income_by_job %>% 
  arrange(mean_income) %>% 
  head(10)

bottom_10

ggplot(bottom_10, aes(x = reorder(job, mean_income), y = mean_income), fill = mean_income) +
  geom_col(fill = 'pink') + 
  coord_flip() +
  xlab('하위 10개 직종') +
  ylab('평균 급여') +
  theme_classic()





# 직종별 인구수가 30명 이상인 직종에 대해서
# 평균 소득 상위 10갸 직공을 찾고, 그래프 작성

income_by_job2 = welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(n = n(), mean_income = mean(income)) %>% 
  filter(n >= 30) %>% 
  arrange(-mean_income) %>% 
  head(10)
  

income_by_job2

ggplot(income_by_job2, aes(x = reorder(job, mean_income), y = mean_income), fill = mean_income) +
  geom_col(fill = 'skyblue') +
  coord_flip() +
  xlab('상위 10개 직종') +
  ylab('평균 급여') +
  theme_classic()


# 지역별 연령대 비율
# 지역(1 ~ 7 권역) 변수 확인 - NA 없음
table(welfare$code_region)

# 연령대(ageg, age_range)
table(welfare$ageg)
table(welfare$age_range)

# 지역 코드와 해당 지역 이름 데이터 프레임 생성

regions = data.frame(
  code_region = c(1:7),
  region = c('서울', '수도권/인천/경기', '부산/울산/경남', '대구/경북', '대전/충남', '강원/충북', '광주/전남/전북/제주')
)

regions

welfare = left_join(welfare, regions, by = 'code_region')
table(welfare$region)

# 지역별, 연령대별 인구 수

region_ageg = welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pct = (n / total) * 100)

region_ageg


ggplot(region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip()


# 지역별, 나이별 인구 수

region_age_range = welfare %>% 
  group_by(region, age_range) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pct = (n / total) * 100)

region_age_range

ggplot(region_age_range, aes(x = region, y = pct, fill = age_range)) +
  geom_col(position = 'dodge') +
  scale_x_discrete(limits = c('서울', '수도권/인천/경기', '부산/울산/경남', '대구/경북', '대전/충남', '강원/충북', '광주/전남/전북/제주')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



######
table(welfare$religion)
welfare$religion = factor(welfare$religion,
                          levels = c(1, 2),
                          labels = c('있음', '없음'))
table(welfare$religion)
class(welfare$religion)
##
table(welfare$marriage)
class(welfare$marriage)
as.factor(welfare$marriage)

welfare$marriage_divorce = ifelse(welfare$marriage == 1, '결혼',
                                  ifelse(welfare$marriage == 3, '이혼', NA))

table(welfare$marriage_divorce)
class(welfare$marriage_divorce)

religion_marriage_divorce = welfare %>% 
  filter(!is.na(marriage_divorce)) %>% 
  group_by(religion, marriage_divorce) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pct = (n / total) * 100)

religion_marriage_divorce

ggplot(religion_marriage_divorce, aes(x = religion, y = pct, fill = marriage_divorce)) +
  geom_col()

religion_divorce = religion_marriage_divorce %>% 
  filter(marriage_divorce == '이혼')

religion_divorce

ggplot(religion_divorce, aes(x = religion, y = pct, fill = pct)) +
  geom_col(fill = 'skyblue') +
  xlab('종교') +
  ylab('%') +
  theme_classic()

######
ageg_marriage_divorce = welfare %>% 
  filter(!is.na(marriage_divorce)) %>% 
  group_by(ageg, religion, marriage_divorce) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pct = n / total * 100 ) %>% 
  filter(marriage_divorce == '이혼')
  
ageg_marriage_divorce

ggplot(ageg_marriage_divorce, aes(x = ageg, y = pct, fill = religion)) +
  geom_col(position = 'dodge')

######
age_range_marriage_divorce = welfare %>% 
  filter(!is.na(marriage_divorce)) %>% 
  group_by(age_range, religion, marriage_divorce) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pct = (n / total) * 100) %>% 
  filter(marriage_divorce == '이혼')

age_range_marriage_divorce

ggplot(age_range_marriage_divorce, aes(x = religion, y = pct, fill = age_range)) +
  geom_col(position = 'dodge') +
  theme_classic()



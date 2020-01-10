mpg = as.data.frame(ggplot2::mpg)

str(mpg)

# (1)
disp4 = mpg %>% filter(displ <= 4) 
disp5 = mpg %>% filter(displ >= 5) 

mean(disp4$hwy)
mean(disp5$hwy)

# (2)
audi = mpg %>% filter(manufacturer == 'audi')
toyota = mpg %>% filter(manufacturer == 'toyota')

mean(audi$cty)
mean(toyota$cty)

# (3)





mpg_2 = mpg %>% select(class, cty)
mpg_2 %>% head()


mpg %>% select(class) %>% filter(class %in% c('suv', 'compact'))

suv = mpg_2 %>% filter(class == 'suv')
mean(suv$cty)

com = mpg_2 %>% filter(class == 'compact')
mean(com$cty)





mpg %>% filter(manufacturer == 'audi') %>% arrange(-hwy) %>% head(5)




mpg %>% mutate(mpg1 = cty + hwy, mpg2 = mpg1 / 2) %>% arrange(-mpg2) %>% head(3)









#• Q1. mpg 데이터의 class 는 "suv", "compact" 등 자동차를 특징에 따라 일곱 종류로 분류한 변수입니다.
#어떤 차종의 연비가 높은지 비교해보려고 합니다. class 별 cty 평균을 구해보세요.
#• Q2. 앞 문제의 출력 결과는 class 값 알파벳 순으로 정렬되어 있습니다. 어떤 차종의 도시 연비가 높은지
#쉽게 알아볼 수 있도록 cty 평균이 높은 순으로 정렬해 출력하세요.
#• Q3. 어떤 회사 자동차의 hwy(고속도로 연비)가 가장 높은지 알아보려고 합니다. hwy 평균이 가장 높은 회사
#세 곳을 출력하세요.
#• Q4. 어떤 회사에서 "compact"(경차) 차종을 가장 많이 생산하는지 알아보려고 합니다. 각 회사별
#"compact" 차종 수를 내림차순으로 정렬해 출력하세요.







mpg %>% group_by(class) %>% summarise(mean_cty = mean(cty))

mpg %>% group_by(class) %>% summarise(mean_cty = mean(cty)) %>% arrange(-mean_cty)

mpg %>% group_by(manufacturer) %>% summarise(mean_hwy = mean(hwy)) %>% arrange(-mean_hwy) %>% head(3)

mpg %>% 
  filter(class == 'compact') %>% 
  group_by(manufacturer) %>% 
  mutate(n_compact = ifelse(class == 'compact', 1, 0)) %>% 
  summarise(sum_comp = sum(n_compact)) %>% 
  arrange(desc(sum_comp))

mpg %>% 
  filter(class == 'compact') %>% 
  group_by(manufacturer) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))



mpg %>% group_by(manufacturer, class) %>% summarise(count = n()) %>% dim


fuel_price = data.frame(fl = c('c', 'd', 'e', 'p', 'r'),
                        price_fl = c('2.35', '2.38', '2.11', '2.76', '2.22'))

left_join(mpg, fuel_price, by = 'fl') %>% select(model, fl, price_fl) %>% head(5)
left_join(mpg, fuel_price, by = 'fl') %>% select(model, fl, price_fl) %>% tail(5)



str(midwest)


midwest = ggplot2::midwest

midwest = midwest %>% 
  mutate(popyoung = (poptotal - popadults) / poptotal * 100)

midwest %>% 
  arrange(desc(popyoung)) %>% 
  select(county, popyoung) %>% 
  head(5)

midwest = midwest %>% mutate(grade = ifelse(popyoung >= 40, 'large',
                                  ifelse(popyoung >= 30, 'middle', 'small')))

midwest %>% select(popyoung, grade) %>% head(20)

table(midwest$grade)
midwest %>% group_by(grade) %>% summarise(n())


midwest = midwest %>% mutate(p_asian = popasian / poptotal * 100)

midwest %>% select(state, county, p_asian) %>% arrange(p_asian) %>% head(10)
midwest %>% select(state, county, p_asian) %>% arrange(-p_asian) %>% head(10)




                                                                               
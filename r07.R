# dplyr 패키지를 사용한 데이터 전처리(가공)

# 패키지, 라이브러리: 여러 기능(함수) 또는 데이터를 모아둔 꾸러미

# install.packages('dplyr') 
search() # 메모리에 로딩된 패키지 이름들을 확인
library(dplyr) # #메모리(라이브러리)를 메모리에 로딩

rm(list = ls())


# csv 파일을 읽어서 데이터 프레임 생성
exam = read.csv('data/csv_exam.csv')
exam
# 생성된 데이터 프레임의 구조(structure)를 확인
str(exam)
summary(exam)

# 1반 학생들의 모든 정보를 출력
# filter(데이터프레임, 검색조건)
filter(exam, class == 1)

# dplyr 패키지의 모든 함수는 첫번째 파라미터가 데이터 프레임
# 변수 이름들은 데이터 프레임 이름을 생략하고 사용하면 됨

# 1반, 2반 학생들의 정보 출력
filter(exam, class == 1 | class == 2)
filter(exam, class %in% 1:2)
filter(exam, class %in% c(1, 2))

# 3반이 아닌 학생들의 정보 출력
filter(exam, class != 3)

# 수학 점수가 60점 이상이 학생들 출력
filter(exam, math >= 60)


# dplyr 패키지의 함수들을 호출하는 방법
# 1) function(data_frame, ... )
# 2) data_frame %>% function(...) # pipe 연산자 %>% (Ctrl + Shit + M)

# pipe 연산자를 사용한 filter() 함수 호출
exam %>% filter(class == 1)

# 1반 학생들 중에서 수학 점수가 평균 이상인 학생들을 출력
exam %>% filter(class == 1 & math >= mean(math))

# filter 함수의 결과를 변수에 저장 -> 데이터 프레임
# 1반 학생들로만 이루어진 데이터 프레임
class1 = exam %>%  filter(class == 1)
str(class1)
# 1반 학생들의 각 과목 평균 점수
mean(class1$math)
mean(class1$english)
mean(class1$science)




# R에서 사용하는 기호들 
# 논리 연산자 기능 

# < 작다 
# <= 작거나 같다 
# > 크다 
# >= 크거나 같다 
# == 같다 
# != 같지 않다 
# │ 또는 
# & 그리고 
# %in% 매칭 확인


# R에서 사용하는 기호들 
# 산술 연산자 기능 

# + 더하기 
# - 빼기 
# * 곱하기 
# / 나누기 
# ^ , ** 제곱 
# %/% 나눗셈의 몫 
# %% 나눗셈의 나머지



# 데이터 프레임에서 변수를 선택하는 함수 - dplyr::select()
# 1) select(data_frame, columns)
# 2) data_frame %>%  select(coumns)

# exam 데이터 프레임에서 수학 점수만 선택
exam %>% select(math)
# id와 math 변수를 선택
exam %>%  select(id, math)


# class 변수를 제외한 모든 변수를 선택
exam %>% select(id, math, english, science) # 선택할것
exam %>% select(-class) # 제외할것


# 1반 학생들의 id, math를 출력
# select id, math from exam where class = 1;
# 1) 1반 학생들만 있는 데이터 프레임
class1= exam %>% filter(class == 1)
class1
class1 %>% select(id, math)

exam %>% filter(class == 1) %>% select(id, math)

exam_sub = exam %>%  select(id, math, class)
exam_sub
exam_sub %>%  filter(class == 1)

exam %>% select(id, class, math) %>% filter(class == 1)


# math, english, science를 출력, 앞에 있는 6건만 출력
exam %>% select(math, english, science) %>% head()
exam %>% head() %>% select(math, english, science)


# 정렬: dplyr::arrange(data_frame, 정렬 기준 컬럼들)
exam %>% arrange(math) # 정렬 기본 방식은 오름차순 정렬
exam %>% arrange(desc(math)) # 내림차순 정렬을 할때는 desc()함수 이용
exam %>%arrange(-math) %>% head()

# 2개 이상의 변수로 정렬
# class 순서 -> math 점수 순서
exam %>% arrange(class, math)
exam %>% arrange(class, desc(math))


# 새로운 변수(컬럼) 만들기:
# data_frame$new_var = 식; --> 데이터 프레임이 변경됨
exam2 = exam
exam2$total = exam2$math + exam2$english + exam2$science
dim(exam2)
exam2

# dplyr::mutate(data_frame, new_var = 식) 
# 원본 데이터 프레임은 수정되지 않고, 컬럼이 추가된 새로운 데이터 프레임이 리턴(반환)
exam %>% mutate(total = math + english + science)
exam3 = exam %>% mutate(total = math + english + science)


# exam 데이터 프레임에서 
# 수학, 영어, 과학 점수의 합계를 total
# 3과목 점수의 평균을 average
# 변수를 추가한 결과를 출력
exam %>% mutate(total = math + english + science, average = total / 3)
exam %>% mutate(total = math + english + science, average = round(total / 3, 2))

# 과학점수 60점 이상이면 "pass", 60점 미만이면 "fail"
# 값을 갖는 컬럼(변수) test 를 추가
exam %>% mutate(test = ifelse(science >= 60, 'pass', 'fail'))

#
exam %>% mutate(test = ifelse((math + english + science) / 3 >= 60 & (math & english & science >= 50), 'pass', 'fail'))


summary(exam) # 기술 통계량을 출력
# dplyr::summarise(), dplyr::summarize()
# 벡터를 스칼라로 만들어줌 - 통계 함수들을 적용하기 위해서

exam %>% summarise(m_math = mean(math))

exam %>% summarise(mean = mean(math), sd = sd(math))
mean(exam$math) # 평균
sd(exam$math) # 표준편차(standard deviation)

# 반별로 수학 점수의 평균
exam %>% group_by(class) %>% summarise(mean_math = mean(math))
# 반별로 수학, 영어, 과학 점수의 평균
exam %>% group_by(class) %>% summarise(mean_math = mean(math), mean_eng = mean(english), mean_sci = mean(science))


exam %>% group_by(class) %>% summarise(mean_math = mean(math), mean_eng = mean(english), mean_sci = mean(science)) %>% arrange(-mean_eng)


mpg %>% 
  group_by(manufacturer, drv) %>% 
  summarise(mean = mean(cty)) %>% 
  head(10)

mpg %>% 
  group_by(drv, manufacturer) %>% 
  summarise(mean = mean(cty)) %>% 
  head(10)


# dplyr::n()
# summarise, filter, mutate 함수 안에서 사용
# 갯수를 러턴하는 함수
# Oracle에서 Count 함수와 비슷한 역할
# select count(*) from emp;

mpg %>% summarise(n())
mpg %>% summarise(count = n())

# 제조사별 자동차 모델 갯수
mpg %>% group_by(manufacturer) %>% summarise(count = n())

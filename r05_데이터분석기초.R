# 데이터 분석 기초

# cvs 파일을 읽어서 데이터 프레임 생성

exam = read.csv('data/csv_exam.csv')

# 데이터 프레임의 모든 데이터들을 출력
exam

# Viewer창에 데이터들을 출력
View(exam)

# 데이터 프레임에서 처음에 나오는 일부 데이터만 출력
head(exam)
head(exam, n = 3)

# 데이터 프레임에서 마지막에 나오는 일부 데이터만 출력
tail(exam)
tail(exam, n = 3)

# 데이터 프레임 차원(dimension): 행(관측지)과 열(변수)의 갯수
dim(exam)
dimension = dim(exam)
dimension[1] # 데이터 프레임의 행의 갯수
dimension[2] # 데이터 프레임의 열의 갯수

dim(exam)[1]


# 데이터 프레임의 구조(structure)를 확인
str(exam)
# 'data.frame':	20 obs. of  5 variables:
#  $ id     : int  1 2 3 4 5 6 7 8 9 10 ...
#  $ class  : int  1 1 1 1 2 2 2 2 3 3 ...
#  $ math   : int  50 60 45 30 25 50 80 90 20 50 ...
#  $ english: int  98 97 86 98 80 89 90 78 98 98 ...
#  $ science: int  50 60 78 58 65 98 45 25 15 45 ...


# 요약 통계량(기술 통계량, descriptive statistics) 출력
summary(exam)
# id            class        math          english        science     
# Min.   : 1.00   Min.   :1   Min.   :20.00   Min.   :56.0   Min.   :12.00  
# 1st Qu.: 5.75   1st Qu.:2   1st Qu.:45.75   1st Qu.:78.0   1st Qu.:45.00  
# Median :10.50   Median :3   Median :54.00   Median :86.5   Median :62.50  
# Mean   :10.50   Mean   :3   Mean   :57.45   Mean   :84.9   Mean   :59.45  
# 3rd Qu.:15.25   3rd Qu.:4   3rd Qu.:75.75   3rd Qu.:98.0   3rd Qu.:78.00  
# Max.   :20.00   Max.   :5   Max.   :90.00   Max.   :98.0   Max.   :98.00  

# Min: 최솟값
# 1rst Quartile: 1사분위 값
# Median : 중앙값
# Mean: 평균 
# 3rd Quartile: 3사분위 값
# Max: 최대값


###########################

# ggplot2 패키지에는 mpg 데이터 프레임이 있습니다.
# as.data.frame(변수):
# 변수가 데이터 프레임이면, 데이터 프레임을 반환
# 변수가 데이터 프레임이 아니고, 데이처 프레임으로 변환 가능하면,
# 데이터 프레임을 생성해서 리턴

mpg = as.data.frame(ggplot2::mpg)
# mpg 데이터 프레임에 차원(행/열 갯수)
dim(mpg)

# mpg 데이터 프레임에 구조
str(mpg)

# mpg 데이터 프레임에서 데이터 일부만 출력
head(mpg)
tail(mpg)

# 기술 통계량
summary(mpg)


###########################

# 데이터 가공(조작)을 위한 패키지 설치
install.packages('dplyr')
# 설치란 패키지를 검색 경로(메모리)에 로드
library(dplyr)

# 검색 경로에 로딩된 패키지 확인
search()


# 변수 이름 바꾸기
df = data.frame(v1 = c(1, 2,3 ),
                v2 = c(11, 22, 33))
df
 # 변수의 이름들을 의미가 있는 이름을 바꿔서
# 나중에 데이터 분석을 할 때 편리하게 함
# rename(데이터프레임, 바꿀변수이름 = 원래변수이름)
# dplyr::rename 함수는 변수 이름들을 바꾼 새로운 데이터 프레임 생성해서 반환(리턴)
# 원본 데이터 프레이믕ㄹ 변경하는 것은 아님

rename(df, id = v1, score = v2)
df # rename() 호출 후에도 df는 원본 그대로 유지됨

# 원본 데이터 프레임을 변경하고 싶을 때
df = rename(df, id = v1, score = v2)
df



# 새로운 변수(컬럼) 추가
# 데이터프레임이름$추가할변수 = 벡터(값)
df_score = data.frame(id = c(1, 2, 3),
                      math = c(90, 80, 100),
                      kor = c(70, 90, 80))
df_score
# 총점 변수(컬럼)을 추가
df_score$total = df_score$math + df_score$kor
df_score



# mpg 데이터 프레임에서 
# 시내 주행 연비(cty) 고속도로 주행 연비(hwy)의 평균을 계산해서 mpg에 avg_mpg 변수를 추가

mpg$avg_mpg = (mpg$cty + mpg$hwy) / 2
head(mpg)
tail(mpg)

# 새로 작성된 평균 연비 변수(컬럼)의 기술 통계량
summary(mpg$avg_mpg)

# 조건 함수(ifelse) 이용
# 위에서 계산된 평균 연비(avg_mpg)의 값이
# 30 이상이면, 1등급
# 20 이상, 30 미만이면, 2등급
# 10 이상, 20 미만이면, 1등급

mpg$grade = ifelse(mpg$avg_mpg >= 30, 1, 
                 ifelse(mpg$avg_mpg >= 20, 2, 3))
head(mpg)
tail(mpg)
# 1, 2, 3 등급 차량들의 등급
table(mpg$grade) # 도수분포표 (frequency table)
hist(mpg$grade) # 히스토그램


# ifelse(조건식,
#        조건식이 참일 때 반환할 값,
#        조건이 거짓일 때 반환할 값)
x = 0
result = ifelse(x > 0, 'positive', 'negative')

result


result2 = ifelse(x > 0, 'positive', 
                 ifelse(x == 0, 'zero', 'negative'))
result2

# ifelse() 함수는 스칼라와 벡터 모두 적용 가능
df = data.frame(id = c(1, 2, 3, 4, 5),
                score = c(100, 95, 88, 73, 61))
df
df$grade = ifelse(df$score > 80, 'Pass', 'Fail')
df
df$grade2 = ifelse(df$score > 90 , 'A',
                   ifelse(df$score > 80 , 'B',
                          ifelse(df$score > 70, 'C',
                                 ifelse(df$score > 60, 'D', 'F'))))
df                   

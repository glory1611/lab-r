# 데이터 합치기

# 1) 가로로 합치기 - 컬럼을 이어 붙이기
#    관계형 데이터 베이스(Oracle)처럼 여러 테이블에 나눠져 있는 데이터들을 join해서 하나의 데이터 프레임으로 만드는 것
     
# 2 세로로 합치기 - 행을 이어 붙이기
#   어떤 데이터가 행의 갯수가 너무 많아서 여러개의 파일로 쪼개져 있을 결우, 다시 하나의 데이터 프레임으로 합치는 것

# join
# 중간고사 성적
midterm = data.frame(id = c(1, 2, 3), mid = c(99, 88, 100))
final = data.frame(id = c(1, 2, 3), final = c(95, 95, 50))

# 두개의 데이터 프레임을 join해서 하나의 데이터 프레임으로 생성
total = left_join(midterm, final, by = 'id')
total

# join의 종류들
df1 = data.frame(id = c(1, 2, 3, 4), var1 = c(11, 22, 33, 44))
df1
df2 = data.frame(id = c(1, 2, 3, 5), var2 = c(50, 60, 70, 80))
df2
# inner join:
inner_join(df1, df2, by = 'id')
# left join:
left_join(df1, df2, by = 'id')
# right join:
right_join(df1, df2, by = 'id')
# full join:
full_join(df1, df2, by = 'id')

# NA(Not Available): 값이 없음

# csv 파일을 읽어서 데이터 프레임 생성
exam = read.csv('data/csv_exam.csv')
head(exam)

# 각 반 선생님 이름을 저장하고 있는 데이터 프레임
teachers = data.frame(class = c(1:5), teacher = c('aa','bb','cc','dd','ee'))
teachers
left_join(exam, teachers, by = 'class')



# 데이터 프레임을 세로로 합치기(행 추가하기)
group1 = data.frame(id = c(1, 2, 3), data = c(10, 20, 30))
group1
group2 = data.frame(id = c(4, 5, 6, 7, 8), data = seq(40, 80, 10))
group2

# group1과 group2를 세로로 이어 붙이기
group_total = bind_rows(group1, group2)
group_total





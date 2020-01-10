# 연관 규칙(Associate Rules) 학습

# 1. 데이터 준비
groceries = read.csv('mlwr/groceries.csv')
str(groceries)
head(groceries)
# 변수 이름이 없느 파일

groceries = read.csv('mlwr/groceries.csv', header = F)
str(groceries)
head(groceries, 10)

# csv 파일의 각 행에는 영수증의 구매 아이템들이 있음
# 영수증마다 구매 앙쳄의 갯수가 다르게 갬에,
# 컬럼의 갯사일정하시 않음

# -> 해결 방버비 sparse matrix(희소 행렬)을 사용


# arules 패키지: associoation rules(연관 규칙)
install.packages('arules')
library(arules)

# 장바구니 영수증 데이터를 회소 행렬로 만듦
groceries = read.transactions('mlwr/groceries.csv',
                              header = F,
                              sep = ",")

# read.transaction 함수에서
# header 파라미터의 기본값은 F
# sep 파라미터의 기본 값은 "" 이기 때문에, 바느시 "," 를 전달해야 함

str(groceries)
summary(groceries)

# 각 영수증 출력
inspect(groceries)
inspect(groceries[1:3])
inspect(groceries[, 1:3])


# 영수증에 등장하는 아이템들의 빈도(frequency)
itemFrequency(groceries[, 1:5])
itemFrequency(groceries)


# 거래 아이템들의 빈도 분포
itemFrequencyPlot(groceries, support = 0.1)
  # support: 영수증에 아이템이 나타나는 횟수
  # 최소 10% 이상 나타나는 아이템들만 그래프에 표시
itemFrequencyPlot(groceries, topN = 20)
  # topN: 그래프에 표시할 빈도 순위, 상위 N개

# 회소 행렬(sparse matrix)를 그래프로 표시
image(groceries[1:200, ], )
  # 데이터의 이상치나 경향을 파악할 수도 있다.


# 3. 모델 학습 - 자율(비지도) 학습의 한 종류인 a priori 알고리즘
grocery_rules = apriori(data = groceries)
summary(grocery_rules)
  # 만들어진 규칙이 없음
  # apriori 함수의 임계값ㅂ 파라미터 기본값들이 
  # support = 0.1(10%), confidence = 0.8(80%)로 되어 있는데,
  # 이 경우 순준이 너무 높에서 연관 규칙을 만들 수 없음

# 하루에 10번 판매되는 아이템 * 30일
300/9835
grocery_rules2 = apriori(data = groceries,
                       parameter = list(support = 0.03,
                                        confidence = 0.25,
                                        minlen = 2)
                       )
summary(grocery_rules2)
inspect(grocery_rules2)


grocery_rules3 = apriori(data = groceries,
                         parameter = list(support = 0.006,
                                          confidence = 0.25,
                                          minlen = 2)
                        )
summary(grocery_rules3)
inspect(grocery_rules3[460:463])


inspect(sort(grocery_rules3, by = 'lift')[1:10])
    
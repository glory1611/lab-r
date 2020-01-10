# Ch 3. k-NN 알고리즘

# 위스콘신 유방암 데이터 준비
wbcd = read.csv('data/wisc_bc_data.csv', stringsAsFactors = F)
str(wbcd)
head(wbcd)
tail(wbcd)
table(wbcd$diagnosis)

# 암인지 아닌지 구별할 때 필요하지 않은 환자 아이디(id)는 데이터 프레임에서 제외해서 거리 계산에 사용되지 않도록 함
wbcd = wbcd[-1] # 첫번째 컬럼(변수)를 제외한 모든 컬럼 선택
str(wbcd)

# 진단 결과(diagnosis) 컬럼(변수)을 범주로 만듦(factor)
wbcd$diagnosis = factor(wbcd$diagnosis, 
                        c('B', 'M'),
                        c('양성', '악성'))
str(wbcd$diagnosis)

# 양성(B)/악성(M) 종양의 갯수/비율
table(wbcd$diagnosis)
prop.table(table(wbcd$diagnosis))

# 각 변수들의 요약 정보(기술통계량) 확인
str(wbcd)
summary(wbcd[c('radius_mean', 'area_mean', 'smoothness_mean')])
# 각 변수들의 단위가 다르기 때문에, 거리를 계산할때 차지하는 비율이 서로 다르게 됨.
# -> 정규화 또는 표준화 필요

# 최소-최대 정규화 함수 정의
# gkatndlfma = function(파라미터이름) { 함수가 수행할 코드 }
normalize = function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# 정규화 함수 테스트
v1 = c(1, 2, 3, 4, 5)
normalize(v1)
v2 = c(10, 20, 30, 40, 50)
normalize(v2)
v3 = c(0.1, 0.3, 0.5, 0.9, 0.01)
normalize(v3)

# 모든 변수들이 정규화된 데이터 프레임으로 변환
str(wbcd)
# 첫번째 컬럼은 진단결과 이기 때문에 정규화 대상이 아님(숫자가 아님)
# lapply(데이터프레임, 함수이름)
# 데이터 프레임의 각 컬럼들을 차례로 함수의 매개변수로 전달
wbcd_n = as.data.frame(lapply(wbcd[2:31], normalize))

# 정규화 적용 결과 확인
summary(wbcd_n[c('radius_mean', 'area_mean', 'smoothness_mean')])


# 정규화된 데이터 프레임을 학습 데이터 세트와 테스트 데이터 세트로 나눔
head(wbcd$diagnosis, 10)
tail(wbcd$diagnosis, 10)

wbcd_train = wbcd_n[1:469, ] # 학습(훈련) 데이터 세트 ,, [행, 열]
# k-NN 알고리즘에게 문제(종양 데이터)와 정답(B/ M)을 알려줄 데이터
head(wbcd_train)

wbcd_test = wbcd_n[470:569, ] # 테스트 데이터 세트
# k-NN 알고리즘이 어느정도의 정확도를 갖는지 테스트라기 위한 데이터

# 학습 데이터/테스트 데이터의 진단 정보를 가지고 있는 벡터(정답)
wbcd_train_label = wbcd[1:469, 1] # 학습 데이터 정답지
table(wbcd_train_label)

wbcd_test_label = wbcd[470:569, 1] # 테스트 데이터 정답지
table(wbcd_test_label)

# k-NN 알고리즘을 구현한 패키지를 설치
install.packages('class')
library(class)

# class::knn(학습데이터, 테스트데이터, 학습데이터의 정답, k값)
# k값은 학습 데이터 개수의 제곱근
sqrt(469)
predict = knn(train = wbcd_train,
              test = wbcd_test,
              cl = wbcd_train_label,
              k = 21)
str(predict)
table(predict)
table(wbcd_test_label)


# 실제 진단 결과와 예측 결과의 차이를 분석
# -> 교차 이원표(Cross Table)
library(gmodels)

CrossTable(x = wbcd_test_label, y = predict, prop.chisq = F)


# k값을 변화시키면서 k-NN 알고리즘을 평가
predict = knn(train = wbcd_train,
               test = wbcd_test,
               cl = wbcd_train_label,
               k = 5)
CrossTable(x = wbcd_test_label, y = predict, prop.chisq = F)


# 변수들을 전규화하는 대신에 표준화를 할 때 예측 정확도?
wbcd_z = as.data.frame(scale(wbcd[-1]))
str(wbcd_z)
summary(wbcd_z[c('radius_mean', 'area_mean', 'smoothness_mean')])

# z-score 표준화가된 데이터 프레임을 학습 데이터/데스트 데이터 세트로 나눔
train_data = wbcd_z[1:469, ] # 학습(훈련) 데이터
test_data = wbcd_z[470:569, ] # 테스트 데이터
train_label = wbcd[1:469, 1]
test_label = wbcd[470:569, 1]

predict = knn(train_data,
              test_data,
              train_label,
              k = 5)
CrossTable(x = test_label, y = predict, prop.chisq = F)

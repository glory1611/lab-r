rm(list = ls())
# Naive Bayse를 이용한 스팸 SMS 분류

#데이터 준비
sms_raw = read.csv('mlwr/sms_spam.csv', stringsAsFactors = F, encoding = 'UTF-8')
str(sms_raw)
head(sms_raw)

table(sms_raw$type)
sms_raw$type = factor(sms_raw$type,
                      c('spam', 'ham'),
                      c('spam', 'ham')
                      )
str(sms_raw)

table(sms_raw$type)
prop.table(table(sms_raw$type))

install.packages('tm')
library(tm)
search()

# SMS 메시지들의 전집(corpus)을 생성
sms_corpus = VCorpus(VectorSource(sms_raw$text))

# corpus 확인
print(sms_corpus)

sms_corpus[[1]]
sms_corpus[[1]]$content
sms_corpus[[1]]$meta

# 모든 문자 메세지를 소문자로 변환
# corpus 객체의 content만 소문자로 변환
# 변환 함수가 tm 패키지에 없는 경우에는 
# tm_map(corpus, content_transformer(함수)) 형식으로 호출
# 변환 함수가 tm에 있는 경우에는 
# tm_map(corpus, 함수) 형식으로 호출

sms_corpus_clean = tm_map(sms_corpus, content_transformer(tolower))
sms_corpus_clean[[1]]
sms_corpus_clean[[1]]$content


# 숫자들 제거
sms_corpus_clean = tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean[[3]]$content

# stopwprds(a, an, the, to, ...)들을 제거
stopwords()
sms_corpus_clean = tm_map(sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean[[1]]$content
sms_corpus_clean[[1072]]$content

# punctuation(구두점, 문장부호) 제거
sms_corpus_clean = tm_map(sms_corpus_clean, removePunctuation)
sms_corpus_clean[[1072]]$content

# 단어나 구두점 등을 제거하면서 생긴 추가적인 공백들을 제거
sms_corpus_clean = tm_map(sms_corpus_clean, stripWhitespace)
sms_corpus_clean[[1072]]$content

# 형태소 분석
install.packages("SnowballC")
library(SnowballC)
wordStem(c('learn', 'learned', 'learns', 'learning'))
wordStem(c('play', 'played', 'plays', 'playing'))

sms_corpus_clean = tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean[[1072]]$content

# DTM(Document-Term Matrix): 문서-단어 행렬 
# 행(row): Document - 문자 메시지, 이메일
# 열(column): 모든 문서에서 추출한 단어들 

sms_dtm = DocumentTermMatrix(sms_corpus_clean)
str(sms_dtm)


# DTM을 사용해서 학습 데이터 세트(75%)와
# 테스트 데이터 서트(25%)로 나눔

sms_dtm_train = sms_dtm[1:4169, ]
sms_dtm_test = sms_dtm[4170:5559, ]

# 학습 데이터 레이블, 테스트 데이터 레이블(spam/ham)
sms_train_label = sms_raw[1:4169, 1]
sms_test_label = sms_raw[4170:5559, 1]

table(sms_train_label)
table(sms_test_label)

# DTM은 희소 행렬(sparse matrix):
# 행렬의 대부분의 원소들의 값이 0이고, 0이 아닌 값을 갖는 원소들은 매우 희소한(적은) 행렬
# DTM에서 자주 등장하는 단어(term)들만 선택
findFreqTerms(sms_dtm_train)
freq_terms = findFreqTerms(sms_dtm_train, lowfreq = 5)

sms_dtm_freq_train = sms_dtm_train[ , freq_terms]
sms_dtm_freq_test = sms_dtm_test[ , freq_terms]

str(sms_dtm_train)
str(sms_dtm_freq_train)
str(sms_dtm_test)
str(sms_dtm_freq_test)


# 나이브 베이즈 알고리즘 함수는 명목형 변수들만 처리할 수 있음
# DTM에서 각 원소의 값이 0보다 크면 'Y', 그렇지 않으면 'N'
# 변환 함수
convert = function(x) {
  x = ifelse(x > 0, 'Y', 'N')
  return(x)
}

x = c(0, 1, 3)
convert(x)


sms_train = apply(X = sms_dtm_freq_train,
                  MARGIN = 2,
                  FUN = convert)

sms_test = apply(X = sms_dtm_freq_test,
                 MARGIN = 2,
                 FUN = convert)
# MARGIN = 1: FUN을 호출할 때 X의 행(row)를 파라미터로 전달
# MARGIN = 2: FUN을 호출할 때 X의 열(column)을 파라미터로 전달

# 나이브 베이즈 알고리즘을 구현한 패키지를 설치

install.packages('e1071')
library(e1071)

# 학습 데이터 세트를 가지고 분류기(classifier)를 생성
sms_classifier = naiveBayes(sms_train, sms_train_label)
# 분류기를 사용해서 테스트 데이터의 분류 결과 예측
sms_pred = predict(sms_classifier, sms_test)
table(sms_pred)
table(sms_test_label)
sms_test[991, ]

library(gmodels)

CrossTable(x = sms_test_label, y = sms_pred, prop.chisq = F)

sms_test


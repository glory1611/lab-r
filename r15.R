rm(list = ls())
# word cloud
#필요한 패키지 설치

# R에서 Java를 사용한 패키지를 이용할 수 있도록 해주는 패키지
install.packages('rJava')

# KoNLP 패키지(한국어 분서ㄱ)가 사용하는 패키지
install.packages('memoise')
# KoNLP(Korean Natural Language Processing): 한국어 자연어 처리
install.packages('KoNLP')
# 문자열을 처리하는 여가지 함수들을 가지고 있는패키지
install.packages('stringr')


# word cloud 작성 패키지
install.packages('wordcloud')

library(dplyr)
library(KoNLP)
library(stringr)
library(wordcloud)

search()

# 한국어 분석에 사용할 사전 로드
useSejongDic()

# 텍스트 파일(*.txt)네서 한줄씩 문자열들을 읽음
txt = readLines('data/hiphop.txt')
str(txt)
head(txt)
txt[1:10]
txt[1001:1010]

# 특수문자(', ?, !, ...)들은 제외
txt = str_replace_all(txt, '\\W', ' ')
txt[1:10]

# 각 문장에서 명사들만 추출
nouns = extractNoun(txt)
str(nouns)
head(nouns)
tail(nouns)

# 명사들로 이루어진 리스트를 도수분포표 형태로 변환
word_count = table(unlist(nouns))
word_count

# 도수분포표(데이블)을 데이터 프레임으로 변환
# 문자열을 범주형 변수로 취급하지 않음
df = as.data.frame(word_count, stringsAsFactors = F)
head(df)
tail(df)

# 데이터 프레임을 분석하기 쉽게 변수 이름을 변경
df = rename(df, word = Var1, freq = Freq)

str(df)

# word의 길이는 2글자 이상인 것만 선택
df = filter(df, nchar(word) >= 2)
str(df)


# 가장 많이 사용된 명사 20개
top20 = df %>% 
  arrange(-freq) %>% 
  head(20)
 top20

 
# wordcloud 작성
# 팔래트 설정
pal = brewer.pal(8, 'Dark2')

# wordcloud는 생성할때마다 랜덤하게 만들어 지는데,
# 실행할 때마다 항상 같은 경과를 주기 위해서
set.seed(1234)

# wordcloud 작성
wordcloud(words = df$word,    # 글자
          freq = df$freq,     # 단어의 빈도수
          min.freq = 2,       # 단어의 최소 빈도수
          max.words = 500,    # wordcloud에 보여줄 단어의 최대 갯수
          random.order = F,   # 단어들을 무작위 배치?
          rot.per = 0.1,      # 횝전 단어 비율
          scale = c(3.5, 0.2),  # 단어 크기 비율
          colors = pal)



twitter = read.csv('data/twitter.csv', header = T, encoding = 'UTF-8', stringsAsFactors = F)
head(twitter, 1)
str(twitter)
twitter = str_replace_all(twitter, '\\W', ' ')
nouns = extractNoun(twitter)
str(nouns)
word_count2 = table(unlist(nouns))
head(word_count2, 1)
df = as.data.frame(df, stringsAsFactors = F)
str(df)
df = rename(df, word = Var1, freq = Freq)
df = filter(df, nchar(word) >= 2)
str(df)
head(df, 1)
df = table(unlist(df))
wordcloud(words = df$word,    # 글자
          freq = df$freq,     # 단어의 빈도수
          min.freq = 2,       # 단어의 최소 빈도수
          max.words = 500,    # wordcloud에 보여줄 단어의 최대 갯수
          random.order = F,   # 단어들을 무작위 배치?
          rot.per = 0.1,      # 횝전 단어 비율
          scale = c(3.5, 0.2),  # 단어 크기 비율
          colors = pal)

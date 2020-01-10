# SVM(Support Vector Machine)

letters = read.csv('mlwr/letterdata.csv')

str(letters)
head(letters, 10)
table(letters$letter)

# 학습 데이터(80%) 테스트 데이터(20%)

train = letters[1:16000, ]
test = letters[16001:20000, ]

table(train$letter)
table(test$letter)

prop.table(table(train$letter))
prop.table(table(test$letter))

# 3. 모델 생성 - SVM
# kernlab 패키지

install.packages("kernlab")
library(kernlab)
search()

# SVM 알고리즘 모델 생성
letter_classifier = ksvm(letter ~ .,
                         train,
                         kernel = "vanilladot")


# 모델 평가
letters_predict = predict(letter_classifier, test)
 head(letters_predict)
table(letters_predict)
table(letters_predict, test$letter)

letters_predict[1] == test$letter[1]
correct_count = table(letters_predict == test$letter)
correct_count
correct_ratio = prop.table(table(letters_predict == test$letter))
correct_ratio

cor(letters_predict, test$letter)

# 5. 모델 수정 -> 재평가 -> 성능 개선
classifier2 = ksvm(letter ~ .,
                   data = train,
                   kernel = 'rbfdot')
letters_predict2 = predict(classifier2, test)
head(letters_predict2, 10)
head(test$letter, 10)
table(letters_predict2, test$letter)

correct_count2 = table(letters_predict2 == test$letter)
correct_count2
correct_ratio2 = prop.table(table(letters_predict2 == test$letter))
correct_ratio2

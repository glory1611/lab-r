exam = read.csv('data/csv_exam.csv')

str(exam)

head(exam, 5)
tail(exam)


exam_class = exam %>% 
  group_by(class) %>% 
  summarise(math = mean(math), english = mean(english), science = mean(science))

exam_class

ggplot(exam_class, aes(x = class, y = math)) +
  geom_col()

ggplot(exam_class, aes(x = class)) +
  geom_col(aes(y = math), position = 'dodge') +
  geom_col(aes(y = english), position = 'dodge')

######
install.packages('tidyr')
library(tidyr)

# tidyr::gather() 함수
# 데이터 프레임에서 행과 열의 위치를 바꿔줌


df = exam_class %>% 
  gather(key ='subject', value = 'mean', -class)
df

##

ggplot(df, aes(x = class, y = mean, fill = subject)) +
  geom_col(position = 'dodge')


exam_class %>% 
  gather('var', 'value', science, math)




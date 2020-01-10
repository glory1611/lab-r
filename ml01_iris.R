iris = read.csv('data/Iris.csv')

str(iris)
iris = iris[-1]
summary(iris)

normalize = function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}  

library(dplyr)
library(class)
library(gmodels)

iris = sample_n(iris, 150)
str(iris) 

iris_n = as.data.frame(lapply(iris[1:4], normalize))
str(iris_n)
summary(iris_n)

data_train = iris_n[1:100, ]
data_test = iris_n[101:150, ]
label_train = iris[1:100, 5]
label_test = iris[101:150, 5]

predict = knn(data_train,
              data_test,
              label_train,
              9)
table(predict)
table(label_test)

CrossTable(x = label_test, y = predict, prop.chisq = F)


 iris_z = as.data.frame(scale(iris[-5]))
summary(iris_z)

data_train2 = iris_z[1:100, ]
data_test2 = iris_z[101:150, ]

predict_z = knn(data_train2,
              data_test2,
              label_train,
              5)
table(predict_z)
CrossTable(x = label_test, y = predict_z, prop.chisq = F)









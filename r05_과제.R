midwest = as.data.frame(ggplot2::midwest)
head(midwest)
tail(midwest)
summary(midwest)
dim(midwest)
str(midwest)

midwest = rename(midwest, total = poptotal, asian = popasian)
head(midwest)
midwest$asian_p = round((midwest$asian/midwest$total)*100, digits = 2)
head(midwest)

asian_avg = mean(midwest$asian_p)
asian_avg
midwest$asian_l_s = ifelse(midwest$asian_p > asian_avg, 'large', 'small')
head(midwest)

table(midwest$asian_l_s)
ggplot2::qplot(midwest$asian_l_s)

midwest$asian_l_s2 = ifelse(midwest$asian_p > asian_avg, 1, 0)
hist(midwest$asian_l_s2)
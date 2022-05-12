df <- read.table(file = file.choose(), header = TRUE, sep = ";", dec = ",")

#гистограмма: показаны данные и нормальное распределение
hist(df$height, breaks = 10, freq = F,
     col = "navajowhite", xlab = "Height", main = "Histogram of height")
lines(density(df$height), col = "red", lwd = 2)
curve(dnorm(x, mean(df$height), sd(df$height)), add=TRUE, col="darkblue", lwd=2) 

#график Квантиль-квантиль, q-q plot, quantile-quantile plot
df$height_std <- scale(df$height)
summary(df$height_std)
qnorm(0.1, mean = 0, sd = 1)
quantile(df$height_std, 0.1)
qqnorm(df$height_std)
qqline(df$height_std)
qqline(df$height_std, distribution = qnorm, col = "red")

#проверка гипотез
#если p-value меньше уровня значимости, то принимаем H1

install.packages(pkgs=c("nortest"))
library(nortest)
install.packages("moments")
library(moments)

set.seed(123) #эта команда позволяет воспроизводить результаты случайной генерации чисел
#если вы ее не прогоните, то сможете получить выборку, для которой критерий отвергнет нулевую гипотезу
sample2 <- rnorm(n = 200) #генерируем выборку из стандартного нормального распределения

#критейрий Колмогорова-Смирнова
ks.test(sample2, "pnorm") #проверка выборки на нормальность
ks.test(df$height, "pnorm", 
        mean = mean(df$height, na.rm = T), 
        sd = sd(df$height, na.rm = T)) #проверка столбца из базы данных на нормальность

shapiro.test(df$height) #критерий Шапиро-Уилка

lillie.test(df$height) #критерий Лиллифорса

cvm.test(df$height) #критерий Крамера-фон Мизеса (Смирнова)

ad.test(df$height) #критерий Андерсона-Дарлинга

sf.test(df$height) #критерий Шапиро-Франсиа

pearson.test(df$height) #критерий хи-квадрат Пирсона

jarque.test(df$height) #критерий Жарка-Бэра

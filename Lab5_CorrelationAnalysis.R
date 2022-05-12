x <- c(1, 2, 6, 8, 9, 7, 7.5, 10, 3, 4, 5.5)
y <- c(2, 4, 11, 15, 19, 16, 14, 23, 7, 6, 11)

plot(x, y) #диаграмма рассеяния

cor(x, y) #коэффициент корреляции
cor(x, y, use = "complete.obs")  #коэффициент корреляции, если есть NA в выборке
cor(x, y, method = 'spearman') #коэффициент корреляции Спирмена

cor.test(x, y) #проверка гипотезы о значимости парного коэффициента корреляции
cor.test(x, y, method = 'spearman') #проверка гипотезы о значимости коэффициента корреляции Спирмена
cor.test(x, y, method='kendall') #проверка гипотезы о значимости коэффициента корреляции Кэнделла

install.packages("Hmisc")
library(Hmisc)

df <- read.csv(file.choose(), header = TRUE, sep = ",")
df <- na.omit(df)
d <- df[3:ncol(df)-1]

plot(d$mstat, d$econ, xlab = "Матстатиска", ylab = "Экономика") #диаграмма рассеяния более сложная
plot <- ggplot(d, aes(d$mstat, d$econ)) + geom_point(aes()) #диаграмма рассеяния ещё более сложная

rcorr(as.matrix(d)) #корреляционная матрица и значимость коэффициентов в ней


#коррелограмма
install.packages("ggplot2")
install.packages("car")
install.packages("gclus")
install.packages("corrplot")
install.packages("corrgram")  #пакет с коррелограммами
install.packages("gplots")
install.packages("ggm") 
install.packages("ellipse")

library(ggplot2)
library(car)
library(gclus)
library(corrplot)
library(corrgram)
library(gplots)
library(ggm) 
library(ellipse)

scatterplotMatrix(d) #матрица диаграмм рассеяния (graph matrix), у которой на диагонали гистограммы для соответствующих переменных

require(corrgram)
corrgram(d, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)

corrplot(cor(d), "pie", "lower")

corrplot(cor(d))

corrplot(cor(d), method="color", diag=FALSE)

corrplot(cor(d), method="number", diag=FALSE)

plotcorr(cor(df))


#таблица сопряжённости и критерий Хи-квадрат
install.packages("descr")
library(descr)

guitar <- c(31, (100-31))
bag <- c(9, (100-9))
guitar_exp <- rbind(guitar, bag)

#проверка независимости двух переменных с помощью таблицы сопряженности
chisq.test(guitar_exp) #критерий Хи-квадрата

fisher.test(guitar_exp) #точный тест Фишера

mosaicplot(guitar_exp, shade = TRUE)#результат графически (таблица сопряженности)

#построение таблиц сопряженности из сырых данных
table(mtcars$cyl, mtcars$am)
table(Number_of_cylinders=mtcars$cyl, Transmission=mtcars$am)
crosstab(mtcars$cyl, mtcars$am, format = "SPSS", expected = TRUE)

x <- c(1, 2, 6, 8, 9, 7, 7.5, 10, 3, 4, 5.5)
y <- c(2, 4, 11, 15, 19, 16, 14, 23, 7, 6, 11)

plot(x, y) #диаграмма рассе€ни€

cor(x, y) #коэффициент коррел€ции
cor(x, y, use = "complete.obs")  #коэффициент коррел€ции, если есть NA в выборке
cor(x, y, method = 'spearman') #коэффициент коррел€ции —пирмена

cor.test(x, y) #проверка гипотезы о значимости парного коэффициента коррел€ции
cor.test(x, y, method = 'spearman') #проверка гипотезы о значимости коэффициента коррел€ции —пирмена
cor.test(x, y, method='kendall') #проверка гипотезы о значимости коэффициента коррел€ции  энделла

install.packages("Hmisc")
library(Hmisc)

df <- read.csv(file.choose(), header = TRUE, sep = ",")
df <- na.omit(df)
d <- df[3:ncol(df)-1]

plot(d$mstat, d$econ, xlab = "ћатстатиска", ylab = "Ёкономика") #диаграмма рассе€ни€ более сложна€
plot <- ggplot(d, aes(d$mstat, d$econ)) + geom_point(aes()) #диаграмма рассе€ни€ ещЄ более сложна€

rcorr(as.matrix(d)) #коррел€ционна€ матрица и значимость коэффициентов в ней


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

scatterplotMatrix(d) #матрица диаграмм рассе€ни€ (graph matrix), у которой на диагонали гистограммы дл€ соответствующих переменных

require(corrgram)
corrgram(d, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)

corrplot(cor(d), "pie", "lower")

corrplot(cor(d))

corrplot(cor(d), method="color", diag=FALSE)

corrplot(cor(d), method="number", diag=FALSE)

plotcorr(cor(df))


#таблица сопр€жЄнности и критерий ’и-квадрат
install.packages("descr")
library(descr)

guitar <- c(31, (100-31))
bag <- c(9, (100-9))
guitar_exp <- rbind(guitar, bag)

#проверка независимости двух переменных с помощью таблицы сопр€женности
chisq.test(guitar_exp) #критерий ’и-квадрата

fisher.test(guitar_exp) #точный тест ‘ишера

mosaicplot(guitar_exp, shade = TRUE)#результат графически (таблица сопр€женности)

#построение таблиц сопр€женности из сырых данных
table(mtcars$cyl, mtcars$am)
table(Number_of_cylinders=mtcars$cyl, Transmission=mtcars$am)
crosstab(mtcars$cyl, mtcars$am, format = "SPSS", expected = TRUE)
install.packages("stargazer")
install.packages("memisc")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lmtest")
install.packages("pander")
install.packages("dplyr")

library("tidyverse") #коллекция пакетов: ggplot2, dplyr, ...
library("lmtest") #тесты для линейных моделей
library("memisc") #сравнение моделей
library("pander") #таблички в markdown
library("broom") #стандартизация информации о модели 
library("psych") #описательные статистики
library("modelr") #добавление прогнозов/остатков
library("ggplot2")

h <- Orange
summary(h)
plot(h$age, h$circumference) #поле корреляции
ggplot(Orange) + 
  geom_point(aes(x = age, y = circumference, color = Tree)) + 
  labs(x = "Возраст дерева, лет", 
       y = "Диаметр ствола, мм",
       title = "Пять апельсиновых деревьев") +
  scale_colour_discrete(name = "Дерево п/п")

model <- lm(circumference ~ age, data = h)  #оцениваем модель
model  #выводим на экран только коэффициенты
summary(model)  #создаём отчет по модели #выводим отчет на экран

attributes(model)
model$coefficients
model$residuals
model$fitted

#показатели информационных критериев Акаике и Байесовского (Шварца)
AIC(model)
BIC(model)

confint(model, level = 0.9) #доверительные интервалы для коэффициентов 90%
confint(model, level = 0.95) #95%


#построение прогнозов

#с доверительным интервалом для среднего
new.data <- data.frame(age = c(100, 200, 300)) #зададим значений объясняющей переменной и обозначим ее как новую переменную
predict(model, new.data, interval = "confidence")

#с предиктивным интервалом
predict(model, new.data, interval = "prediction")

#визуализация
plot <- ggplot(h, aes(h$age, h$circumference)) + geom_point(aes())
plot + 
  xlab('Возраст деревьев') + #подписываем ось x
  ylab('Длина окружности ствола') + #подписываем ось y
  theme_bw()+ #выбираем ч/б тему
  geom_smooth(method=lm)


#оценка моделей
#создадим две другие
model1 <- lm(circumference ~ age+I(age^2), data = h)  #оцениваем квадратичную модель
summary(model1)

ln_age<-log(h$age)
ln_cir<-log(h$circumference) 
model2 <- lm(ln_cir ~ ln_age, data = h)  #оцениваем логарифмическую модель
summary(model2) 

mtable(model, model1, model2) #сводим все три модели в одну табличку
#сами выбираем лучшую

resid<-resid(model2) #смотрим на остатки наилучшей модели
#проверим их на нормальность
#qq plot
qqnorm(resid)
qqline(resid)
qqline(resid, distribution = qnorm, col = "red")
#гистограмма
hist(resid, breaks = 6, freq = FALSE, col = "lightblue")
lines(density(resid), col = "red", lwd = 2)
curve(dnorm(x, mean=mean(resid), sd=sd(resid)), add=TRUE, col="darkblue", lwd=2)
#критерий Жака-Бера
install.packages("moments")
library("moments")
jarque.test(as.vector(resid))
#критерий Колмогорова-Смирнова
install.packages("nortest")
library(nortest)
ks.test(resid, "pnorm", 
        mean = mean(resid, na.rm = T), 
        sd = sd(resid, na.rm = T))
#критерий Шапиро-Уилка
shapiro.test(resid)
#критерий Лиллифорса
lillie.test(resid)
#критерий Крамера-фон Мизеса и Андерсона-Дарлинга
ad.test(resid)
#критерий хи-квадрат Пирсона
pearson.test(resid)


#задание

model3 = lm(circumference ~ age+I(age^2)+I(age^3), data = h)
#model
model4 <- lm(ln_cir ~ ln_age + I(ln_age^2), data = h)  
#model1

mtable(model, model1, model3,model4)
AIC(model)
BIC(model)
AIC(model1)
BIC(model1)
AIC(model3)
BIC(model3)
AIC(model4)
BIC(model4)
#4 лучше поскольку R-squared больше (не для множественной), проверяем критериями, чем ниже число, тем лучше

resid<-resid(model4)

qqnorm(resid)
qqline(resid)
qqline(resid, distribution = qnorm, col = "red")

hist(resid, breaks = 6, freq = FALSE, col = "lightblue")
lines(density(resid), col = "red", lwd = 2)
curve(dnorm(x, mean=mean(resid), sd=sd(resid)), add=TRUE, col="darkblue", lwd=2)

jarque.test(as.vector(resid))
ks.test(resid, "pnorm", 
        mean = mean(resid, na.rm = T), 
        sd = sd(resid, na.rm = T))
shapiro.test(resid)
lillie.test(resid)
ad.test(resid)
pearson.test(resid)

t.test(resid, mu = 0)

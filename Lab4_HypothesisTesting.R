df <- read.csv("https://raw.githubusercontent.com/agconti/kaggle-titanic/master/data/train.csv")
df <- na.omit(df)
surv1 <- df[df$Survived == 1, ] #берём выживших
notSurv = df[df$Survived == 0, ] #берём невыживших

t.test(surv1$Age, mu = 30) #Критерий Стьюдента о равенстве среднего числу, по умолчанию двустронняя гипотеза(Н1 != числу)
t.test(surv1$Age, mu = 30, alternative = "less") # H1: mu < 30

#проверка гипотез о вероятности
tapply(surv1$Age, surv1$Pclass, length) #узнаём сколько выживших в каждом классе пассажиров
prop.test(122, 290, p = 0.25) #(число выживших, всего пассажиров, вероятность) доля пассажиров первого класса равна или не равна 0.25
prop.test(122, 290, p = 0.25, alternative = "greater")
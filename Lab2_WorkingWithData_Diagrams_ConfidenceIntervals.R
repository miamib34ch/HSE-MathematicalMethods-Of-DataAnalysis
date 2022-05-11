dat <- read.table(file = file.choose(), header = TRUE, sep = ",", dec = ".") #загрузим файл .csv

nrow(dat) #число строк
ncol(dat) #число столбцов

head(dat) #первые несколько значений
tail(dat) #последние несколько значений

head(complete.cases(dat)) #функция complete.cases() выдает логический вектор, где TRUE означает полностью заполненную строку, а FALSE - содержащую пропуски (NAs).
sum(complete.cases(dat)) #посчитаем сколько полностью заполненных
View(dat[!complete.cases(dat), ]) #посмотрим незаполненные

dat <- na.omit(dat) #удаляем строки, содержащие NA

attach(dat) # “закрепить” базу данных в окружении
head(Promotion) #Promotion - имя столбца в таблице
detach(dat) #возвращаем обратно 

dat$Campaign <- factor(dat$Promotion) #добавляем в бд новую переменную с данными из другого столбца

#фильтрация по условиям
subset(dat, Week == 1)
subset(dat, Week == 1 & MarketSize == "Medium") #несколько условий
dat1 <- subset(dat, select = c(MarketID, SalesInThousands)) #указываем имя базы, оставляем MarketID и SalesInThousands
dat2 <- subset(dat, select = -c(Week, AgeOfStore)) #перед вектором столбцов стоит минус, эти стобцы будут исключены, а остальные возьмутся 


#задание 2

install.packages("car")
library(car)
data(mtcars)

#эмпирические характеристики
mean(mtcars$mpg) #арифметическая средняя
median(mtcars$mpg) #медиана
var(mtcars$mpg) #дисперсия
sd(mtcars$mpg) #стандартное отклонение
min(mtcars$mpg) #минимальное значение
max(mtcars$mpg) #максимальное значение
quantile(mtcars$mpg) #квантили

IQR(mtcars$mpg) #разница между первым и третим квартилями носит название интерквартильный размах 
quantile(mtcars$mpg, p = seq(0, 1, 0.1)) #функция quantile() позволяет рассчитать и другие квантили. Например, децили.

#отсутствующие значения в данных могут несколько усложнить вычисления. В качестве демонстрации заменим 3-е значение переменной mpg на NA (от not available - не доступно) - обозначение, используемое в R для отсутствующих наблюдений, - а затем попытаемся вычислить среднее значение:
mtcars$mpg[3] <- NA 
head(mtcars$mpg)
mean(mtcars$mpg)
mean(mtcars$mpg, na.rm = TRUE) #посчитать среднее удалив NA

#функции, которые позволяют выяснить порядковые номера элементов, обладающих минимальным и максимальным значениями соответственно:
which.min(mtcars$mpg)
which.max(mtcars$mpg)

summary(mtcars) #описательные статистики для каждой колонки

#переменные vs и am являются факторами (качественная переменная (указывает на характеристику), число считается не числом), но уровни их закодированы при помощи чисел 0 и 1. К сожалению, система R не распознала эти две переменные как факторы и рассчитала соответствующие параметры описательной статистики, как для обычных числовых переменных. Однако мы можем изменить такое поведение R, самостоятельно преобразовав vs и am в факторы при помощи функции as.factor():
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
is.factor(mtcars$vs) #проверим, удалась ли конвертация
is.factor(mtcars$am)
#обратите внимание на сводки по vs и am: поскольку эти переменные теперь распознаны программой как факторы, единственный способ описать их - это подсчитать количество наблюдений для каждого уровня.

tapply(X = mtcars$disp, INDEX = mtcars$am, FUN = mean)
#Х - числовой вектор
#INDEX - список факторов, для уровней которых рассчитываются значения функции FUN
#FUN - любая, в том числе пользовательская, функция
tapply(X = mtcars$disp, INDEX = list(mtcars$am, mtcars$vs), FUN = mean)

SE <- function(x) {sd(x)/sqrt(length(x))} #создали функцию
tapply(X = mtcars$disp, INDEX = mtcars$am, FUN = SE) #с пользовательской функцией


#задание 3

install.packages("moments")
library(moments)  #загрузка пакета moments

kurtosis(mtcars$mpg, na.rm = TRUE) #эксцесс
skewness(mtcars$mpg, na.rm = TRUE) #коэффициент асимметрии

plot(mtcars$disp, mtcars$mpg) #plot(x, y)
plot(data=mtcars, mpg ~ disp)  #plot(data=тут ваш дата фрейм, y ~ x)
plot(data=mtcars, mpg ~ disp, 
     main="Расход топлива от объема двигателя", 
     xlab="Объем, куб. дюймы", 
     ylab = "Миль на галлон",
     pch=3)

pairs(mtcars, main="Много графиков!")

dotchart(mtcars$mpg, labels = row.names(mtcars),
         main="Экономия топлива у 32 моделей автомобилей",
         xlab="Миль/галлон", cex = 0.8)

#расскрасим
x <- mtcars[order(mtcars$mpg), ]
x$cyl <- factor(x$cyl)
x$color[x$cyl==4] <- 1
x$color[x$cyl==6] <- 2
x$color[x$cyl==8] <- 3
dotchart(x$mpg, labels = row.names(x),
         groups = x$cyl, gcolor = "blue", pch = 16, color = x$color,
         main="Экономичность двигателя у 32 моделей автомобилей",
         xlab="Миль/галлон", cex = 0.8)

#ящик с усами
boxplot(data=mtcars, mpg ~ am, 
        main="Ящики с усами", 
        xlab="Ручная коробка?", 
        ylab="MPG",
        col = "coral")

boxplot(mpg ~ am,
        ylab = "MPG",
        xlab = "Ручная коробка",
        main = "Ящик с усами",
        col = "coral", horizontal = TRUE,
        data=mtcars)

#круговая диаграмма
table(mtcars$cyl)
pie(table(mtcars$cyl), main = "Круговая диаграмма,\n но не очень интересная")
pie(table(mtcars$cyl), cex = 0.6, radius = 0.9, init.angle = -10,
    main = "Круговая диаграмма",
    col = c(2:8))

#гистограмма
hist(mtcars$qsec)
hist(mtcars$qsec, breaks = 10)
hist(mtcars$qsec, breaks = 10, freg = FALSE)
hist(mtcars$qsec, breaks = 10, freg = FALSE,  col = "lightblue")
hist(mtcars$qsec, breaks = c(10,18,19,25), density = 10, col="darkgreen")

install.packages("ggplot2")
library(ggplot2)
ggplot(mtcars, aes(x=qsec, y=mpg)) + geom_line() #пустой холст, на который мы передаём данные и рисуем линии


#задание 4

install.packages("dplyr")
library(dplyr)
dat <- read.table(file = file.choose(), header = TRUE, sep = ",", dec = ".")

small1 <- select(dat, sex, age, income, vote) #функция select(), которая позволяет выбрать интересующие нас столбцы в датафрейме
small2 <- select(dat, -c(X, region, population))

head(select(dat, sex:vote)) #столбцы от sex до vote

old <- filter(dat, age > 45) #фильтруем данные
old_m <- filter(dat, age > 45 & sex == "M")

#в библиотеке dplyr есть особый оператор %>%, который позволяет выполнять операции
#пошагово. Смысл этого оператора такой: возьми, то, что слева от %>% и передай это на вход
#функции, стоящей справа от %>%.
dat %>% View
dat %>% select(sex, age, income, vote) %>% head

dat %>% arrange(statusquo) %>% head #arrange() – функция, которая сортирует таблицу в соответствии со значениями переменной (или переменных), расположенных по возрастанию (если переменная текстовая, то по алфавиту).
dat %>% arrange(statusquo) %>% tail

dat %>% mutate(log_income = log(income)) #mutate() – используется для создания и добавления в датафрейм новой переменной.
dat <- dat %>% mutate(log_income = log(income))
dat <- dat %>% mutate(log_income = log(income), log_population = log(population))

#получить какую-то сводную информацию по переменным.
dat %>% summarise(total = n()) #n() используется для подсчета элементов.

dat %>% summarise(avg_age = mean(age, na.rm = TRUE), 
                  min_age = min(age, na.rm = TRUE), 
                  max_age = max(age,na.rm = TRUE)) #есть параметр na.rm, который позволяет зафиксировать, исключать ли пропущенные значения (rm от remove) при подсчёте или нет.

#получение сводной информации по сгруппированным данным
dat %>% group_by(region) %>% summarise(count_reg = n())
dat %>% group_by(region) %>% summarise(avg_income = mean(age, na.rm = TRUE))
dat %>% group_by(sex) %>% summarise(count_sex = n())
dat %>% group_by(sex) %>% tally() #tally() используется для подсчета элементов.


#задание 5

install.packages("DescTools") #для построения доверительных интервалов
library(DescTools)

ci90 <- BinomCI(30, 100, conf.level = 0.90) #на первом месте указано число успехов (число интересующих нас объектов), на втором - объем выборки. В conf.level мы указываем уровень доверия.
#Результат: Первое число - это выборочная доля (30/100). Второе — это нижняя граница доверительного интервала (lwr.ci — от lower), а третье — верхняя граница (upr.ci - от upper).

l90 <- ci90[3] - ci90[2] #длина интервала

df <- read.csv(file = file.choose())
df <- na.omit(df)
head(df)
table(df$vote)
yes = 836
no = 867
BinomCI(yes, yes + no, conf.level = 0.95)
voted <- df[df$vote == "Y", ]

MeanCI(voted$age) #доверительный интервал для среднего, уроваень доверия не выставлен, по умолчанию 95%

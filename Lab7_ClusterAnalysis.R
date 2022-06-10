x1 <- c(5, 5, 3, 2, 3.5)
x2 <- c(2, 1, 2, 1, 4)
dat <- as.data.frame(cbind(x1, x2))
View(dat)

Mdist <- dist(dat) #евклидово расстояние между всеми парами точек
hc <- hclust(Mdist, method = "single") #иерархический кластерный анализ, метод ближнего соседа
plot(hc) #дендрограмма

#тоже самое, но шкалируем (приведенное к единой шкале)
Mdist1 <- dist(scale(dat))
hc1 <- hclust(Mdist1, method = "single")
plot(hc1)

df <- read.csv(file.choose(), dec = ",")
df <- na.omit(df)

#выберем случайным образом 30 стран из базы 
set.seed(1234)
data <- df[sample(nrow(df), 30), ]

#теперь отберем только те столбцы, которые нужны нам для кластерного анализа. Мы будем определять кластеры стран по значениям индексов-компонентов WGI и Freedom House. 
library(dplyr)
d <- data %>% select(va:fh_score)
View(d)
rownames(d) <- data$country

#сохраним отдельно копию для дальнейшего
date0<-d
View(date0)

Mdist <- dist(d)
Mdist #смотрим матрицу расстояний

hc1 <- hclust(Mdist, method = "single") #ближний сосед
plot(hc1)

Mdist2 <- dist(d)
hc_w <- hclust(Mdist2, method = "ward.D2") #метод Варда
plot(hc_w, cex = 0.6)

#выделем два кластера
plot(hc_w, cex = 0.6, main = "2 clusters") 
rect.hclust(hc_w, k = 2, border="red") 

#выделем четыре кластера
plot(hc_w, cex = 0.6, main = "4 clusters")
rect.hclust(hc_w, k = 4, border="red") # 4 кластера

#вытащим из полученного разбиения на кластеры метки для наблюдений, чтобы было ясно, какие страны в одном кластере, а какие – в разных
groups4 <- cutree(hc_w, k = 4) 
groups4 

#теперь добавим столбец с метками для кластеров в нашу базу
d1 <- d %>% mutate(groups4 = factor(groups4), country = data$country)
View(d1)

library(ggplot2)
ggplot(data = d1, aes(x = fh_score, y = va, color = groups4)) + geom_point() 
# vjust и hjust - чтобы подписи были чуть в стороне и не закрывали точки
ggplot(data = d1, aes(x = fh_score, y = va, color = groups4)) + geom_point() +
  geom_text(aes(label = country, vjust = 0, hjust = 0))


plot(hc_w, cex = 0.6, main = "5 clusters")
rect.hclust(hc_w, k = 5, border="red") #рассмотрим 5 кластера
groups5 <- cutree(hc_w, k = 5) 
groups5 # посмотрим на метки
d1 <- d1 %>% mutate(groups5 = factor(groups5), country = data$cnt_code)
View(d1)

#оценка кластеризации: содержательно
d1 %>% filter(groups5 == 1) %>% View
d1 %>% filter(groups5 == 2) %>% View
d1 %>% filter(groups5 == 3) %>% View
d1 %>% filter(groups5 == 4) %>% View
d1 %>% filter(groups5 == 5) %>% View

kruskal.test(d1$va ~ d1$groups5) #оценка критерием
kruskal.test(d1$fh_score ~ d1$groups5)

install.packages("factoextra")
library(factoextra)

#Построим график, где по оси абсцисс отмечено число кластеров k, а по оси ординат – значения функции W(k), которая определяет внутригрупповой разброс в зависимости от числа кластеров
#Elbow method (“метод согнутого колена”, он же “метод каменистой осыпи”). 
fviz_nbclust(d, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# geom_vline - добавить вертикальную линию
fviz_nbclust(d, kmeans, method = "wss") +
  labs(subtitle = "Elbow method") +
  geom_vline(xintercept = 4, linetype = 2)

#Silhouette method (“силуэтный метод”).
fviz_nbclust(d, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")


#неирархическая классификация

d2 <- date0
cl <- kmeans(d2, 4) #метод kmeans

cl
d2$kmeans4 <- cl$cluster
View(d2)

#все столбцы для анализа должны быть числовыми
is.numeric(d2$kmeans4)
d2$kmeans4 = as.numeric(d2$kmeans4)

fviz_cluster(cl, d2,  ellipse.type = 'convex')

#сколько кластеров взять
fviz_nbclust(d2, kmeans, method = "wss") + labs(subtitle = "Elbow method")
fviz_nbclust(d2, kmeans, method = "wss") +
  labs(subtitle = "Elbow method") +
  geom_vline(xintercept = 4, linetype = 2)

#проверка гипотезы о том, что данные в выборках взяты из одного распределения
kruskal.test(d2$va ~ d2$kmeans4)
kruskal.test(d2$fh_score ~ d2$kmeans4)

x1 <- c(5, 5, 3, 2, 3.5)
x2 <- c(2, 1, 2, 1, 4)
dat <- as.data.frame(cbind(x1, x2))
View(dat)

Mdist <- dist(dat)
hc <- hclust(Mdist, method = "single")
plot(hc)

Mdist1 <- dist(scale(dat))
hc1 <- hclust(Mdist1, method = "single")
plot(hc1)

df <- read.csv(file.choose(), dec = ",")
df <- na.omit(df)
set.seed(1234) # для воспроизводимости выделите эту строку и следующую и запустите
data <- df[sample(nrow(df), 30), ]


library(dplyr)
d <- data %>% select(va:fh_score)
View(d)
rownames(d) <- data$country


date0<-d
View(date0)
Mdist <- dist(d)
Mdist

hc1 <- hclust(Mdist, method = "single")
plot(hc1)

Mdist2 <- dist(d)
hc_w <- hclust(Mdist2, method = "ward.D2") 
plot(hc_w, cex = 0.6)

plot(hc_w, cex = 0.6, main = "2 clusters") 
rect.hclust(hc_w, k = 2, border="red") 

plot(hc_w, cex = 0.6, main = "4 clusters")
rect.hclust(hc_w, k = 4, border="red") # 4 кластера


groups4 <- cutree(hc_w, k = 4) 
groups4 

d1 <- d %>% mutate(groups4 = factor(groups4), country = data$country)
View(d1)


library(ggplot2)
ggplot(data = d1, aes(x = fh_score, y = va, color = groups4)) + geom_point() 
# vjust и hjust - чтобы подписи были чуть в стороне и не закрывали точки
ggplot(data = d1, aes(x = fh_score, y = va, color = groups4)) + geom_point() +
  geom_text(aes(label = country, vjust = 0, hjust = 0))


plot(hc_w, cex = 0.6, main = "5 clusters")
rect.hclust(hc_w, k = 5, border="red") # 5 кластера
groups5 <- cutree(hc_w, k = 5) 
groups5 # посмотрим на метки
d1 <- d1 %>% mutate(groups5 = factor(groups5), country = data$cnt_code)
View(d1)

d1 %>% filter(groups5 == 1) %>% View
d1 %>% filter(groups5 == 2) %>% View
d1 %>% filter(groups5 == 3) %>% View
d1 %>% filter(groups5 == 4) %>% View
d1 %>% filter(groups5 == 5) %>% View

kruskal.test(d1$va ~ d1$groups5)

kruskal.test(d1$fh_score ~ d1$groups5)
library(factoextra)

fviz_nbclust(d, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# geom_vline - добавить вертикальную линию
fviz_nbclust(d, kmeans, method = "wss") +
  labs(subtitle = "Elbow method") +
  geom_vline(xintercept = 4, linetype = 2)

fviz_nbclust(d, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")

d2 <- d1
cl <- kmeans(d, 4)

cl
d2$kmeans4 <- cl$cluster
View(d2)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")

fviz_cluster(cl, data = d2, ellipse.type = 'convex')


fviz_nbclust(d2, kmeans, method = "wss") + labs(subtitle = "Elbow method")
fviz_nbclust(d2, kmeans, method = "wss") +
  labs(subtitle = "Elbow method") +
  geom_vline(xintercept = 4, linetype = 2)

kruskal.test(d2$va ~ d2$kmeans4)
kruskal.test(d2$fh_score ~ d2$kmeans4)


podr <- read.csv(file.choose(), dec = ",")
podr

set.seed(1234) # для воспроизводимости выделите эту строку и следующую и запустите
data1 <- podr[sample(nrow(podr), 1000), ]

data1$gradyear = NULL
data1$gender = NULL
data1$age = NULL

View(data1)

data1 <- na.omit(data1)

dz <- data1 %>% select(friends:softball)
View(dz)
Mdist <- dist(dz)

hc1 <- hclust(Mdist, method = "single")
plot(hc1)
hc1

fviz_nbclust(dz, kmeans, method = "wss") +
  labs(subtitle = "Elbow method") 

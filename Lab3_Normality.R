df <- read.table(file = file.choose(), header = TRUE, sep = ";", dec = ",")

#�����������: �������� ������ � ���������� �������������
hist(df$height, breaks = 10, freq = F,
     col = "navajowhite", xlab = "Height", main = "Histogram of height")
lines(density(df$height), col = "red", lwd = 2)
curve(dnorm(x, mean(df$height), sd(df$height)), add=TRUE, col="darkblue", lwd=2) 

#������ ��������-��������, q-q plot, quantile-quantile plot
df$height_std <- scale(df$height)
summary(df$height_std)
qnorm(0.1, mean = 0, sd = 1)
quantile(df$height_std, 0.1)
qqnorm(df$height_std)
qqline(df$height_std)
qqline(df$height_std, distribution = qnorm, col = "red")

#�������� �������
#���� p-value ������ ������ ����������, �� ��������� H1

install.packages(pkgs=c("nortest"))
library(nortest)
install.packages("moments")
library(moments)

set.seed(123) #��� ������� ��������� �������������� ���������� ��������� ��������� �����
#���� �� �� �� ���������, �� ������� �������� �������, ��� ������� �������� ��������� ������� ��������
sample2 <- rnorm(n = 200) #���������� ������� �� ������������ ����������� �������������

#��������� �����������-��������
ks.test(sample2, "pnorm") #�������� ������� �� ������������
ks.test(df$height, "pnorm", 
        mean = mean(df$height, na.rm = T), 
        sd = sd(df$height, na.rm = T)) #�������� ������� �� ���� ������ �� ������������

shapiro.test(df$height) #�������� ������-�����

lillie.test(df$height) #�������� ����������

cvm.test(df$height) #�������� �������-��� ������ (��������)

ad.test(df$height) #�������� ���������-��������

sf.test(df$height) #�������� ������-�������

pearson.test(df$height) #�������� ��-������� �������

jarque.test(df$height) #�������� �����-����
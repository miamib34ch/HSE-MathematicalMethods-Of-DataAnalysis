df <- read.csv("https://raw.githubusercontent.com/agconti/kaggle-titanic/master/data/train.csv")
df <- na.omit(df)
surv1 <- df[df$Survived == 1, ] #���� ��������
notSurv = df[df$Survived == 0, ] #���� ����������

t.test(surv1$Age, mu = 30) #�������� ��������� � ��������� �������� �����, �� ��������� ����������� ��������(�1 != �����)
t.test(surv1$Age, mu = 30, alternative = "less") # H1: mu < 30

#�������� ������� � �����������
tapply(surv1$Age, surv1$Pclass, length) #����� ������� �������� � ������ ������ ����������
prop.test(122, 290, p = 0.25) #(����� ��������, ����� ����������, �����������) ���� ���������� ������� ������ ����� ��� �� ����� 0.25
prop.test(122, 290, p = 0.25, alternative = "greater")
#������ ��� �������� ��������� � �������. ����������� ��������� �������������� ����� ������, ���� ��� ���� ����������
set.seed(42)
data <- rnorm(n = 30, mean = 7, sd = 2)

#�������� �����������
hist(data)

#�������� ������ ���������
qqnorm(data)

#�������� ���� ������-�����
shapiro.test(data)

#�������� ��������, ��� ������ � ������ ������������ ���������. 
#������, ����� ��������� t-test
t.test(data, mu = 5.31)

#������ ��� ���������� ���������
library(tidyverse)
df <- read_csv("https://raw.githubusercontent.com/agricolamz/DS_for_DH/master/data/icelandic.csv")

#������� �� ���������� ������� �������������� ���������
df %>% 
  filter(aspiration == "yes") %>% 
  pull(vowel.dur) -> asp

#������� �� ���������� ������� ���������������� ���������
df %>% 
  filter(aspiration == "no") %>% 
  pull(vowel.dur) -> non_asp

#�������� �� ������������ ������ ����������
hist(asp)
qqnorm(asp)
shapiro.test(asp)

#�������� �� ������������ ������ ����������
hist(non_asp)
qqnorm(non_asp)
shapiro.test(non_asp)

#������ ���������� ������������ �� ���������, ������� ����� ����������������� ����
wilcox.test(asp, non_asp)
wilcox.test(asp, non_asp, paired = TRUE)
wilcox.test(data, mu = 5.31)

#������ ��� ���������� � ������������ �������
dialect_forms <- read_csv("https://raw.githubusercontent.com/agricolamz/DS_for_DH/master/data/dialect_forms_fake.csv")

#������ �������� �������������
table(dialect_forms)
#��������� � ���� �������� �� �������
chisq.test(table(dialect_forms))
#��������� �� ������� ��������� � ����������� ��������
chisq.test(table(dialect_forms))$expected
chisq.test(table(dialect_forms))$observed
#������ ���� ������
fisher.test(table(dialect_forms))

#������� ��� ����� � ���������
dialect_sounds <- read.csv('https://clck.ru/YJahM')
mcnemar.test(table(dialect_sounds))

#����������� ��������� ������, ����� ���������, ���� �� ����� ���� ����������
set.seed(42)
m <- rnorm(700,mean = 50,sd=10)
n <- m+ rnorm(700,mean = 50,sd=10)
#�������� �������� ���������
plot(m,n)
#������ ����� ������
abline(lm(n~m),col='red',lwd=2) #� ������� lm �� ������ ����� ��������� ����������
#��������� ���������� ������� ��������
cor(m,n, method='spearman')



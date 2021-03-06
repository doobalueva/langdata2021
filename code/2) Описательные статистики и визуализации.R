# ������������� �������� ��� �������� 
install.packages("DescTools")
library(DescTools)

lang <- c(15, 20, 5, 8, 12, 7, 3)
MeanCI(lang, conf.level = 0.95)

library(tidyverse)

#�������� ������� � ��������� �� ���� �������������� ����������
dataset <- read_tsv('https://clck.ru/XYy7S')
dataset$ge_function #���� ���������� �� �������� ����� � ������� �������

#������ �� ���������������� ������ �������������� (�� ������� � ������ ������ (�����������) ������� ����� �������� � ��������� ���� ���������)
dataset %>% 
  count(ge_function) -> geste_types

#������������� �������������� ������
pie(geste_types$n, labels = geste_types$ge_function)
barplot(height=geste_types$n,names.arg=geste_types$ge_function, main="������� ����� ������ ��� �����������") 

#������ ������� �� ������� �������� �������������� ����������
dataset1 <- read_tsv('https://clck.ru/XYvxL')
dataset1$sDifference

#�������� ����������� � ������� �� � ����������
h <- hist(dataset1$sDifference)

h$breaks #��������� �� ������� ��� x
h$counts #��������� �� ��, ������� �������� � ������ ���������

#������� �������, ������� � ���� �������������
median(dataset1$sDifference)
mean(dataset1$sDifference)

install.packages('modeest')
library(modeest)
mlv(dataset1$sDifference, method = "mfv") #������� ����

#�������� ������ ���������
plot(density(dataset1$sDifference))

#�������� ���������� ���������
install.packages('vioplot')
library(vioplot)
vioplot(dataset1$sDifference)

#���� � �����
boxplot(dataset1$sDifference)


# ��������� ���� ������� �����, ����� ������ ������� ��������������� ����� ������
n <- 10000
ii <- rbinom(n, 1, 0.5)
data <- rnorm(n, mean = 130, sd = 10) * ii +
  rnorm(n, mean = 80, sd = 5) * (1 - ii)
data <- c(data,210) #��������� ���� "������"

#� ������ �������� �� ���� ��� ��������� �������, ������� �� � ������, ����� ����� ���������� ���������
boxplot(data)
hist(data)
plot(density(data))
vioplot(data)

#� ������� ����� ����� �������� �� ����� ggplot2 (�������� ����������� �� ��� �� ���������� sDifference)
install.packages('ggplot2')
library(ggplot2)

dataset1 %>% 
  ggplot(aes(sDifference)) +
  geom_histogram()

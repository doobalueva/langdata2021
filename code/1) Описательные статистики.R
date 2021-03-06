##### �������� 09.09 #####


# ���������� � ���������� �����

a <- 2

# ���������� � ���������� ������

b <- 'string'

# ���������� � ���������� ������

c <- c(1,2,3,4,5)

# � ��������� ����� �������� ����������� �������������� ��������

d <- c(1,2,3,4,5) * c(1,2,3,4,5)


# ������� ��������������
mean(c(15, 20, 5, 8, 12, 7, 3))

# ���������
var(c(15, 20, 5, 8, 12, 7, 3))

# ����������� ����������
sd(c(15, 20, 5, 8, 12, 7, 3))

# ����������� ���������� �� �����������
sqrt(var(c(15, 20, 5, 8, 12, 7, 3)))


# ��� ������� ����� ��������� ������������ ���������
summary(c(15, 20, 5, 8, 12, 7, 3))

# ��� ������ ������������ ���������, ������ ����� ������� � ���������� ����� "����"
install.packages("psych")
library(psych)
describe(c(15, 20, 5, 8, 12, 7, 3))

# ������� ���������� ��������������� ������ �� R: ���������� ���������� tidyverse � ������ �����
install.packages("tidyverse")
library(tidyverse)

c(15, 20, 5, 8, 12, 7, 3) %>% 
  mean() %>% 
  sqrt()

#������ ���� ���������� ����, ����� ��������
sqrt(mean(c(15, 20, 5, 8, 12, 7, 3)))

#�������� ������ ������� �� ������ � ���������� ������� � ������ ����
repr <- c(100, 23,9,9,12,37,23,24,24) 
value <- c("0", "2", "3", "4", "5", '6-7','8-9','10+', '???')

#�������� ���������
pie(repr, labels = value, main="������� ������� � ������ ����")

#����������� ���������. ����������� ���������� �����, ����� ������� ��� �� ����� ���������� �����
barplot(height=repr,names.arg=value, main="������� ������� � ������ ����") 

#���� � �����
boxplot(repr)

#�������� ������������� ������ �� ����� CSV

dataset <- read.csv('file.csv')

#�������� ������������� ������ �� ����� TSV

dataset <- read.csv('file.csv', sep='\t')

#������ ������ ������������� tsv, �� ����� ���� ���� ���������� ���������� tidyverse (�� �� ���� ��� ����������)




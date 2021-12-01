# ƒоверительный интервал дл€ среднего 
install.packages("DescTools")
library(DescTools)

lang <- c(15, 20, 5, 8, 12, 7, 3)
MeanCI(lang, conf.level = 0.95)

library(tidyverse)

#«агрузим датасет и посмотрим на одну категориальную переменную
dataset <- read_tsv('https://clck.ru/XYy7S')
dataset$ge_function #одну переменную из датасета берут с помощью доллара

#ƒелаем из неагрегированных данных агрегированные (из столбца с типами жестов (категори€ми) создаем новую табличку с частотами этих категорий)
dataset %>% 
  count(ge_function) -> geste_types

#¬изуализируем категориальные данные
pie(geste_types$n, labels = geste_types$ge_function)
barplot(height=geste_types$n,names.arg=geste_types$ge_function, main="—колько каких жестов нам встретилось") 

#“еперь возьмем из другого датасета количественную переменную
dataset1 <- read_tsv('https://clck.ru/XYvxL')
dataset1$sDifference

#ѕостроим гистограмму и запишем ее в переменную
h <- hist(dataset1$sDifference)

h$breaks #посмотрим на делени€ оси x
h$counts #посмотрим на то, сколько значений в каждой категории

#сравним среднее, медиану и моду распределени€
median(dataset1$sDifference)
mean(dataset1$sDifference)

install.packages('modeest')
library(modeest)
mlv(dataset1$sDifference, method = "mfv") #выводит моду

#ѕостроим график плотности
plot(density(dataset1$sDifference))

#ѕостроим скрипичную диаграмму
install.packages('vioplot')
library(vioplot)
vioplot(dataset1$sDifference)

#ящик с усами
boxplot(dataset1$sDifference)


# —ледующие п€ть строчек нужны, чтобы задать большой мультимодальный набор данных
n <- 10000
ii <- rbinom(n, 1, 0.5)
data <- rnorm(n, mean = 130, sd = 10) * ii +
  rnorm(n, mean = 80, sd = 5) * (1 - ii)
data <- c(data,210) #ƒобавл€ем один "выброс"

#ј теперь построим по нему все возможные графики, сравним их и поймем, зачем нужна скрипична€ диаграмма
boxplot(data)
hist(data)
plot(density(data))
vioplot(data)

#» наконец краем глаза взгл€нем на пакет ggplot2 (построим гистограмму на той же переменной sDifference)
install.packages('ggplot2')
library(ggplot2)

dataset1 %>% 
  ggplot(aes(sDifference)) +
  geom_histogram()

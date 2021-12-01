#Задача про скорость говорения в деревне. Сгенерируем нормально распределенный набор данных, типа это наши испытуемые
set.seed(42)
data <- rnorm(n = 30, mean = 7, sd = 2)

#Построим гистограмму
hist(data)

#Построим график квантилей
qqnorm(data)

#Применим тест Шапиро-Уилка
shapiro.test(data)

#Проверки показали, что данные и правда распределены нормально. 
#Значит, можно применить t-test
t.test(data, mu = 5.31)

#Задача про исландские согласные
library(tidyverse)
df <- read_csv("https://raw.githubusercontent.com/agricolamz/DS_for_DH/master/data/icelandic.csv")

#Вытащим из датафрейма выборку аспирированных согласных
df %>% 
  filter(aspiration == "yes") %>% 
  pull(vowel.dur) -> asp

#Вытащим из датафрейма выборку неаспирированных согласных
df %>% 
  filter(aspiration == "no") %>% 
  pull(vowel.dur) -> non_asp

#Проверим на нормальность первую переменную
hist(asp)
qqnorm(asp)
shapiro.test(asp)

#Проверим на нормальность вторую переменную
hist(non_asp)
qqnorm(non_asp)
shapiro.test(non_asp)

#Вторая переменная распределена не нормально, значитЮ нужен непараметрический тест
wilcox.test(asp, non_asp)
wilcox.test(asp, non_asp, paired = TRUE)
wilcox.test(data, mu = 5.31)

#Задача про диалектную и недиалектную графику
dialect_forms <- read_csv("https://raw.githubusercontent.com/agricolamz/DS_for_DH/master/data/dialect_forms_fake.csv")

#Делаем табличку сопряженности
table(dialect_forms)
#Применяем к этой табличке хи квадрат
chisq.test(table(dialect_forms))
#Посмотрим на таблицы ожидаемых и наблюдаемых значений
chisq.test(table(dialect_forms))$expected
chisq.test(table(dialect_forms))$observed
#Точный тест Фишера
fisher.test(table(dialect_forms))

#Задачка про звуки в диалектах
dialect_sounds <- read.csv('https://clck.ru/YJahM')
mcnemar.test(table(dialect_sounds))

#Сгенирируем рандомные данные, чтобы проверить, есть ли между ними корреляция
set.seed(42)
m <- rnorm(700,mean = 50,sd=10)
n <- m+ rnorm(700,mean = 50,sd=10)
#Построим точечную диаграмму
plot(m,n)
#Строим линию тренда
abline(lm(n~m),col='red',lwd=2) #В функции lm на первом месте зависимая переменная
#Посчитаем корреляцию методом Спирмена
cor(m,n, method='spearman')



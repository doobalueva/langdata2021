#загрузим еще раз датасет про узнавание английских слов и сделаем модель
english <- read.csv('https://clck.ru/YTHoq')
mod2 <- lm(data = english, RTlexdec ~ LengthInLetters + WrittenFrequency +
             NumberSimplexSynsets + WordCategory)
mod2
summary(mod2)

#запустив функцию plot, можно увидеть гетероскедастичность и влиятельные наблюдения
plot(mod2)

#скорректируем рассчет стандартных ошибок - поменяются и пи вэльюс
library(lmtest)
library(sandwich)
coeftest(mod2, vcov = vcovHC(mod2, type = "HC0"))

#удалим влиятельное наблюдение
english1 <- english[ -2603, ]

#и посмотрим на модель без него
mod1 <- lm(data = english1, RTlexdec ~ LengthInLetters + WrittenFrequency +
                NumberSimplexSynsets+WordCategory)
summary(mod1)

#сделаем три разные модели
mod2 <- lm(data = english, RTlexdec ~ LengthInLetters + WrittenFrequency +
             NumberSimplexSynsets + WordCategory)
mod3 <- lm(data = english, RTlexdec ~ LengthInLetters + WrittenFrequency +
             NumberSimplexSynsets)
mod4 <- lm(data = english, RTlexdec ~ NumberSimplexSynsets + WordCategory)

#сравним их при помощи критерия акаике
AIC(mod2,mod3,mod4)

#а теперь вытащим из их саммари их r квадраты
sum2 <- summary(mod2)
sum3 <- summary(mod3)
sum4 <- summary(mod4)

sum2$adj.r.squared
sum3$adj.r.squared
sum4$adj.r.squared

#Логистическая регрессия
#Загрузим датасет про абруптивные согласные
phon <- read.csv('https://raw.githubusercontent.com/agricolamz/r_on_line_course_data/master/phoible_ejectives.csv')

#Сделаем логистическую регрессию
logit <- glm(data = phon, have_ejectives ~ total + area, family = 'binomial')
summary(logit)

#Возьмем логарифм от коэффициентов
exp(coef(logit))

#Посмотрим на предсказанные значения (двумя спосабами, а потом сделаем из них красивую табличку)
predict(logit,type='response')
logit$fitted.values
library(tidyverse)
predictions <- tibble(predictions = logit$fitted.values)

#x*1.08 = 1.08x = x + 0.08x

#Сделаем матрицу ошибок при пороге 50%
treshold <- 0.5
predicted_values <- ifelse(logit$fitted.values>treshold,TRUE,FALSE)
actual_values <- phon$have_ejectives
conf_matrix <- table(predicted_values,actual_values)

#Посчитаем чувствительность и специфичность
library(caret)
sensitivity(conf_matrix)
specificity(conf_matrix)

#Посчитаем AUC и построим ROC-кривую
library(pROC)
roc <- roc(actual_values~logit$fitted.values)
roc$auc
plot(roc)
text(x = 1, y = 0.8, "AUC: 0.85")

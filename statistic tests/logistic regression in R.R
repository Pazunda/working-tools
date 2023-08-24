# Используем модельные данные о соотношении среднего и высшего образования в американских школах.
# Данные доступны по ссылке: https://stepic.org/media/attachments/lesson/11478/data.csv 
# Про часть испытуемых известно, поступили они в университет или нет
# (переменная admit, 1 = поступили, 0 = не поступили), про остальных таких данных нет (NA). 
# По имеющимся данным в переменной admit постройте логистическую регрессионную модель,
# предсказывающую результат поступления по престижности учебного заведения среднего образования
# (переменная rank, 1 — наиболее престижное, 4 — наименее престижное) и результатов GPA (переменная gpa)
# с учётом их взаимодействия. Примените эту модель к той части данных, где результат поступления неизвестен.

# Ответом в задаче будет предсказанное моделью число поступивших из тех,
# для кого результат поступления был неизвестен. Считаем человека поступившим,
# когда вероятность его поступления не меньше 0.4.

# считываем и смотрим датасет
df <- read.csv('https://stepic.org/media/attachments/lesson/11478/data.csv')
str(df)

# делаем столбцы факторами
df$admit <- factor(df$admit)
df$rank <- factor(df$rank)

# Делим датасет на обучающий (с известным значением admit) и обучаемый (с неизвестным значением admit)
learn <- subset(df,!is.na(df$admit))
teach <- subset(df,is.na(df$admit))

# строим модель по обучающему датасету
fit <- glm(admit ~ gpa*rank,learn, family = 'binomial')
summary(fit)

# предсказываем вероятности
head(predict(object = fit,type = 'response'))
learn$prob <- predict(object = fit,type = 'response')

# строим ROC кривую, а также кривые специфичности, чувствительности и точности.
# По идее мы должны были определить порог отсечения(~0.355), но он у нас задан как 0.4

library(ROCR)
pred_fit <- prediction(learn$prob,learn$admit)
perf_fit <- performance(pred_fit,'tpr','fpr')
plot(perf_fit,colorize = T,print.cutoffs.at = seq(0,1, by = 0.1))

perf3 <- performance(pred_fit,x.measure = 'cutoff', measure = 'spec')
perf4 <- performance(pred_fit,x.measure = 'cutoff', measure = 'sens')
perf5 <- performance(pred_fit,x.measure = 'cutoff', measure = 'acc')

plot(perf3,col = 'red',lwd = 2)
plot(add = T,perf4,col = 'green',lwd = 2)
plot(add = T,perf5,lwd = 2)
abline(v = 0.4,lwd = 2)

# обучаем наш датасет с неизвестными значениями admit на построенной модели

teach$prob <- predict(fit,newdata = teach,type = 'response')
teach$pred_resp <- (ifelse(teach$prob > 0.4,1,0))

# считаем предсказанное количество поступивших для ответа
sum(teach$pred_resp)



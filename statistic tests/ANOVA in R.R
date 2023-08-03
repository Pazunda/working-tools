
library(ggplot2)

mydata <- read.csv('https://stepic.org/media/attachments/lesson/11505/shops.csv')
str(mydata)

# one-way ANOVA

boxplot(price ~ origin, data = mydata)

ggplot(mydata,aes(x = origin, y = price)) +
  geom_boxplot()


fit <- aov(price ~ origin,data = mydata)

summary(fit)

# видим, что происхождение продукта значимо влияет на цену

#two-way ANOVA

fit1 <- aov(price ~ origin + store,data = mydata)
summary(fit1)

# видим, что происхождение продукта значимо влияет на цену, а тим магазина не влияет на цену
# выведем некоторые дополнительную информацию по модели
model.tables(fit1,'means')

# interaction
pd = position_dodge(0.1)
ggplot(mydata,aes(x = store,y = price,color = origin,group = origin )) + 
  stat_summary(fun.data = mean_cl_boot,geom = 'errorbar',width = 0.2, lwd = 0.8,position = pd) +
  stat_summary(fun.data = mean_cl_boot,geom = 'line',size = 1, position = pd) +
  stat_summary(fun.data = mean_cl_boot,geom = 'point',size = 3, position = pd, pch = 1.5) + 
  theme_bw()

fit3 = aov(price ~ origin + store + origin:store,data = mydata)
summary(fit3)

# Воспользуемся встроенными данными npk, иллюстрирующими влияние применения различных удобрений
# на урожайность гороха (yield). Нашей задачей будет выяснить, существенно ли одновременное
# применение азота (фактор N) и фосфата (фактор P). Примените дисперсионный анализ,
# где будет проверяться влияние фактора применения азота (N), влияние фактора применения фосфата (P)
# и их взаимодействие.
# В ответе укажите p-value для взаимодействия факторов N и P.

data <- npk
str(data)
fit_pr <- aov(yield ~ N*P,data)
summary(fit_pr)

ggplot(data,aes(x = P,y = yield,color = N,group = N )) + 
  stat_summary(fun.data = mean_cl_boot,geom = 'errorbar',width = 0.2, lwd = 0.8,position = pd) +
  stat_summary(fun.data = mean_cl_boot,geom = 'line',size = 1, position = pd) +
  stat_summary(fun.data = mean_cl_boot,geom = 'point',size = 3, position = pd, pch = 1.5) + 
  theme_bw()

# Теперь проведите трехфакторный дисперсионный анализ,
# где зависимая переменная - это урожайность (yield), а три фактора - типы удобрений (N, P, K).
# После проведения данного анализа вы получите три значения p - уровня значимости
# (о значимости каждого из факторов).
# Соотнесите названия факторов и значения p - уровня значимости.

fit_pr <- aov(yield ~ N + P + K,data)
summary(fit_pr)

# Pairwise comparisions
ggplot(mydata,aes(x = food, y = price)) +
  geom_boxplot()

fit5 <- aov(price ~ food,mydata)
summary(fit5)

TukeyHSD(fit5)


# Проведите однофакторный дисперсионный анализ на встроенных данных iris.
# Зависимая переменная - ширина чашелистика (Sepal.Width), независимая переменная - вид (Species).
# Затем проведите попарные сравнения видов. Какие виды статистически значимо различаются по ширине
# чашелистика (p < 0.05)?
str(iris)

ggplot(iris,aes(x = Species, y = Sepal.Width)) +
  geom_boxplot()

TukeyHSD(aov(Sepal.Width ~ Species,iris))


# repeated measures

mydata2 <- read.csv('https://stepic.org/media/attachments/lesson/11505/therapy_data.csv')
str(mydata2)

mydata2$subject <- as.factor(mydata2$subject)
fit6 <- aov(well_being ~ therapy,mydata2)
summary(fit6)

# внезапно состояние не зависит от терапии
# Для того, чтобы учесть, что каждый пациент проходил три курса терапии сделаем следующее

fit6b <-aov(well_being ~ therapy +Error(subject/therapy),data = mydata2)
summary(fit6b)


fit7 <- aov(well_being ~ therapy*price,mydata2)
summary(fit7)

ggplot(mydata2,aes(x = price, y = well_being )) +
  geom_boxplot()

fit7b <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)),data = mydata2)
summary(fit7b)

ggplot(mydata2,aes(x = price, y = well_being )) +
  geom_boxplot() +
  facet_grid(~subject)


# В этой задаче вам дан набор данных, в котором представлена информация о температуре 
# нескольких пациентов, которые лечатся разными таблетками и у разных врачей.
# Проведите однофакторный дисперсионный анализ с повторными измерениями: влияние
# типа таблетки (pill) на температуру (temperature) с учётом испытуемого (patient).
# Каково p-value для влияния типа таблеток на температуру?

data2 <- read.csv('https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv')
data2$patient <- as.factor(data2$patient)

fit_pr3 <- aov(temperature ~ pill+ Error(patient/temperature),data2)
summary(fit_pr3)


# Теперь вашей задачей будет провести двухфакторный дисперсионный анализ с повторными измерениями:
# влияние факторов doctor, влияние фактора pill и их взаимодействие на temperature. 
# Учтите обе внутригрупповые переменные: и тот факт, что один и тот же больной принимает
# разные таблетки, и тот факт, что  один и тот же больной лечится у разных врачей! 
# Каково F-значение для взаимодействия факторов доктора (doctor) и типа таблеток (pill)?

fit_pr4 <- aov(temperature ~ doctor*pill+ Error(patient/doctor*pill),data2)
summary(fit_pr4)



obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2)) 
  
obj

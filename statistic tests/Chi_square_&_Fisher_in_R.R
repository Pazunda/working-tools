##########################################################
df <- read.csv("https://stepic.org/media/attachments/lesson/11502/grants.csv", stringsAsFactors = T)
t1 <- df$status
t1 <- table(t1)
t2 <- table(field=df$field,status=df$status)
t2

#Binomial test

binom.test(x = 5,n = 20, p = 0.5)

binom.test(t1)

######### Pirson`s Chi-square test #######
chi <- chisq.test(t1)
chi$expected
chi$observed
chi$p.value
chisq.test(t2)
chi2


####### Fisher`s exact test ##########
fish <- fisher.test(t2)
fish


# На основе таблицы HairEyeColor создайте ещё одну таблицу, в которой хранится информация
# о распределении цвета глаз у женщин-шатенок (Hair = 'Brown'). Проведите тест равномерности
# распределения цвета глаз у шатенок и выведите значение хи-квадрата для этого теста.

test <- HairEyeColor['Brown',,'Female']
chi <- chisq.test(test)
chi$statistic

# Воспользуемся данными diamonds из библиотеки ggplot2. При помощи критерия Хи - квадрат 
# проверьте гипотезу о взаимосвязи качества огранки бриллианта (сut) и его цвета (color).
# В переменную main_stat сохраните значение статистики критерия Хи - квадрат.
# Обратите внимание, main_stat должен быть вектором из одного элемента, а не списком (листом). 

typeof(main_stat)
tab <- table(diamonds$cut,diamonds$color)
main_stat <- chisq.test(tab)$statistic

# Опять воспользуемся данными diamonds из библиотеки ggplot2. При помощи критерия Хи - квадрат
# проверьте гипотезу о взаимосвязи цены (price) и каратов (carat) бриллиантов.
# Для этого сначала нужно перевести эти количественные переменные в формат пригодный для Хи - квадрат.
#Создайте две новые переменные в данных diamonds:
  
#  factor_price - где будет 1, если значение цены больше либо равно чем среднее, и 0, 
#  если значение цены ниже среднего цены по выборке.
#  factor_carat - где будет 1, если число карат больше либо равно чем среднее,
#  и 0, если ниже среднего числа карат по выборке.

# Важный момент - на больших данных цикл for() работает довольно медленно, 
# постарайтесь решить эту задачу без его использования!
# Используя эти шкалы при помощи Хи - квадрат проверьте исходную гипотезу.
# Сохраните в переменную main_stat значение критерия  Хи - квадрат

str(diamonds)

carat <- ifelse(diamonds$carat < mean(diamonds$carat),0,1 )
price <- ifelse(diamonds$price < mean(diamonds$price),0,1 )
tab <- table(carat,price)
chi <- chisq.test(tab)
main_stat <- chi$statistic
main_stat

# При помощи точного критерия Фишера проверьте гипотезу о взаимосвязи типа коробки передач (am)
# и типа двигателя (vs) в данных mtcars. Результат выполнения критерия сохраните в переменную.
# Получившийся p - уровень значимости сохраните в переменную fisher_test.


fish <- fisher.test(mtcars$am,mtcars$vs)
fisher_test <- fish$p.value
table(mtcars$am,mtcars$vs)
     

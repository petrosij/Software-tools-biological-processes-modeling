
library(dplyr)
library(corrplot)
library(lsr)  # для вычисления коэффициента V Крамера
library(questionr)  # для вычисления коэффициента V Крамера


rm(list = ls())

# Перечень AU в файле данных:

# AU01 - подниматель внутренней части брови
# AU02 - подниматель внешней части брови
# AU04 - опускатель брови
# AU05 - подниматель верхнего века
# AU06 - подниматель щеки
# AU07 - натягиватель века
#=========================МОИ======================
# AU09 - сморщиватель носа
# AU10 - подниматель верхней губы
# AU12 - подниматель уголка губы
#==================================================
# AU14 - ямочка
# AU15 - опускатель уголка губы
# AU17 - подниматель подбородка
# AU20 - растягиватель губ
# AU23 - натягиватель губ
# AU25 - губы разведены
# AU26 - челюсть опущена
# AU45 - моргание


# Считывание данных
r02 <- read.table("r02.csv", header = TRUE,
                          sep = ";", dec = ",")

# Агрегирование данных
AU09 <- data.frame(count(r02, mark, AU09_c))
AU09

AU10 <- data.frame(count(r02, mark, AU10_c))
AU10

AU12 <- data.frame(count(r02, mark, AU12_c))
AU12

cNames9 <- unique(AU09$AU09_c)
cNames9

rNames9 <- unique(AU09$mark)
rNames9

cNames10 <- unique(AU10$AU10_c)
cNames10

rNames10 <- unique(AU10$mark)
rNames10

cNames12 <- unique(AU12$AU12_c)
cNames12

rNames12 <- unique(AU12$mark)
rNames12

AU09matrix <- matrix(AU09$n, nrow = nrow(AU09) / 2, byrow = TRUE)
AU09matrix

row.names(AU09matrix) <- rNames9
colnames(AU09matrix) <- cNames9
AU09matrix

AU10matrix <- matrix(AU10$n, nrow = nrow(AU10) / 2, byrow = TRUE)
AU10matrix

row.names(AU10matrix) <- rNames10
colnames(AU10matrix) <- cNames10
AU10matrix

AU12matrix <- matrix(AU12$n, nrow = nrow(AU12) / 2, byrow = TRUE)
AU12matrix

row.names(AU12matrix) <- rNames12
colnames(AU12matrix) <- cNames12
AU12matrix

# Построение мозаичной диаграммы
mosaicplot(AU10matrix, main = 'AU10 Mosaic Plot',
           xlab = 'Mark', ylab = 'AU10_c', shade = TRUE)

mosaicplot(AU12matrix, main = 'AU12 Mosaic Plot',
           xlab = 'Mark', ylab = 'AU12_c', shade = TRUE)



# Тест хи-квадрат
AU10chisq <- chisq.test(AU10matrix)
AU10chisq


AU10chisq$observed  # таблица наблюдаемых значений (чисел)
round(AU10chisq$expected, 0)  # таблица ожидаемых значений (чисел)
round(AU10chisq$residuals, 0)  # таблица остатков Пирсона

AU12chisq <- chisq.test(AU12matrix)
AU12chisq


AU12chisq$observed  # таблица наблюдаемых значений (чисел)
round(AU12chisq$expected, 0)  # таблица ожидаемых значений (чисел)
round(AU12chisq$residuals, 0)  # таблица остатков Пирсона

# Визуализация таблицы остатков Пирсона
corrplot(AU10chisq$residuals, is.corr = FALSE, cl.pos = 'n')

AU10contrib <- AU10chisq$residuals ^ 2 / AU10chisq$statistic * 100
AU10contrib <- round(AU10contrib, 0)
  
corrplot(AU10contrib, is.corr = FALSE, cl.pos = 'n')  


corrplot(AU12chisq$residuals, is.corr = FALSE, cl.pos = 'n')

AU12contrib <- AU12chisq$residuals ^ 2 / AU12chisq$statistic * 100
AU12contrib <- round(AU12contrib, 0)

corrplot(AU12contrib, is.corr = FALSE, cl.pos = 'n')  
  

# Все возможные сравнения для 10
# {0-2} {0-3} {0-4} {0-5}
#       {2-3} {2-4} {2-5}
#             {3-4} {3-5}
#                   {4-5}


# {0-2}
AU10.0to2 <- AU10matrix[1:2, ]
AU10.0to2
chisq.test(AU10.0to2)  # различия есть (p value  мал) 

# {0-3}
AU10.0to3 <- AU10matrix[c(1,3), ]
AU10.0to3
chisq.test(AU10.0to3) # различия есть

# {0-4}
AU10.0to4 <- AU10matrix[c(1,4), ]
AU10.0to4
chisq.test(AU10.0to4)# различия есть

# {0-5}
AU10.0to5 <- AU10matrix[c(1,5), ]
AU10.0to5
chisq.test(AU10.0to5) # различия есть

# {2-3}
AU10.2to3 <- AU10matrix[2:3, ]
AU10.2to3
chisq.test(AU10.2to3) # различия есть

# {2-4}
AU10.2to4 <- AU10matrix[c(2,4), ]
AU10.2to4
chisq.test(AU10.2to4) # различий нет!!!! но не соседние (не обьединяем)

# {2-5}
AU10.2to5 <- AU10matrix[c(2,5), ]
AU10.2to5
chisq.test(AU10.2to5)# различия есть

# {3-4}
AU10.3to4 <- AU10matrix[3:4, ]
AU10.3to4
chisq.test(AU10.3to4) # различия есть

# {3-5}
AU10.3to5 <- AU10matrix[c(3,5), ]
AU10.3to5
chisq.test(AU10.3to5) # различия есть

# {4-5}
AU10.4to5 <- AU10matrix[4:5, ]
AU10.4to5
chisq.test(AU10.4to5)  # различия есть

# Все возможные сравнения для 12
# {0-2} {0-3} {0-4} {0-5}
#       {2-3} {2-4} {2-5}
#             {3-4} {3-5}
#                   {4-5}


# {0-2}
AU12.0to2 <- AU12matrix[1:2, ]
AU12.0to2
chisq.test(AU12.0to2)  # различия есть (p value  мал) 

# {0-3}
AU12.0to3 <- AU12matrix[c(1,3), ]
AU12.0to3
chisq.test(AU12.0to3) # различия есть

# {0-4}
AU12.0to4 <- AU12matrix[c(1,4), ]
AU12.0to4
chisq.test(AU12.0to4)# различий нет (p = 0.02) но не соседние (не обьединяем)

# {0-5}
AU12.0to5 <- AU12matrix[c(1,5), ]
AU12.0to5
chisq.test(AU12.0to5) # различия есть

# {2-3}
AU12.2to3 <- AU12matrix[2:3, ]
AU12.2to3
chisq.test(AU12.2to3) # различия есть

# {2-4}
AU12.2to4 <- AU12matrix[c(2,4), ]
AU12.2to4
chisq.test(AU12.2to4) # различий нет!!!! (p = 0.11) - но не соседние (не обьединяем)


# {2-5}
AU12.2to5 <- AU12matrix[c(2,5), ]
AU12.2to5
chisq.test(AU12.2to5)# различия есть

# {3-4}
AU12.3to4 <- AU12matrix[3:4, ]
AU12.3to4
chisq.test(AU12.3to4) # различия есть

# {3-5}
AU12.3to5 <- AU12matrix[c(3,5), ]
AU12.3to5
chisq.test(AU12.3to5) # различия есть

# {4-5}
AU12.4to5 <- AU12matrix[4:5, ]
AU12.4to5
chisq.test(AU12.4to5)  # различия есть

# Вычисление коэффициента V Крамера
cramersV(AU10matrix)
cramer.v(AU10matrix)

cramersV(AU12matrix)
cramer.v(AU12matrix)


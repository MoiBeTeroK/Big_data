# Загрузка данных
data_csv <- read.csv("C:/Users/Людмила/Desktop/big data/rstudioLAB/Ответы.csv", row.names = 2, check.names = FALSE)
data_csv <- data_csv[-1]

# Анализ по каждой пицце
mean_scores <- sort(colMeans(data_csv), decreasing = TRUE)
mean_df <- data.frame(
  Пицца = names(mean_scores),
  Среднее = mean_scores,
  row.names = NULL
)

min_scores <- sort(apply(data_csv, 2, min))
min_df <- data.frame(
  Пицца = names(min_scores),
  Минимальное = min_scores,
  row.names = NULL
)

max_scores <- sort(apply(data_csv, 2, max))
max_df <- data.frame(
  Пицца = names(max_scores),
  Максимальное = max_scores,
  row.names = NULL
)

median_scores <- sort(apply(data_csv, 2, median))
median_df <- data.frame(
  Пицца = names(median_scores),
  Медиана = median_scores,
  row.names = NULL
)

find_mode <- function(values) {
  unique_values <- unique(values)
  counts <- tabulate(match(values, unique_values))
  
  max_count <- max(counts)
  
  mode_values <- unique_values[counts == max_count]
  if (length(mode_values) != 1) {
    return(NA)
  }
  return(mode_values)
}

mode_scores <- sort(sapply(data_csv, find_mode))
mode_df <- data.frame(
  Пицца = names(mode_scores),
  Мода = mode_scores,
  row.names = NULL
)

# Межквартильный рахмах
IQR(data_csv$Пепперони) 

summary(data_csv)

par(mar = c(5, 10, 4, 2))
boxplot(data_csv,  
        main = "Оценки пицц", 
        xlab = "Оценка", 
        las = 1,
        horizontal = TRUE,
        cex.axis = 0.8)

barplot(mean_scores, 
        main = "Общая столбчатая диаграмма", 
        xlab = "Среднее значение", 
        las = 1,
        horiz = TRUE, 
        cex.names = 0.8, 
        xlim = c(0, 10))




# Отедельный набор данных
subdataset <- data_csv[data_csv$Песто <=6,]
dimensions <- dim(subdataset)

median_pesto <- apply(subdataset, 2, median)
moda_pesto <- apply(subdataset, 2, find_mode)

average_values <- sort(colMeans(subdataset), decreasing=TRUE)

boxplot(subdataset,  
        main = "Оценки набора Песто <= 6", 
        xlab = "Оценка", 
        las = 1,
        horizontal = TRUE,
        cex.axis = 0.8)

barplot(average_values, 
        main = "Столбчатая диаграмма для Песто <= 6", 
        xlab = "Среднее значение", 
        las = 1,
        horiz = TRUE, 
        cex.names = 0.8)

hist(subdataset$Песто,
     main = "Оценки", 
     xlab = "Оценка", 
     ylab = "Частота", 
     breaks = seq(1, 10, by = 1))


merged_data <- merge(data_csv, subdataset, by.x = "row.names", by.y = "row.names", all.x = TRUE)
subdataset1 <- data_csv[data_csv$Песто == 10 & data_csv$Вегетарианская == 1 ,]
subdataset <- rbind(subdataset, subdataset1)

subdataset <- subdataset[!subdataset$Пепперони == 7,]









par(mar = c(8, 5, 4, 2))
boxplot(data_csv, 
        main = "Оценки пицц", 
        ylab = "Оценка", 
        las = 2) 

barplot(mean_scores, 
        main = "Общая столбчатая диаграмма", 
        ylab = "Среднее значение", 
        las = 2)

boxplot(subdataset,  
        main = "Оценки набора Песто <= 6", 
        ylab = "Оценка", 
        las = 2)

barplot(average_values, 
        main = "Столбчатая диаграмма для Песто <= 6", 
        ylab = "Среднее значение", 
        las = 2)


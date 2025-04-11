library(readxl)
setwd("C:/Users/Людмила/Desktop/big data/rstudioLAB")  

df <- read_excel("Covid_Russia.xlsx", sheet = 1)
names <- df$Город
df <- df[-1]
row.names(df) <- names

# 2 Дескриптивный анализ
summary(df)
means = apply(df, 2, mean)
medians = apply(df, 2, median)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
modas = apply(df, 2, getmode)


#дисперсия, стандартное отклонения, мин и макс
variances = apply(df, 2, var)
sds = apply(df, 2, sd)
min = apply(df, 2, min)
max = apply(df, 2, max)

par(mfrow = c(2, 3))
for (i in 1:ncol(df)) {
  boxplot(df[, i], 
          main = colnames(df)[i],
          ylab = "Значения")
}
for (i in 1:ncol(df)) {
  h <- hist(as.numeric(df[[i]]), plot = FALSE)
  bar_colors <- rainbow(length(h$counts))
  plot(h,
       main = colnames(df)[i],
       xlab = "Значения",
       ylab = "Частота",
       col = bar_colors,
       ylim = c(0, nrow(df)+15))
}

# 3 Оценить оптимальное число кластеров, для этого построить диаграмму "Метод
# силуэта", “Метод локтя”, "Статистику разрыва" и Алгоритм консенсуса
library(BBmisc)
df_scaled = scale(df)  # Стандартизация данных
df_sc_norm = normalize(df_scaled, method = "range", range = c(0, 1))  # Нормализация в диапазоне [0, 1]

library (factoextra)
library (cluster)
library(parameters)
# Метод силуэта
fviz_nbclust(df_sc_norm, kmeans, method = "silhouette") + labs(subtitle = "Метод силуэта")

# Метод локтя
fviz_nbclust(df_sc_norm, kmeans, method = "wss") + labs(subtitle = "Метод локтя")

# Статистика разрыва
gap_stat <- clusGap(df_sc_norm, FUN = kmeans, K.max = 12)
fviz_gap_stat(gap_stat) + labs(subtitle = "Статистика разрыва")

# Алгоритм консенсуса
n_clust <- n_clusters(data.frame(df_sc_norm),
                      package = c("easystats", "NbClust", "mclust"),
                      standardize = FALSE)
plot(n_clust)


# 4. Выполнить иерархическую кластеризацию вашего набора данных, построив дендрограмму
dist.datas=dist(df_sc_norm)
clust.datas=hclust(dist.datas,'ward.D')
plot(clust.datas,cex=0.5, ann = FALSE)
title(main = "Дендрограмма кластеризации (3 группы)")
rect.hclust(clust.datas,k=3,border="red")
plot(clust.datas,cex=0.5, ann = FALSE)
title(main = "Дендрограмма кластеризации (4 группы)")
rect.hclust(clust.datas,k=4,border="blue")

cluster_cut3 <- cutree(clust.datas, k = 3)
group1_indices <- which(cluster_cut3 == 1)
group1_data_norm <- df_sc_norm[group1_indices, ]
print(group1_data_norm)

# 5. Построить диаграмму со столбчатыми диаграммами и боксплотами групп.
plot_cluster_profiles <- function(clust_obj, df_norm, k) {
  cluster_cut <- cutree(clust_obj, k = k)
  cluster_means <- t(sapply(1:k, function(i) {
    colMeans(df_norm[cluster_cut == i, ])
  }))
  param_colors <- rainbow(ncol(df_norm))
  par(mfrow = c(1, k + 1), mar = c(6, 4, 4, 2), oma = c(0, 0, 2, 0), xpd = NA)
  for (i in 1:k) {
    barplot(cluster_means[i, ],
            col = param_colors,
            ylab = "Нормированное значение",
            names.arg = FALSE,
            ylim = c(0, 1))
    mtext(paste("Кластер", i), side = 1, line = 2, font = 2)
  }
  plot.new()
  legend("topright",
         legend = colnames(df_norm),
         fill = param_colors,
         bty = "n")
  mtext("Профили кластеров по нормированным COVID-показателям", outer = TRUE, cex = 1, font = 2, adj = 0.5)
}

plot_cluster_profiles(clust.datas, df_sc_norm, k = 3)
plot_cluster_profiles(clust.datas, df_sc_norm, k = 4)


plot_cluster_boxplots <- function(clust_obj, df_norm, k) {
  cluster_cut <- cutree(clust_obj, k = k)
  par(mfrow = c(2, 3), mar = c(5, 4, 4, 2), oma = c(0, 0, 2, 0))
  for (i in 1:ncol(df_norm)) {
    boxplot(df_norm[, i] ~ cluster_cut,
            main = colnames(df_norm)[i],
            ylab = "Нормированное значение",
            xlab = "Кластеры")
  }
  mtext(paste("Распределение показателей по", k, "кластерам"), outer = TRUE, cex = 1, font = 2)
}
plot_cluster_boxplots(clust.datas, df_sc_norm, k = 3)
plot_cluster_boxplots(clust.datas, df_sc_norm, k = 4)

# 6. Выполнить кластеризацию датасета по k-means
set.seed(123)
kmeans_3 <- kmeans(df_sc_norm, 3)
fviz_cluster(kmeans_3, df_sc_norm, palette="Set2", labelsize = 8) +
  ggtitle("K-means кластеризация (k = 3)")
split(row.names(df), kmeans_3$cluster)

# Для 4 кластеров
set.seed(123)
kmeans_4 <- kmeans(df_sc_norm, 4)
fviz_cluster(kmeans_4, df_sc_norm, palette="Set2", labelsize = 8) +
  ggtitle("K-means кластеризация (k = 4)")

# 7. Выполнить построение scatterplot с помощью функций plot или pairs.
plot_cluster_pairs <- function(kmeans, k) {
  cluster_cut <- kmeans$cluster
  pairs(df_sc_norm,
        col = cluster_cut,
        pch = 19,
        main = paste("Диаграммы рассеяния с цветами кластеров (k =", k, ")"))
}
plot_cluster_pairs(kmeans_3, 3)
plot_cluster_pairs(kmeans_4, 4)

# 8.	Построить трехмерную кластеризацию по scatterplot3d.
library(scatterplot3d)
plot_3d_clusters <- function(df_norm, kmeans_result, k) {
  x <- df_norm[, 1]
  y <- df_norm[, 2]
  z <- df_norm[, 3]
  cluster_palette <- rainbow(k)
  point_colors <- cluster_palette[kmeans_result$cluster]
  scatterplot3d(x, y, z,
                color = point_colors,
                pch = 19,
                main = paste("3D кластеризация (k =", k, ")"),
                xlab = colnames(df_norm)[1],
                ylab = colnames(df_norm)[2],
                zlab = colnames(df_norm)[3])
  legend("topleft",
         legend = paste("Кластер", 1:k),
         col = cluster_palette,
         pch = 19,
         cex = 0.8,
         inset = 0.02,
         box.lty = 0)
}

plot_3d_clusters(df_sc_norm, kmeans_3, k = 3)
plot_3d_clusters(df_sc_norm, kmeans_4, k = 4)

library(readxl)
setwd("C:/Users/Людмила/Desktop/big data/rstudioLAB")  

df_mix <- read_excel("Olimp.xlsx", sheet = 1)
df_woman <- read_excel("Olimp.xlsx", sheet = 2)
df_man <- read_excel("Olimp.xlsx", sheet = 3)


# Столбчатые диаграммы
df_mix_count <- apply(df_mix[,-1], 2, sum)
df_woman_count <- apply(df_woman[,-1], 2, sum)
df_man_count <- apply(df_man[,-1], 2, sum)

par(mfrow = c(1, 3))

barplot(df_woman_count, 
        names.arg = colnames(df_woman)[-1], 
        col = "pink", 
        main = "Женщины", 
        xlab = "Места", 
        ylab = "Количество",
        )

barplot(df_mix_count, 
        names.arg = colnames(df_mix)[-1], 
        col = "lightblue", 
        main = "Микс", 
        xlab = "Места", 
        ylab = "Количество")

barplot(df_man_count, 
        names.arg = colnames(df_man)[-1], 
        col = "blue", 
        main = "Мужчины", 
        xlab = "Места", 
        ylab = "Количество")


years_woman <- df_woman$Год[df_woman[, 2] == 1]
years_mix <- df_mix$Год[df_mix[, 2] == 1]
years_man <- df_man$Год[df_man[, 2] == 1]
# Данные для первых мест
first_place_woman <- sum(df_woman[,2] == 1)
first_place_mix <- sum(df_mix[,2] == 1)
first_place_man <- sum(df_man[,2] == 1)


# Круговая диаграмма 
par(mfrow = c(1, 3))
legend_colors <- c("darkgreen", "blue", "red", "orange", "yellow", "green", "purple", "lightgreen")

if (first_place_woman == 0) {
  pie(c(1, 0), labels = first_place_woman, main = "Женщины", col = "gray")
  legend("topright", legend = c("Нет первых мест"), fill = "gray")
} else {
  pie(c(first_place_woman, 0), labels = first_place_woman, main = "Женщины", col = legend_colors)
  legend("topright", legend = paste(years_woman, collapse = ", "), fill = legend_colors)
}

if (first_place_mix == 0) {
  pie(c(1, 0), labels = first_place_mix, main = "Микс", col = "gray")
  legend("topright", legend = c("Нет первых мест"), fill = "gray")
} else {
  pie(c(first_place_mix, 0), labels = first_place_mix, main = "Микс", col = legend_colors)
  legend("topright", legend = paste(years_mix, collapse = ", "), fill = legend_colors)
}

if (first_place_man == 0) {
  pie(c(1, 0), labels = first_place_man, main = "Мужчины", col = "gray")
  legend("topright", legend = c("Нет первых мест"), fill = "gray")
} else {
  pie(c(first_place_man, 0), labels = first_place_man, main = "Мужчины", col = legend_colors)
  legend("topright", legend = paste(years_man, collapse = ", "), fill = legend_colors)
}


# Функциональные графики
plot_graph <- function(df, color, title) {
  total_places <- rowSums(df[, 2:4], na.rm = TRUE)
  matplot(df$Год, total_places,main = title,  type = "b", pch = 19, lty = 1, col = color,
          xlab = "Год", ylab = "Количество мест", xaxt = "n", 
          ylim = c(0, max(df[, 2]+0.5)))
  axis(1, at = df$Год, labels = df$Год)
}

par(mfrow = c(3, 1))

plot_graph(df_woman, "red", "Тенденции изменения кол-ва призовых мест для женщин за последние 30 лет")
plot_graph(df_man, "blue", "Тенденции изменения кол-ва призовых мест для мужчин за последние 30 лет")
plot_graph(df_mix, "darkgreen", "Тенденции изменения кол-ва призовых мест для микса за последние 30 лет")

# 3 Графики изменения спортивных достижений (золото)
df_gold <- read_excel("Olimp.xlsx", sheet = 4)

par(mar = c(5, 4, 4, 9)) 
matplot(df_gold$Год, 
        df_gold[, -1], 
        main = "Изменение кол-ва 1-х мест по 7-ми странам-призерам\n за последние 6 олимпиад", 
        type = "b", pch = 19, 
        lty = 1, 
        col = legend_colors,
        xlab = "Год", ylab = "Количество мест (золото)", 
        xaxt = "n", 
        ylim = c(0, max(df_gold[, -1]+10)))
axis(1, at = df_gold$Год, labels = df$df_gold)
legend("topright", legend = colnames(df_gold)[-1], xpd = TRUE, bty = "n", inset = -c(0.4, 0), fill = legend_colors)


# 4 Графики изменения спортивных достижений (все призовые)
df_prize_places <- read_excel("Olimp.xlsx", sheet = 5)

par(mar = c(5, 4, 4, 9)) 
matplot(df_prize_places$Год, 
        df_prize_places[, -1], 
        main = "Изменение кол-ва призовых мест по 7-ми странам-призерам\n за последние 6 олимпиад", 
        type = "b", pch = 19, 
        lty = 1, 
        col = legend_colors,
        xlab = "Год", ylab = "Количество мест (призовые)", 
        xaxt = "n", 
        ylim = c(0, max(df_prize_places[, -1]+10)))
axis(1, at = df_prize_places$Год, labels = df_prize_places$Год)
legend("topright", legend = colnames(df_prize_places)[-1], xpd = TRUE, bty = "n", inset = -c(0.35, 0), fill = legend_colors)


# Динамика фигурного катания по всем за последние 6 Олимпиад
all <- cbind(df_mix[, 1:4], df_woman[,2:4], df_man[,2:4])
total_places1 <- rowSums(df_mix[, 2:4], na.rm = TRUE)
total_places2 <- rowSums(df_woman[, 2:4], na.rm = TRUE)
total_places3 <- rowSums(df_man[, 2:4], na.rm = TRUE)
total_places_all <- cbind(df_mix[, 1], total_places1, total_places2 + 0.01, total_places3)

year <- all[1:6, ]
par(mfrow = c(1, 3), oma = c(0, 0, 2, 0))
matplot(total_places_all$Год, 
        total_places_all, 
        type = "b", pch = 19, 
        lty = c(1, 6, 3),
        col = c("blue", "darkgreen", "red"),
        xlab = "Год", ylab = "Количество мест (призовые)", 
        xaxt = "n", 
        lwd = 3,
        ylim = c(0, max(year[, -1])+0.5))
axis(1, at = year$Год, labels = year$Год)
legend('topleft', c('Мужчины', 'Микс', 'Женщины'), lty = c(1, 6, 3), cex = 0.8, lwd=4, col = c("blue", "darkgreen", "red"))



total_places_barplot <- cbind(df_mix[, 1], total_places1, total_places2, total_places3)
colnames(total_places_barplot) <- c("Год", "Микс", "Женщины", "Мужчины")
total_places_barplot_t <- t(total_places_barplot)
total_places_barplot_t_sorted <- total_places_barplot_t[, order(total_places_barplot_t[1, ])]
barplot(as.matrix(total_places_barplot_t_sorted[-1,]), 
        beside = TRUE,
        col = c("darkgreen", "blue", "red"), 
        xlab = "Год", 
        names.arg = total_places_barplot_t_sorted[1,],
        ylab = "Количество мест (призовые)", 
        ylim = c(0,3),
        cex.names = 0.8)
legend('topright', c('Микс', 'Женщины', 'Мужчины'), fill=c("darkgreen", "blue", "red"))



total_places_pie <- sapply(total_places_barplot[,-1], sum)
pie(total_places_pie, 
    labels=c(total_places_pie[1], total_places_pie[2], total_places_pie[3]),
    col=c("darkgreen", "blue", "red"), 
    sub="Кол-во призовых мест за 6 Олимпиад\n (микс, женщины, мужчины)")
legend('topright', c('Микс', 'Женщины', 'Мужчины'), fill=c("darkgreen", "blue", "red"))

mtext("Общее количество призовых мест на 6 Олимпиадах по фигурному катанию", 
      outer = TRUE, cex = 1, font = 2)


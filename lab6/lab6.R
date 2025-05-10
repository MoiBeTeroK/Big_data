library(car)
setwd("C:/Users/Людмила/Desktop/big data/rstudioLAB")  
df<-read.csv("athlete_events.csv", sep=",", header=TRUE)
df_figure <- df[df$Sport == "Figure Skating", ]

# 1. Дескриптивный анализ
df_numeric <- df_figure[, c("Age", "Height", "Weight")]

summary(df_numeric)
means = apply(df_numeric, 2, function(x) mean(x, na.rm = TRUE))
medians = apply(df_numeric, 2, function(x) median(x, na.rm = TRUE))
getmode <- function(v) {
  v <- na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
modas = apply(df_numeric, 2, getmode)

par(mfrow = c(2, 3))
for (i in 1:ncol(df_numeric)) {
  boxplot(df_numeric[, i], 
          main = colnames(df_numeric)[i],
          ylab = "Значения")
}
for (i in 1:ncol(df_numeric)) {
  h <- hist(as.numeric(df_numeric[[i]]), plot = FALSE)
  bar_colors <- rainbow(length(h$counts))
  plot(h,
       main = colnames(df_numeric)[i],
       xlab = "Значения",
       ylab = "Частота",
       col = bar_colors,
       ylim = c(0, 1000))
}

# 2. Провести проверку на нормальность и дисперсию. Из чего сделать вывод о требуемом тесте.
# Q-Q графики
df_grouped <- df_figure[, c("Sex", "Age", "Weight")]
df_grouped$Sex <- as.factor(df_grouped$Sex)
df_grouped <- na.omit(df_grouped)
par(mfrow = c(1, 3))
var_data <- df_grouped$Weight

# Q-Q график (базовый)
qqnorm(var_data, main = "Q-Q Plot: Weight")
qqline(var_data, col = "red")

# Q-Q график (из пакета car)
qqPlot(var_data, main = "Q-Q Plot (car): Weight")

# Гистограмма с нормальной кривой плотности
x2 <- seq(min(var_data), max(var_data), length.out = 100)
fun <- dnorm(x2, mean = mean(var_data), sd = sd(var_data))
hist(var_data, freq = FALSE, col = "gray", main = "Density Plot: Weight")
lines(x2, fun, col = 2, lwd = 2)

set.seed(0)
shapiro.test(var_data)

# Дисперсия
leveneTest(Weight ~ Sex, data = df_grouped)
boxplot(Weight ~ Sex, data = df_grouped, main = "Boxplot: Weight by Sex", ylab = "Weight")

# 3.
wilcox.test(var_data, mu=65)
wilcox.test(var_data, mu=59)

# 4. 
df_selected_sports <- df[df$Sport %in% c("Figure Skating", "Gymnastics"), c("Sex", "Age", "Height", "Weight", "Sport")]
df_selected_sports <- na.omit(df_selected_sports)

df_women <- df_selected_sports[df_selected_sports$Sex == "F", ]
df_men <- df_selected_sports[df_selected_sports$Sex == "M", ]

df_women_figure = df_women[df_women$Sport == "Figure Skating",]$Weight
df_women_gymnastics = df_women[df_women$Sport == "Gymnastics",]$Weight

df_men_figure = df_men[df_men$Sport == "Figure Skating",]$Weight
df_men_gymnastics = df_men[df_men$Sport == "Gymnastics",]$Weight

shapiro.test(df_women_figure)
shapiro.test(df_men_figure)

shapiro.test(sample(df_women_gymnastics, 5000))
shapiro.test(sample(df_men_gymnastics, 5000))

plot_graphs <- function(data, title_prefix) {
  par(mfrow = c(1, 2))
  
  # Q-Q график
  qqnorm(data, main = paste(title_prefix, "Q-Q Plot"))
  qqline(data, col = "red")
  
  # Гистограмма с нормальной кривой
  x2 <- seq(min(data), max(data), length.out = 100)
  fun <- dnorm(x2, mean = mean(data), sd = sd(data))
  hist(data, freq = FALSE, col = "gray", main = paste(title_prefix, "Density Plot"))
  lines(x2, fun, col = 2, lwd = 2)
}
plot_graphs(df_women_figure, "Women Figure Skating")
plot_graphs(df_women_gymnastics, "Women Gymnastics")

plot_graphs(df_men_figure, "Men Figure Skating")
plot_graphs(df_men_gymnastics, "Men Gymnastics")


wilcox.test(Weight ~ Sport, data = df_women)
wilcox.test(Weight ~ Sport, data = df_men)

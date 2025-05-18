# 7.1
ncol(longley)
nrow(longley)

summary(longley)
means = apply(longley, 2, mean)
medians = apply(longley, 2, median)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
modas = apply(longley, 2, getmode)


cor_matrix <- cor(longley)
heatmap(cor_matrix, main="Корреляционная матрица", col=topo.colors(10), scale="none", margins = c(8, 8))

par(mfrow=c(3, 3))
for (col in colnames(longley)) {
  qqnorm(longley[[col]], main=paste("Q-Q график", col))
  qqline(longley[[col]], col="red")
}
sapply(longley, function(x) shapiro.test(x))

# 7.2
# Строим матрицу диаграмм рассеяния
x<-rexp(50);
cor(x,log(x),method="spearman")
y <- log(x)

rank_x <- rank(x)
rank_y <- rank(y)

plot(rank_x, rank_y,
     main = "График рангов: rank(x) vs rank(log(x))",
     xlab = "Ранг x",
     ylab = "Ранг log(x)",
     pch = 19, col = "darkgreen")
abline(lm(rank_y ~ rank_x), col = "red")


# 7.
library(car)
library(stringr)

# 1.
setwd("C:/Users/Людмила/Desktop/big data/rstudioLAB")  
df_Sweden<-read.csv("Lab8_Data.csv", sep=",", header=TRUE, encoding="UTF-8")
df_Sweden[df_Sweden == ".."] <- NA

# 2.
extract_data <- function(df, codes) {
  subset <- df[df$Series.Code %in% codes, 4:ncol(df)]
  subset <- subset[, colSums(is.na(subset)) == 0]
  # Преобразуем в числовую матрицу без изменения имён столбцов и строк:
  numeric_subset <- matrix(as.numeric(as.matrix(subset)), nrow = nrow(subset))
  rownames(numeric_subset) <- rownames(subset)
  colnames(numeric_subset) <- colnames(subset)
  return(numeric_subset)
}

# a) Рост ВВП и прирост населения.
GDP_and_Population <- extract_data(df_Sweden, c("NY.GDP.MKTP.KD.ZG", "SP.POP.GROW"))
cor(GDP_and_Population[1,], GDP_and_Population[2,], method = "spearman")

x <- as.numeric(gsub("^X", "", colnames(GDP_and_Population)))
y <- as.numeric(GDP_and_Population[1,])

plot(x, y, ylim=c(min(y), max(y)), type="b", lty=1, pch=1, xlab="Года", ylab="Значения", main="ВВП", xaxt="n")
axis(1, at=x, labels=x, las=2)

# b) Прирост населения и безработица
pop_unemp <- extract_data(df_Sweden, c("SP.POP.GROW", "SL.UEM.ADVN.ZS"))
cor(pop_unemp[1,], pop_unemp[2,], method = "spearman")

# c) Расходы на медицину, продолжительность жизни и смертность
health <- extract_data(df_Sweden, c("SH.XPD.GHED.PC.CD", "SP.DYN.LE00.IN", "SP.DYN.CDRT.IN"))
cor(health[1,], health[2,], method = "spearman")
cor(health[1,], health[3,], method = "spearman")

# d) Образование, экспорт и высокотехнологичное производство
edu_prod <- extract_data(df_Sweden, c("SE.TER.CUAT.BA.ZS", "NE.EXP.GNFS.KD.ZG", "NV.MNF.TECH.ZS.UN"))
cor(edu_prod[1,], edu_prod[2,], method = "spearman")
cor(edu_prod[1,], edu_prod[3,], method = "spearman")

# e) Расходы на образование и бакалавры-женщины
edu_female <- extract_data(df_Sweden, c("SE.XPD.TOTL.GD.ZS", "SE.TER.CUAT.BA.FE.ZS"))
cor(edu_female[1,], edu_female[2,], method = "spearman")

# f) Высшее образование и статьи в научных журналах
edu_articles <- extract_data(df_Sweden, c("SE.TER.CUAT.BA.ZS", "IP.JRN.ARTC.SC"))
cor(edu_articles[1,], edu_articles[2,], method = "spearman")


# g) Найти и отобразить на графике наиболее коррелирующие параметры.
data_all <- df_Sweden[, -c(1,2)]
for (i in 2:ncol(data_all)) {
  data_all[[i]] <- as.numeric(data_all[[i]])
}
data_matrix <- as.data.frame(t(data_all[, -1]))
colnames(data_matrix) <- data_all$Series.Code
data_matrix <- data_matrix[, colSums(is.na(data_matrix)) <= 8]
data_matrix <- data_matrix[, colSums(is.na(data_matrix)) < nrow(data_matrix)]
cor_matrix <- cor(data_matrix, use = "pairwise.complete.obs", method = "spearman")

off_diag <- row(cor_matrix) != col(cor_matrix)

max_cor_value <- max(abs(cor_matrix[off_diag]), na.rm = TRUE)

which_max <- which(abs(cor_matrix) == max_cor_value & off_diag, arr.ind = TRUE)
var1 <- colnames(cor_matrix)[which_max[1, "col"]]
var2 <- rownames(cor_matrix)[which_max[1, "row"]]

cat("Наиболее скоррелированные параметры:", var1, "и", var2, "\n")

plot(data_matrix[[var1]], data_matrix[[var2]],
     xlab = var1, ylab = var2,
     main = paste("Корреляция Spearman =", round(max_cor_value, 2)),
     pch = 19, col = "blue")
abline(lm(data_matrix[[var2]] ~ data_matrix[[var1]]), col = "red")

# h) С помощью регрессионного анализа найдите зависимые переменные и поясните влияние на них независимых переменных.
results <- list()
for (target in colnames(data_matrix)) {
  predictors <- setdiff(colnames(data_matrix), target)
  formula <- as.formula(paste(target, "~", paste(predictors, collapse = "+")))
  model <- lm(formula, data = data_matrix)
  r2 <- summary(model)$r.squared
  results[[target]] <- r2
}
sorted_r2 <- sort(unlist(results), decreasing = TRUE)
top_targets <- names(sorted_r2)[1:3]
for (target in top_targets) {
  predictors <- setdiff(colnames(data_matrix), target)
  formula <- as.formula(paste(target, "~", paste(predictors, collapse = "+")))
  model <- lm(formula, data = data_matrix)
  
  cat("\nМодель для зависимой переменной:", target, "\n")
  print(summary(model)$coefficients)
}

# i) С помощью функции predict() постройте прогноз по любому понравившемуся Вам атрибуту.
target <- "NY.GDP.MKTP.KD.ZG"  # Прирост ВВП
predictors <- setdiff(colnames(data_matrix), target)
formula <- as.formula(paste(target, "~", paste(predictors, collapse = "+")))

model <- lm(formula, data = data_matrix, na.action = na.exclude)

required_vars <- all.vars(formula)
data_complete <- data_matrix[complete.cases(data_matrix[, required_vars]), ]

predictions <- predict(model, newdata = data_complete)
years <- as.numeric(gsub("^X", "", names(predictions)))
actual <- data_matrix[[target]]

x_filtered <- 1989:2017
actual_filtered <- actual[!is.na(actual)]


par(mfrow=c(1, 2)) 
plot(x_filtered, actual_filtered, type="l", col="blue", lwd=2,
     xlab="Годы", ylab="Прирост ВВП", main="Реальные значения ВВП")
plot(years, predictions, type="l", col="red", lwd=2,
     xlab="Годы", ylab="Прирост ВВП", main="Прогнозируемые значения ВВП")



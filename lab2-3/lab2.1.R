library(googlesheets4)
library(ggplot2)

sheet_url <- "https://docs.google.com/spreadsheets/d/1noCj_ju3l8lpR1jeR2dLDF32QdvHBnLwdkiF7vlihAo/edit?resourcekey=&gid=2042822275#gid=2042822275"
df <- read_sheet(sheet_url)

df_pizza <- df[-c(1, 2)]

mean_vals <- data.frame(Pizza_Type = character(), Rating = numeric())

for (col_name in names(df_pizza)) {
  x <- df_pizza[[col_name]]
  
  max_val <- max(x)
  min_val <- min(x)
  mean_val <- mean(x)
  
  mean_vals <- rbind(mean_vals, data.frame(Pizza_Type = col_name, Rating = mean_val))
  
  cat(col_name, ": max =", max_val, ", min =", min_val, ", mean =", mean_val, "\n")
}

# ///////////////////////////////////

sorted_pizza <- sort(colMeans(df_pizza), decreasing = TRUE)
for (pizza in names(sorted_pizza)) {
  print(paste(pizza, ": ", round(sorted_pizza[pizza], 2)))
}

# //////////////////////////////////

respondent_names <- df$Фамилия

for (col_name in names(df_pizza)) {
  x <- df_pizza[[col_name]]
  
  y1 <- x[x > 7]
  y2 <- x[x < 3]
  
  names1 <- respondent_names[x > 7]
  names2 <- respondent_names[x < 3]
  
  cat('Название пиццы:', col_name, '\n')
  cat('Кол-во людей, оценивших выше 7 баллов:', length(y1), '\n')
  cat('Вектор людей с оценкой выше 7:\n')
  if (length(names1) > 0) {
    print(names1)
  } else {
    cat("Людей нет\n")
  }
  
  cat('Кол-во людей, оценивших ниже 3 баллов:', length(y2), '\n')
  cat('Вектор людей с оценкой ниже 3:\n')
  if (length(names2) > 0) {
    print(names2)
  } else {
    cat("Людей нет\n")
  }
  cat('\n')
}

# //////////////////////////////////
 
ggplot(mean_vals, aes(x = Pizza_Type, y = Rating, fill = Pizza_Type)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(title = "Средняя оценка разных видов пиццы", x = "Вид пиццы", y = "Средняя оценка") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limits = mean_vals$Pizza_Type[order(mean_vals$Rating, decreasing = TRUE)]) +
  scale_y_continuous(limits = c(0, 10))

+



# sorted_pizza_df <- data.frame(Pizza_Type = names(sorted_pizza), Rating = sorted_pizza)
# 
# ggplot(mean_vals, aes(x = Pizza_Type, y = Rating, fill = Pizza_Type)) +
#   geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
#   labs(title = "Средняя оценка разных видов пиццы", x = "Вид пиццы", y = "Средняя оценка") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_x_discrete(limits = sorted_pizza_df$Pizza_Type) +
#   scale_y_continuous(limits = c(0, 10))

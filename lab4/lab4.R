library(rvest)
years <- 2014:2021
get_data <- function(year) {
  url <- paste0('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=', year)
  page <- read_html(url)
  nodes <- html_nodes(page, 'table#t2')
  df <- html_table(nodes) %>% as.data.frame()
  rownames(df) <- df[, 2]
  df <- df[, 3:11]
  return(df)
}

data_list <- lapply(years, get_data)
names(data_list) <- years

countries <- c('Russia', 'Ukraine', 'Belarus', 'Georgia', 'Armenia')

extract_index <- function(df, country, column_name ) {
  sapply(country, function(ctry) {
    if (ctry %in% rownames(df)) {
      value <- df[ctry, column_name]
      if (value == "-") {
        return(NA)
      } else {
        return(as.numeric(value))
      }
    } else {
      return(NA)
    }
  })
}
colors = c('blue', 'green', 'red', 'purple', 'darkred')

index_names <- colnames(data_list[[1]])
index_names_rus <- c(
  "Quality.of.Life.Index" = "Индекс качества жизни",
  "Purchasing.Power.Index" = "Индекс покупательной способности",
  "Safety.Index" = "Индекс безопасности",
  "Health.Care.Index" = "Индекс здравоохранения",
  "Cost.of.Living.Index" = "Индекс стоимости жизни",
  "Property.Price.to.Income.Ratio" = "Соотношение цен на жилье и доходов",
  "Traffic.Commute.Time.Index" = "Индекс времени в пути",
  "Pollution.Index" = "Индекс загрязнения",
  "Climate.Index" = "Индекс климата"
)
index_names_rus <- index_names_rus[index_names]

par(mar = c(5, 5, 4, 8), xpd = TRUE) 
for (i in 1:length(index_names)) {
  index <- index_names[i]
  matrix_data <- t(sapply(data_list, extract_index, countries, index))
  
  y_min <- min(matrix_data, na.rm = TRUE) - 5
  y_max <- max(matrix_data, na.rm = TRUE) + 5
  
  plot(NA, xlim = range(years), ylim = c(y_min, y_max),xlab = 'Года', ylab = index_names_rus[i],
       main = paste('Динамика', index_names_rus[i], 'по странам'))
  
  for (j in 1:length(countries)) {
    lines(years, matrix_data[, j], type = 'b', col = colors[j], lty = 1, pch = 16, lwd = 2)
  }
  legend("topright", inset = c(-0.25, 0), legend = countries, fill = colors, bty = 'n')
}

library(rvest)

# 1
url14<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2014')
url15<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2015')
url16<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2016')
url17<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2017')
url18<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2018')
url19<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2019')
url20<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2020')
url21<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2021')

# таблица по индексу
nodes14<-html_nodes(url14, 'table#t2')
nodes15<-html_nodes(url15, 'table#t2')
nodes16<-html_nodes(url16, 'table#t2')
nodes17<-html_nodes(url17, 'table#t2')
nodes18<-html_nodes(url18, 'table#t2')
nodes19<-html_nodes(url19, 'table#t2')
nodes20<-html_nodes(url20, 'table#t2')
nodes21<-html_nodes(url21, 'table#t2')

# датафрейм из таблицы
df_14<-html_table(nodes14)%>%as.data.frame()
df_15<-html_table(nodes15)%>%as.data.frame()
df_16<-html_table(nodes16)%>%as.data.frame()
df_17<-html_table(nodes17)%>%as.data.frame()
df_18<-html_table(nodes18)%>%as.data.frame()
df_19<-html_table(nodes19)%>%as.data.frame()
df_20<-html_table(nodes20)%>%as.data.frame()
df_21<-html_table(nodes21)%>%as.data.frame()

# страны как названия строк
rownames(df_14)<-df_14[, 2]
rownames(df_15)<-df_15[, 2]
rownames(df_16)<-df_16[, 2]
rownames(df_17)<-df_17[, 2]
rownames(df_18)<-df_18[, 2]
rownames(df_19)<-df_19[, 2]
rownames(df_20)<-df_20[, 2]
rownames(df_21)<-df_21[, 2]

# в датафреймах остаются только критерии оценивания
df_14<-df_14[, 3:11]
df_15<-df_15[, 3:11]
df_16<-df_16[, 3:11]
df_17<-df_17[, 3:11]
df_18<-df_18[, 3:11]
df_19<-df_19[, 3:11]
df_20<-df_20[, 3:11]
df_21<-df_21[, 3:11]

# 2
country <- c('Russia', 'Ukraine', 'Belarus', 'Georgia', 'Armenia')

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


# 3

frame1 <- as.data.frame(
  rbind(
    extract_index(df_14, country, "Quality.of.Life.Index"),
    extract_index(df_15, country, "Quality.of.Life.Index"),
    extract_index(df_16, country, "Quality.of.Life.Index"),
    extract_index(df_17, country, "Quality.of.Life.Index"),
    extract_index(df_18, country, "Quality.of.Life.Index"),
    extract_index(df_19, country, "Quality.of.Life.Index"),
    extract_index(df_20, country, "Quality.of.Life.Index"),
    extract_index(df_21, country, "Quality.of.Life.Index")
  )
)
row.names(frame1) <- 2014:2021
colnames(frame1) <- country

plot(NA, xlim = c(2014, 2021),  ylim=c(min(frame1, na.rm = TRUE)-5, max(frame1, na.rm = TRUE)+10),
     xlab='Года', ylab='Индекс качества жизни', 
     main='Сравнение индекса качества жизни')

lines(2014:2021, frame1$'Russia', type='b', col='blue', lty=1, pch=16, lwd=2)
lines(2014:2021, frame1$'Ukraine', type='b', col='green', lty=1, pch=16, lwd=2)
lines(2014:2021, frame1$'Belarus', type='b', col='red', lty=1, pch=16, lwd=2)
lines(2014:2021, frame1$'Georgia', type='b', col='purple', lty=1, pch=16, lwd=2)
lines(2014:2021, frame1$'Armenia', type='b', col='darkred', lty=1, pch=16, lwd=2)
legend('bottomright', cex=0.7, country, fill=colors)



frame2 <- as.data.frame(
  rbind(
    extract_index(df_14, country, "Purchasing.Power.Index"),
    extract_index(df_15, country, "Purchasing.Power.Index"),
    extract_index(df_16, country, "Purchasing.Power.Index"),
    extract_index(df_17, country, "Purchasing.Power.Index"),
    extract_index(df_18, country, "Purchasing.Power.Index"),
    extract_index(df_19, country, "Purchasing.Power.Index"),
    extract_index(df_20, country, "Purchasing.Power.Index"),
    extract_index(df_21, country, "Purchasing.Power.Index")
  )
)
row.names(frame2) <- 2014:2021
colnames(frame2) <- country

plot(NA, xlim = c(2014, 2021),  ylim=c(min(frame2, na.rm = TRUE)-5, max(frame2, na.rm = TRUE)+5),
     xlab='Года', ylab='Индекс покупательной способности', 
     main='Сравнение индекса покупательной способности')

lines(2014:2021, frame2$'Russia', type='b', col='blue', lty=1, pch=16, lwd=2)
lines(2014:2021, frame2$'Ukraine', type='b', col='green', lty=1, pch=16, lwd=2)
lines(2014:2021, frame2$'Belarus', type='b', col='red', lty=1, pch=16, lwd=2)
lines(2014:2021, frame2$'Georgia', type='b', col='purple', lty=1, pch=16, lwd=2)
lines(2014:2021, frame2$'Armenia', type='b', col='darkred', lty=1, pch=16, lwd=2)
legend('topright', cex=0.7, country, fill=colors)



frame3 <- as.data.frame(
  rbind(
    extract_index(df_14, country, "Safety.Index"),
    extract_index(df_15, country, "Safety.Index"),
    extract_index(df_16, country, "Safety.Index"),
    extract_index(df_17, country, "Safety.Index"),
    extract_index(df_18, country, "Safety.Index"),
    extract_index(df_19, country, "Safety.Index"),
    extract_index(df_20, country, "Safety.Index"),
    extract_index(df_21, country, "Safety.Index")
  )
)
row.names(frame3) <- 2014:2021
colnames(frame3) <- country

plot(NA, xlim = c(2014, 2021),  ylim=c(min(frame3, na.rm = TRUE)-5, max(frame3, na.rm = TRUE)+5),
     xlab='Года', ylab='Индекс безопасности', 
     main='Сравнение индекса безопасности')

lines(2014:2021, frame3$'Russia', type='b', col='blue', lty=1, pch=16, lwd=2)
lines(2014:2021, frame3$'Ukraine', type='b', col='green', lty=1, pch=16, lwd=2)
lines(2014:2021, frame3$'Belarus', type='b', col='red', lty=1, pch=16, lwd=2)
lines(2014:2021, frame3$'Georgia', type='b', col='purple', lty=1, pch=16, lwd=2)
lines(2014:2021, frame3$'Armenia', type='b', col='darkred', lty=1, pch=16, lwd=2)
legend('topright', cex=0.7, country, fill=colors, ncol = length(country))



frame4 <- as.data.frame(
  rbind(
    extract_index(df_14, country, "Health.Care.Index"),
    extract_index(df_15, country, "Health.Care.Index"),
    extract_index(df_16, country, "Health.Care.Index"),
    extract_index(df_17, country, "Health.Care.Index"),
    extract_index(df_18, country, "Health.Care.Index"),
    extract_index(df_19, country, "Health.Care.Index"),
    extract_index(df_20, country, "Health.Care.Index"),
    extract_index(df_21, country, "Health.Care.Index")
  )
)
row.names(frame4) <- 2014:2021
colnames(frame4) <- country

plot(NA, xlim = c(2014, 2021),  ylim=c(min(frame4, na.rm = TRUE)-5, max(frame4, na.rm = TRUE)+5),
     xlab='Года', ylab='Индекс здравоохранения', 
     main='Сравнение индекса здравоохранения')

lines(2014:2021, frame4$'Russia', type='b', col='blue', lty=1, pch=16, lwd=2)
lines(2014:2021, frame4$'Ukraine', type='b', col='green', lty=1, pch=16, lwd=2)
lines(2014:2021, frame4$'Belarus', type='b', col='red', lty=1, pch=16, lwd=2)
lines(2014:2021, frame4$'Georgia', type='b', col='purple', lty=1, pch=16, lwd=2)
lines(2014:2021, frame4$'Armenia', type='b', col='darkred', lty=1, pch=16, lwd=2)
legend('topright', cex=0.7, country, fill=colors)



frame5 <- as.data.frame(
  rbind(
    extract_index(df_14, country, "Cost.of.Living.Index"),
    extract_index(df_15, country, "Cost.of.Living.Index"),
    extract_index(df_16, country, "Cost.of.Living.Index"),
    extract_index(df_17, country, "Cost.of.Living.Index"),
    extract_index(df_18, country, "Cost.of.Living.Index"),
    extract_index(df_19, country, "Cost.of.Living.Index"),
    extract_index(df_20, country, "Cost.of.Living.Index"),
    extract_index(df_21, country, "Cost.of.Living.Index")
  )
)
row.names(frame5) <- 2014:2021
colnames(frame5) <- country

plot(NA, xlim = c(2014, 2021),  ylim=c(min(frame5, na.rm = TRUE)-5, max(frame5, na.rm = TRUE)+5),
     xlab='Года', ylab='Индекс стоимости жизни', 
     main='Сравнение индекса стоимости жизни')

lines(2014:2021, frame5$'Russia', type='b', col='blue', lty=1, pch=16, lwd=2)
lines(2014:2021, frame5$'Ukraine', type='b', col='green', lty=1, pch=16, lwd=2)
lines(2014:2021, frame5$'Belarus', type='b', col='red', lty=1, pch=16, lwd=2)
lines(2014:2021, frame5$'Georgia', type='b', col='purple', lty=1, pch=16, lwd=2)
lines(2014:2021, frame5$'Armenia', type='b', col='darkred', lty=1, pch=16, lwd=2)
legend('topright', cex=0.7, country, fill=colors)



frame6 <- as.data.frame(
  rbind(
    extract_index(df_14, country, "Property.Price.to.Income.Ratio"),
    extract_index(df_15, country, "Property.Price.to.Income.Ratio"),
    extract_index(df_16, country, "Property.Price.to.Income.Ratio"),
    extract_index(df_17, country, "Property.Price.to.Income.Ratio"),
    extract_index(df_18, country, "Property.Price.to.Income.Ratio"),
    extract_index(df_19, country, "Property.Price.to.Income.Ratio"),
    extract_index(df_20, country, "Property.Price.to.Income.Ratio"),
    extract_index(df_21, country, "Property.Price.to.Income.Ratio")
  )
)
row.names(frame6) <- 2014:2021
colnames(frame6) <- country

plot(NA, xlim = c(2014, 2021),  ylim=c(min(frame6, na.rm = TRUE)-5, max(frame6, na.rm = TRUE)+5),
     xlab='Года', ylab='Соотношение цены недвижимости к доходу', 
     main='Сравнение соотношения цены недвижимости к доходу')

lines(2014:2021, frame6$'Russia', type='b', col='blue', lty=1, pch=16, lwd=2)
lines(2014:2021, frame6$'Ukraine', type='b', col='green', lty=1, pch=16, lwd=2)
lines(2014:2021, frame6$'Belarus', type='b', col='red', lty=1, pch=16, lwd=2)
lines(2014:2021, frame6$'Georgia', type='b', col='purple', lty=1, pch=16, lwd=2)
lines(2014:2021, frame6$'Armenia', type='b', col='darkred', lty=1, pch=16, lwd=2)
legend('topright', cex=0.7, country, fill=colors)



frame7 <- as.data.frame(
  rbind(
    extract_index(df_14, country, "Traffic.Commute.Time.Index"),
    extract_index(df_15, country, "Traffic.Commute.Time.Index"),
    extract_index(df_16, country, "Traffic.Commute.Time.Index"),
    extract_index(df_17, country, "Traffic.Commute.Time.Index"),
    extract_index(df_18, country, "Traffic.Commute.Time.Index"),
    extract_index(df_19, country, "Traffic.Commute.Time.Index"),
    extract_index(df_20, country, "Traffic.Commute.Time.Index"),
    extract_index(df_21, country, "Traffic.Commute.Time.Index")
  )
)
row.names(frame7) <- 2014:2021
colnames(frame7) <- country

plot(NA, xlim = c(2014, 2021),  ylim=c(min(frame7, na.rm = TRUE)-5, max(frame7, na.rm = TRUE)+5),
     xlab='Года', ylab='Индекс времени в пути по трафику', 
     main='Сравнение индекса времени в пути по трафику')

lines(2014:2021, frame7$'Russia', type='b', col='blue', lty=1, pch=16, lwd=2)
lines(2014:2021, frame7$'Ukraine', type='b', col='green', lty=1, pch=16, lwd=2)
lines(2014:2021, frame7$'Belarus', type='b', col='red', lty=1, pch=16, lwd=2)
lines(2014:2021, frame7$'Georgia', type='b', col='purple', lty=1, pch=16, lwd=2)
lines(2014:2021, frame7$'Armenia', type='b', col='darkred', lty=1, pch=16, lwd=2)
legend('topright', cex=0.7, country, fill=colors, ncol = length(country))



frame8 <- as.data.frame(
  rbind(
    extract_index(df_14, country, "Pollution.Index"),
    extract_index(df_15, country, "Pollution.Index"),
    extract_index(df_16, country, "Pollution.Index"),
    extract_index(df_17, country, "Pollution.Index"),
    extract_index(df_18, country, "Pollution.Index"),
    extract_index(df_19, country, "Pollution.Index"),
    extract_index(df_20, country, "Pollution.Index"),
    extract_index(df_21, country, "Pollution.Index")
  )
)
row.names(frame8) <- 2014:2021
colnames(frame8) <- country

plot(NA, xlim = c(2014, 2021),  ylim=c(min(frame8, na.rm = TRUE)-5, max(frame8, na.rm = TRUE)+5),
     xlab='Года', ylab='Индекс загрязнения', 
     main='Сравнение индекса загрязнения')

lines(2014:2021, frame8$'Russia', type='b', col='blue', lty=1, pch=16, lwd=2)
lines(2014:2021, frame8$'Ukraine', type='b', col='green', lty=1, pch=16, lwd=2)
lines(2014:2021, frame8$'Belarus', type='b', col='red', lty=1, pch=16, lwd=2)
lines(2014:2021, frame8$'Georgia', type='b', col='purple', lty=1, pch=16, lwd=2)
lines(2014:2021, frame8$'Armenia', type='b', col='darkred', lty=1, pch=16, lwd=2)
legend('topright', cex=0.7, country, fill=colors, ncol = length(country))



frame9 <- as.data.frame(
  rbind(
    extract_index(df_14, country, "Climate.Index"),
    extract_index(df_15, country, "Climate.Index"),
    extract_index(df_16, country, "Climate.Index"),
    extract_index(df_17, country, "Climate.Index"),
    extract_index(df_18, country, "Climate.Index"),
    extract_index(df_19, country, "Climate.Index"),
    extract_index(df_20, country, "Climate.Index"),
    extract_index(df_21, country, "Climate.Index")
  )
)
row.names(frame9) <- 2014:2021
colnames(frame9) <- country

plot(NA, xlim = c(2014, 2021),  ylim=c(min(frame9, na.rm = TRUE)-5, max(frame9, na.rm = TRUE)+5),
     xlab='Года', ylab='Климатический индекс', 
     main='Сравнение климатического индекса')

lines(2014:2021, frame9$'Russia', type='b', col='blue', lty=1, pch=16, lwd=2)
lines(2014:2021, frame9$'Ukraine', type='b', col='green', lty=1, pch=16, lwd=2)
lines(2014:2021, frame9$'Belarus', type='b', col='red', lty=1, pch=16, lwd=2)
lines(2014:2021, frame9$'Georgia', type='b', col='purple', lty=1, pch=16, lwd=2)
lines(2014:2021, frame9$'Armenia', type='b', col='darkred', lty=1, pch=16, lwd=2)
legend('topleft', cex=0.7, country, fill=colors)

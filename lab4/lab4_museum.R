library(rvest)

# 
url<-read_html('https://kudago.com/spb/list/33-luchshih-muzeya-peterburga/')
articles <- url %>% html_nodes("article.post-list-item")


titles <- articles %>% html_nodes("a.post-list-item-title-link") %>% html_text(trim = TRUE)
links <- articles %>% html_nodes("a.post-list-item-title-link") %>% html_attr("href")
addresses <- articles %>%
  html_nodes("address.post-list-item-info") %>%
  html_text(trim = TRUE) %>%
  substr(., 2, nchar(.))  
descriptions <- articles %>%html_nodes("div.post-list-item-description") %>%
  .[sapply(., function(x) {
    children <- html_children(x)
    length(children) >= 2 && length(html_children(children[2])) == 1 && html_name(html_children(children[2])) == "p"
  })] %>%
  html_nodes("div p") %>%
  html_text(trim = TRUE)

museum_data <- data.frame(
  Название = titles,
  Ссылка = links,
  Описание = descriptions,
  Адрес = addresses
)


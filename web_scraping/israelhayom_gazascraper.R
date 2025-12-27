# Load libraries
library(chromote)
library(rvest)
library(dplyr)
library(purrr)
library(readr)

# Start Chromote session
b <- ChromoteSession$new()

# Collect URLs
base_url <- "https://www.israelhayom.com/tag/gaza/page/"
page_numbers <- 1:180
all_urls <- c()

for (page_num in page_numbers) {
  url <- paste0(base_url, page_num, "/")
  b$Page$navigate(url)
  Sys.sleep(3)
  
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  page <- read_html(html)
  
  page_urls <- page %>%
    html_elements("h3.jeg_post_title a") %>%
    html_attr("href") %>%
    unique() %>%
    na.omit()
  
  all_urls <- c(all_urls, page_urls)
  message(paste("Page", page_num, "processed,", length(page_urls), "links found"))
}

all_urls <- unique(all_urls)
write_csv(data.frame(url = all_urls), "israelhayom_gaza_article_urls.csv")

# Scrape articles
scrape_article <- function(url) {
  tryCatch({
    b$Page$navigate(url)
    Sys.sleep(3)
    
    html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    page <- read_html(html)
    
    headline <- page %>% html_element("h1.jeg_post_title") %>% html_text(trim = TRUE)
    date_published <- page %>% html_element("div.jeg_meta_date a") %>% html_text(trim = TRUE)
    article_text <- page %>% html_elements("div.entry-content.with-share div.content-inner p") %>% 
      html_text(trim = TRUE) %>% paste(collapse = "\n\n")
    
    tibble(
      Headline = headline,
      Date = date_published,
      URL = url,
      Content = article_text
    )
    
  }, error = function(e) {
    message(paste("Failed to scrape:", url))
    tibble(
      Headline = NA_character_,
      Date = NA_character_,
      URL = url,
      Content = NA_character_
    )
  })
}

all_articles <- map_dfr(all_urls, scrape_article)
write_csv(all_articles, "israelhayom_gaza_all_articles.csv")

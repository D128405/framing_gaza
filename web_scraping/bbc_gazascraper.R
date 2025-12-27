# Load libraries
library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(stringr)

# Collect all article URLs from 901 search result pages
base_url <- "https://www.bbc.com/search?q=gaza&page="

pages_to_scrape <- 1:901
all_urls <- c()

for (i in pages_to_scrape) {
  list_url <- paste0(base_url, i)
  message("Scraping search page: ", i)
  
  html <- tryCatch(read_html(list_url), error = function(e) NULL)
  
  if (!is.null(html)) {
    page_urls <- html %>%
      html_elements("a[href*='/news/articles/']") %>%
      html_attr("href") %>%
      unique() %>%
      na.omit()
    
    full_page_urls <- ifelse(str_starts(page_urls, "http"),
                             page_urls,
                             paste0("https://www.bbc.com", page_urls))
    
    all_urls <- c(all_urls, full_page_urls)
  }
  
  Sys.sleep(1)
}

all_urls <- unique(all_urls)
write_csv(data.frame(URL = all_urls), "bbc_article_urls.csv")
message("Collected ", length(all_urls), " unique BBC article URLs.")

# Scrape articles
scrape_article <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    headline <- page %>%
      html_element("h1") %>%
      html_text(trim = TRUE)
    
    date_published <- page %>%
      html_element("time") %>%
      html_attr("datetime") %>%
      ymd_hms()
    
    content <- page %>%
      html_elements("article p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    tibble(
      Headline = headline,
      Date = date_published,
      URL = url,
      Content = content
    )
  }, error = function(e) {
    message("Failed to scrape URL: ", url)
    tibble(
      Headline = NA_character_,
      Date = NA_character_,
      URL = url,
      Content = NA_character_
    )
  })
}

urls_df <- read_csv("bbc_article_urls.csv")

all_articles <- urls_df %>%
  pull(URL) %>%
  map_dfr(scrape_article)

write_csv(all_articles, "bbc_gaza_all_articles.csv")
print(head(all_articles))

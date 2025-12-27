# Load libraries
library(rvest)
library(polite)
library(dplyr)
library(readr)
library(purrr)
library(stringr)

urls_to_scrape <- paste0("https://theintercept.com/search/gaza/page/", 1:80)

all_urls <- c()

# Collect URLs
for (url in urls_to_scrape) {
  session <- bow(
    url = url,
    user_agent = "InterceptScraperBot/1.0",
    delay = 1
  )
  
  html <- scrape(session)
  
  page_urls <- html %>%
    html_elements("a.group.flex.flex-col") %>%      # alle Artikel Links
    html_attr("href") %>%
    str_subset("^https://theintercept.com/\\d{4}/\\d{2}/\\d{2}/") %>%  # nur Artikel
    unique() %>%
    na.omit()
  
  all_urls <- c(all_urls, page_urls)
}

all_urls <- unique(all_urls)

write_csv(data.frame(url = all_urls), "theintercept_article_urls.csv")

# Function: Scrape articles
scrape_article <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    # Headline
    headline <- page %>%
      html_element("h1.post__title") %>%
      html_text(trim = TRUE)
    
    # Date
    date_published <- page %>%
      html_element("time") %>%
      html_attr("datetime")
    
    # Article content
    article_text <- page %>%
      html_elements("article p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    tibble(
      Headline = headline,
      Date = date_published,
      URL = url,
      Content = article_text
    )
    
  }, error = function(e) {
    message(paste("Failed to scrape URL:", url))
    tibble(
      Headline = NA_character_,
      Date = NA_character_,
      URL = url,
      Content = NA_character_
    )
  })
}

# Scrape articles
urls_df <- read_csv("theintercept_article_urls.csv")

all_articles <- urls_df %>%
  pull(url) %>%
  map_dfr(scrape_article)

# Save CSV
write_csv(all_articles, "theintercept_gaza_all_articles.csv")

# Print
print(head(all_articles))

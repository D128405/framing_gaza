# Load libraries
library(rvest)
library(httr)
library(dplyr)
library(purrr)
library(stringr)
library(readr)

# Scrape URLs
search_pages <- 1:57
all_urls <- c()

for (i in search_pages) {
  list_url <- paste0("https://www.lemonde.fr/en/search/?search_keywords=gaza&page=", i)
  
  # Retry logic
  success <- FALSE
  retries <- 0
  max_retries <- 5

  while(!success && retries < max_retries) {
    Sys.sleep(runif(1, 3, 5))  # random delay 5–10 sec
    
    try({
      response <- GET(list_url, user_agent("LeMondeScraperBot/1.0"))
      
      if (status_code(response) == 200) {
        html <- content(response, as = "parsed")
        
        page_urls <- html %>%
          html_elements("a.teaser__link, a.article__link") %>%
          html_attr("href") %>%
          unique() %>%
          na.omit()
        
        full_page_urls <- ifelse(
          str_detect(page_urls, "^https?://"),
          page_urls,
          paste0("https://www.lemonde.fr", page_urls)
        )
        
        all_urls <- c(all_urls, full_page_urls)
        success <- TRUE
      } else if (status_code(response) == 429) {
        message(paste("429 Too Many Requests on page", i, "- waiting longer..."))
        Sys.sleep(runif(1, 10, 20))  # longer wait after 429
      } else {
        message(paste("Error", status_code(response), "on page", i))
        success <- TRUE  # skip non-429 errors
      }
      
    }, silent = TRUE)
    
    retries <- retries + 1
  }
}

# Remove duplicates and save
all_urls <- unique(all_urls)
write_csv(data.frame(URL = all_urls), "lemonde_article_urls.csv")

# Scrape articles

scrape_article <- function(url) {
  Sys.sleep(runif(1, 3, 5))  # random delay between articles
  
  tryCatch({
    page <- read_html(GET(url, user_agent("LeMondeScraperBot/1.0")))
    
    headline <- page %>%
      html_element("h1") %>%
      html_text(trim = TRUE)
    
    date_published <- page %>%
      html_element("span.meta__date--header") %>%
      html_text(trim = TRUE)
    
    content <- page %>%
      html_elements("section.article__wrapper p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    tibble(
      Headline = headline,
      Date = date_published,
      URL = url,
      Content = content
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

# Load URLs and scrape all articles
urls_df <- read_csv("lemonde_article_urls.csv")

all_articles <- urls_df %>%
  pull(URL) %>%
  map_dfr(scrape_article)

# Save all articles to CSV
write_csv(all_articles, "lemonde_gaza_all_articles.csv")

# Optional preview
print(head(all_articles))

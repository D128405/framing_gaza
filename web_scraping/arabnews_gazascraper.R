# Load libraries
library(chromote)
library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

# Collect URLs

base_url <- "https://www.arabnews.com/search/site/gaza"
urls_to_scrape <- c(
  paste0(base_url, "?solrsort=score%20desc"),               # first page
  paste0(base_url, "?page=", 1:99, "&solrsort=score%20desc") # pages 1-99
)

all_urls <- c()

b <- ChromoteSession$new()

for (page_url in urls_to_scrape) {
  b$Page$navigate(page_url)
  Sys.sleep(3) # wait for JS to load
  
  # Get page source after JS rendered
  html_content <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  page <- read_html(html_content)
  
  # Scrape article links
  page_urls <- page %>%
    html_elements("h4 > a") %>%
    html_attr("href") %>%
    unique() %>%
    na.omit()
  
  all_urls <- c(all_urls, page_urls)
}

all_urls <- unique(all_urls)

# Save URLs to CSV
all_urls_df <- data.frame(url = all_urls, stringsAsFactors = FALSE)
write_csv(all_urls_df, "arabnews_article_urls.csv")

# Scrape articles

# Load URLs
urls_df <- read_csv("arabnews_article_urls.csv")

# Safety check
if (!"url" %in% colnames(urls_df)) stop("Die CSV muss eine Spalte 'url' enthalten!")

scrape_article <- function(url) {
  tryCatch({
    b$Page$navigate(url)
    Sys.sleep(3)
    
    html_content <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    page <- read_html(html_content)
    
    headline <- page %>%
      html_element("div.entry-title h1") %>%
      html_text(trim = TRUE)
    
    date_published <- page %>%
      html_element("div.entry-date time") %>%
      html_text(trim = TRUE)
    
    article_text <- page %>%
      html_elements("div.entry-content p") %>%
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
    message("Error:", e$message)
    return(tibble(
      Headline = NA_character_,
      Date = NA_character_,
      URL = url,
      Content = NA_character_
    ))
  })
}

# Scrape all articles and combine
all_articles <- urls_df %>%
  pull(url) %>%
  map_dfr(scrape_article)

# Save combined data
write_csv(all_articles, "arabnews_gaza_all_articles.csv")

# Preview
print(head(all_articles))

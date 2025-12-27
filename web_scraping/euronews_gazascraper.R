# Load libraries
library(httr)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(pbapply)

# Function to get URLs
get_article_links <- function(page_num) {
  # Construct the search page URL with page number
  url <- paste0("https://www.euronews.com/search?query=gaza&p=", page_num)
  
  # GET request with user-agent
  page <- httr::GET(url, httr::add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
  )) %>%
    read_html()
  
  # Extract article URLs
  links <- page %>%
    html_elements("a.the-media-object__link") %>%
    html_attr("href") %>%
    str_trim() %>%
    unique()
  
  # Filter only article links (starting with /YYYY/MM/DD/)
  links <- links[str_detect(links, "^/\\d{4}/\\d{2}/\\d{2}/")]
  
  # Prepend base URL
  full_links <- paste0("https://www.euronews.com", links)
  return(full_links)
}

# Scrape URLs
total_pages <- 145
article_urls <- pbsapply(1:total_pages, function(i) {
  Sys.sleep(1)
  cat(sprintf("Scraping search page %d\n", i))
  get_article_links(i)
}) %>% unlist() %>% unique()

cat(sprintf("Found %d article URLs\n", length(article_urls)))

# Scrape articles
scrape_article <- function(url) {
  page <- httr::GET(url, httr::add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
  )) %>%
    read_html()
  
  # Headline
  headline <- page %>%
    html_element("h1.c-article-redesign-title") %>%
    html_text(trim = TRUE)
  
  # Date published
  date_published <- page %>%
    html_element("div.c-article-publication-date time") %>%
    html_attr("datetime") %>%
    as.POSIXct(format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Article text
  article_text <- page %>%
    html_elements("div.c-article-content p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "\n\n")
  
  tibble(
    Headline = headline,
    Date = date_published,
    URL = url,
    Content = article_text
  )
}

# Safe scraping to avoid breaking on errors
safe_scrape <- safely(scrape_article, otherwise = tibble(
  Headline = NA, Date = NA, URL = NA, Content = NA
))

# Scrape articles and progress bar
articles_data <- pbsapply(article_urls, function(url) {
  Sys.sleep(0.5)  # polite delay
  cat(sprintf("Scraping article: %s\n", url))
  safe_scrape(url)$result
}, simplify = FALSE) %>% bind_rows()

# Save to CSV
write.csv(articles_data, "euronews_gaza_all_articles.csv", row.names = FALSE)
cat(sprintf("Saved %d articles to euronews_gaza_all_articles.csv\n", nrow(articles_data)))

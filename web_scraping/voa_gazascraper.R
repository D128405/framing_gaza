# Load libraries
library(rvest)
library(polite)
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(lubridate)
library(progress)

# Collect URLs
base_url <- "https://www.voanews.com/s?k=gaza&tab=news&time=2023-10-07;2025-08-20&pp=10&ordering=newest&pi="

# Initialize list for all URLs
all_urls <- c()

# Loop through pages 1 to 286
for (i in 1:286) {
  cat("Scraping page:", i, "\n")
  
  page_url <- paste0(base_url, i)
  session <- bow(page_url, user_agent = "VOAScraperBot/1.0", delay = 1)
  page <- scrape(session)
  
  page_urls <- page %>%
    html_elements("a") %>%
    html_attr("href") %>%
    .[str_detect(., "^/a/")] %>%       # filter only article URLs
    unique() %>%
    na.omit()
  
  full_urls <- paste0("https://www.voanews.com", page_urls)
  all_urls <- c(all_urls, full_urls)
}

# Remove duplicates
all_urls <- unique(all_urls)

write_csv(data.frame(url = all_urls), "voa_article_urls.csv")

urls_df <- read_csv("voa_article_urls.csv")

scrape_voa_article <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    headline <- page %>%
      html_element("h1.title.pg-title") %>%
      html_text(trim = TRUE)
    
    date_published <- page %>%
      html_element("time") %>%
      html_attr("datetime")
    
    # Convert to POSIXct safely
    date_published <- if (!is.na(date_published)) ymd_hms(date_published, tz = "UTC") else as.POSIXct(NA)
    
    article_text <- page %>%
      html_elements("div#article-content div.wsw p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    tibble(
      Headline = headline,
      Date = date_published,
      URL = url,
      Content = article_text
    )
    
  }, error = function(e) {
    message("Failed to scrape URL: ", url)
    tibble(
      Headline = NA_character_,
      Date = as.POSIXct(NA),
      URL = url,
      Content = NA_character_
    )
  })
}

# Scrape articles and progress bar
pb <- progress_bar$new(
  format = "  Scraping articles [:bar] :current/:total (:percent) eta: :eta",
  total = nrow(urls_df), clear = FALSE, width= 80
)

all_articles <- map_dfr(urls_df$url, function(u) {
  pb$tick()
  scrape_voa_article(u)
})

# Save combined data to CSV
write_csv(all_articles, "voa_gaza_all_articles.csv")

# Optional: preview
print(head(all_articles))

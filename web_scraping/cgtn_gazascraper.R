# Load libraries
library(chromote)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(progress)

# Define max amount of articles
max_articles <- 4840

# Start Chromote session
b <- ChromoteSession$new()

url <- "https://www.cgtn.com/searching/index.html?keyword=gaza&newsType=General%20News&sections=All&sortBy=Date%20Order"
b$Page$navigate(url)
Sys.sleep(15)

# Progress bar
pb_load <- progress_bar$new(
  format = "Lade Artikel [:bar] :current (:percent) eta: :eta",
  total = max_articles,
  clear = FALSE, width = 60
)

# Collect URLs and press load more button
repeat {
  current_articles <- b$Runtime$evaluate(
    "document.querySelectorAll('a.article-flex-item').length"
  )$result$value
  
  pb_load$update(min(current_articles / max_articles, 1))
  
  if (current_articles >= max_articles) break
  
  more_exists <- b$Runtime$evaluate(
    "document.querySelector('#more') !== null"
  )$result$value
  
  if (!more_exists) break
  
  b$Runtime$evaluate("document.querySelector('#more').click()")
  Sys.sleep(15)
}

page_source <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
page <- read_html(page_source)

# Scrape articles
urls_df <- page %>%
  html_elements("a.article-flex-item") %>%
  html_attr("href") %>%
  unique() %>%
  head(max_articles) %>%
  tibble(url = .)

write_csv(urls_df, "cgtn_article_urls.csv")

scrape_article <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    headline <- page %>%
      html_element("h1.news-title") %>%
      html_text(trim = TRUE)
    
    date_published <- page %>%
      html_element("div.news-date-wrapper span.date") %>%
      html_text(trim = TRUE)
    
    article_text <- page %>%
      html_elements("div.text.en p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    tibble(
      Headline = headline %||% NA_character_,
      Date = date_published %||% NA_character_,
      URL = url,
      Content = article_text %||% NA_character_
    )
  }, error = function(e) {
    message("Fehler bei: ", url)
    tibble(
      Headline = NA_character_,
      Date = NA_character_,
      URL = url,
      Content = NA_character_
    )
  })
}

# Progress bar
pb_scrape <- progress_bar$new(
  format = "Scraping Artikel [:bar] :current/:total (:percent) eta: :eta",
  total = nrow(urls_df),
  clear = FALSE, width = 60
)

all_articles <- list()

for (i in seq_along(urls_df$url)) {
  res <- scrape_article(urls_df$url[i])
  all_articles[[i]] <- res
  pb_scrape$tick()
  
  # Save every 500
  if (i %% 500 == 0) {
    partial <- bind_rows(all_articles)
    filename <- paste0("cgtn_partial_", i, ".csv")
    write_csv(partial, filename)
    message("Zwischenspeicherung: ", filename)
  }
}

all_articles_df <- bind_rows(all_articles)
write_csv(all_articles_df, "cgtn_gaza_all_articles.csv")

print(head(all_articles_df))

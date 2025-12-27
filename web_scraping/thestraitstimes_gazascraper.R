# Load libraries
library(chromote)
library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(progress)

search_url <- "https://www.straitstimes.com/search?searchkey=gaza&sort=date&daterange=10%2F07%2F2023%2C08%2F26%2F2025"
max_articles <- 5500
pause_time <- 10

# Start Chromote session
b <- ChromoteSession$new()
b$Page$navigate(search_url)
Sys.sleep(10)

# Progress bar
pb_urls <- progress_bar$new(
  format = "Collecting URLs [:bar] :current (:percent) eta: :eta",
  total = max_articles,
  clear = FALSE, width = 60
)

all_urls <- character()

repeat {
  # Extract current page source
  page_html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  doc <- read_html(page_html)
  
  # Collect article URLs
  urls <- doc %>%
    html_elements("a[aria-label='link']") %>%
    html_attr("href") %>%
    unique() %>%
    str_subset("^/") %>%
    paste0("https://www.straitstimes.com", .)
  
  all_urls <- unique(c(all_urls, urls))
  pb_urls$update(min(length(all_urls) / max_articles, 1))
  
  if (length(all_urls) >= max_articles) {
    message("Reached max_articles limit.")
    break
  }
  
  # Check if load more button exists and is clickable
  more_exists <- b$Runtime$evaluate(
    "document.querySelector('button[data-testid=\"load-more-test-id\"]') !== null"
  )$result$value
  
  if (!isTRUE(more_exists)) {
    message("No more 'Load more' button found. Stopping.")
    break
  }
  
  # Click load more
  b$Runtime$evaluate(
    "document.querySelector('button[data-testid=\"load-more-test-id\"]').click()"
  )
  Sys.sleep(pause_time)
}

urls_df <- tibble(url = head(all_urls, max_articles))
write_csv(urls_df, "straitstimes_article_urls.csv")
message("Collected ", nrow(urls_df), " article URLs.")

# Scrape articles
scrape_article <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    headline <- page %>%
      html_element("h1[data-testid='heading-test-id']") %>%
      html_text(trim = TRUE)
    
    date_text <- page %>%
      html_element("div[data-testid='timestamp-test-id'] p") %>%
      html_text(trim = TRUE)
    
    if (!is.na(date_text)) {
      date_published <- str_remove(date_text, "^Published\\s+")
    } else {
      date_published <- NA_character_
    }
    
    content <- page %>%
      html_elements("p[data-testid='article-paragraph-annotation-test-id']") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    tibble(
      Headline = headline %||% NA_character_,
      Date = date_published %||% NA_character_,
      URL = url,
      Content = content %||% NA_character_
    )
  }, error = function(e) {
    message("Error scraping: ", url)
    tibble(Headline = NA, Date = NA, URL = url, Content = NA)
  })
}

# Progress bar
pb_scrape <- progress_bar$new(
  format = "Scraping articles [:bar] :current/:total (:percent) eta: :eta",
  total = nrow(urls_df),
  clear = FALSE, width = 60
)

all_articles <- list()
for (i in seq_along(urls_df$url)) {
  all_articles[[i]] <- scrape_article(urls_df$url[i])
  pb_scrape$tick()
  
  if (i %% 200 == 0) {
    partial <- bind_rows(all_articles)
    filename <- paste0("straitstimes_partial_", i, ".csv")
    write_csv(partial, filename)
    message("Saved checkpoint: ", filename)
  }
}

# Save CSV
all_articles_df <- bind_rows(all_articles)
write_csv(all_articles_df, "thestraitstimes_gaza_all_articles.csv")
message("Finished scraping ", nrow(all_articles_df), " articles.")

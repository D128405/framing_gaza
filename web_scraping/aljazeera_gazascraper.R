# Load libraries
library(chromote)
library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(progress)
library(furrr)
library(httr2)

tag_url <- "https://www.aljazeera.com/tag/gaza/"
num_articles <- 5000
output_csv <- "aljazeera_gaza_all_articles.csv"
save_every <- 500
resume <- TRUE
parallel_workers <- 6

# Get all article URLs
b <- ChromoteSession$new()
b$Page$navigate(tag_url)
b$Page$loadEventFired(wait_ = TRUE, timeout = 60000)
Sys.sleep(12)

click_show_more <- function(session, target_count = 20, max_retries = 3) {
  repeat {
    page_html <- session$DOM$getDocument() %>%
      .$root %>%
      .$nodeId %>%
      {session$DOM$getOuterHTML(nodeId = .)$outerHTML}
    page <- read_html(page_html)
    articles_loaded <- length(html_nodes(page, "a.u-clickable-card__link"))
    if (articles_loaded >= target_count) break
    
    show_more_exists <- session$Runtime$evaluate(
      "document.querySelector('button.show-more-button') !== null"
    )$result$value
    if (!show_more_exists) break
    
    tryCatch({
      session$Runtime$evaluate(
        "document.querySelector('button.show-more-button').click()"
      )
    }, error = function(e) message("Show More click failed: ", e$message))
    
    Sys.sleep(10)
  }
}

click_show_more(b, num_articles)

page_html <- b$DOM$getDocument() %>%
  .$root %>%
  .$nodeId %>%
  {b$DOM$getOuterHTML(nodeId = .)$outerHTML}

page <- read_html(page_html)
article_urls <- page %>%
  html_nodes("a.u-clickable-card__link") %>%
  html_attr("href") %>%
  str_c("https://www.aljazeera.com", .) %>%
  unique()

article_urls <- article_urls[!str_detect(article_urls, "/video/|/gallery/|/liveblog/")]
article_urls <- head(article_urls, num_articles)

# Resume handling
start_index <- 1
if (resume && file.exists(output_csv)) {
  existing <- read_csv(output_csv, show_col_types = FALSE)
  scraped_urls <- existing$URL
  article_urls <- setdiff(article_urls, scraped_urls)
  start_index <- nrow(existing) + 1
  all_articles <- existing
  message("Resuming from article ", start_index)
} else {
  all_articles <- tibble()
}

# Scraping articles
scrape_article <- function(url) {
  tryCatch({
    resp <- request(url) |> req_perform()
    html_doc <- read_html(resp_body_string(resp))
    
    headline <- html_doc %>% html_node("header.article-header h1") %>% html_text(trim = TRUE)
    date <- html_doc %>% html_node("div.date-simple span[aria-hidden='true']") %>% html_text(trim = TRUE)
    content <- html_doc %>% html_node("div.wysiwyg--all-content") %>% html_text(trim = TRUE)
    
    tibble(
      Headline = headline %||% NA,
      Date = date %||% NA,
      URL = url,
      Content = content %||% NA
    )
  }, error = function(e) {
    message("Failed: ", url, " (", e$message, ")")
    return(NULL)
  })
}

# Parallel scraping
plan(multisession, workers = parallel_workers)
pb <- progress_bar$new(
  format = "Scraping [:bar] :current/:total (:percent) eta: :eta",
  total = length(article_urls), clear = FALSE, width = 60
)

batches <- split(article_urls, ceiling(seq_along(article_urls) / save_every))
for (batch in batches) {
  results <- future_map_dfr(batch, ~{pb$tick(); scrape_article(.x)})
  all_articles <- bind_rows(all_articles, results)
  
  # Save checkpoint
  write_csv(all_articles, output_csv)
}

message("Scraping finished. Results saved to ", output_csv)

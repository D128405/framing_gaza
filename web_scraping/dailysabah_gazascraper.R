# Load libraries
library(chromote)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(progress)

# Scrape webpages
scrape_search_page <- function(page_no) {
  url <- paste0("https://www.dailysabah.com/search?query=gaza&pgno=", page_no)
  
  b <- ChromoteSession$new()
  on.exit(b$close(), add = TRUE)   # always close the session
  
  # Navigate and wait a fixed time instead of waiting for loadEventFired
  tryCatch({
    b$Page$navigate(url)
    Sys.sleep(15)   # give JS time to render
  }, error = function(e) {
    message("Error when navigating (", url, "): ", e$message)
    return(character(0))
  })
  
  # Grab HTML
  html <- b$DOM$getDocument()
  node_html <- b$DOM$getOuterHTML(nodeId = html$root$nodeId)[["outerHTML"]]
  
  page <- read_html(node_html)
  
  links <- page %>%
    html_elements("h3 > a") %>%
    html_attr("href") %>%
    unique() %>%
    na.omit()
  
  message("Seite ", page_no, ": ", length(links), " URLs gefunden")
  
  return(links)
}

scrape_with_retry <- function(page_no, max_tries = 15) {
  for (i in 1:max_tries) {
    links <- scrape_search_page(page_no)
    if (length(links) > 0) return(links)
    message("Retry Page ", page_no, " (Versuch ", i, ")")
    Sys.sleep(10)
  }
  return(character(0))
}

pages_to_scrape <- 1:577   # Scrape 577 webpages
all_urls <- map(pages_to_scrape, scrape_with_retry) %>%
  unlist() %>%
  unique()

write_csv(data.frame(URL = all_urls), "dailysabah_article_urls.csv")

urls_df <- read_csv("dailysabah_article_urls.csv", show_col_types = FALSE)
urls <- urls_df$URL
n_urls <- length(urls)

b <- ChromoteSession$new()
on.exit(b$close(), add = TRUE)

# Scrape articles
scrape_article <- function(url, browser) {
  tryCatch({
    browser$Page$navigate(url)
    Sys.sleep(8)  # Wait for JS to render
    
    html <- browser$DOM$getDocument()
    node_html <- browser$DOM$getOuterHTML(nodeId = html$root$nodeId)[["outerHTML"]]
    page <- read_html(node_html)
    
    headline <- page %>% html_element("h1") %>% html_text(trim = TRUE)
    date_published <- page %>% html_element("meta[property='article:published_time']") %>% html_attr("content")
    content <- page %>% html_element("div.article_body") %>% html_elements("p") %>% html_text(trim = TRUE) %>% paste(collapse="\n\n")
    
    tibble(
      Headline = headline,
      Date = date_published,
      URL = url,
      Content = content
    )
    
  }, error = function(e) {
    message("Error scraping: ", url)
    tibble(
      Headline = NA_character_,
      Date = NA_character_,
      URL = url,
      Content = NA_character_
    )
  })
}

# Scrape articles
results <- list()
pb <- progress_bar$new(
  format = "  Scraping [:bar] :current/:total (:percent) ETA: :eta",
  total = n_urls, clear = FALSE, width = 60
)

for(i in seq_along(urls)) {
  Sys.sleep(5)  # polite pause
  results[[i]] <- scrape_article(urls[i], b)
  pb$tick()
  
  # Save partial results every 1000 articles
  if(i %% 1000 == 0) {
    message("\nSaving partial results at article ", i)
    partial_df <- bind_rows(results)
    write_csv(partial_df, paste0("dailysabah_articles_partial_", i, ".csv"))
  }
}

# Final save
all_articles <- bind_rows(results)
write_csv(all_articles, "dailysabah_gaza_all_articles.csv")

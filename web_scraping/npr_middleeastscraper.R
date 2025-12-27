# Load libraries
library(chromote)
library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(progressr)

# Collect URLs
b <- ChromoteSession$new()
base_url <- "https://www.npr.org/sections/middle-east/"
b$Page$navigate(base_url)
Sys.sleep(15)

all_urls <- c()
load_more <- TRUE
iteration <- 1
save_every <- 500
max_iterations <- 200

while(load_more & iteration <= max_iterations) {
  # Get rendered HTML
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  page <- read_html(html)
  
  # Extract article links
  page_urls <- page %>%
    html_elements("article h2 a") %>%
    html_attr("href") %>%
    unique() %>%
    na.omit()
  
  all_urls <- unique(c(all_urls, page_urls))
  message(paste("Iteration", iteration, "-", length(all_urls), "URLs collected"))
  
  # Save every 200 URLs
  if(length(all_urls) %% save_every < length(page_urls)) {
    write_csv(data.frame(url = all_urls), "npr_middleeast_article_urls_partial.csv")
    message("Partial URL file saved.")
  }
  
  # Check if "Load more stories" button exists
  button_exists <- b$Runtime$evaluate(
    "document.querySelector('button.options__load-more') !== null"
  )$result$value
  
  if(button_exists) {
    # Click the button
    b$Runtime$evaluate("document.querySelector('button.options__load-more').click()")
    Sys.sleep(15)
    iteration <- iteration + 1
  } else {
    load_more <- FALSE
  }
}

# Save final URLs
write_csv(data.frame(url = all_urls), "npr_middleeast_article_urls.csv")
message(paste("Total URLs collected:", length(all_urls)))

# Scrape articles
urls_df <- read_csv("npr_middleeast_article_urls.csv")

scrape_npr_article <- function(url) {
  tryCatch({
    b$Page$navigate(url)
    Sys.sleep(15) # allow page to load
    
    html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    page <- read_html(html)
    
    headline <- page %>%
      html_element("h1") %>%
      html_text(trim = TRUE)
    
    date_published <- page %>%
      html_element("time") %>%
      html_attr("datetime") %>%
      as.POSIXct(format="%Y-%m-%dT%H:%M:%S", tz="UTC")
    
    article_text <- page %>%
      html_elements("div.storytext p") %>%
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
      Date = NA,
      URL = url,
      Content = NA_character_
    )
  })
}

# Function: Scrape articles and progress bar
handlers(global = TRUE)
all_articles <- urls_df$url %>%
  { with_progress({
    p <- progressor(along = .)
    map_dfr(., function(u) {
      p()
      scrape_npr_article(u)
    })
  })
  }

# Save final CSV
write_csv(all_articles, "npr_middleeast_all_articles.csv")

# Close Chromote session
b$close()

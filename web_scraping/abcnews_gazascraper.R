# Packages
library(chromote)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)

# Start Chromote
b <- ChromoteSession$new()

# Function: all URLs of a page
get_abc_urls <- function(page_number) {
  url <- paste0("https://abcnews.go.com/search?searchtext=gaza&type=Story&sort=date&page=", page_number)
  b$Page$navigate(url)
  Sys.sleep(5)
  
  dom <- b$DOM$getDocument()
  html_list <- b$DOM$getOuterHTML(dom$root$nodeId)
  html_content <- html_list$outerHTML
  
  page <- read_html(html_content)
  
  page %>%
    html_elements("div.ContentRoll__Headline > h2 > a") %>%
    html_attr("href") %>%
    unique()
}

# Function: scrape an article
scrape_abc_article <- function(url) {
  tryCatch({
    b$Page$navigate(url)
    Sys.sleep(5)
    
    dom <- b$DOM$getDocument()
    html_list <- b$DOM$getOuterHTML(dom$root$nodeId)
    html_content <- html_list$outerHTML
    
    page <- read_html(html_content)
    
    headline <- page %>%
      html_element("h1") %>%
      html_text(trim = TRUE)
    
    date <- page %>%
      html_element("div.jTKbV") %>%
      html_text(trim = TRUE)
    
    content <- page %>%
      html_elements("p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    tibble(
      Headline = headline,
      Date = date,
      URL = url,
      Content = content
    )
    
  }, error = function(e) {
    message("Failed to scrape: ", url)
    tibble(
      Headline = NA_character_,
      Date = NA_character_,
      URL = url,
      Content = NA_character_
    )
  })
}

# Scrape page 1 to 140
pages_to_scrape <- 1:140

# Collect all URLs
all_urls <- map(pages_to_scrape, get_abc_urls) %>% unlist() %>% unique()

# Scrape all URLs
all_articles <- map_dfr(all_urls, scrape_abc_article)

# Save CSV
write.csv(all_articles, "abcnews_gaza_all_articles.csv", row.names = FALSE)

# Close Chromote
b$close()

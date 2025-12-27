# Load libraries
library(chromote)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(lubridate)

# Start Chromote session
b <- ChromoteSession$new()

# Collect URLs
get_vox_urls <- function(page_num) {
  search_url <- paste0("https://www.vox.com/search?q=gaza&page=", page_num)
  message("Loading search page: ", page_num)
  
  # Navigate to the page
  b$Page$navigate(search_url)
  Sys.sleep(3)
  
  # Get the HTML of the rendered page
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value %>% 
    read_html()
  
  # Extract article links
  urls <- html %>%
    html_elements("a.qcd9z0.qcd9z4") %>%  # article link selector
    html_attr("href") %>%
    na.omit() %>%
    unique()
  
  # Make absolute URLs
  full_urls <- ifelse(startsWith(urls, "http"),
                      urls,
                      paste0("https://www.vox.com", urls))
  return(full_urls)
}

page_numbers <- 1:10  # adjust as needed
all_urls <- map(page_numbers, get_vox_urls) %>% unlist() %>% unique()
write_csv(data.frame(url = all_urls), "vox_gaza_article_urls.csv")
message("Total URLs collected: ", length(all_urls))

# Scrape articles
scrape_vox_article <- function(url) {
  tryCatch({
    b$Page$navigate(url)
    Sys.sleep(2)
    
    html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value %>% 
      read_html()
    
    headline <- html %>%
      html_element("h1") %>%
      html_text(trim = TRUE)
    
    date_published <- html %>%
      html_element("span.duet--article--timestamp time") %>%  # updated time selector
      html_attr("datetime") %>%
      ymd_hms(tz = "UTC")
    
    article_text <- html %>%
      html_elements("div.duet--article--article-body-component p") %>%  # updated body selector
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    tibble(
      Headline = headline,
      Date = date_published,
      URL = url,
      Content = article_text
    )
    
  }, error = function(e) {
    message("Failed to scrape: ", url)
    return(tibble(
      Headline = NA_character_,
      Date = NA_character_,
      URL = url,
      Content = NA_character_
    ))
  })
}

all_articles <- map_dfr(all_urls, function(u) {
  Sys.sleep(2) # polite delay
  scrape_vox_article(u)
})

# Save CSV
write_csv(all_articles, "vox_gaza_all_articles.csv")
print(head(all_articles))

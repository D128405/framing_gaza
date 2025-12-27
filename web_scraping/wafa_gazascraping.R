# Load libraries
library(rvest)
library(polite)
library(dplyr)
library(stringr)
library(purrr)
library(readr)

base_url <- "https://english.wafa.ps/Home/Search?searchString=Gaza&fromDate=10%2F7%2F2023%2012%3A00%3A00%20AM&category=Microsoft.AspNetCore.Mvc.Rendering.SelectList&pageNumber="

page_numbers <- 1:277

all_urls <- c()

# Collect URLs
for (page_num in page_numbers) {
  url <- paste0(base_url, page_num)
  
  session <- bow(url, user_agent = "WAFAScraperBot/1.0", delay = 1)
  html <- scrape(session)
  
  page_urls <- html %>%
    html_elements("div.body h4 a") %>%
    html_attr("href") %>%
    unique() %>%
    na.omit()
  
  # Absolute URLs
  full_page_urls <- ifelse(startsWith(page_urls, "http"),
                           page_urls,
                           paste0("https://english.wafa.ps", page_urls))
  
  all_urls <- c(all_urls, full_page_urls)
  
  message(paste("Seite", page_num, "verarbeitet, bisher", length(all_urls), "Artikel-Links gesammelt"))
}

# Remove duplicates
all_urls <- unique(all_urls)

write_csv(data.frame(url = all_urls), "wafa_gaza_article_urls.csv")

# Scrape articles
scrape_article <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    # Headline
    headline <- page %>%
      html_element("h3.title") %>%
      html_text(trim = TRUE)
    
    # Date
    date_published <- page %>%
      html_element("span.date") %>%
      html_text(trim = TRUE)
    
    # Content
    article_text <- page %>%
      html_elements("div.content p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    tibble(
      Headline = headline,
      Date = date_published,
      URL = url,
      Content = article_text
    )
    
  }, error = function(e) {
    message(paste("Fehler bei URL:", url))
    return(tibble(
      Headline = NA_character_,
      Date = NA_character_,
      URL = url,
      Content = NA_character_
    ))
  })
}

all_articles <- all_urls %>%
  map_dfr(scrape_article)

# Save CSV
write_csv(all_articles, "wafa_gaza_all_articles.csv")

# Preview
print(head(all_articles))

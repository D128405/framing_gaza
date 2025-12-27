# Load libraries
library(rvest)
library(polite)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(readr)

base_url <- "https://www.france24.com/en/tag/israel-hamas-war/"

page_numbers <- 1:45

# Scrape URLs
all_urls <- map(page_numbers, function(page_num) {
  page_url <- if (page_num == 1) base_url else paste0(base_url, page_num, "/")
  
  session <- bow(page_url, user_agent = "France24ScraperBot/1.0", delay = 1)
  page <- try(scrape(session), silent = TRUE)
  
  if (inherits(page, "try-error")) return(character(0))
  
  links <- page %>%
    html_elements("a[data-article-item-link]") %>%
    html_attr("href")
  
  # Normalize URLs: handle both relative and absolute
  links <- ifelse(
    str_starts(links, "http"),
    links,
    paste0("https://www.france24.com", links)
  )
  
  links
}) %>%
  unlist() %>%
  unique()

cat("Total article URLs found:", length(all_urls), "\n")

# Scrape articles
scraped_articles <- map_dfr(all_urls, function(url) {
  Sys.sleep(1)
  
  session <- bow(url, user_agent = "France24ScraperBot/1.0")
  page <- try(scrape(session), silent = TRUE)
  
  if (inherits(page, "try-error")) {
    message("Failed to scrape: ", url)
    return(tibble(Headline = NA, Date = NA, URL = url, Content = NA))
  }
  
  # Extract headline
  headline <- page %>%
    html_element("h1") %>%
    html_text(trim = TRUE)
  
  # Extract date
  date_published <- page %>%
    html_element("p.m-pub-dates time") %>%
    html_attr("datetime")
  
  # Extract article body
  article_text <- page %>%
    html_elements("div.t-content__body p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "\n\n")
  
  tibble(
    Headline = headline,
    Date = date_published,
    URL = url,
    Content = article_text
  )
})

# Save CSV
write_excel_csv(scraped_articles, "france24_gaza_all_articles.csv")
cat("Scraping complete! Saved to france24_gaza_all_articles.csv\n")

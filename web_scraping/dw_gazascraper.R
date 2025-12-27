# Load libraries
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)

# Collect article URLs
base_url <- "https://www.dw.com/en/gaza/t-19074241/page-"
pages_to_scrape <- 1:17  # adjust number of pages if needed

all_urls <- c()

for (i in pages_to_scrape) {
  page_url <- paste0(base_url, i)
  page <- read_html(page_url)
  
  # Extract article links
  page_urls <- page %>%
    html_elements("a") %>%
    html_attr("href") %>%
    na.omit() %>%
    str_subset("^/en/.*/a-\\d+$") %>%
    unique()
  
  # Absolute URLs
  full_page_urls <- paste0("https://www.dw.com", page_urls)
  
  all_urls <- c(all_urls, full_page_urls)
}

all_urls <- unique(all_urls)
write_csv(data.frame(URL = all_urls), "dw_article_urls_gaza.csv")

# Function to scrape articles
scrape_dw_article <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    # Headline
    headline <- page %>%
      html_element("h1") %>%
      html_text(trim = TRUE)
    
    # Date
    date <- page %>%
      html_element("span.publication time") %>%
      html_text(trim = TRUE)
    
    # Teaser
    teaser_text <- page %>%
      html_element("p.teaser-text") %>%
      html_text(trim = TRUE)
    
    # Article body
    # Select the "rich-text" container and extract readable text (preserve inline links/bold/italic)
    full_text <- page %>%
      html_element("div[data-tracking-name='rich-text']") %>%
      html_elements("p, h2, li") %>%
      map_chr(~ html_text(.x, trim = TRUE)) %>%   # keep text inside inline tags
      paste(collapse = "\n\n")
    
    # Combine teaser + full text
    combined_text <- if (!is.na(teaser_text) && nchar(teaser_text) > 0) {
      paste(teaser_text, full_text, sep = "\n\n")
    } else {
      full_text
    }
    
    tibble(
      Headline = headline,
      Date = date,
      URL = url,
      Full_Text = combined_text
    )
    
  }, error = function(e) {
    message(paste("Failed to scrape URL:", url))
    message("Error:", e$message)
    tibble(
      Headline = NA_character_,
      Date = NA_character_,
      URL = url,
      Content = NA_character_
    )
  })
}

# Scrape all articles
urls_df <- read_csv("dw_article_urls_gaza.csv")

all_articles <- urls_df %>%
  pull(URL) %>%
  map_dfr(scrape_dw_article)

# Save results
write_csv(all_articles, "dw_gaza_all_articles.csv")

# Preview
print(head(all_articles))

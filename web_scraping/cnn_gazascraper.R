# Load libraries
library(chromote)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)

# Start Chromote session
b <- ChromoteSession$new()

# Collect all article URLs
all_urls <- c()
num_pages <- 24

for (page_num in 1:num_pages) {
  search_url <- paste0(
    "https://www.cnn.com/search?q=gaza&from=", (page_num - 1) * 10,
    "&size=10&page=", page_num,
    "&sort=newest&types=all&section="
  )
  
  message("Scraping page: ", page_num)
  b$Page$navigate(search_url)
  Sys.sleep(2)  # allow page to render fully
  
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  page <- read_html(html)
  
  page_urls <- page %>%
    html_elements("a.container__link--type-NewsArticle") %>%
    html_attr("href") %>%
    unique() %>%
    na.omit()
  
  # Ensure full URLs
  page_urls <- ifelse(str_detect(page_urls, "^http"), page_urls,
                      paste0("https://www.cnn.com", page_urls))
  
  all_urls <- c(all_urls, page_urls)
}

all_urls <- unique(all_urls)
write_csv(data.frame(url = all_urls), "cnn_gaza_article_urls.csv")
message("Total article URLs collected: ", length(all_urls))

# Scrape articles
scrape_article <- function(url) {
  tryCatch({
    b$Page$navigate(url)
    Sys.sleep(2)
    
    html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    page <- read_html(html)
    
    # Headline
    headline <- page %>%
      html_element("h1.headline__text") %>%
      html_text(trim = TRUE)
    
    # Date
    date <- page %>%
      html_element(
        xpath = "//div[contains(@class,'headline__byline-sub-text')]//div[contains(@class,'timestamp')]"
      ) %>%
      html_text(trim = TRUE) %>%
      str_remove("^Updated\\s*") %>%
      str_squish()
    
    # Content
    content <- page %>%
      html_elements("div.article__content p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    tibble(
      Headline = headline,
      Date = date,
      URL = url,
      Full_Text = content
    )
    
  }, error = function(e) {
    message(paste("Failed to scrape URL:", url))
    tibble(
      Headline = NA_character_,
      Date = NA_character_,
      URL = url,
      Full_Text = NA_character_
    )
  })
}

# Apply scraping function to all URLs
all_articles <- map_dfr(all_urls, scrape_article)

# Save results
write_csv(all_articles, "cnn_gaza_all_articles.csv")

# Preview
print(head(all_articles))

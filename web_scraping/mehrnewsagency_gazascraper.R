# Load libraries
library(chromote)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(progress)

# Start Chromote session
b <- ChromoteSession$new()

# Scrape URLs
scrape_article <- function(url, retries = 3) {
  for (i in seq_len(retries)) {
    try({
      b$Page$navigate(url)
      Sys.sleep(10)
      html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
      page <- read_html(html)
      
      headline <- page %>% html_element("h1") %>% html_text(trim = TRUE)
      date_raw <- page %>% html_element("div.item-date span") %>% html_text(trim = TRUE)
      date_clean <- str_replace_all(date_raw, "\u202f", " ")
      
      # Always return Date as character (robust against parse errors)
      date_final <- ifelse(length(date_clean) == 0, NA_character_, date_clean)
      
      content <- page %>%
        html_elements(".item-text p") %>%
        html_text(trim = TRUE) %>%
        paste(collapse = "\n\n")
      
      return(tibble(
        Headline = ifelse(length(headline) == 0, NA_character_, headline),
        Date = date_final,
        URL = url,
        Content = ifelse(length(content) == 0, NA_character_, content)
      ))
    }, silent = TRUE)
    
    Sys.sleep(3) # backoff before retry
  }
  
  # If all retries fail, return NA row
  message(paste("Failed to scrape URL after retries:", url))
  tibble(
    Headline = NA_character_,
    Date = NA_character_,
    URL = url,
    Content = NA_character_
  )
}

urls_df <- read_csv("mehrnews_article_urls.csv")

# Scrape articles
all_articles <- tibble()
pb_articles <- progress_bar$new(
  format = "Scraping articles [:bar] :current/:total (:percent) eta: :eta",
  total = nrow(urls_df), clear = FALSE, width = 60
)

for (i in seq_along(urls_df$URL)) {
  res <- scrape_article(urls_df$URL[i])
  all_articles <- bind_rows(all_articles, res)
  
  pb_articles$tick()
  
  # Save partial results every 500 articles
  if (i %% 500 == 0) {
    part_file <- str_replace("mehrnewsagency_gaza_all_articles.csv",
                             ".csv$", paste0("_part_", i, ".csv"))
    write_csv(all_articles, part_file)
  }
}

# Save CSV
write_csv(all_articles, "mehrnewsagency_gaza_all_articles.csv")

print(head(all_articles))

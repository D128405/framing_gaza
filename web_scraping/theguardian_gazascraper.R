# Load libraries
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(lubridate)

# Collect URLs
base_url <- "https://www.theguardian.com/world/gaza?page="
max_pages <- 251   # upper bound; loop will stop earlier if no links found
delay_seconds <- 1 # be gentle with Guardian

all_urls <- character(0)

for (i in seq_len(max_pages)) {
  list_url <- paste0(base_url, i)
  message("Fetching listing page ", i, " -> ", list_url)
  
  html <- tryCatch(read_html(list_url), error = function(e) {
    message("  -> failed to load page: ", e$message)
    NULL
  })
  
  if (is.null(html)) {
    Sys.sleep(delay_seconds)
    next
  }
  
  # Use the container-based nested selector pattern
  page_hrefs <- html %>%
    html_elements("div[id^='container-'] ul li div div a[href]") %>%
    html_attr("href") %>%
    na.omit() %>%
    unique()
  
  # If nothing found with the specific nested selector, try a slightly broader one
  if (length(page_hrefs) == 0) {
    page_hrefs <- html %>%
      html_elements("div[id^='container-'] a[href]") %>%
      html_attr("href") %>%
      na.omit() %>%
      unique()
  }
  
  # Filter for world-related article links (example showed /world/live/...)
  page_hrefs <- page_hrefs[str_detect(page_hrefs, "^/world")]
  
  if (length(page_hrefs) == 0) {
    message("  -> no /world links found on page ", i, ". Stopping collection.")
    break
  }
  
  # Normalize to absolute URLs
  normalize_href <- function(h) {
    if (str_starts(h, "http")) {
      h
    } else if (str_starts(h, "//")) {
      paste0("https:", h)
    } else {
      paste0("https://www.theguardian.com", h)
    }
  }
  
  full_urls <- map_chr(page_hrefs, normalize_href)
  all_urls <- c(all_urls, full_urls)
  
  Sys.sleep(delay_seconds)
}

all_urls <- unique(all_urls)
message("Collected ", length(all_urls), " unique article URLs.")
write_csv(data.frame(URL = all_urls), "theguardian_article_urls.csv")

# Scrape articles
# Helper that tries multiple content selectors
get_article_content <- function(page) {
  # priority selectors
  selectors <- c(
    "div[data-gu-name='body'] p",                         # common Guardian body
    "div.article-body-commercial-selector p",             # alternative
    "div.content__article-body p",                       # older patterns
    "article p"                                          # generic fallback
  )
  
  paragraphs <- character(0)
  for (sel in selectors) {
    paragraphs <- page %>%
      html_elements(sel) %>%
      html_text(trim = TRUE)
    if (length(paragraphs) > 0) break
  }
  paste(paragraphs, collapse = "\n\n")
}

scrape_article <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    headline <- page %>% html_element("h1") %>% html_text(trim = TRUE)
    
    # Try to get published time from meta tag, fallback to time element
    date_iso <- page %>%
      html_element("meta[property='article:published_time']") %>%
      html_attr("content")
    
    if (is.na(date_iso) || is.null(date_iso)) {
      date_iso <- page %>%
        html_element("time") %>%
        html_attr("datetime")
    }
    
    # Parse with lubridate; if parse fails keep NA
    date_parsed <- tryCatch({
      if (!is.na(date_iso) && nzchar(date_iso)) {
        # lubridate will handle timezone if present
        ymd_hms(date_iso, quiet = TRUE)
      } else {
        NA
      }
    }, error = function(e) NA)
    
    content <- get_article_content(page)
    
    tibble::tibble(
      Headline = ifelse(length(headline) > 0, headline, NA_character_),
      Date = ifelse(!is.na(date_parsed), as.character(date_parsed), NA_character_),
      URL = url,
      Content = ifelse(nzchar(content), content, NA_character_)
    )
  }, error = function(e) {
    message("  -> failed to scrape article: ", url)
    message("     error: ", e$message)
    tibble::tibble(
      Headline = NA_character_,
      Date = NA_character_,
      URL = url,
      Content = NA_character_
    )
  })
}

# If you already wrote theguardian_article_urls.csv above, read from it; otherwise use collected all_urls
if (file.exists("theguardian_article_urls.csv")) {
  urls_df <- read_csv("theguardian_article_urls.csv", show_col_types = FALSE)
  urls_to_scrape <- urls_df$URL
} else {
  urls_to_scrape <- all_urls
}

message("Starting article scraping for ", length(urls_to_scrape), " URLs.")

all_articles <- map_dfr(urls_to_scrape, function(u) {
  message("Scraping article: ", u)
  res <- scrape_article(u)
  Sys.sleep(1)
  res
})

write_csv(all_articles, "theguardian_gaza_all_articles.csv")

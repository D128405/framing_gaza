# Load libraries
library(chromote)
library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

search_url <- "https://www.scmp.com/search/gaza?q=gaza"
max_articles <- 6000
max_scrolls  <- 1000
scroll_pause <- 15
polite_pause  <- 1

# Scrape URLs
b <- ChromoteSession$new()
on.exit({
  try(b$close(), silent = TRUE)
})

b$Page$navigate(search_url)
b$Page$loadEventFired()

all_urls <- character()
scroll_count <- 0

repeat {
  scroll_count <- scroll_count + 1
  # Scroll to bottom
  b$Runtime$evaluate("window.scrollBy(0, document.body.scrollHeight);")
  Sys.sleep(scroll_pause)
  
  # Get fully rendered HTML
  doc <- b$DOM$getDocument()
  root_html <- b$DOM$getOuterHTML(nodeId = doc$root$nodeId)[["outerHTML"]]
  page <- read_html(root_html)
  
  # Collect hrefs from the search result anchors
  hrefs <- page %>%
    html_elements("a[data-qa='BaseLink-renderAnchor-StyledAnchor']") %>%
    html_attr("href") %>%
    na.omit() %>%
    unique() %>%
    trimws()
  
  if (length(hrefs) == 0) {
    message("Keine hrefs gefunden in diesem Zyklus.")
  }
  
  # Normalize to absolute URLs
  abs_urls <- ifelse(grepl("^https?://", hrefs), hrefs, paste0("https://www.scmp.com", hrefs))
  
  # Keep only real article URLs: pattern /news/.../article/<digits> (tough but conservative)
  article_pattern <- "/news/.*/article/[0-9]+"
  article_urls <- abs_urls[grepl(article_pattern, abs_urls, perl = TRUE)]
  
  # Add only new ones
  new_urls <- setdiff(article_urls, all_urls)
  if (length(new_urls) > 0) {
    all_urls <- unique(c(all_urls, new_urls))
    message("Gesammelt: ", length(all_urls), " eindeutige Artikel-URLs (plus ", length(new_urls), " neue).")
  } else {
    message("Keine neuen Artikel-URLs in diesem Zyklus.")
  }
  
  # Stop conditions
  if (length(all_urls) >= max_articles) {
    all_urls <- head(all_urls, max_articles)
    message("➡️ Limit erreicht (", max_articles, " Artikel). Stoppe das Scrollen.")
    break
  }
  if (scroll_count >= max_scrolls) {
    message("Maximale Scroll-Versuche (", max_scrolls, ") erreicht. Stoppe.")
    break
  }
  Sys.sleep(1)
}

write_csv(data.frame(url = all_urls), "scmp_article_urls.csv")
message("Fertig mit Sammeln. Insgesamt: ", length(all_urls), " URLs. Gespeichert in scmp_article_urls.csv")

# Scrape articles
scrape_article <- function(url) {
  tryCatch({
    Sys.sleep(polite_pause)
    page <- read_html(url)
    
    # Headline: h1 (mit data-qa oder fallback)
    headline <- page %>%
      html_element("h1[data-qa='ContentHeadline-ContainerWithTag'], h1") %>%
      html_text(trim = TRUE)
    if (is.null(headline) || nchar(headline) == 0) headline <- NA_character_
    
    # Datum: bevorzugt time[data-qa='Article20Date-PublishedDate'] -> attr datetime
    pub_time_node <- page %>% html_element("time[data-qa='Article20Date-PublishedDate']")
    if (!is.null(pub_time_node)) {
      datetime <- html_attr(pub_time_node, "datetime")
      if (!is.na(datetime)) {
        # trims milliseconds/Z falls vorhanden
        datetime_trim <- substr(datetime, 1, 19)  # "YYYY-mm-ddTHH:MM:SS"
        date_published <- as.POSIXct(datetime_trim, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      } else {
        date_published <- NA
      }
    } else {
      # Fallback: meta tag
      meta_time <- page %>% html_element("meta[property='article:published_time']") %>% html_attr("content")
      if (!is.na(meta_time)) {
        date_published <- as.POSIXct(substr(meta_time,1,19), format="%Y-%m-%dT%H:%M:%S", tz="UTC")
      } else {
        date_published <- NA
      }
    }
    
    # Content: paragraphs inside section[data-qa='ContentBody-ContentBodyContainer']
    content_nodes <- page %>%
      html_elements("section[data-qa='ContentBody-ContentBodyContainer'] p[data-qa='Component-Component'], section[data-qa='ContentBody-ContentBodyContainer'] p, article p, div.article__body p")
    
    if (length(content_nodes) > 0) {
      # remove obvious "Advertisement" paragraphs if they exist
      texts <- content_nodes %>% html_text(trim = TRUE)
      texts <- texts[!grepl("^Advertisement$|^Read full article$|^READ FULL ARTICLE$", texts, ignore.case = TRUE)]
      article_text <- paste(texts, collapse = "\n\n")
      if (nchar(article_text) == 0) article_text <- NA_character_
    } else {
      article_text <- NA_character_
    }
    
    tibble(
      Headline = headline,
      Date = date_published,
      URL = url,
      Content = article_text
    )
  }, error = function(e) {
    message("Fehler beim Scrapen: ", url, " — ", e$message)
    tibble(
      Headline = NA_character_,
      Date = NA_character_,
      URL = url,
      Content = NA_character_
    )
  })
}

urls_df <- read_csv("scmp_article_urls.csv", show_col_types = FALSE)

all_articles <- urls_df$url %>%
  map_dfr(scrape_article)

# Save CSV
write_csv(all_articles, "scmp_gaza_all_articles.csv")
message("Fertig: gescrapte Artikel gespeichert in scmp_gaza_all_articles.csv")
print(all_articles)

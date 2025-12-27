# Load libraries
library(chromote)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(httr2)
library(progressr)

handlers(global = TRUE) # Enable progress bars

# Collect URLs
url <- "https://timesofindia.indiatimes.com/topic/gaza/news"

b <- ChromoteSession$new()
b$Page$navigate(url)
Sys.sleep(15)

all_urls <- c()
max_articles <- 6000

repeat {
  # Get current page HTML
  page_html <- b$DOM$getDocument()
  html <- b$DOM$getOuterHTML(nodeId = page_html$root$nodeId)
  doc <- read_html(html$outerHTML)
  
  urls <- doc %>%
    html_elements("a[href*='/articleshow/']") %>%
    html_attr("href") %>%
    unique()
  
  urls <- ifelse(grepl("^http", urls),
                 urls,
                 paste0("https://timesofindia.indiatimes.com", urls))
  
  all_urls <- unique(c(all_urls, urls))
  message("Collected ", length(all_urls), " URLs")
  
  if (length(all_urls) >= max_articles) break
  
  # Check for load more button
  btn_exists <- b$Runtime$evaluate(
    "Array.from(document.querySelectorAll('button')).some(b => b.textContent.includes('Load More Articles'))"
  )$result$value
  
  if (btn_exists) {
    # Scroll to bottom
    b$Runtime$evaluate("window.scrollTo(0, document.body.scrollHeight)")
    Sys.sleep(15)
    
    # Click the button
    b$Runtime$evaluate(
      "Array.from(document.querySelectorAll('button')).find(b => b.textContent.includes('Load More Articles')).click()"
    )
    
    # Wait for new articles
    old_count <- length(all_urls)
    for (i in 1:30) {
      Sys.sleep(2)
      page_html <- b$DOM$getDocument()
      html <- b$DOM$getOuterHTML(nodeId = page_html$root$nodeId)
      doc <- read_html(html$outerHTML)
      
      new_urls <- doc %>%
        html_elements("a[href*='/articleshow/']") %>%
        html_attr("href") %>%
        unique()
      
      if (length(new_urls) > old_count) break
    }
  } else {
    message("No more 'Load More Articles' button found. Stopping.")
    break
  }
}

all_urls <- head(all_urls, max_articles)

# Scrape articles
scrape_article <- function(url, retries = 3) {
  for (i in 1:retries) {
    tryCatch({
      resp <- request(url) |> req_perform()
      doc <- read_html(resp_body_string(resp))
      
      # Headline
      headline <- doc %>%
        html_element("h1") %>%
        html_text(trim = TRUE)
      
      # Publication date
      date_text <- doc %>%
        html_elements("span") %>%
        html_text(trim = TRUE) %>%
        str_subset("Updated:|Published:") %>%
        .[1]
      
      if (!is.na(date_text)) {
        date_text <- str_remove(date_text, ".*?:\\s*")
        date_published <- suppressWarnings(
          as.POSIXct(date_text, format="%b %d, %Y, %H:%M IST", tz="Asia/Kolkata")
        )
      } else {
        date_published <- NA
      }
      
      # Content
      article_text <- doc %>%
        html_element("[data-articlebody='1']") %>%
        html_text(trim = TRUE)
      
      if (is.na(article_text) || article_text == "") {
        article_text <- doc %>%
          html_elements("[data-articlebody='1'] p, [class*='fewcent-'][class*='js_tbl_article'] p") %>%
          html_text(trim = TRUE) %>%
          paste(collapse = "\n\n")
      }
      
      if (!is.na(article_text) && article_text != "") {
        article_text <- article_text %>%
          str_replace_all("\\s+", " ") %>%
          str_trim()
      }
      
      return(tibble(
        Headline = headline,
        Date = date_published,
        URL = url,
        Content = article_text
      ))
      
    }, error = function(e) {
      message("Error scraping ", url, " (attempt ", i, "): ", e$message)
      Sys.sleep(5 * i) # backoff
      return(NULL)
    })
  }
  return(NULL)
}

# Progress bar
articles <- with_progress({
  p <- progressor(along = all_urls)
  map_dfr(all_urls, function(u) {
    result <- scrape_article(u)
    p(message = paste("Scraped:", u))
    result
  })
})

# Save CSV
print(articles)
write.csv(articles, "timesofindia_gaza_all_articles.csv", row.names = FALSE)

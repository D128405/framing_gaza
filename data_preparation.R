##### Cleaning data

library(tidyverse)

### ABC News

# Load your data, exclude missings and duplicates
abcnews <- na.omit(read.csv("abcnews_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
abcnews$Date <- as.POSIXct(abcnews$Date, format = "%B %d, %Y, %I:%M %p")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
abcnews <- abcnews %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(abcnews$Date)

# Save clean CSV
write.csv(abcnews, "abcnews_gaza_all_articles_clean.csv", row.names = FALSE)
View(abcnews)

### Al Jazeera

# Load your data, exclude missings and duplicates
aljazeera <- na.omit(read.csv("aljazeera_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
aljazeera$Date <- as.POSIXct(aljazeera$Date, format = "%d %b %Y")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
aljazeera <- aljazeera %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(aljazeera$Date)

# Save clean CSV
write.csv(aljazeera, "aljazeera_gaza_all_articles_clean.csv", row.names = FALSE)
View(aljazeera)

### Arab News

# Load your data, exclude missings and duplicates
arabnews <- na.omit(read.csv("arabnews_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
arabnews$Date <- as.POSIXct(arabnews$Date, format = "%B %d, %Y %H:%M")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
arabnews <- arabnews %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(arabnews$Date)

# Save clean CSV
write.csv(arabnews, "arabnews_gaza_all_articles_clean.csv", row.names = FALSE)
View(arabnews)

### BBC

# Load your data, exclude missings and duplicates
bbc <- na.omit(read.csv("bbc_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
bbc$Date <- as.POSIXct(bbc$Date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
bbc <- bbc %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(bbc$Date)

# Save clean CSV
write.csv(bbc, "bbc_gaza_all_articles_clean.csv", row.names = FALSE)
View(bbc)

### CGTN

# Load your data, exclude missings and duplicates
cgtn <- na.omit(read.csv("cgtn_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
cgtn$Date <- as.POSIXct(cgtn$Date, format = "%H:%M, %d-%b-%Y")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
cgtn <- cgtn %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(cgtn$Date)

# Save clean CSV
write.csv(cgtn, "cgtn_gaza_all_articles_clean.csv", row.names = FALSE)
View(cgtn)

### CNN

# Load your data, exclude missings and duplicates
cnn <- na.omit(read.csv("cnn_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Function sort_date to parse different date/time formats in the scraped CNN dataset. sort_date (Author: David Jiahui Luu, LMU Munich) in dev atm, about to be implemented in tidycomm
sort_date <- function(data, date_column, oldest = NULL, newest = NULL,
                      descending = TRUE) {
  date_column <- rlang::enquo(date_column)
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.")
  }
  
  col_name <- rlang::as_name(date_column)
  if (!col_name %in% names(data)) {
    stop("`date_column` must exist in `data`.")
  }
  
  current_locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", current_locale))
  Sys.setlocale("LC_TIME", "C")
  
  formats <- c(
    # Date only
    "%d.%m.%Y", "%Y-%m-%d", "%m/%d/%Y", "%d-%m-%Y", "%d/%m/%Y",
    "%Y/%m/%d", "%d %b %Y", "%b %d %Y", "%b %d, %Y",
    "%d %B %Y", "%B %d %Y", "%B %d, %Y",
    "%m-%d-%Y", "%Y.%m.%d", "%d.%m.%y", "%m/%d/%y",
    
    # Date + time
    "%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S",
    "%d.%m.%Y %H:%M", "%d.%m.%Y %H:%M:%S",
    "%m/%d/%Y %H:%M", "%m/%d/%Y %H:%M:%S",
    "%d-%m-%Y %H:%M", "%d-%m-%Y %H:%M:%S",
    "%d %b %Y %H:%M", "%d %b %Y %H:%M:%S",
    "%d %B %Y %H:%M", "%d %B %Y %H:%M:%S",
    "%b %d %Y %H:%M", "%b %d %Y %H:%M:%S",
    "%B %d %Y %H:%M", "%B %d %Y %H:%M:%S",
    "%b %d, %Y %H:%M", "%b %d, %Y %H:%M:%S",
    "%B %d, %Y %H:%M", "%B %d, %Y %H:%M:%S",
    
    # Custom datetime without timezone
    "%I:%M %p, %a %B %d, %Y",
    "%I:%M %p, %a %b %d, %Y",
    "%I:%M %p, %A %B %d, %Y",
    "%I:%M %p, %A %b %d, %Y"
  )
  
  parse_mixed_dates <- function(x) {
    extract_first <- function(s) {
      s <- stringr::str_trim(s)
      
      # Remove trailing time markers
      s <- stringr::str_replace_all(s, "\\b(Uhr|h)\\b", "")
      
      # Remove timezone abbreviations before commas
      s <- stringr::str_replace_all(s, "\\b[A-Z]{2,4}\\b(?=,)", "")
      
      # Handle "X hr ago PUBLISHED ..." → extract the part after "PUBLISHED"
      if (stringr::str_detect(s, "(?i)\\bPUBLISHED\\b")) {
        s <- stringr::str_replace(s, ".*(?i)\\bPUBLISHED\\b\\s*", "")
      }
      
      # Cleanup extra spaces or commas
      s <- stringr::str_replace_all(s, ",\\s*,", ",")
      s <- stringr::str_replace_all(s, "\\s{2,}", " ")
      
      # Patterns to extract standard date strings
      patterns <- c(
        "\\d{1,2}:\\d{2}\\s*[AP]M,?\\s*[A-Za-z]+\\s+[A-Za-z]+\\s+\\d{1,2},\\s+\\d{4}",
        "\\d{1,2}\\.\\d{1,2}\\.\\d{4}\\s+\\d{1,2}:\\d{2}",
        "\\d{4}-\\d{2}-\\d{2}",
        "\\d{1,2}/\\d{1,2}/\\d{4}",
        "\\d{1,2}-\\d{1,2}-\\d{4}",
        "\\d{1,2} [A-Za-z]{3,9} \\d{4}",
        "[A-Za-z]{3,9} \\d{1,2}, \\d{4}",
        "(Mon|Tue|Wed|Thu|Fri|Sat|Sun) [A-Za-z]{3,9} \\d{1,2}, \\d{4}",
        "[A-Za-z]+ \\d{1,2}, \\d{4}"
      )
      
      for (p in patterns) {
        m <- stringr::str_extract(s, p)
        if (!is.na(m)) {
          m <- stringr::str_remove_all(m, "\\b(Mon|Tue|Wed|Thu|Fri|Sat|Sun),?\\s+")
          return(m)
        }
      }
      
      NA_character_
    }
    
    x_char <- as.character(x)
    date_strings <- vapply(x_char, extract_first, character(1))
    out <- as.Date(rep(NA_character_, length(date_strings)))
    for (fmt in formats) {
      idx <- is.na(out) & !is.na(date_strings)
      if (!any(idx)) break
      out[idx] <- suppressWarnings(as.POSIXct(date_strings[idx], format = fmt))
    }
    as.Date(out)
  }
  
  data[[col_name]] <- parse_mixed_dates(data[[col_name]])
  
  if (all(is.na(data[[col_name]]))) {
    stop("No valid dates found in `date_column`.")
  }
  
  if (!is.null(oldest)) {
    oldest <- as.Date(oldest, "%Y-%m-%d")
    if (is.na(oldest)) stop("`oldest` must be in format '%Y-%m-%d'.")
    data <- dplyr::filter(data, !!date_column >= oldest)
  }
  
  if (!is.null(newest)) {
    newest <- as.Date(newest, "%Y-%m-%d")
    if (is.na(newest)) stop("`newest` must be in format '%Y-%m-%d'.")
    data <- dplyr::filter(data, !!date_column <= newest)
  }
  
  if (isTRUE(descending)) {
    data <- dplyr::arrange(data, dplyr::desc(!!date_column))
  } else {
    data <- dplyr::arrange(data, !!date_column)
  }
  
  data <- dplyr::mutate(
    data,
    !!date_column := format(!!date_column, "%Y-%m-%d")
  )
  
  return(data)
}

cnn <- cnn %>%
  sort_date(Date) %>%
  rename(Content = Full_Text) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(cnn$Date)

# Save clean CSV
write.csv(cnn, "cnn_gaza_all_articles_clean.csv", row.names = FALSE)
View(cnn)

### Daily Sabah

# Load your data, exclude missings and duplicates
dailysabah <- na.omit(read.csv("dailysabah_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
dailysabah$Date <- as.POSIXct(dailysabah$Date, format = "%Y-%m-%dT%H:%M:%S")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
dailysabah <- dailysabah %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(dailysabah$Date)

# Save clean CSV
write.csv(dailysabah, "dailysabah_gaza_all_articles_clean.csv", row.names = FALSE)
View(dailysabah)

### DW

# Load your data, exclude missings and duplicates
dw <- na.omit(read.csv("dw_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
dw$Date <- as.POSIXct(dw$Date, format = "%m/%d/%y")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
dw <- dw %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(dw$Date)

# Save clean CSV
write.csv(dw, "dw_gaza_all_articles_clean.csv", row.names = FALSE)
View(dw)

### Euronews

# Load your data, exclude missings and duplicates
euronews <- na.omit(read.csv("euronews_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
euronews$Date <- as.POSIXct(euronews$Date, format = "%Y-%m-%d %H:%M:%S")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
euronews <- euronews %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(euronews$Date)

# Save clean CSV
write.csv(euronews, "euronews_gaza_all_articles_clean.csv", row.names = FALSE)
View(euronews)

### France24

# Load your data, exclude missings and duplicates
france24 <- na.omit(read.csv("france24_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
france24$Date <- as.POSIXct(france24$Date, format = "%Y-%m-%dT%H:%M:%S")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
france24 <- france24 %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(france24$Date)

# Save clean CSV
write.csv(france24, "france24_gaza_all_articles_clean.csv", row.names = FALSE)
View(france24)

### Israel Hayom

# Load your data, exclude missings and duplicates
israelhayom <- na.omit(read.csv("israelhayom_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
israelhayom$Date <- as.POSIXct(israelhayom$Date, format = "%m-%d-%Y %H:%M")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
israelhayom <- israelhayom %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(israelhayom$Date)

# Save clean CSV
write.csv(israelhayom, "israelhayom_gaza_all_articles_clean.csv", row.names = FALSE)
View(israelhayom)

### Le Monde

# Load your data, exclude missings and duplicates
lemonde <- na.omit(read.csv("lemonde_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Manual date format unification in Excel due to several different date formats
write_csv(lemonde, "lemonde_gaza_all_articles_dateunification.csv")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
lemonde <- read_csv("lemonde_gaza_all_articles_dateunification.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(lemonde$Date)

# Save clean CSV
write.csv(lemonde, "lemonde_gaza_all_articles_clean.csv", row.names = FALSE)
View(lemonde)

### Mehr News Agency

# Load your data, exclude missings and duplicates
mehrnewsagency <- na.omit(read.csv("mehrnewsagency_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
mehrnewsagency$Date <- as.POSIXct(mehrnewsagency$Date, format = "%B %d, %Y, %I:%M %p")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
mehrnewsagency <- mehrnewsagency %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(mehrnewsagency$Date)

# Save clean CSV
write.csv(mehrnewsagency, "mehrnewsagency_gaza_all_articles_clean.csv", row.names = FALSE)
View(mehrnewsagency)

### NPR

# Load your data, exclude missings and duplicates
npr <- na.omit(read.csv("npr_middleeast_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
npr$Date <- as.POSIXct(npr$Date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
npr <- npr %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(npr$Date)

# Save clean CSV
write.csv(npr, "npr_middleeast_all_articles_clean.csv", row.names = FALSE)
View(npr)

### SCMP

# Load your data, exclude missings and duplicates
scmp <- na.omit(read.csv("scmp_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
scmp$Date <- as.POSIXct(scmp$Date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
scmp <- scmp %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(scmp$Date)

# Save clean CSV
write.csv(scmp, "scmp_gaza_all_articles_clean.csv", row.names = FALSE)
View(scmp)

### The Guardian

# Load your data, exclude missings and duplicates
theguardian <- na.omit(read.csv("theguardian_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
theguardian$Date <- as.POSIXct(theguardian$Date, format = "%Y-%m-%d %H:%M:%S")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
theguardian <- theguardian %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(theguardian$Date)

# Save clean CSV
write.csv(theguardian, "theguardian_gaza_all_articles_clean.csv", row.names = FALSE)
View(theguardian)

### The Intercept

# Load your data, exclude missings and duplicates
theintercept <- na.omit(read.csv("theintercept_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
theintercept$Date <- as.POSIXct(theintercept$Date, format = "%Y-%m-%dT%H:%M:%S")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
theintercept <- theintercept %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(theintercept$Date)

# Save clean CSV
write.csv(theintercept, "theintercept_gaza_all_articles_clean.csv", row.names = FALSE)
View(theintercept)

### The Straits Times

# Load your data, exclude missings and duplicates
thestraitstimes <- na.omit(read.csv("thestraitstimes_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
thestraitstimes$Date <- as.POSIXct(thestraitstimes$Date, format = "%b %d, %Y, %H:%M %p")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
thestraitstimes <- thestraitstimes %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(thestraitstimes$Date)

# Save clean CSV
write.csv(thestraitstimes, "thestraitstimes_gaza_all_articles_clean.csv", row.names = FALSE)
View(thestraitstimes)

### Times of India

# Load your data, exclude missings and duplicates
timesofindia <- na.omit(read.csv("timesofindia_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
timesofindia$Date <- as.POSIXct(timesofindia$Date, format = "%Y-%m-%d %H:%M:%S")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
timesofindia <- timesofindia %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(timesofindia$Date)

# Save clean CSV
write.csv(timesofindia, "timesofindia_gaza_all_articles_clean.csv", row.names = FALSE)
View(timesofindia)

### VOA

# Load your data, exclude missings and duplicates
voa <- na.omit(read.csv("voa_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
voa$Date <- as.POSIXct(voa$Date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
voa <- voa %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(voa$Date)

# Save clean CSV
write.csv(voa, "voa_gaza_all_articles_clean.csv", row.names = FALSE)
View(voa)

### Vox

# Load your data, exclude missings and duplicates
vox <- na.omit(read.csv("vox_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
vox$Date <- as.POSIXct(vox$Date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
vox <- vox %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(vox$Date)

# Save clean CSV
write.csv(vox, "vox_gaza_all_articles_clean.csv", row.names = FALSE)
View(vox)

### WAFA

# Load your data, exclude missings and duplicates
wafa <- na.omit(read.csv("wafa_gaza_all_articles.csv", stringsAsFactors = FALSE)) %>%
  distinct(URL, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  drop_na()

# Convert the Date column from text to POSIXct
wafa$Date <- as.POSIXct(wafa$Date, format = "%d/%b/%Y %H:%M %p")

# Convert to Date format (YYYY-MM-DD), filter date, sort date
wafa <- wafa %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2025-08-11")) %>%
  arrange(desc(Date))

# Check result
head(wafa$Date)

# Save clean CSV
write.csv(wafa, "wafa_gaza_all_articles_clean.csv", row.names = FALSE)
View(wafa)

##### Cluster data

# Define objects for clustering
abcnews <- read_csv("abcnews_gaza_all_articles_clean.csv")
aljazeera <- read_csv("aljazeera_gaza_all_articles_clean.csv")
arabnews <- read_csv("arabnews_gaza_all_articles_clean.csv")
bbc <- read_csv("bbc_gaza_all_articles_clean.csv")
cgtn <- read_csv("cgtn_gaza_all_articles_clean.csv")
cnn <- read_csv("cnn_gaza_all_articles_clean.csv")
dailysabah <- read_csv("dailysabah_gaza_all_articles_clean.csv")
dw <- read_csv("dw_gaza_all_articles_clean.csv")
euronews <- read_csv("euronews_gaza_all_articles_clean.csv")
france24 <- read_csv("france24_gaza_all_articles_clean.csv")
israelhayom <- read_csv("israelhayom_gaza_all_articles_clean.csv")
lemonde <- read_csv("lemonde_gaza_all_articles_clean.csv")
mehrnewsagency <- read_csv("mehrnewsagency_gaza_all_articles_clean.csv")
npr <- read_csv("npr_middleeast_all_articles_clean.csv")
scmp <- read_csv("scmp_gaza_all_articles_clean.csv")
theguardian <- read_csv("theguardian_gaza_all_articles_clean.csv")
theintercept <- read_csv("theintercept_gaza_all_articles_clean.csv")
thestraitstimes <- read_csv("thestraitstimes_gaza_all_articles_clean.csv")
timesofindia <- read_csv("timesofindia_gaza_all_articles_clean.csv")
voa <- read_csv("voa_gaza_all_articles_clean.csv")
vox <- read_csv("vox_gaza_all_articles_clean.csv")
wafa <- read_csv("wafa_gaza_all_articles_clean.csv")

# Save CSV Gaza all articles 47.439 from October 7, 2023 until August 11, 2025
gaza_all_articles_clean <- rbind(abcnews, aljazeera, arabnews, bbc, cgtn, cnn, dailysabah, dw, euronews, france24, israelhayom, lemonde, mehrnewsagency, npr, scmp, theguardian, theintercept, thestraitstimes, timesofindia, voa, vox, wafa) %>%
  arrange(desc(Date))
write.csv(gaza_all_articles_clean, "gaza_all_articles_clean.csv")



##### TOKENIZE FILE WITH ALL ARTICLES AND TokenList AS LIST

library(readr)
library(quanteda)
library(dplyr)
library(ggplot2)
library(scales)

gaza_all_articles_tokenized_list <- read_csv("gaza_all_articles_clean.csv")

# Load data and create new file integrating headline and content
gaza_all_articles_tokenized_list <- gaza_all_articles_tokenized_list %>%
  mutate(Text = paste(Headline, Content, sep = "\n\n"))

# Tokenize and remove English stopwords
toks <- tokens(
  gaza_all_articles_tokenized_list$Text, 
  what = "word", 
  remove_punct = TRUE
) %>%
  tokens_remove(stopwords("en"))  # remove English stopwords

# Convert to list of character vectors for tibble
tokens_list <- as.list(toks)

# Add columns
gaza_all_articles_tokenized_list <- gaza_all_articles_tokenized_list %>%
  mutate(
    TokenList = tokens_list,  # compatible with tibble
    TokenString = sapply(tokens_list, function(x) paste(x, collapse = " ")),  # space-separated string
    TokenCount = lengths(tokens_list)  # number of tokens after stopword removal
  )

glimpse(gaza_all_articles_tokenized_list)

# Save to CSV (tokens_list won’t serialize well, but tokens_string + counts will)
write_csv(gaza_all_articles_tokenized_list, "gaza_all_articles_tokenized_list.csv")



##### TOKENIZE FILE WITH ALL ARTICLES AND SAVE TOKENLIST AS STRING (TokenList as '|'-separated string) #####

library(readr)
library(quanteda)
library(dplyr)
library(ggplot2)
library(scales)

# Read in the combined dataset
gaza_all_articles_tokenized <- read_csv("gaza_all_articles_clean.csv", show_col_types = FALSE)

# Ensure Headline and Content are safe to merge
gaza_all_articles_tokenized <- gaza_all_articles_tokenized %>%
  mutate(
    Headline = ifelse(is.na(Headline), "", as.character(Headline)),
    Content  = ifelse(is.na(Content),  "", as.character(Content)),
    Text = paste(Headline, Content, sep = "\n\n")
  )

# Tokenize and remove English stopwords
toks <- tokens(
  gaza_all_articles_tokenized$Text, 
  what = "word",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = FALSE
) %>%
  tokens_remove(stopwords("en"))

# Convert to plain character lists of equal length
n <- nrow(gaza_all_articles_tokenized)
tokens_list <- vector("list", n)
toks_len <- length(toks)
for (i in seq_len(n)) {
  if (i <= toks_len && !is.null(toks[[i]])) {
    tokens_list[[i]] <- as.character(toks[[i]])
  } else {
    tokens_list[[i]] <- character(0)
  }
}

# Create a CSV-safe TokenList column (joined with "|")
TokenList_csvsafe <- vapply(tokens_list, function(x) {
  if (length(x) == 0) "" else paste(x, collapse = "|")
}, FUN.VALUE = character(1), USE.NAMES = FALSE)

# Create TokenString (space-separated) and TokenCount
TokenString <- vapply(tokens_list, function(x) {
  if (length(x) == 0) "" else paste(x, collapse = " ")
}, FUN.VALUE = character(1), USE.NAMES = FALSE)

TokenCount <- vapply(tokens_list, length, FUN.VALUE = integer(1), USE.NAMES = FALSE)

# Combine into one tibble
gaza_all_articles_tokenized <- gaza_all_articles_tokenized %>%
  mutate(
    TokenList = TokenList_csvsafe,
    TokenString = TokenString,
    TokenCount = TokenCount
  )

# Inspect structure
glimpse(gaza_all_articles_tokenized)

# Save to CSV — TokenList is safely embedded as '|' separated string
write_csv(gaza_all_articles_tokenized, "gaza_all_articles_tokenized.csv")

message("Saved: gaza_all_articles_tokenized.csv (TokenList embedded as '|' separated string)")



##### TOKENIZE EACH NEWS OUTLET'S FILE AND SAVE TOKENLIST AS LIST
# USE PYTHON PACKAGES IN CASE LIST FORMAT CAN NOT BE SEQUNCED BY R PACKAGES

# Load required libraries
library(readr)
library(quanteda)
library(dplyr)
library(ggplot2)
library(scales)

# List of outlet prefixes
outlets <- c(
  "abcnews", "aljazeera", "arabnews", "bbc", "cgtn", "cnn",
  "dailysabah", "dw", "euronews", "france24", "israelhayom",
  "lemonde", "mehrnewsagency", "npr", "scmp", "theguardian",
  "theintercept", "thestraitstimes", "timesofindia", "voa", "vox", "wafa"
)

# Loop through each outlet
for (outlet in outlets) {
  
  message("=========================================")
  message("Processing outlet: ", outlet)
  message("=========================================")
  
  # Define filenames (NPR uses "middleeast" instead of "gaza")
  file_in <- if (outlet == "npr") {
    paste0(outlet, "_middleeast_all_articles_clean.csv")
  } else {
    paste0(outlet, "_gaza_all_articles_clean.csv")
  }
  
  file_out <- if (outlet == "npr") {
    paste0(outlet, "_middleeast_all_articles_tokenized_list.csv")
  } else {
    paste0(outlet, "_gaza_all_articles_tokenized_list.csv")
  }
  
  # Read in the dataset
  message("Reading file: ", file_in)
  df <- read_csv(file_in)
  
  # Load data and create new column integrating Headline and Content
  df <- df %>%
    mutate(Text = paste(Headline, Content, sep = "\n\n"))
  
  # Tokenize and remove English stopwords
  toks <- tokens(
    df$Text,
    what = "word",
    remove_punct = TRUE
  ) %>%
    tokens_remove(stopwords("en"))  # remove English stopwords
  
  # Convert to list of character vectors for tibble
  tokens_list <- as.list(toks)
  
  # Add tokenization results
  df <- df %>%
    mutate(
      TokenList = tokens_list,  # list-column
      TokenString = sapply(tokens_list, function(x) paste(x, collapse = " ")),  # space-separated
      TokenCount = lengths(tokens_list)  # number of tokens
    )
  
  # Display structure for inspection
  glimpse(df)
  
  # Save to CSV (TokenList doesn’t serialize well)
  write_csv(df, file_out)
  
  message("Saved tokenized file: ", file_out)
  message("")  # spacing
}

message("All outlets processed successfully!")



##### TOKENIZE EACH NEWS OUTLET'S FILE
##### TOKENIZE EACH NEWS OUTLET AND SAVE TOKENLIST AS STRING IN SAME CSV #####

library(readr)
library(quanteda)
library(dplyr)
library(ggplot2)
library(scales)

safe_tokenize_and_save <- function(file_in, file_out) {
  message("Reading: ", file_in)
  df <- read_csv(file_in, show_col_types = FALSE)
  
  # Ensure Headline and Content exist and are character (no NAs)
  if (!"Headline" %in% names(df)) df$Headline <- ""
  if (!"Content" %in% names(df))  df$Content  <- ""
  df <- df %>%
    mutate(
      Headline = ifelse(is.na(Headline), "", as.character(Headline)),
      Content  = ifelse(is.na(Content),  "", as.character(Content)),
      Text = paste(Headline, Content, sep = "\n\n")
    )
  
  df$Text <- enc2utf8(as.character(df$Text))
  
  # Tokenize and clean
  toks <- tokens(
    df$Text,
    what = "word",
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = FALSE
  ) %>%
    tokens_remove(stopwords("en"))
  
  # Convert to plain character list (guarantee same length as df)
  n <- nrow(df)
  tokens_list <- vector("list", n)
  toks_len <- length(toks)
  for (i in seq_len(n)) {
    if (i <= toks_len && !is.null(toks[[i]])) {
      tokens_list[[i]] <- as.character(toks[[i]])
    } else {
      tokens_list[[i]] <- character(0)
    }
  }
  
  # Create CSV-safe TokenList column (use "|" as separator)
  TokenList_csvsafe <- vapply(tokens_list, function(x) {
    if (length(x) == 0) "" else paste(x, collapse = "|")
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
  
  # Create TokenString and TokenCount columns
  TokenString <- vapply(tokens_list, function(x) {
    if (length(x) == 0) "" else paste(x, collapse = " ")
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
  
  TokenCount <- vapply(tokens_list, length, FUN.VALUE = integer(1), USE.NAMES = FALSE)
  
  # Combine into one dataframe
  df_out <- df %>%
    mutate(
      TokenList = TokenList_csvsafe,
      TokenString = TokenString,
      TokenCount = TokenCount
    )
  
  # Write to CSV (now everything serializes cleanly)
  write_csv(df_out, file_out)
  message("Saved: ", file_out, " (TokenList embedded as '|' separated string)")
}

# Run for all outlets
outlets <- c(
  "abcnews", "aljazeera", "arabnews", "bbc", "cgtn", "cnn",
  "dailysabah", "dw", "euronews", "france24", "israelhayom",
  "lemonde", "mehrnewsagency", "npr", "scmp", "theguardian",
  "theintercept", "thestraitstimes", "timesofindia", "voa", "vox", "wafa"
)

for (outlet in outlets) {
  file_in  <- if (outlet == "npr") paste0(outlet, "_middleeast_all_articles_clean.csv") else paste0(outlet, "_gaza_all_articles_clean.csv")
  file_out <- if (outlet == "npr") paste0(outlet, "_middleeast_all_articles_tokenized.csv") else paste0(outlet, "_gaza_all_articles_tokenized.csv")
  
  tryCatch(
    safe_tokenize_and_save(file_in, file_out),
    error = function(e) message("Error processing ", outlet, ": ", conditionMessage(e))
  )
}

message("All outlets processed successfully! TokenList included in CSVs.")



# Define objects for clustering with TokenList as String
abcnews <- read_csv("abcnews_gaza_all_articles_tokenized.csv")
aljazeera <- read_csv("aljazeera_gaza_all_articles_tokenized.csv")
arabnews <- read_csv("arabnews_gaza_all_articles_tokenized.csv")
bbc <- read_csv("bbc_gaza_all_articles_tokenized.csv")
cgtn <- read_csv("cgtn_gaza_all_articles_tokenized.csv")
cnn <- read_csv("cnn_gaza_all_articles_tokenized.csv")
dailysabah <- read_csv("dailysabah_gaza_all_articles_tokenized.csv")
dw <- read_csv("dw_gaza_all_articles_tokenized.csv")
euronews <- read_csv("euronews_gaza_all_articles_tokenized.csv")
france24 <- read_csv("france24_gaza_all_articles_tokenized.csv")
israelhayom <- read_csv("israelhayom_gaza_all_articles_tokenized.csv")
lemonde <- read_csv("lemonde_gaza_all_articles_tokenized.csv")
mehrnewsagency <- read_csv("mehrnewsagency_gaza_all_articles_tokenized.csv")
npr <- read_csv("npr_middleeast_all_articles_tokenized.csv")
scmp <- read_csv("scmp_gaza_all_articles_tokenized.csv")
theguardian <- read_csv("theguardian_gaza_all_articles_tokenized.csv")
theintercept <- read_csv("theintercept_gaza_all_articles_tokenized.csv")
thestraitstimes <- read_csv("thestraitstimes_gaza_all_articles_tokenized.csv")
timesofindia <- read_csv("timesofindia_gaza_all_articles_tokenized.csv")
voa <- read_csv("voa_gaza_all_articles_tokenized.csv")
vox <- read_csv("vox_gaza_all_articles_tokenized.csv")
wafa <- read_csv("wafa_gaza_all_articles_tokenized.csv")

### Cluster 1 Political Compliance
# Combine Israel
cluster1_politicalcompliance_il_tokenized <- rbind(israelhayom, abcnews, bbc, cnn, dw, euronews, france24, lemonde, npr, theguardian, theintercept, vox, voa) %>%
  arrange(desc(Date))

# Save CSV
write.csv(cluster1_politicalcompliance_il_tokenized, "cluster1_politicalcompliance_il_tokenized.csv", row.names = FALSE)
View(cluster1_politicalcompliance_il_tokenized)

# Cluster 1 Date t1 Initial Attacks until UN finds grounds for genocide by Israel
cluster1_politicalcompliance_il_t1_tokenized <- cluster1_politicalcompliance_il_tokenized %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2024-03-26"))

# Save CSV
write.csv(cluster1_politicalcompliance_il_t1_tokenized, "cluster1_politicalcompliance_il_t1_tokenized.csv", row.names = FALSE)

# Cluster 1 Date t2 UN finds grounds for genocide by Israel until ICJ calls for immediate end of Rafah offensive by Israel
cluster1_politicalcompliance_il_t2_tokenized <- cluster1_politicalcompliance_il_tokenized %>%
  filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-05-24"))

# Save CSV
write.csv(cluster1_politicalcompliance_il_t2_tokenized, "cluster1_politicalcompliance_il_t2_tokenized.csv", row.names = FALSE)

# Cluster 1 Date t3 ICJ calls for immediate end of Rafah offensive by Israel until Israel kills Hamas leader Yahya Sinwar
cluster1_politicalcompliance_il_t3_tokenized <- cluster1_politicalcompliance_il_tokenized %>%
  filter(Date >= as.Date("2024-05-25") & Date <= as.Date("2024-10-17"))

# Save CSV
write.csv(cluster1_politicalcompliance_il_t3_tokenized, "cluster1_politicalcompliance_il_t3_tokenized.csv", row.names = FALSE)

# Cluster 1 Date t4 Israel kills Hamas leader Yahya Sinwar until Israel breaks ceasefire
cluster1_politicalcompliance_il_t4_tokenized <- cluster1_politicalcompliance_il_tokenized %>%
  filter(Date >= as.Date("2024-10-18") & Date <= as.Date("2025-03-18"))

# Save CSV
write.csv(cluster1_politicalcompliance_il_t4_tokenized, "cluster1_politicalcompliance_il_t4_tokenized.csv", row.names = FALSE)

# Cluster 1 Date t5 Israel breaks ceasefire until Australia, Belgium, Canada, France and the UK to recognize a Palestinian state
cluster1_politicalcompliance_il_t5_tokenized <- cluster1_politicalcompliance_il_tokenized %>%
  filter(Date >= as.Date("2025-03-19") & Date <= as.Date("2025-08-11"))

# Save CSV
write.csv(cluster1_politicalcompliance_il_t5_tokenized, "cluster1_politicalcompliance_il_t5_tokenized.csv", row.names = FALSE)

# Combine Palestine
cluster1_politicalcompliance_ps_tokenized <- rbind(aljazeera, arabnews, dailysabah, mehrnewsagency, wafa) %>%
  arrange(desc(Date))

# Save CSV
write.csv(cluster1_politicalcompliance_ps_tokenized, "cluster1_politicalcompliance_ps_tokenized.csv", row.names = FALSE)
View(cluster1_politicalcompliance_ps_tokenized)

# Cluster 1 Date t1 Initial Attacks until UN finds grounds for genocide by Israel
cluster1_politicalcompliance_ps_t1_tokenized <- cluster1_politicalcompliance_ps_tokenized %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2024-03-26"))

# Save CSV
write.csv(cluster1_politicalcompliance_ps_t1_tokenized, "cluster1_politicalcompliance_ps_t1_tokenized.csv", row.names = FALSE)

# Cluster 1 Date t2 UN finds grounds for genocide by Israel until ICJ calls for immediate end of Rafah offensive by Israel
cluster1_politicalcompliance_ps_t2_tokenized <- cluster1_politicalcompliance_ps_tokenized %>%
  filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-05-24"))

# Save CSV
write.csv(cluster1_politicalcompliance_ps_t2_tokenized, "cluster1_politicalcompliance_ps_t2_tokenized.csv", row.names = FALSE)

# Cluster 1 Date t3 ICJ calls for immediate end of Rafah offensive by Israel until Israel kills Hamas leader Yahya Sinwar
cluster1_politicalcompliance_ps_t3_tokenized <- cluster1_politicalcompliance_ps_tokenized %>%
  filter(Date >= as.Date("2024-05-25") & Date <= as.Date("2024-10-17"))

# Save CSV
write.csv(cluster1_politicalcompliance_ps_t3_tokenized, "cluster1_politicalcompliance_ps_t3_tokenized.csv", row.names = FALSE)

# Cluster 1 Date t4 Israel kills Hamas leader Yahya Sinwar until Israel breaks ceasefire
cluster1_politicalcompliance_ps_t4_tokenized <- cluster1_politicalcompliance_ps_tokenized %>%
  filter(Date >= as.Date("2024-10-18") & Date <= as.Date("2025-03-18"))

# Save CSV
write.csv(cluster1_politicalcompliance_ps_t4_tokenized, "cluster1_politicalcompliance_ps_t4_tokenized.csv", row.names = FALSE)

# Cluster 1 Date t5 Israel breaks ceasefire until Australia, Belgium, Canada, France and the UK to recognize a Palestinian state
cluster1_politicalcompliance_ps_t5_tokenized <- cluster1_politicalcompliance_ps_tokenized %>%
  filter(Date >= as.Date("2025-03-19") & Date <= as.Date("2025-08-11"))

# Save CSV
write.csv(cluster1_politicalcompliance_ps_t5_tokenized, "cluster1_politicalcompliance_ps_t5_tokenized.csv", row.names = FALSE)

# Combine No strong compliance
cluster1_politicalcompliance_nc_tokenized <- rbind(cgtn, scmp, thestraitstimes, timesofindia) %>%
  arrange(desc(Date))

# Save CSV
write.csv(cluster1_politicalcompliance_nc_tokenized, "cluster1_politicalcompliance_nc_tokenized.csv", row.names = FALSE)
View(cluster1_politicalcompliance_nc_tokenized)

# Cluster 1 Date t1 Initial Attacks until UN finds grounds for genocide by Israel
cluster1_politicalcompliance_nc_t1_tokenized <- cluster1_politicalcompliance_nc_tokenized %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2024-03-26"))

# Save CSV
write.csv(cluster1_politicalcompliance_nc_t1_tokenized, "cluster1_politicalcompliance_nc_t1_tokenized.csv", row.names = FALSE)

# Cluster 1 Date t2 UN finds grounds for genocide by Israel until ICJ calls for immediate end of Rafah offensive by Israel
cluster1_politicalcompliance_nc_t2_tokenized <- cluster1_politicalcompliance_nc_tokenized %>%
  filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-05-24"))

# Save CSV
write.csv(cluster1_politicalcompliance_nc_t2_tokenized, "cluster1_politicalcompliance_nc_t2_tokenized.csv", row.names = FALSE)

# Cluster 1 Date t3 ICJ calls for immediate end of Rafah offensive by Israel until Israel kills Hamas leader Yahya Sinwar
cluster1_politicalcompliance_nc_t3_tokenized <- cluster1_politicalcompliance_nc_tokenized %>%
  filter(Date >= as.Date("2024-05-25") & Date <= as.Date("2024-10-17"))

# Save CSV
write.csv(cluster1_politicalcompliance_nc_t3_tokenized, "cluster1_politicalcompliance_nc_t3_tokenized.csv", row.names = FALSE)

# Cluster 1 Date t4 Israel kills Hamas leader Yahya Sinwar until Israel breaks ceasefire
cluster1_politicalcompliance_nc_t4_tokenized <- cluster1_politicalcompliance_nc_tokenized %>%
  filter(Date >= as.Date("2024-10-18") & Date <= as.Date("2025-03-18"))

# Save CSV
write.csv(cluster1_politicalcompliance_nc_t4_tokenized, "cluster1_politicalcompliance_nc_t4_tokenized.csv", row.names = FALSE)

# Cluster 1 Date t5 Israel breaks ceasefire until Australia, Belgium, Canada, France and the UK to recognize a Palestinian state
cluster1_politicalcompliance_nc_t5_tokenized <- cluster1_politicalcompliance_nc_tokenized %>%
  filter(Date >= as.Date("2025-03-19") & Date <= as.Date("2025-08-11"))

# Save CSV
write.csv(cluster1_politicalcompliance_nc_t5_tokenized, "cluster1_politicalcompliance_nc_t5_tokenized.csv", row.names = FALSE)

### Cluster 2 Political System
# Combine democratic
cluster2_politicalsystem_democratic_tokenized <- rbind(israelhayom, abcnews, bbc, cnn, dw, euronews, france24, lemonde, npr, theguardian, theintercept, vox, voa) %>%
  arrange(desc(Date))

# Save CSV
write.csv(cluster2_politicalsystem_democratic_tokenized, "cluster2_politicalsystem_democratic_tokenized.csv", row.names = FALSE)
View(cluster2_politicalsystem_democratic_tokenized)

# Cluster 2 Date t1 Initial Attacks until UN finds grounds for genocide by Israel
cluster2_politicalsystem_democratic_t1_tokenized <- cluster2_politicalsystem_democratic_tokenized %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2024-03-26"))

# Save CSV
write.csv(cluster2_politicalsystem_democratic_t1_tokenized, "cluster2_politicalsystem_democratic_t1_tokenized.csv", row.names = FALSE)

# Cluster 2 Date t2 UN finds grounds for genocide by Israel until ICJ calls for immediate end of Rafah offensive by Israel
cluster2_politicalsystem_democratic_t2_tokenized <- cluster2_politicalsystem_democratic_tokenized %>%
  filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-05-24"))

# Save CSV
write.csv(cluster2_politicalsystem_democratic_t2_tokenized, "cluster2_politicalsystem_democratic_t2_tokenized.csv", row.names = FALSE)

# Cluster 2 Date t3 ICJ calls for immediate end of Rafah offensive by Israel until Israel kills Hamas leader Yahya Sinwar
cluster2_politicalsystem_democratic_t3_tokenized <- cluster2_politicalsystem_democratic_tokenized %>%
  filter(Date >= as.Date("2024-05-25") & Date <= as.Date("2024-10-17"))

# Save CSV
write.csv(cluster2_politicalsystem_democratic_t3_tokenized, "cluster2_politicalsystem_democratic_t3_tokenized.csv", row.names = FALSE)

# Cluster 2 Date t4 Israel kills Hamas leader Yahya Sinwar until Israel breaks ceasefire
cluster2_politicalsystem_democratic_t4_tokenized <- cluster2_politicalsystem_democratic_tokenized %>%
  filter(Date >= as.Date("2024-10-18") & Date <= as.Date("2025-03-18"))

# Save CSV
write.csv(cluster2_politicalsystem_democratic_t4_tokenized, "cluster2_politicalsystem_democratic_t4_tokenized.csv", row.names = FALSE)

# Cluster 2 Date t5 Israel breaks ceasefire until Australia, Belgium, Canada, France and the UK to recognize a Palestinian state
cluster2_politicalsystem_democratic_t5_tokenized <- cluster2_politicalsystem_democratic_tokenized %>%
  filter(Date >= as.Date("2025-03-19") & Date <= as.Date("2025-08-11"))

# Save CSV
write.csv(cluster2_politicalsystem_democratic_t5_tokenized, "cluster2_politicalsystem_democratic_t5_tokenized.csv", row.names = FALSE)

# Combine Non-democratic
cluster2_politicalsystem_nondemocratic_tokenized <- rbind(wafa, aljazeera, arabnews, cgtn, dailysabah, mehrnewsagency, scmp, thestraitstimes, timesofindia) %>%
  arrange(desc(Date))

# Save CSV
write.csv(cluster2_politicalsystem_nondemocratic_tokenized, "cluster2_politicalsystem_nondemocratic_tokenized.csv", row.names = FALSE)
View(cluster2_politicalsystem_nondemocratic_tokenized)

# Cluster 2 Date t1 Initial Attacks until UN finds grounds for genocide by Israel
cluster2_politicalsystem_nondemocratic_t1_tokenized <- cluster2_politicalsystem_nondemocratic_tokenized %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2024-03-26"))

# Save CSV
write.csv(cluster2_politicalsystem_nondemocratic_t1_tokenized, "cluster2_politicalsystem_nondemocratic_t1_tokenized.csv", row.names = FALSE)

# Cluster 2 Date t2 UN finds grounds for genocide by Israel until ICJ calls for immediate end of Rafah offensive by Israel
cluster2_politicalsystem_nondemocratic_t2_tokenized <- cluster2_politicalsystem_nondemocratic_tokenized %>%
  filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-05-24"))

# Save CSV
write.csv(cluster2_politicalsystem_nondemocratic_t2_tokenized, "cluster2_politicalsystem_nondemocratic_t2_tokenized.csv", row.names = FALSE)

# Cluster 2 Date t3 ICJ calls for immediate end of Rafah offensive by Israel until Israel kills Hamas leader Yahya Sinwar
cluster2_politicalsystem_nondemocratic_t3_tokenized <- cluster2_politicalsystem_nondemocratic_tokenized %>%
  filter(Date >= as.Date("2024-05-25") & Date <= as.Date("2024-10-17"))

# Save CSV
write.csv(cluster2_politicalsystem_nondemocratic_t3_tokenized, "cluster2_politicalsystem_nondemocratic_t3_tokenized.csv", row.names = FALSE)

# Cluster 2 Date t4 Israel kills Hamas leader Yahya Sinwar until Israel breaks ceasefire
cluster2_politicalsystem_nondemocratic_t4_tokenized <- cluster2_politicalsystem_nondemocratic_tokenized %>%
  filter(Date >= as.Date("2024-10-18") & Date <= as.Date("2025-03-18"))

# Save CSV
write.csv(cluster2_politicalsystem_nondemocratic_t4_tokenized, "cluster2_politicalsystem_nondemocratic_t4_tokenized.csv", row.names = FALSE)

# Cluster 2 Date t5 Israel breaks ceasefire until Australia, Belgium, Canada, France and the UK to recognize a Palestinian state
cluster2_politicalsystem_nondemocratic_t5_tokenized <- cluster2_politicalsystem_nondemocratic_tokenized %>%
  filter(Date >= as.Date("2025-03-19") & Date <= as.Date("2025-08-11"))

# Save CSV
write.csv(cluster2_politicalsystem_nondemocratic_t5_tokenized, "cluster2_politicalsystem_nondemocratic_t5_tokenized.csv", row.names = FALSE)



# Define objects for clustering with TokenList as List
# USE PYTHON PACKAGES IN CASE LIST FORMAT CAN NOT BE SEQUNCED BY R PACKAGES
abcnews <- read_csv("abcnews_gaza_all_articles_tokenized_list.csv")
aljazeera <- read_csv("aljazeera_gaza_all_articles_tokenized_list.csv")
arabnews <- read_csv("arabnews_gaza_all_articles_tokenized_list.csv")
bbc <- read_csv("bbc_gaza_all_articles_tokenized_list.csv")
cgtn <- read_csv("cgtn_gaza_all_articles_tokenized_list.csv")
cnn <- read_csv("cnn_gaza_all_articles_tokenized_list.csv")
dailysabah <- read_csv("dailysabah_gaza_all_articles_tokenized_list.csv")
dw <- read_csv("dw_gaza_all_articles_tokenized_list.csv")
euronews <- read_csv("euronews_gaza_all_articles_tokenized_list.csv")
france24 <- read_csv("france24_gaza_all_articles_tokenized_list.csv")
israelhayom <- read_csv("israelhayom_gaza_all_articles_tokenized_list.csv")
lemonde <- read_csv("lemonde_gaza_all_articles_tokenized_list.csv")
mehrnewsagency <- read_csv("mehrnewsagency_gaza_all_articles_tokenized_list.csv")
npr <- read_csv("npr_middleeast_all_articles_tokenized_list.csv")
scmp <- read_csv("scmp_gaza_all_articles_tokenized_list.csv")
theguardian <- read_csv("theguardian_gaza_all_articles_tokenized_list.csv")
theintercept <- read_csv("theintercept_gaza_all_articles_tokenized_list.csv")
thestraitstimes <- read_csv("thestraitstimes_gaza_all_articles_tokenized_list.csv")
timesofindia <- read_csv("timesofindia_gaza_all_articles_tokenized_list.csv")
voa <- read_csv("voa_gaza_all_articles_tokenized_list.csv")
vox <- read_csv("vox_gaza_all_articles_tokenized_list.csv")
wafa <- read_csv("wafa_gaza_all_articles_tokenized_list.csv")

### Cluster 1 Political Compliance
# Combine Israel
cluster1_politicalcompliance_il_list <- rbind(israelhayom, abcnews, bbc, cnn, dw, euronews, france24, lemonde, npr, theguardian, theintercept, vox, voa) %>%
  arrange(desc(Date))

# Save CSV
write.csv(cluster1_politicalcompliance_il_list, "cluster1_politicalcompliance_il_list.csv", row.names = FALSE)
View(cluster1_politicalcompliance_il_list)

# Cluster 1 Date t1 Initial Attacks until UN finds grounds for genocide by Israel
cluster1_politicalcompliance_il_t1_list <- cluster1_politicalcompliance_il_list %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2024-03-26"))

# Save CSV
write.csv(cluster1_politicalcompliance_il_t1_list, "cluster1_politicalcompliance_il_t1_list.csv", row.names = FALSE)

# Cluster 1 Date t2 UN finds grounds for genocide by Israel until ICJ calls for immediate end of Rafah offensive by Israel
cluster1_politicalcompliance_il_t2_list <- cluster1_politicalcompliance_il_list %>%
  filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-05-24"))

# Save CSV
write.csv(cluster1_politicalcompliance_il_t2_list, "cluster1_politicalcompliance_il_t2_list.csv", row.names = FALSE)

# Cluster 1 Date t3 ICJ calls for immediate end of Rafah offensive by Israel until Israel kills Hamas leader Yahya Sinwar
cluster1_politicalcompliance_il_t3_list <- cluster1_politicalcompliance_il_list %>%
  filter(Date >= as.Date("2024-05-25") & Date <= as.Date("2024-10-17"))

# Save CSV
write.csv(cluster1_politicalcompliance_il_t3_list, "cluster1_politicalcompliance_il_t3_list.csv", row.names = FALSE)

# Cluster 1 Date t4 Israel kills Hamas leader Yahya Sinwar until Israel breaks ceasefire
cluster1_politicalcompliance_il_t4_list <- cluster1_politicalcompliance_il_list %>%
  filter(Date >= as.Date("2024-10-18") & Date <= as.Date("2025-03-18"))

# Save CSV
write.csv(cluster1_politicalcompliance_il_t4_list, "cluster1_politicalcompliance_il_t4_list.csv", row.names = FALSE)

# Cluster 1 Date t5 Israel breaks ceasefire until Australia, Belgium, Canada, France and the UK to recognize a Palestinian state
cluster1_politicalcompliance_il_t5_list <- cluster1_politicalcompliance_il_list %>%
  filter(Date >= as.Date("2025-03-19") & Date <= as.Date("2025-08-11"))

# Save CSV
write.csv(cluster1_politicalcompliance_il_t5_list, "cluster1_politicalcompliance_il_t5_list.csv", row.names = FALSE)

# Combine Palestine
cluster1_politicalcompliance_ps_list <- rbind(aljazeera, arabnews, dailysabah, mehrnewsagency, wafa) %>%
  arrange(desc(Date))

# Save CSV
write.csv(cluster1_politicalcompliance_ps_list, "cluster1_politicalcompliance_ps_list.csv", row.names = FALSE)
View(cluster1_politicalcompliance_ps_list)

# Cluster 1 Date t1 Initial Attacks until UN finds grounds for genocide by Israel
cluster1_politicalcompliance_ps_t1_list <- cluster1_politicalcompliance_ps_list %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2024-03-26"))

# Save CSV
write.csv(cluster1_politicalcompliance_ps_t1_list, "cluster1_politicalcompliance_ps_t1_list.csv", row.names = FALSE)

# Cluster 1 Date t2 UN finds grounds for genocide by Israel until ICJ calls for immediate end of Rafah offensive by Israel
cluster1_politicalcompliance_ps_t2_list <- cluster1_politicalcompliance_ps_list %>%
  filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-05-24"))

# Save CSV
write.csv(cluster1_politicalcompliance_ps_t2_list, "cluster1_politicalcompliance_ps_t2_list.csv", row.names = FALSE)

# Cluster 1 Date t3 ICJ calls for immediate end of Rafah offensive by Israel until Israel kills Hamas leader Yahya Sinwar
cluster1_politicalcompliance_ps_t3_list <- cluster1_politicalcompliance_ps_list %>%
  filter(Date >= as.Date("2024-05-25") & Date <= as.Date("2024-10-17"))

# Save CSV
write.csv(cluster1_politicalcompliance_ps_t3_list, "cluster1_politicalcompliance_ps_t3_list.csv", row.names = FALSE)

# Cluster 1 Date t4 Israel kills Hamas leader Yahya Sinwar until Israel breaks ceasefire
cluster1_politicalcompliance_ps_t4_list <- cluster1_politicalcompliance_ps_list %>%
  filter(Date >= as.Date("2024-10-18") & Date <= as.Date("2025-03-18"))

# Save CSV
write.csv(cluster1_politicalcompliance_ps_t4_list, "cluster1_politicalcompliance_ps_t4_list.csv", row.names = FALSE)

# Cluster 1 Date t5 Israel breaks ceasefire until Australia, Belgium, Canada, France and the UK to recognize a Palestinian state
cluster1_politicalcompliance_ps_t5_list <- cluster1_politicalcompliance_ps_list %>%
  filter(Date >= as.Date("2025-03-19") & Date <= as.Date("2025-08-11"))

# Save CSV
write.csv(cluster1_politicalcompliance_ps_t5_list, "cluster1_politicalcompliance_ps_t5_list.csv", row.names = FALSE)

# Combine No strong compliance
cluster1_politicalcompliance_nc_list <- rbind(cgtn, scmp, thestraitstimes, timesofindia) %>%
  arrange(desc(Date))

# Save CSV
write.csv(cluster1_politicalcompliance_nc_list, "cluster1_politicalcompliance_nc_list.csv", row.names = FALSE)
View(cluster1_politicalcompliance_nc_list)

# Cluster 1 Date t1 Initial Attacks until UN finds grounds for genocide by Israel
cluster1_politicalcompliance_nc_t1_list <- cluster1_politicalcompliance_nc_list %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2024-03-26"))

# Save CSV
write.csv(cluster1_politicalcompliance_nc_t1_list, "cluster1_politicalcompliance_nc_t1_list.csv", row.names = FALSE)

# Cluster 1 Date t2 UN finds grounds for genocide by Israel until ICJ calls for immediate end of Rafah offensive by Israel
cluster1_politicalcompliance_nc_t2_list <- cluster1_politicalcompliance_nc_list %>%
  filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-05-24"))

# Save CSV
write.csv(cluster1_politicalcompliance_nc_t2_list, "cluster1_politicalcompliance_nc_t2_list.csv", row.names = FALSE)

# Cluster 1 Date t3 ICJ calls for immediate end of Rafah offensive by Israel until Israel kills Hamas leader Yahya Sinwar
cluster1_politicalcompliance_nc_t3_list <- cluster1_politicalcompliance_nc_list %>%
  filter(Date >= as.Date("2024-05-25") & Date <= as.Date("2024-10-17"))

# Save CSV
write.csv(cluster1_politicalcompliance_nc_t3_list, "cluster1_politicalcompliance_nc_t3_list.csv", row.names = FALSE)

# Cluster 1 Date t4 Israel kills Hamas leader Yahya Sinwar until Israel breaks ceasefire
cluster1_politicalcompliance_nc_t4_list <- cluster1_politicalcompliance_nc_list %>%
  filter(Date >= as.Date("2024-10-18") & Date <= as.Date("2025-03-18"))

# Save CSV
write.csv(cluster1_politicalcompliance_nc_t4_list, "cluster1_politicalcompliance_nc_t4_list.csv", row.names = FALSE)

# Cluster 1 Date t5 Israel breaks ceasefire until Australia, Belgium, Canada, France and the UK to recognize a Palestinian state
cluster1_politicalcompliance_nc_t5_list <- cluster1_politicalcompliance_nc_list %>%
  filter(Date >= as.Date("2025-03-19") & Date <= as.Date("2025-08-11"))

# Save CSV
write.csv(cluster1_politicalcompliance_nc_t5_list, "cluster1_politicalcompliance_nc_t5_list.csv", row.names = FALSE)

### Cluster 2 Political System
# Combine democratic
cluster2_politicalsystem_democratic_list <- rbind(israelhayom, abcnews, bbc, cnn, dw, euronews, france24, lemonde, npr, theguardian, theintercept, vox, voa) %>%
  arrange(desc(Date))

# Save CSV
write.csv(cluster2_politicalsystem_democratic_list, "cluster2_politicalsystem_democratic_list.csv", row.names = FALSE)
View(cluster2_politicalsystem_democratic_list)

# Cluster 2 Date t1 Initial Attacks until UN finds grounds for genocide by Israel
cluster2_politicalsystem_democratic_t1_list <- cluster2_politicalsystem_democratic_list %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2024-03-26"))

# Save CSV
write.csv(cluster2_politicalsystem_democratic_t1_list, "cluster2_politicalsystem_democratic_t1_list.csv", row.names = FALSE)

# Cluster 2 Date t2 UN finds grounds for genocide by Israel until ICJ calls for immediate end of Rafah offensive by Israel
cluster2_politicalsystem_democratic_t2_list <- cluster2_politicalsystem_democratic_list %>%
  filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-05-24"))

# Save CSV
write.csv(cluster2_politicalsystem_democratic_t2_list, "cluster2_politicalsystem_democratic_t2_list.csv", row.names = FALSE)

# Cluster 2 Date t3 ICJ calls for immediate end of Rafah offensive by Israel until Israel kills Hamas leader Yahya Sinwar
cluster2_politicalsystem_democratic_t3_list <- cluster2_politicalsystem_democratic_list %>%
  filter(Date >= as.Date("2024-05-25") & Date <= as.Date("2024-10-17"))

# Save CSV
write.csv(cluster2_politicalsystem_democratic_t3_list, "cluster2_politicalsystem_democratic_t3_list.csv", row.names = FALSE)

# Cluster 2 Date t4 Israel kills Hamas leader Yahya Sinwar until Israel breaks ceasefire
cluster2_politicalsystem_democratic_t4_list <- cluster2_politicalsystem_democratic_list %>%
  filter(Date >= as.Date("2024-10-18") & Date <= as.Date("2025-03-18"))

# Save CSV
write.csv(cluster2_politicalsystem_democratic_t4_list, "cluster2_politicalsystem_democratic_t4_list.csv", row.names = FALSE)

# Cluster 2 Date t5 Israel breaks ceasefire until Australia, Belgium, Canada, France and the UK to recognize a Palestinian state
cluster2_politicalsystem_democratic_t5_list <- cluster2_politicalsystem_democratic_list %>%
  filter(Date >= as.Date("2025-03-19") & Date <= as.Date("2025-08-11"))

# Save CSV
write.csv(cluster2_politicalsystem_democratic_t5_list, "cluster2_politicalsystem_democratic_t5_list.csv", row.names = FALSE)

# Combine Non-democratic
cluster2_politicalsystem_nondemocratic_list <- rbind(wafa, aljazeera, arabnews, cgtn, dailysabah, mehrnewsagency, scmp, thestraitstimes, timesofindia) %>%
  arrange(desc(Date))

# Save CSV
write.csv(cluster2_politicalsystem_nondemocratic_list, "cluster2_politicalsystem_nondemocratic_list.csv", row.names = FALSE)
View(cluster2_politicalsystem_nondemocratic_list)

# Cluster 2 Date t1 Initial Attacks until UN finds grounds for genocide by Israel
cluster2_politicalsystem_nondemocratic_t1_list <- cluster2_politicalsystem_nondemocratic_list %>%
  filter(Date >= as.Date("2023-10-07") & Date <= as.Date("2024-03-26"))

# Save CSV
write.csv(cluster2_politicalsystem_nondemocratic_t1_list, "cluster2_politicalsystem_nondemocratic_t1_list.csv", row.names = FALSE)

# Cluster 2 Date t2 UN finds grounds for genocide by Israel until ICJ calls for immediate end of Rafah offensive by Israel
cluster2_politicalsystem_nondemocratic_t2_list <- cluster2_politicalsystem_nondemocratic_list %>%
  filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-05-24"))

# Save CSV
write.csv(cluster2_politicalsystem_nondemocratic_t2_list, "cluster2_politicalsystem_nondemocratic_t2_list.csv", row.names = FALSE)

# Cluster 2 Date t3 ICJ calls for immediate end of Rafah offensive by Israel until Israel kills Hamas leader Yahya Sinwar
cluster2_politicalsystem_nondemocratic_t3_list <- cluster2_politicalsystem_nondemocratic_list %>%
  filter(Date >= as.Date("2024-05-25") & Date <= as.Date("2024-10-17"))

# Save CSV
write.csv(cluster2_politicalsystem_nondemocratic_t3_list, "cluster2_politicalsystem_nondemocratic_t3_list.csv", row.names = FALSE)

# Cluster 2 Date t4 Israel kills Hamas leader Yahya Sinwar until Israel breaks ceasefire
cluster2_politicalsystem_nondemocratic_t4_list <- cluster2_politicalsystem_nondemocratic_list %>%
  filter(Date >= as.Date("2024-10-18") & Date <= as.Date("2025-03-18"))

# Save CSV
write.csv(cluster2_politicalsystem_nondemocratic_t4_list, "cluster2_politicalsystem_nondemocratic_t4_list.csv", row.names = FALSE)

# Cluster 2 Date t5 Israel breaks ceasefire until Australia, Belgium, Canada, France and the UK to recognize a Palestinian state
cluster2_politicalsystem_nondemocratic_t5_list <- cluster2_politicalsystem_nondemocratic_list %>%
  filter(Date >= as.Date("2025-03-19") & Date <= as.Date("2025-08-11"))

# Save CSV
write.csv(cluster2_politicalsystem_nondemocratic_t5_list, "cluster2_politicalsystem_nondemocratic_t5_list.csv", row.names = FALSE)



##### LABEL 10.000 RANDOM NEWS ARTICLES MANUALLY FOR SML

# Pick 10.000 random news articles

library(dplyr)

# Sample 10.000 random rows
gaza_all_articles_tokenized_sample_tobelabeled <- read.csv("gaza_all_articles_tokenized.csv") %>%
  sample_n(10000)

# Save to CSV
write.csv(gaza_all_articles_tokenized_sample_tobelabeled, "gaza_all_articles_tokenized_sample_tobelabeled.csv", row.names = FALSE)



# --- Distribution of tokens by bin with information about percentiles ---
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(readr)
})

# Known total population size
N_total <- 47439

# Load the sample if it isn't already in the environment
if (!exists("gaza_all_articles_tokenized_tobelabeled")) {
  gaza_all_articles_tokenized_tobelabeled <-
    readr::read_csv("gaza_all_articles_tokenized_sample_tobelabeled.csv",
                    locale = readr::locale(encoding = "ISO-8859-1"),
                    show_col_types = FALSE)
}

# Clean and prepare
df <- gaza_all_articles_tokenized_tobelabeled %>%
  mutate(TokenCount = as.numeric(TokenCount)) %>%
  filter(!is.na(TokenCount))

n_sample <- nrow(df)

# ---- Bin table (25-token width), with % of sample ----
bin_width <- 25
token_per_25 <- df %>%
  mutate(bin = cut(
    TokenCount,
    breaks = seq(0, max(TokenCount, na.rm = TRUE) + bin_width, by = bin_width),
    right = TRUE,
    include.lowest = TRUE
  )) %>%
  count(bin, name = "ArticleCount") %>%
  mutate(SharePct_of_sample = 100 * ArticleCount / sum(ArticleCount))

# Show the first few rows to mirror your console output
print(head(token_per_25))

# ---- Percentiles (computed) and labels (matching your request) ----
probs <- c(0.25, 0.5, 0.75, 0.8, 0.9, 0.99)
ptiles <- quantile(df$TokenCount, probs = probs, na.rm = TRUE, names = FALSE)

# Helper to format percentile numbers: integers as whole numbers, otherwise 2 decimals
fmt_ptile <- function(x) {
  ifelse(abs(x - round(x)) < 0.005,
         format(round(x), big.mark = ","),
         format(round(x, 2), big.mark = ","))
}

ptile_labels <- paste0(c("25%","50%","75%","80%","90%","99%"),
                       "\n", fmt_ptile(ptiles))

# ---- Plot: y-axis = percent of articles in the sample ----
p <- ggplot(df, aes(x = TokenCount)) +
  geom_histogram(
    aes(y = after_stat(100 * count / sum(count))),  # percent of sample
    binwidth = bin_width,
    fill = "lightblue", color = "white", alpha = 0.8,
    boundary = 0, closed = "right"
  ) +
  # If you want a smoothed line, use the correctly scaled density (commented by default):
  # geom_density(aes(y = after_stat(100 * density * bin_width)),
  #              alpha = 0.25, fill = "lightblue")
  
  scale_x_continuous(
    breaks = c(0, 128, 256, 512, 1024, 2048),
    labels = comma_format(),
    limits = c(0, 2100),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),  # values already 0–100
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    title = "Distribution of Token Counts in Gaza News Articles (Sample)",
    subtitle = "Vertical dashed lines mark key percentiles",
    x = "Number of Tokens",
    y = "Percent of articles in sample (%)",
    caption = paste0("Sample n = ", format(n_sample, big.mark = ","),
                     "; Population N = ", format(N_total, big.mark = ","))
  ) +
  
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman", size = 15),
    plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
    plot.subtitle = element_text(face = "bold", color = "#909090", hjust = 0.5),
    axis.text = element_text(color = "black", size = 15),
    axis.title = element_text(face = "bold", size = 20)
  ) +
  
  geom_vline(xintercept = ptiles, color = "#de1312", linetype = "dashed", linewidth = 0.5) +
  annotate("text",
           x = ptiles,
           y = 0.6,  # place labels at ~0.6% so they sit above the baseline
           label = ptile_labels,
           angle = 90, hjust = 0, vjust = 0.5, size = 2.5, color = "#de1312",
           family = "Times New Roman")

print(p)

# ---- Optional: echo the exact percentile numbers to the console (as you provided) ----
print(quantile(df$TokenCount, probs = probs, na.rm = TRUE))


##### FINE-TUNE MODEL

#

##### ANALYZE FULL DATASET WITH FINE-TUNED BERT or BigBird MODEL

#
# Cleaned and fixed script for loading, parsing dates, de-duplicating, filtering and clustering
# Requirements: tidyverse, lubridate
install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)

# Set global date range
start_date <- as.Date("2023-10-07")
end_date   <- as.Date("2025-08-20")

# Helper function to clean a single news CSV
# file: path to csv
# url_col: name of the URL column (string)
# date_col: name of the date column (string)
# orders: vector of lubridate parse orders to try (e.g. c("mdy HMS p", "ymdHMS", "B d, Y, I:M p") )
# regex_extract: optional regex to first extract a date-like substring from a text column (set to NULL if not needed)
# tz: optional timezone for parsing (e.g. "UTC")
clean_news <- function(file, url_col = "URL", date_col = "Date", orders = c("ymd_HMS"), regex_extract = NULL, tz = NULL) {
  # Read (use read_csv for better parsing; fallback to read.csv if needed)
  df <- tryCatch(
    read_csv(file, show_col_types = FALSE),
    error = function(e) read.csv(file, stringsAsFactors = FALSE) %>% as_tibble()
  )
  
  # Basic checks - ensure cols exist
  if (!url_col %in% names(df)) stop(paste0("URL column '", url_col, "' not found in ", file))
  if (!date_col %in% names(df)) stop(paste0("Date column '", date_col, "' not found in ", file))
  
  # Optionally extract a date substring first (some scrapers put date inside text)
  if (!is.null(regex_extract)) {
    # create a temporary col 'date_text_extracted'
    df <- df %>%
      mutate(.date_text_extracted = str_extract(!!sym(date_col), regex_extract))
    parse_col <- ".date_text_extracted"
  } else {
    parse_col <- date_col
  }
  
  # Remove NA rows (for any essential columns) and deduplicate by URL
  df_clean <- df %>%
    filter(!is.na(!!sym(url_col))) %>%
    distinct(!!sym(url_col), .keep_all = TRUE)
  
  # Parse the date-time robustly using lubridate parse_date_time with supplied orders.
  # We try multiple orders; if tz provided, attach it.
  parsed_dt <- parse_date_time(df_clean[[parse_col]], orders = orders, tz = tz)
  
  # If parse_date_time returned all NA (rare), try additional common orders
  if (all(is.na(parsed_dt))) {
    fallback_orders <- c("bdY HMS p", "b d, Y H:M:S", "mdy HMS p", "YmdHMS", "Y-m-dTHMSz")
    parsed_dt <- parse_date_time(df_clean[[parse_col]], orders = c(orders, fallback_orders), tz = tz)
  }
  
  # Attach parsed datetime; convert to Date
  df_clean <- df_clean %>%
    mutate(
      DateTime = parsed_dt,
      Date = as.Date(DateTime)
    ) %>%
    # If Date still NA, try direct as.Date on simple ISO strings
    mutate(Date = if_else(is.na(Date) & !is.na(!!sym(date_col)),
                          as.Date(!!sym(date_col), format = "%Y-%m-%d"),
                          Date)) %>%
    # Filter between start and end
    filter(!is.na(Date)) %>%
    filter(Date >= start_date & Date <= end_date) %>%
    arrange(desc(Date))
  
  return(df_clean)
}

# EXAMPLE CALLS
# For each dataset below you can modify 'orders' to match your original scrapers' date format.
# Common orders used:
# - "%B %d, %Y, %I:%M %p" ~ lubridate order: "B d, Y, I:M p" -> use "B d, Y I:M p" or "mdy HM p"
# - ISO 8601 e.g. "2023-10-07T12:34:56Z" -> order "YmdHMS" or use ymd_hms()

# --- ABC News
abcnews <- clean_news(
  file = "abcnews_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  # e.g. "October 7, 2023, 5:00 PM" -> B d, Y I:M p (lubridate style: "B d, Y I:M p")
  orders = c("B d, Y I:M p", "mdy I:M p", "b d, Y"),
  tz = "UTC"
)
write_csv(abcnews, "abcnews_gaza_all_articles_clean.csv")

# --- Al Jazeera
aljazeera <- clean_news(
  file = "aljazeera_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("mdy", "B d, Y", "Y-m-d H:M:S"),
  tz = "UTC"
)
write_csv(aljazeera, "aljazeera_gaza_all_articles_clean.csv")

# --- Arab News
arabnews <- clean_news(
  file = "arabnews_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("mdy HMS p"),
  tz = "UTC"
)
write_csv(arabnews, "arabnews_gaza_all_articles_clean.csv")

# --- BBC (ISO)
bbc <- clean_news(
  file = "bbc_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("YmdHMS", "Y-m-dTHMSz"),
  tz = "UTC"
)
write_csv(bbc, "bbc_gaza_all_articles_clean.csv")

# --- CGTN
cgtn <- clean_news(
  file = "cgtn_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("mdy IMS p", "mdy I:M:S p"),
  tz = "UTC"
)
write_csv(cgtn, "cgtn_gaza_all_articles_clean.csv")

# --- CNN (some scrapers embed a human text; if the Date column has long text, pass regex_extract)
# Example regex: extract "Oct 7, 2023" style -> "\\w{3} \\d{1,2}, \\d{4}"
cnn <- clean_news(
  file = "cnn_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("b d, Y", "B d, Y", "mdy"),
  regex_extract = "\\w{3,9} \\d{1,2}, \\d{4}",
  tz = "UTC"
)
write_csv(cnn, "cnn_gaza_all_articles_clean.csv")

# --- Daily Sabah
dailysabah <- clean_news(
  file = "dailysabah_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("Y-m-dTHMS", "Y-m-d H:M:S"),
  tz = "UTC"
)
write_csv(dailysabah, "dailysabah_gaza_all_articles_clean.csv")

# --- DW
dw <- clean_news(
  file = "dw_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("mdy", "B d, Y"),
  tz = "UTC"
)
write_csv(dw, "dw_gaza_all_articles_clean.csv")

# --- Euronews
euronews <- clean_news(
  file = "euronews_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("mdy IMS p", "mdy I:M:S p", "Y-m-d H:M:S"),
  tz = "UTC"
)
write_csv(euronews, "euronews_gaza_all_articles_clean.csv")

# --- France24
france24 <- clean_news(
  file = "france24_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("Y-m-dTHMS", "Y-m-d H:M:S"),
  tz = "UTC"
)
write_csv(france24, "france24_gaza_all_articles_clean.csv")

# --- Israel Hayom
israelhayom <- clean_news(
  file = "israelhayom_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("mdy IMS p", "B d, Y I:M p"),
  tz = "UTC"
)
write_csv(israelhayom, "israelhayom_gaza_all_articles_clean.csv")

# --- Le Monde (if date sits inside text; use regex)
lemonde <- clean_news(
  file = "lemonde_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("B d, Y", "b d, Y"),
  regex_extract = "\\w+ \\d{1,2}, \\d{4}",
  tz = "UTC"
)
write_csv(lemonde, "lemonde_gaza_all_articles_clean.csv")

# --- Mehr News Agency
mehrnewsagency <- clean_news(
  file = "mehrnewsagency_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("B d, Y I:M p", "B d, Y"),
  tz = "UTC"
)
write_csv(mehrnewsagency, "mehrnewsagency_gaza_all_articles_clean.csv")

# --- NPR
npr <- clean_news(
  file = "npr_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("YmdHMS", "Y-m-dTHMSz"),
  tz = "UTC"
)
write_csv(npr, "npr_gaza_all_articles_clean.csv")

# --- SCMP
scmp <- clean_news(
  file = "scmp_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("YmdHMS", "Y-m-dTHMSz"),
  tz = "UTC"
)
write_csv(scmp, "scmp_gaza_all_articles_clean.csv")

# --- The Guardian
theguardian <- clean_news(
  file = "theguardian_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("mdy IMS p", "mdy I:M:S p"),
  tz = "UTC"
)
write_csv(theguardian, "theguardian_gaza_all_articles_clean.csv")

# --- The Intercept
theintercept <- clean_news(
  file = "theintercept_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("Y-m-dTHMS", "Y-m-d H:M:S"),
  tz = "UTC"
)
write_csv(theintercept, "theintercept_gaza_all_articles_clean.csv")

# --- The Straits Times
thestraitstimes <- clean_news(
  file = "thestraitstimes_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("b d, Y, H:M p", "b d, Y H:M p", "mdy H:M"),
  tz = "UTC"
)
write_csv(thestraitstimes, "thestraitstimes_gaza_all_articles_clean.csv")

# --- Times of India
timesofindia <- clean_news(
  file = "timesofindia_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("mdy IMS p", "mdy I:M:S p"),
  tz = "UTC"
)
write_csv(timesofindia, "timesofindia_gaza_all_articles_clean.csv")

# --- VOA
voa <- clean_news(
  file = "voa_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("YmdHMS", "Y-m-dTHMSz"),
  tz = "UTC"
)
write_csv(voa, "voa_gaza_all_articles_clean.csv")

# --- Vox
vox <- clean_news(
  file = "vox_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("YmdHMS", "Y-m-dTHMSz"),
  tz = "UTC"
)
write_csv(vox, "vox_gaza_all_articles_clean.csv")

# --- WAFA
wafa <- clean_news(
  file = "wafa_gaza_all_articles.csv",
  url_col = "URL",
  date_col = "Date",
  orders = c("mdy IMS p", "mdy I:M:S p"),
  tz = "UTC"
)
write_csv(wafa, "wafa_gaza_all_articles_clean.csv")


# -------------------------
# CLUSTER COMBINATIONS
# -------------------------
# Note: the original script used many rbind(...) chains.
# We'll create clusters using bind_rows() (safer with tibbles) and then arrange by Date.

# Cluster 1 Scale
cluster1_scale_local  <- bind_rows(israelhayom, wafa) %>% arrange(desc(Date))
write_csv(cluster1_scale_local, "cluster1_scale_local.csv")

cluster1_scale_global <- bind_rows(abcnews, aljazeera, arabnews, bbc, cgtn, cnn, dailysabah,
                                   dw, euronews, france24, lemonde, mehrnewsagency, npr,
                                   scmp, theguardian, theintercept, thestraitstimes, timesofindia,
                                   voa, vox) %>%
  arrange(desc(Date))
write_csv(cluster1_scale_global, "cluster1_scale_global.csv")

# Cluster 2 Political System
cluster2_politicalsystem_democratic <- bind_rows(israelhayom, abcnews, bbc, cnn, dw, euronews,
                                                france24, lemonde, npr, theguardian, theintercept, vox, voa) %>%
  arrange(desc(Date))
write_csv(cluster2_politicalsystem_democratic, "cluster2_politicalsystem_democratic.csv")

cluster2_politicalsystem_nondemocratic <- bind_rows(wafa, aljazeera, arabnews, cgtn, dailysabah,
                                                   mehrnewsagency, scmp, thestraitstimes, timesofindia) %>%
  arrange(desc(Date))
write_csv(cluster2_politicalsystem_nondemocratic, "cluster2_politicalsystem_nondemocratic.csv")

# Cluster 3 Region
cluster3_region_usa <- bind_rows(abcnews, cnn, npr, theintercept, vox, voa) %>% arrange(desc(Date))
write_csv(cluster3_region_usa, "cluster3_region_usa.csv")

cluster3_region_weu <- bind_rows(bbc, dw, euronews, france24, lemonde, theguardian) %>% arrange(desc(Date))
write_csv(cluster3_region_weu, "cluster3_region_weu.csv")

# Middle East & Africa (MEA)
cluster3_region_mea <- bind_rows(israelhayom, wafa, aljazeera, arabnews, dailysabah, mehrnewsagency) %>%
  arrange(desc(Date))
write_csv(cluster3_region_mea, "cluster3_region_mea.csv")

cluster3_region_sea <- bind_rows(cgtn, scmp, thestraitstimes, timesofindia) %>% arrange(desc(Date))
write_csv(cluster3_region_sea, "cluster3_region_sea.csv")

# Cluster 4 Religion
cluster4_religion_chr <- bind_rows(abcnews, bbc, cnn, dw, euronews, france24, lemonde,
                                   npr, theguardian, theintercept, vox, voa) %>%
  arrange(desc(Date))
write_csv(cluster4_religion_chr, "cluster4_religion_chr.csv")

cluster4_religion_hin <- timesofindia %>% arrange(desc(Date))
write_csv(cluster4_religion_hin, "cluster4_religion_hin.csv")

cluster4_religion_isl <- bind_rows(wafa, aljazeera, arabnews, dailysabah, mehrnewsagency) %>%
  arrange(desc(Date))
write_csv(cluster4_religion_isl, "cluster4_religion_isl.csv")

cluster4_religion_jud <- israelhayom %>% arrange(desc(Date))
write_csv(cluster4_religion_jud, "cluster4_religion_jud.csv")

cluster4_religion_nre <- bind_rows(cgtn, scmp, thestraitstimes) %>% arrange(desc(Date))
write_csv(cluster4_religion_nre, "cluster4_religion_nre.csv")

# Cluster 5 Political Compliance
cluster5_politicalcompliance_il <- bind_rows(israelhayom, abcnews, bbc, cnn, dw, euronews,
                                             france24, lemonde, npr, theguardian, theintercept, vox, voa) %>%
  arrange(desc(Date))
write_csv(cluster5_politicalcompliance_il, "cluster5_politicalcompliance_il.csv")

cluster5_politicalcompliance_ps <- bind_rows(wafa, aljazeera, arabnews, dailysabah, mehrnewsagency) %>%
  arrange(desc(Date))
write_csv(cluster5_politicalcompliance_ps, "cluster5_politicalcompliance_ps.csv")

cluster5_politicalcompliance_nc <- bind_rows(cgtn, scmp, thestraitstimes, timesofindia) %>%
  arrange(desc(Date))
write_csv(cluster5_politicalcompliance_nc, "cluster5_politicalcompliance_nc.csv")


# End of cleaning + clustering script
message("All cleaning and clustering finished. Clean files and cluster files written to working directory.")

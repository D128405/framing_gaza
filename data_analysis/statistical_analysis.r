# statistical_analysis_visualization.r

### PURPOSE & ROADMAP

# DATA
# - two sets of CSVs, each split into five time slices t1..t5 and one containing all t1-t5:
#   - C1 (political compliance): cluster1_politicalcompliance_<il|ps|nc>_t{1..5}_tokenized.csv
#   - C2 (political system):    cluster2_politicalsystem_<democratic|nondemocratic>_t{1..5}_tokenized.csv
# - Each article can contain multiple frames in column LabelFrames_pred (pipe-separated).
# - reshaped to “long” format, one row per (article × frame-present) event.
#
# KEY VARIABLES
# - Period: factor {t1..t5}.
# - Compliance: {il, ps, nc}.
# - System: {democratic, nondemocratic}.
# - Frame: 15 canonical labels
#
# PROPORTION — DETAILED DEFINITION
# - Unit is an article with its headline. A frame is counted as present if it appears >=1 time in the article.
# - For any slice S (e.g., a specific Time T3, or Compliance×Time = ps×T2), and any frame F:
#     numerator   = # of distinct articles in S where F appears at least once
#     denominator = total # of distinct articles in S
#     proportion  = numerator / denominator
# - Notes:
#   - Multi-label is allowed: an article can contribute to the numerator for multiple frames,
#     but each (article, frame) is counted at most once per slice.
#   - The denominator never double-counts: it is the number of articles in the slice.
#   - This answers “How prevalent is a frame in that slice’s coverage?”
#
# OVER-/UNDER-REPRESENTED CELLS — WHAT THEY MEAN
# - After a global LRT (G-test) of independence on a contingency table, standardized residuals per cell are computed.
#   For any cell c with observed count O_c and expected count E_c (under independence):
#     StdResid_c = (O_c - E_c) / SD_c
#   where SD_c is the appropriate χ²-based standard deviation used by chisq.test()-style residuals.
# - Compute two-sided p-values for these residuals and Holm-adjust over all cells (to verify p values).
# - Interpretation:
#   - Over-represented  = O_c > E_c  and adjusted p < α (frame appears MORE than expected).
#   - Under-represented = O_c < E_c  and adjusted p < α (frame appears LESS than expected).
#   - Neutral           = not significant after Holm correction.
# - This localizes where the association is situated (which specific Frame×Group×Time cells deviate
#   from independence) and complements the single global LRT p-value.
#
# STATISTICAL DESIGN & INTERPRETATION
#
# RQ1: “What are dominant frames over time?”
# - Pool ALL articles across C1 + C2 (analyze the global time pattern).
# - Build a Frame × Time table of counts and run LRT (G-test) of independence.
#   - Null: frame usage is independent of time.
# - Post-hoc:
#   - Cellwise standardized residuals with Holm correction -> shows WHERE it’s
#     over/under-represented (Over/Under/Neutral + significance).
#   - Pairwise proportion tests per frame across time with Holm correction.
# - Visuals:
#   - Time series for ALL frames (small multiples) — shows trajectories.
#   - Matrix plot (Frame × Time): point size = proportion; color = residual
#     direction; shape = Holm significance.
#
# RQ2: “How do frames vary by political compliance and over time?”
# - Construct a 3-way table of counts: Frame × Time × Compliance.
# - Fit Poisson log-linear models:
#   M2: main effects + all two-way interactions
#   M3: M2 + three-way interaction (Compliance:Time:Frame)
# - Likelihood-ratio test M2 vs M3 (Likelihood Ratio Test):
#   - Null: no 3-way interaction (compliance differences don’t vary over time).
#   - Alt: 3-way interaction present (compliance gaps are time-dependent).
# - Post-hoc:
#   - For each time (T1..T5): LRT (G-test) on Frame × Compliance, with cellwise residuals.
#   - Also, per time & frame: pairwise proportion tests (il vs ps vs nc; Holm).
# - Visuals:
#   - Heatmap of proportions by Frame × Time, facetted by Compliance, with a text
#     legend panel describing T1..T5 (the historical date ranges) at the right.
#   - 3D matrix: x = Compliance (pretty labels), y = Frames (pretty labels),
#     z = Time (long descriptions). Size = proportion; Color = residual category.
# - Outputs:
#   - rq2_lrt_test.csv                  -> LRT (G-test) on Frame × Compliance (time-pooled).
#   - rq2_lrt_tests_by_time.csv         -> LRT (G-test) on Frame × Compliance within each T.
#   - rq2_loglinear_lrt_m2_vs_m3.csv    -> LRT table (as before).
#   - rq2_loglinear_lrt_m2_vs_m3.png    -> visual summary + interpretation line.
#   - rq2_pairwise_prop_tests_by_time.csv now uses human-readable group labels.
#
# RQ3: “How do frames vary across political systems and over time?”
# - Identical structure to RQ2 but replace Compliance with System.
# - Visuals:
#   - Heatmap with a right-side legend listing countries by System.
#   - 3D matrix: x = System (Democratic, Non-democratic), y = Frames, z = Time
#     (long descriptions). Size and color meanings as in RQ2.
# - Outputs:
#   - rq3_lrt_test.csv                  -> LRT (G-test) on Frame × System (time-pooled).
#   - rq3_lrt_tests_by_time.csv         -> LRT (G-test) on Frame × System within each T.
#   - rq3_loglinear_lrt_m2_vs_m3.png    -> visual summary + interpretation line.
#   - rq3_pairwise_prop_tests_by_time.csv uses “Democratic/Non-democratic” labels.
#
# WHY THESE CHOICES?
# - Presence/absence per article treats “did the frame appear?” as the signal (common
#   in framing research) and avoids double-counting within articles.
# - Proportions normalize for different article volumes across groups/times.
# - LRT (G-test) + cellwise post-hoc localizes changes in a simple, interpretable way.
# - Log-linear models are the standard for multiway tables; the 3-way LRT directly
#   answers whether group differences themselves change over time.
# - Holm controls familywise error; global test + post-hoc reduces false positives.
#

# User parameters

base_dir   <- "/Users/davidluu/Model Training & Performance Testing/results"
output_dir <- "outputs"
LABEL_COL  <- "LabelFrames_pred"
alpha_level  <- 0.05

# Package management

install_if_missing <- function(pkgs) {
  to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

install_if_missing(c(
  "tidyverse","stringr","ggrepel","ggwordcloud","stopwords",
  "scales","broom","ggdendro","viridisLite","scatterplot3d",
  "gridExtra","ggtext","glue"
))

suppressPackageStartupMessages({
  library(tidyverse); library(stringr); library(ggrepel)
  library(ggwordcloud); library(stopwords)
  library(scales); library(broom); library(ggdendro)
  library(viridisLite); library(scatterplot3d)
  library(grid); library(gridExtra); library(ggtext)
})

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Constants, labels, helpers

# Frames
FRAME_LEVELS <- c(
  "ILterrr","ILmltry","ILatckd","ILsprtd",
  "PSsuffr","PSilatc","PSiltrt","PSintrc","PSconil","PSilgil",
  "NChumcr","NCwarrr","NCpeace","NCthmtc","NCepisd"
)

# Pretty labels for frames (used on axes and legends)
FRAME_LABELS <- c(
  ILterrr = "Israel Combatting Hamas",
  ILmltry = "Israel Focusing on Military Targets",
  ILatckd = "Israel Under Attack",
  ILsprtd = "Israel Receiving Support",
  PSsuffr = "Palestinian Suffering",
  PSilatc = "Israel Attacking Gaza",
  PSiltrt = "Israel as Threat",
  PSintrc = "International Recognition of a Palestinian State",
  PSconil = "Global Entities Condemning Israeli Actions",
  PSilgil = "Support or Action of Israel Illegal",
  NChumcr = "Humanitarian Crisis",
  NCwarrr = "War",
  NCpeace = "Peace",
  NCthmtc = "Thematic",
  NCepisd = "Episodic"
)

# Time slices
TIME_LEVELS <- paste0("t", 1:5)
TIME_SHORT  <- c(t1="T1", t2="T2", t3="T3", t4="T4", t5="T5")
# Long descriptions used in legends and 3D z-axis tick labels
TIME_DESC <- c(
  "t1" = "Oct 7, 2023 (Hamas’ attack) → Mar 26, 2024 (UN: reasonable grounds of genocide)",
  "t2" = "Mar 26, 2024 (UN) → May 24, 2024 (ICJ: stop Rafah offensive)",
  "t3" = "May 24, 2024 (ICJ) → Oct 17, 2024 (Israel kills Yahya Sinwar)",
  "t4" = "Oct 17, 2024 (Sinwar's death) → Mar 18, 2025 (Israel breaks ceasefire)",
  "t5" = "Mar 18, 2025 (Ceasefire broken) → Aug 11, 2025 (Recognitions of Palestinian state)"
)

# Compliance & System
COMPLIANCE_LEVELS <- c("il","ps","nc")
COMPLIANCE_LABELS <- c(
  il = "Compliance with Israel",
  ps = "Compliance with Palestine",
  nc = "No Strong/Weak Compliance"
)

SYSTEM_LEVELS <- c("democratic","nondemocratic")
SYSTEM_LABELS <- c(
  democratic    = "Democratic",
  nondemocratic = "Non-democratic"
)

# Helpers

split_frames <- function(df, label_col = LABEL_COL) {
  col_to_use <- label_col
  if (!label_col %in% names(df)) {
    if ("LabelFrames" %in% names(df)) {
      warning("'", label_col, "' not found; falling back to legacy 'LabelFrames'.")
      col_to_use <- "LabelFrames"
    } else stop("Neither '", label_col, "' nor 'LabelFrames' found.")
  }
  df %>%
    mutate(`__labels__` = as.character(.data[[col_to_use]])) %>%
    mutate(`__labels__` = str_replace_all(`__labels__`, "\\s+", "")) %>%
    separate_rows(`__labels__`, sep = "\\|") %>%
    filter(!is.na(`__labels__`), `__labels__` != "") %>%
    mutate(Frame = factor(`__labels__`, levels = FRAME_LEVELS)) %>%
    filter(!is.na(Frame)) %>%
    select(-`__labels__`)
}

# Expand over full level sets so factors always have complete levels for models
compute_counts_and_props <- function(df_long, group_col) {
  group_sym <- rlang::sym(group_col)
  full_levels <- switch(
    group_col,
    "Compliance" = COMPLIANCE_LEVELS,
    "System"     = SYSTEM_LEVELS,
    "All"        = "All",
    unique(as.character(df_long[[group_col]]))
  )
  pres <- df_long %>% distinct(!!group_sym, Period, article_id, Frame)
  totals <- df_long %>%
    distinct(!!group_sym, Period, article_id) %>%
    count(!!group_sym, Period, name = "N_articles")
  counts <- pres %>% count(!!group_sym, Period, Frame, name = "N_with_frame")
  counts %>%
    right_join(
      expand_grid(
        !!group_sym := factor(full_levels, levels = full_levels),
        Period = factor(TIME_LEVELS, levels = TIME_LEVELS),
        Frame  = factor(FRAME_LEVELS, levels = FRAME_LEVELS)
      ),
      by = c(as.character(group_sym), "Period", "Frame")
    ) %>%
    left_join(totals, by = c(as.character(group_sym), "Period")) %>%
    mutate(
      N_with_frame = replace_na(N_with_frame, 0L),
      N_articles   = replace_na(N_articles,   0L),
      prop = ifelse(N_articles > 0, N_with_frame / N_articles, NA_real_)
    )
}

# Likelihood-ratio (G-test) with optional drop of all-zero rows/cols; maps reduced results back
lrt_with_cellwise <- function(tbl, alpha = 0.05, drop_zeros = TRUE) {
  if (is.null(rownames(tbl))) rownames(tbl) <- paste0("R", seq_len(nrow(tbl)))
  if (is.null(colnames(tbl))) colnames(tbl) <- paste0("C", seq_len(ncol(tbl)))
  t_use <- tbl
  if (drop_zeros) {
    r_keep <- rowSums(t_use, na.rm = TRUE) > 0
    c_keep <- colSums(t_use, na.rm = TRUE) > 0
    t_use  <- t_use[r_keep, c_keep, drop = FALSE]
  }
  cell_df_full <- as.data.frame(as.table(tbl), stringsAsFactors = FALSE)
  names(cell_df_full) <- c("Row","Col","Observed")
  cell_df_full$Expected    <- NA_real_
  cell_df_full$StdResid    <- NA_real_
  cell_df_full$P           <- NA_real_
  cell_df_full$P_adjusted  <- NA_real_
  cell_df_full$Significant <- FALSE
  cell_df_full$Direction   <- "Neutral"
  if (nrow(t_use) >= 2 && ncol(t_use) >= 2) {
    # Likelihood-ratio test (G-test) of independence
    total <- sum(t_use)
    rs <- rowSums(t_use)
    cs <- colSums(t_use)
    expected <- outer(rs, cs, FUN = "*") / total
    # G^2 statistic (handle O=0 gracefully)
    ovec <- as.vector(t_use)
    evec <- as.vector(expected)
    contrib <- ifelse(ovec > 0 & evec > 0, 2 * ovec * log(ovec / evec), 0)
    G2 <- sum(contrib)
    df <- (nrow(t_use) - 1L) * (ncol(t_use) - 1L)
    pval <- stats::pchisq(G2, df = df, lower.tail = FALSE)
    # Compose an htest-like object so broom::tidy() works
    ht <- list(
      statistic = c("G^2" = G2),
      parameter = c(df = df),
      p.value = pval,
      method = "Likelihood-ratio test (G-test) of independence",
      data.name = "Contingency table",
      observed = t_use,
      expected = expected
    )
    # Standardized (adjusted) residuals like chisq.test() provides
    rprop <- rs / total
    cprop <- cs / total
    denom <- sqrt(expected * (1 - rprop) %o% (1 - cprop))
    stdres <- (t_use - expected) / denom
    ht$stdres <- stdres
    class(ht) <- "htest"
    # Per-cell p-values from stdres (two-sided, normal approx)
    pvals <- 2 * (1 - stats::pnorm(abs(stdres)))
    padj  <- p.adjust(pvals, method = "holm")
    overunder <- ifelse(stdres > 0, "Over-represented",
                        ifelse(stdres < 0, "Under-represented", "Neutral"))
    cell_df_reduced <- as.data.frame(as.table(t_use), stringsAsFactors = FALSE)
    names(cell_df_reduced) <- c("Row","Col","Observed")
    cell_df_reduced$Expected    <- as.vector(expected)
    cell_df_reduced$StdResid    <- as.vector(stdres)
    cell_df_reduced$P           <- as.vector(pvals)
    cell_df_reduced$P_adjusted  <- as.vector(padj)
    cell_df_reduced$Significant <- as.vector(padj < alpha)
    cell_df_reduced$Direction   <- as.vector(overunder)
    cell_df_full <- dplyr::left_join(
      cell_df_full,
      cell_df_reduced %>% dplyr::select(Row, Col, Expected, StdResid, P, P_adjusted, Significant, Direction),
      by = c("Row","Col")
    ) %>%
      dplyr::mutate(
        Expected    = dplyr::coalesce(Expected.y, Expected.x),
        StdResid    = dplyr::coalesce(StdResid.y, StdResid.x),
        P           = dplyr::coalesce(P.y, P.x),
        P_adjusted  = dplyr::coalesce(P_adjusted.y, P_adjusted.x),
        Significant = dplyr::coalesce(Significant.y, Significant.x),
        Direction   = dplyr::coalesce(Direction.y, Direction.x)
      ) %>%
      dplyr::select(Row, Col, Observed, Expected, StdResid, P, P_adjusted, Significant, Direction)
    return(list(lrt = ht, cell_df = tibble::as_tibble(cell_df_full)))
  } else {
    warning("LRT skipped: table has <2 rows or <2 columns after dropping zeros.")
    return(list(lrt = NULL, cell_df = tibble::as_tibble(cell_df_full)))
  }
}

pairwise_prop_tests <- function(counts, ns, groups, method = "holm") {
  ok <- !is.na(counts) & !is.na(ns) & ns > 0
  if (sum(ok) < 2) return(tibble())
  # Name the count vector so the output uses readable labels (not 1/2/3)
  x <- counts[ok]; names(x) <- as.character(groups[ok])
  n <- ns[ok];     names(n) <- as.character(groups[ok])
  suppressWarnings({
    pw <- pairwise.prop.test(x = x, n = n, p.adjust.method = method)
  })
  if (is.null(pw$p.value)) return(tibble())
  mat <- pw$p.value
  df <- as.data.frame(as.table(mat))
  names(df) <- c("Group1","Group2","p_adjusted")
  df %>% filter(!is.na(p_adjusted)) %>%
    mutate(Frame = NA_character_) %>% select(Frame, everything())
}

write_csv_safely <- function(df, path) {
  tryCatch(readr::write_csv(df, path), error = function(e) {
    message("Failed to write CSV to: ", path, " — ", e$message)
  })
}

pal <- viridisLite::viridis
# Green → Yellow continuous palette for heatmaps (fine-grained)
pal_green_yellow <- function(n) grDevices::colorRampPalette(c("#45fa2d", "#bcfa2d", "#faf02d"))(n)

safe_rescale <- function(x, to = c(0.8, 2.8)) {
  if (length(x) == 0) return(numeric(0))
  if (all(is.na(x))) return(rep(mean(to), length(x)))
  scales::rescale(x, to = to, from = c(0, max(x, na.rm = TRUE)))
}

# Data ingestion

files_c1 <- list.files(base_dir, pattern = "^cluster1_politicalcompliance_.*_t[1-5]_tokenized\\.csv$", full.names = TRUE)
files_c2 <- list.files(base_dir, pattern = "^cluster2_politicalsystem_.*_t[1-5]_tokenized\\.csv$", full.names = TRUE)
if (length(files_c1) == 0 && length(files_c2) == 0) {
  stop("No input files found. Check `base_dir` and expected filename patterns.")
}

read_tagged <- function(fp) {
  fn <- basename(fp)
  if (str_detect(fn, "^cluster1_politicalcompliance_")) {
    compliance <- str_match(fn, "politicalcompliance_([a-z]+)_t([1-5])")[,2]
    period     <- paste0("t", str_match(fn, "politicalcompliance_[a-z]+_t([1-5])")[,2])
    data <- suppressWarnings(readr::read_csv(fp, show_col_types = FALSE))
    data %>% mutate(
      SourceFile = fn,
      ArticleFileRow = row_number(),
      article_id = paste0(fn, "#", ArticleFileRow),
      Compliance = factor(compliance, levels = COMPLIANCE_LEVELS),
      Period = factor(period, levels = TIME_LEVELS),
      Cluster = "C1"
    )
  } else if (str_detect(fn, "^cluster2_politicalsystem_")) {
    sys <- str_match(fn, "politicalsystem_([a-z]+)_t([1-5])")[,2]
    period <- paste0("t", str_match(fn, "politicalsystem_[a-z]+_t([1-5])")[,2])
    data <- suppressWarnings(readr::read_csv(fp, show_col_types = FALSE))
    data %>% mutate(
      SourceFile = fn,
      ArticleFileRow = row_number(),
      article_id = paste0(fn, "#", ArticleFileRow),
      System = factor(sys, levels = SYSTEM_LEVELS),
      Period = factor(period, levels = TIME_LEVELS),
      Cluster = "C2"
    )
  } else stop("Unrecognized file type: ", fn)
}

df_c1_raw <- if (length(files_c1) > 0) purrr::map_dfr(files_c1, read_tagged) else tibble()
df_c2_raw <- if (length(files_c2) > 0) purrr::map_dfr(files_c2, read_tagged) else tibble()

df_c1_long <- if (nrow(df_c1_raw) > 0) split_frames(df_c1_raw, LABEL_COL) else tibble()
df_c2_long <- if (nrow(df_c2_raw) > 0) split_frames(df_c2_raw, LABEL_COL) else tibble()

# For RQ1 pool ALL articles across C1 + C2
df_all_raw  <- bind_rows(df_c1_raw %>% mutate(Source="C1"),
                         df_c2_raw %>% mutate(Source="C2"))
df_all_long <- bind_rows(df_c1_long %>% mutate(Source="C1"),
                         df_c2_long %>% mutate(Source="C2"))

### RQ1 ANALYSIS

if (nrow(df_all_long) > 0) {
  message("RQ1: Computing counts and proportions by Frame × Time (ALL articles, C1 + C2 pooled).")
  
  df_rq1 <- df_all_long %>% mutate(All = "All") %>% compute_counts_and_props("All")
  
  # Contingency table: Frame × Time (article counts with frame)
  tbl_rq1 <- df_rq1 %>%
    select(Period, Frame, N_with_frame) %>%
    pivot_wider(names_from = Period, values_from = N_with_frame, values_fill = 0) %>%
    column_to_rownames("Frame") %>% as.matrix()
  
  rq1_cs <- lrt_with_cellwise(tbl_rq1, alpha = alpha_level, drop_zeros = TRUE)
  
  write_csv_safely(
    broom::tidy(rq1_cs$lrt) %>% mutate(RQ = "RQ1", Test = "Likelihood-ratio (G-test) Frame × Time (All)"),
    file.path(output_dir, "rq1_lrt_test.csv")
  )
  write_csv_safely(rq1_cs$cell_df %>% mutate(RQ = "RQ1"),
                   file.path(output_dir, "rq1_cellwise_posthoc.csv"))
  
  # Time series for all frames (small multiples)
  p_ts <- df_rq1 %>%
    mutate(
      Frame  = factor(Frame,  levels = FRAME_LEVELS, labels = FRAME_LABELS[FRAME_LEVELS]),
      Period = factor(Period, levels = TIME_LEVELS, labels = TIME_SHORT[TIME_LEVELS])
    ) %>%
    ggplot(aes(x = Period, y = prop, group = 1)) +
    geom_line() + geom_point() +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
    labs(title = "RQ1: Evolution of Frames Over Time (All articles, C1 + C2 pooled)",
         subtitle = "Each panel is one frame. Proportion = (# articles using the frame at least once) / (all articles in that time slice).",
         x = "Time (T1–T5)", y = "Proportion of Articles") +
    theme_minimal(base_size = 11) +
    facet_wrap(~ Frame, ncol = 3, scales = "free_y")
  ggsave(file.path(output_dir, "rq1_time_series_all_frames.png"), p_ts, width = 12, height = 16, dpi = 300)
  
  # Matrix plot with clear legend (color = residual category; shape = significance)
  cell_annot <- rq1_cs$cell_df %>%
    rename(Frame = Row, Period = Col) %>%
    mutate(Period = factor(Period, levels = TIME_LEVELS),
           Frame  = factor(Frame,  levels = FRAME_LEVELS))
  
  p_matrix <- df_rq1 %>%
    left_join(cell_annot %>% select(Frame, Period, Significant, Direction), by = c("Frame","Period")) %>%
    mutate(
      FrameLab  = factor(FRAME_LABELS[as.character(Frame)], levels = FRAME_LABELS[FRAME_LEVELS]),
      PeriodLab = factor(TIME_SHORT[as.character(Period)], levels = TIME_SHORT[TIME_LEVELS]),
      Direction = factor(Direction, levels = c("Over-represented","Under-represented","Neutral"))
    ) %>%
    ggplot(aes(x = PeriodLab, y = FrameLab)) +
    geom_point(aes(size = prop, shape = Significant, color = Direction), alpha = 0.85) +
    scale_size_continuous(name = "Proportion", range = c(1.5, 8), labels = percent) +
    scale_shape_manual(name = "Holm significant?", values = c(`TRUE`=16, `FALSE`=1),
                       labels = c(`TRUE`="Yes (α=0.05)", `FALSE`="No")) +
    scale_color_manual(name = "Residual category",
                       values = c("Over-represented"="#338cc7","Under-represented"="#de1312","Neutral"="#909090")) +
    labs(title = "RQ1: Frame × Time Matrix (All articles, C1 + C2 pooled)",
         subtitle = "Point size = proportion of articles; color = cellwise residual direction (G-test); shape = Holm-adjusted significance.",
         x = "Time (T1–T5)", y = "Frames") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "right")
  ggsave(file.path(output_dir, "rq1_matrix_points.png"), p_matrix, width = 14, height = 10, dpi = 300)
  
  # Word clouds per time slice + all-time (stopwords removed)
  token_col <- names(df_all_raw)[str_detect(names(df_all_raw), regex("token|text|words", ignore_case = TRUE))][1]
  STOP <- unique(tolower(stopwords::stopwords("en")))
  
  if (!is.na(token_col)) {
    message("RQ1 Wordclouds: using detected token column: ", token_col)
    tokens_long <- df_all_raw %>%
      select(Period, !!sym(token_col)) %>%
      mutate(Period = factor(Period, levels = TIME_LEVELS)) %>%
      mutate(txt = as.character(!!sym(token_col))) %>%
      filter(!is.na(txt)) %>%
      separate_rows(txt, sep = "\\s+") %>%
      mutate(txt = tolower(txt)) %>%
      filter(str_detect(txt, "^[a-z]+$")) %>%          # keep alphabetic tokens
      filter(!txt %in% STOP, nchar(txt) >= 3) %>%      # remove stopwords/short tokens
      count(Period, txt, name = "n")
    
    # Per-time clouds
    for (t in TIME_LEVELS) {
      dat <- tokens_long %>% filter(Period == t) %>% arrange(desc(n)) %>% slice_head(n = 200)
      if (nrow(dat) >= 10) {
        p_wc <- ggplot(dat, aes(label = txt, size = n)) +
          geom_text_wordcloud() + scale_size_area(max_size = 18) +
          theme_minimal() +
          labs(title = paste0("Word Cloud — ", TIME_SHORT[[t]], " (", TIME_DESC[[t]], ")"))
        ggsave(file.path(output_dir, paste0("rq1_wordcloud_tokens_", t, ".png")),
               p_wc, width = 9, height = 7, dpi = 300)
      }
    }
    # All-time cloud
    dat_all <- tokens_long %>% group_by(txt) %>% summarize(n = sum(n), .groups="drop") %>%
      arrange(desc(n)) %>% slice_head(n = 250)
    if (nrow(dat_all) >= 10) {
      p_wc_all <- ggplot(dat_all, aes(label = txt, size = n)) +
        geom_text_wordcloud() + scale_size_area(max_size = 20) +
        theme_minimal() +
        labs(title = "Word Cloud — All Time (All articles, C1 + C2 pooled)")
      ggsave(file.path(output_dir, "rq1_wordcloud_tokens_all_time.png"),
             p_wc_all, width = 9, height = 7, dpi = 300)
    }
  } else {
    message("Token column not found. Falling back to frame-frequency clouds (stopwords not applicable).")
    frames_counts <- df_all_long %>%
      distinct(Period, article_id, Frame) %>%
      count(Period, Frame, name = "n")
    
    for (t in TIME_LEVELS) {
      dat <- frames_counts %>% filter(Period == t) %>% mutate(lbl = FRAME_LABELS[as.character(Frame)]) %>%
        arrange(desc(n))
      if (nrow(dat) >= 3) {
        p_wc <- ggplot(dat, aes(label = lbl, size = n)) +
          geom_text_wordcloud() + scale_size_area(max_size = 18) +
          theme_minimal() +
          labs(title = paste0("Frame Cloud — ", TIME_SHORT[[t]], " (", TIME_DESC[[t]], ")"))
        ggsave(file.path(output_dir, paste0("rq1_wordcloud_frames_", t, ".png")),
               p_wc, width = 9, height = 7, dpi = 300)
      }
    }
    dat_all <- frames_counts %>% group_by(Frame) %>% summarize(n = sum(n), .groups="drop") %>%
      mutate(lbl = FRAME_LABELS[as.character(Frame)]) %>% arrange(desc(n))
    if (nrow(dat_all) >= 3) {
      p_wc_all <- ggplot(dat_all, aes(label = lbl, size = n)) +
        geom_text_wordcloud() + scale_size_area(max_size = 20) +
        theme_minimal() +
        labs(title = "Frame Cloud — All Time (All articles, C1 + C2 pooled)")
      ggsave(file.path(output_dir, "rq1_wordcloud_frames_all_time.png"),
             p_wc_all, width = 9, height = 7, dpi = 300)
    }
  }
}

### RQ2 ANALYSIS

if (nrow(df_c1_long) > 0) {
  message("RQ2: Frame × Compliance × Time — log-linear tests and post-hocs.")
  df_rq2 <- compute_counts_and_props(df_c1_long, "Compliance")
  
  # Force full levels (display pretty faceting/axes later)
  counts_rq2 <- df_rq2 %>%
    mutate(
      Compliance = factor(Compliance, levels = COMPLIANCE_LEVELS),
      Period     = factor(Period,     levels = TIME_LEVELS),
      Frame      = factor(Frame,      levels = FRAME_LEVELS)
    ) %>%
    select(Compliance, Period, Frame, N_with_frame)
  
  m2way <- glm(N_with_frame ~ Compliance + Period + Frame +
                 Compliance:Period + Compliance:Frame + Period:Frame,
               family = poisson, data = counts_rq2)
  m3way <- update(m2way, . ~ . + Compliance:Period:Frame)
  ll_comp <- anova(m2way, m3way, test = "LRT")
  broom::tidy(ll_comp) %>% write_csv_safely(file.path(output_dir, "rq2_loglinear_lrt_m2_vs_m3.csv"))
  
  # PNG summary + interpretation
  lrt_tidy <- broom::tidy(ll_comp)
  lrt_row  <- lrt_tidy %>% tail(1)
  lrt_text <- paste0(
    "RQ2 LRT (M2 vs M3)

",
    "ΔDeviance = ", formatC(lrt_row$deviance, format='f', digits=3), "  |  ",
    "Δdf = ", lrt_row$df, "
",
    "p-value = ", formatC(lrt_row$p.value, format='e', digits=2), "

",
    if (lrt_row$p.value < alpha_level) "Interpretation: The three-way interaction is SIGNIFICANT.
Compliance differences in frames change over time (frame-specific)." 
    else "Interpretation: The three-way interaction is NOT significant.
Compliance differences appear time-stable (given model)."
  )
  p_lrt <- ggplot() + 
    annotate("text", x=0, y=1, label=lrt_text, hjust=0, vjust=1, size=4.2, family="sans") +
    theme_void()
  ggsave(file.path(output_dir, "rq2_loglinear_lrt_m2_vs_m3.png"), p_lrt, width = 7, height = 5, dpi = 300)
  
  # Per-time LRT + cellwise + pairwise
  cellwise_list <- list(); pw_list <- list(); ch_list <- list()
  for (t in TIME_LEVELS) {
    tmp <- df_rq2 %>% filter(Period == t) %>%
      select(Compliance, Frame, N_with_frame) %>%
      complete(Compliance = factor(COMPLIANCE_LEVELS, levels = COMPLIANCE_LEVELS),
               Frame      = factor(FRAME_LEVELS,      levels = FRAME_LEVELS),
               fill = list(N_with_frame = 0)) %>%
      pivot_wider(names_from = Compliance, values_from = N_with_frame, values_fill = 0) %>%
      column_to_rownames("Frame") %>% as.matrix()
    cs <- lrt_with_cellwise(tmp, alpha = alpha_level, drop_zeros = TRUE)
    cellwise_list[[t]] <- cs$cell_df %>% mutate(Period = t, RQ = "RQ2")
    # LRT summary (by time)
    if (!is.null(cs$lrt)) {
      ch_list[[t]] <- broom::tidy(cs$lrt) %>% mutate(Period = t, RQ = "RQ2",
                                                     Test = "Likelihood-ratio (G-test) Frame × Compliance (within time)")
    }
    # Pairwise tests with readable labels
    for (fr in FRAME_LEVELS) {
      row <- df_rq2 %>% filter(Period == t, Frame == fr) %>%
        mutate(Compliance = factor(Compliance, levels = COMPLIANCE_LEVELS)) %>% arrange(Compliance)
      pw <- pairwise_prop_tests(row$N_with_frame, row$N_articles, 
                                factor(COMPLIANCE_LABELS[as.character(row$Compliance)],
                                       levels = COMPLIANCE_LABELS[COMPLIANCE_LEVELS]), 
                                "holm")
      if (nrow(pw) > 0) { pw$Frame <- FRAME_LABELS[[fr]]; pw$Period <- TIME_SHORT[[t]]; pw$RQ <- "RQ2"; pw$Dimension <- "Compliance"; pw_list[[paste(fr,t,sep="_")]] <- pw }
    }
  }
  if (length(cellwise_list) > 0) write_csv_safely(bind_rows(cellwise_list), file.path(output_dir, "rq2_cellwise_posthoc_by_time.csv"))
  if (length(ch_list) > 0)      write_csv_safely(bind_rows(ch_list),      file.path(output_dir, "rq2_lrt_tests_by_time.csv"))
  if (length(pw_list) > 0)      write_csv_safely(bind_rows(pw_list),      file.path(output_dir, "rq2_pairwise_prop_tests_by_time.csv"))
  
  # Overall LRT (G-test) on Frame × Compliance (time-pooled)
  tbl_rq2_overall <- df_rq2 %>%
    group_by(Compliance, Frame) %>% summarize(N = sum(N_with_frame, na.rm = TRUE), .groups="drop") %>%
    mutate(Compliance = factor(Compliance, levels = COMPLIANCE_LEVELS),
           Frame      = factor(Frame, levels = FRAME_LEVELS)) %>%
    pivot_wider(names_from = Compliance, values_from = N, values_fill = 0) %>%
    column_to_rownames("Frame") %>% as.matrix()
  rq2_overall <- lrt_with_cellwise(tbl_rq2_overall, alpha = alpha_level, drop_zeros = TRUE)
  if (!is.null(rq2_overall$lrt)) {
    write_csv_safely(broom::tidy(rq2_overall$lrt) %>% mutate(RQ="RQ2", Test="Likelihood-ratio (G-test) Frame × Compliance (time-pooled)"),
                     file.path(output_dir, "rq2_lrt_test.csv"))
  }
  
  # Heatmap with right-side time legend panel
  p_hm <- df_rq2 %>%
    mutate(
      FrameLab   = factor(FRAME_LABELS[as.character(Frame)], levels = FRAME_LABELS[FRAME_LEVELS]),
      PeriodLab  = factor(TIME_SHORT[as.character(Period)],   levels = TIME_SHORT[TIME_LEVELS]),
      CompLab    = factor(COMPLIANCE_LABELS[as.character(Compliance)],
                          levels = COMPLIANCE_LABELS[COMPLIANCE_LEVELS])
    ) %>%
    ggplot(aes(x = PeriodLab, y = FrameLab, fill = prop)) +
    geom_tile(color = "white", linewidth = 0.2) +
    facet_wrap(~ CompLab, ncol = 3) +
    scale_fill_gradientn(colors = pal_green_yellow(256), labels = percent, na.value = "#909090") +
    labs(
      title = "RQ2: Frame intensity by Compliance × Time",
      subtitle = "Proportion is the share of articles in each Compliance×Time slice that contain the frame at least once.",
      x = "Time (T1–T5)", y = "Frames", fill = "Proportion"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "right",
      plot.title      = element_text(size = 15, face = "bold"),
      plot.subtitle   = element_text(size = 10, lineheight = 1.15),
      strip.text      = element_text(size = 9, face="bold"),
      axis.text.x     = element_text(size = 10),
      axis.text.y     = element_text(size = 9)
    )
  
  # Side legend text
  time_text <- paste0(
    "Time frames:

",
    "T1 — ", TIME_DESC[["t1"]], "

",
    "T2 — ", TIME_DESC[["t2"]], "

",
    "T3 — ", TIME_DESC[["t3"]], "

",
    "T4 — ", TIME_DESC[["t4"]], "

",
    "T5 — ", TIME_DESC[["t5"]]
  )
  grob_txt <- grid::textGrob(time_text, x=0, y=1, hjust=0, vjust=1,
                             gp=grid::gpar(cex=0.9, lineheight=1.1))
  arranged <- gridExtra::arrangeGrob(p_hm, grob_txt, ncol = 2, widths = c(4, 3))
  ggsave(file.path(output_dir, "rq2_heatmap_by_compliance.png"), arranged, width = 17, height = 10, dpi = 300)
}

### RQ3 ANALYSIS

if (nrow(df_c2_long) > 0) {
  message("RQ3: Frame × System × Time — log-linear tests and post-hocs.")
  df_rq3 <- compute_counts_and_props(df_c2_long, "System")
  
  counts_rq3 <- df_rq3 %>%
    mutate(
      System = factor(System, levels = SYSTEM_LEVELS),
      Period = factor(Period, levels = TIME_LEVELS),
      Frame  = factor(Frame,  levels = FRAME_LEVELS)
    ) %>%
    select(System, Period, Frame, N_with_frame)
  
  m2way_ps <- glm(N_with_frame ~ System + Period + Frame +
                    System:Period + System:Frame + Period:Frame,
                  family = poisson, data = counts_rq3)
  m3way_ps <- update(m2way_ps, . ~ . + System:Period:Frame)
  ll_comp_ps <- anova(m2way_ps, m3way_ps, test = "LRT")
  broom::tidy(ll_comp_ps) %>% write_csv_safely(file.path(output_dir, "rq3_loglinear_lrt_m2_vs_m3.csv"))
  
  # PNG summary + interpretation
  lrt_tidy3 <- broom::tidy(ll_comp_ps)
  lrt_row3  <- lrt_tidy3 %>% tail(1)
  lrt_text3 <- paste0(
    "RQ3 LRT (M2 vs M3)

",
    "ΔDeviance = ", formatC(lrt_row3$deviance, format='f', digits=3), "  |  ",
    "Δdf = ", lrt_row3$df, "
",
    "p-value = ", formatC(lrt_row3$p.value, format='e', digits=2), "

",
    if (lrt_row3$p.value < alpha_level) "Interpretation: The three-way interaction is SIGNIFICANT.
System differences in frames change over time (frame-specific)." 
    else "Interpretation: The three-way interaction is NOT significant.
System differences appear time-stable (given model)."
  )
  p_lrt3 <- ggplot() + 
    annotate("text", x=0, y=1, label=lrt_text3, hjust=0, vjust=1, size=4.2, family="sans") +
    theme_void()
  ggsave(file.path(output_dir, "rq3_loglinear_lrt_m2_vs_m3.png"), p_lrt3, width = 7, height = 5, dpi = 300)
  
  cellwise_list3 <- list(); pw_list3 <- list(); ch_list3 <- list()
  for (t in TIME_LEVELS) {
    tmp <- df_rq3 %>% filter(Period == t) %>%
      select(System, Frame, N_with_frame) %>%
      complete(System = factor(SYSTEM_LEVELS, levels = SYSTEM_LEVELS),
               Frame  = factor(FRAME_LEVELS,  levels = FRAME_LEVELS),
               fill = list(N_with_frame = 0)) %>%
      pivot_wider(names_from = System, values_from = N_with_frame, values_fill = 0) %>%
      column_to_rownames("Frame") %>% as.matrix()
    cs <- lrt_with_cellwise(tmp, alpha = alpha_level, drop_zeros = TRUE)
    cellwise_list3[[t]] <- cs$cell_df %>% mutate(Period = t, RQ = "RQ3")
    if (!is.null(cs$lrt)) {
      ch_list3[[t]] <- broom::tidy(cs$lrt) %>% mutate(Period = t, RQ = "RQ3",
                                                      Test = "Likelihood-ratio (G-test) Frame × System (within time)")
    }
    for (fr in FRAME_LEVELS) {
      row <- df_rq3 %>% filter(Period == t, Frame == fr) %>%
        mutate(System = factor(System, levels = SYSTEM_LEVELS)) %>% arrange(System)
      pw <- pairwise_prop_tests(row$N_with_frame, row$N_articles,
                                factor(SYSTEM_LABELS[as.character(row$System)],
                                       levels = SYSTEM_LABELS[SYSTEM_LEVELS]),
                                "holm")
      if (nrow(pw) > 0) { pw$Frame <- FRAME_LABELS[[fr]]; pw$Period <- TIME_SHORT[[t]]; pw$RQ <- "RQ3"; pw$Dimension <- "System"; pw_list3[[paste(fr,t,sep="_")]] <- pw }
    }
  }
  if (length(cellwise_list3) > 0) write_csv_safely(bind_rows(cellwise_list3), file.path(output_dir, "rq3_cellwise_posthoc_by_time.csv"))
  if (length(ch_list3) > 0)      write_csv_safely(bind_rows(ch_list3),      file.path(output_dir, "rq3_lrt_tests_by_time.csv"))
  if (length(pw_list3) > 0)      write_csv_safely(bind_rows(pw_list3),      file.path(output_dir, "rq3_pairwise_prop_tests_by_time.csv"))
  
  # Overall LRT (G-test) on Frame × System (time-pooled)
  tbl_rq3_overall <- df_rq3 %>%
    group_by(System, Frame) %>% summarize(N = sum(N_with_frame, na.rm = TRUE), .groups="drop") %>%
    mutate(System = factor(System, levels = SYSTEM_LEVELS),
           Frame  = factor(Frame, levels = FRAME_LEVELS)) %>%
    pivot_wider(names_from = System, values_from = N, values_fill = 0) %>%
    column_to_rownames("Frame") %>% as.matrix()
  rq3_overall <- lrt_with_cellwise(tbl_rq3_overall, alpha = alpha_level, drop_zeros = TRUE)
  if (!is.null(rq3_overall$lrt)) {
    write_csv_safely(broom::tidy(rq3_overall$lrt) %>% mutate(RQ="RQ3", Test="Likelihood-ratio (G-test) Frame × System (time-pooled)"),
                     file.path(output_dir, "rq3_lrt_test.csv"))
  }
  
  # Heatmap with country legend panel
  p_hm3 <- df_rq3 %>%
    mutate(
      FrameLab  = factor(FRAME_LABELS[as.character(Frame)], levels = FRAME_LABELS[FRAME_LEVELS]),
      PeriodLab = factor(TIME_SHORT[as.character(Period)],  levels = TIME_SHORT[TIME_LEVELS]),
      SysLab    = factor(SYSTEM_LABELS[as.character(System)], levels = SYSTEM_LABELS[SYSTEM_LEVELS])
    ) %>%
    ggplot(aes(x = PeriodLab, y = FrameLab, fill = prop)) +
    geom_tile(color = "white", linewidth = 0.2) +
    facet_wrap(~ SysLab, ncol = 2) +
    scale_fill_gradientn(colors = pal_green_yellow(256), labels = percent, na.value = "#909090") +
    labs(
      title = "RQ3: Frame intensity by System × Time",
      subtitle = "Proportion is the share of articles in each System×Time slice that contain the frame at least once.",
      x = "Time (T1–T5)", y = "Frames", fill = "Proportion"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "right",
      plot.title      = element_text(size = 15, face = "bold"),
      plot.subtitle   = element_text(size = 10, lineheight = 1.15),
      strip.text      = element_text(size = 9, face="bold"),
      axis.text.x     = element_text(size = 10),
      axis.text.y     = element_text(size = 9)
    )
  
  sys_text <- paste(
    "Country grouping for System:
",
    "Democratic Countries: Germany, France, Israel, United Kingdom, United States of America

",
    "Non-democratic Countries: China, Hong Kong, India, Iran, Qatar, Saudi Arabia, Singapore, Turkey, Palestine",
    sep = ""
  )
  grob_sys <- grid::textGrob(sys_text, x=0, y=1, hjust=0, vjust=1, gp=grid::gpar(cex=0.9, lineheight=1.1))
  arranged3 <- gridExtra::arrangeGrob(p_hm3, grob_sys, ncol = 2, widths = c(4, 3))
  ggsave(file.path(output_dir, "rq3_heatmap_by_system.png"), arranged3, width = 17, height = 10, dpi = 300)
}

### Helpful console summaries

message("Summary tables saved to: ", normalizePath(output_dir, mustWork = FALSE))
if (exists("df_rq1")) message("RQ1 outputs: ", file.path(output_dir, "rq1_lrt_test.csv"), " | ",
                              file.path(output_dir, "rq1_cellwise_posthoc.csv"), " | ",
                              file.path(output_dir, "rq1_time_series_all_frames.png"))
if (exists("df_rq2")) message("RQ2 outputs: ", 
                              file.path(output_dir, "rq2_lrt_test.csv"), " | ",
                              file.path(output_dir, "rq2_lrt_tests_by_time.csv"), " | ",
                              file.path(output_dir, "rq2_loglinear_lrt_m2_vs_m3.csv"), " | ",
                              file.path(output_dir, "rq2_loglinear_lrt_m2_vs_m3.png"), " | ",
                              file.path(output_dir, "rq2_heatmap_by_compliance.png"), " | ",
                              file.path(output_dir, "rq2_pairwise_prop_tests_by_time.csv"))
if (exists("df_rq3")) message("RQ3 outputs: ", 
                              file.path(output_dir, "rq3_lrt_test.csv"), " | ",
                              file.path(output_dir, "rq3_lrt_tests_by_time.csv"), " | ",
                              file.path(output_dir, "rq3_loglinear_lrt_m2_vs_m3.csv"), " | ",
                              file.path(output_dir, "rq3_loglinear_lrt_m2_vs_m3.png"), " | ",
                              file.path(output_dir, "rq3_heatmap_by_system.png"), " | ",
                              file.path(output_dir, "rq3_pairwise_prop_tests_by_time.csv"))

message("
--- Session Info ---"); print(sessionInfo())

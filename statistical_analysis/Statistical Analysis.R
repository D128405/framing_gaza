###############################################################################
# Statistical Analysis
#
# Inferential analysis and visualization for RQ1-RQ3 of the Gaza framing study.
# Updated to process all files in INFERENCE_DIR and ensure row-level outlet classification.
#
# ----------------------------------------------------------------------------
# REVISION LOG (statistical-rigor pass)
# Everything below was ADDED or MODIFIED to close gaps between this script and
# the "Analytical Strategy and Measurement" methodology text. Sections carried
# over unchanged are not flagged; new/changed blocks are marked "ADDED:" or
# "MODIFIED:" inline so the diff against the original file is traceable.
#
#   1. RQ1 had no inferential statistics at all (chi-sq/Cramer's V, logistic
#      regression, OR/CI, BH correction were entirely missing). Added in full.
#   2. Political system is now modeled as the ordinal/linear predictor the
#      methodology describes (System_score, 0-3), with the previous nominal
#      4-category factor retained as an explicit sensitivity specification,
#      plus a formal linear-vs-nominal LRT/AIC comparison.
#   3. RQ3's additive (main-effect) model output, which was computed but never
#      saved, is now written out; RQ3 predicted probabilities (previously
#      absent) are added to mirror RQ2.
#   4. All three pre-registered robustness checks (mixed-effects w/ outlet
#      random intercept, probit link, merged 3-phase time) are now applied to
#      RQ1 and RQ3 as well as RQ2, plus an added outlet-nested-in-country
#      check (see caveat at outlet_country_map below).
#   5. Model-fitting warnings (separation, non-convergence) are no longer
#      silently swallowed by suppressWarnings(); they are captured and written
#      out alongside results.
#   6. BH-FDR correction is now applied within hypothesis-coherent term
#      families (main effect / interaction / control), not pooled across all
#      coefficients in a model indiscriminately.
#   7. Predicted probabilities now carry 95% CIs (via the `marginaleffects`
#      package) rather than bare point estimates.
#   8. A minimum-cell-count sparsity check is run for every frame/model; any
#      frame flagged as sparse also gets a Firth (penalized) bias-reduction
#      sensitivity model (optional dependency: `logistf`).
#   9. McFadden's pseudo-R^2, AIC, and N are reported for every logistic model.
#  10. Heatmaps and stacked bar charts are added alongside the existing trend
#      plots (the methodology promises all three visualization types).
#  11. Reference categories are documented explicitly in comments.
#  12. Outlet-classification coverage (how many articles could not be matched
#      to a named outlet, and are therefore dropped from outlet-clustered
#      robustness checks) is now logged to file instead of silently dropped.
#  13. ADDED (second rigor pass): a marginal-effects plot (via
#      marginaleffects::plot_predictions()) is saved for every frame's primary
#      model in each RQ, visualizing predicted probability by phase (and, for
#      RQ2/RQ3, by alignment/system).
#  14. ADDED: a VIF/GVIF multicollinearity diagnostic (via car::vif()) is
#      reported for the RQ2 and RQ3 additive (no-interaction) models. Skipped
#      for RQ1, which has a single predictor (Period) and so has no
#      multicollinearity to diagnose.
#  15. ADDED: Akaike weights (via MuMIn::Weights()) are added to the RQ3
#      ordinal-vs-nominal linearity-test output, giving the relative
#      probability that each specification is the better Kullback-Leibler
#      approximation, alongside the existing LRT/AIC comparison.
#  16. ADDED: a consolidated report_firth_usage.csv logs, for every Frame x RQ
#      combination, whether the sparsity threshold triggered a Firth
#      sensitivity model -- a quick-reference summary alongside the existing
#      per-coefficient rq*_firth_sensitivity.csv files.
#  17. MODIFIED (visualization-consolidation pass): the per-frame marginal-
#      effects plots described in #13 above were each being written out as
#      their own PNG (8 separate files per RQ -- 24 in total). Each RQ's 8
#      per-frame plots are now assembled into a single combined figure (one
#      file per RQ) with properly labeled panels, a shared legend, a light
#      background, a colorblind-safe palette, and Times New Roman text
#      throughout. No other plot in the script changes -- the existing trend/
#      heatmap/stacked-bar plots already facet all 8 frames into one figure
#      each, so they were already consolidated.
# ----------------------------------------------------------------------------
###############################################################################

library(tidyverse)
library(broom)
library(broom.mixed)
library(vcd)
library(viridis)
library(lme4)
library(marginaleffects)   # ADDED: confidence intervals on predicted probabilities, marginal-effects plots
library(car)               # ADDED: VIF / GVIF multicollinearity diagnostics
library(MuMIn)             # ADDED: Akaike weights for the RQ3 ordinal-vs-nominal comparison
library(patchwork)         # ADDED (visualization-consolidation pass): combines each RQ's 8 per-frame
# marginal-effects plots into a single multi-panel figure
# NOTE: logistf is intentionally NOT loaded with library() here -- fit_firth()
# below checks for it via requireNamespace() and skips gracefully if it isn't
# installed, so the rest of the script still runs without it. Loading it
# unconditionally would make the whole script fail to source on a machine
# that doesn't have it installed, for the sake of one optional sensitivity
# check. Install with install.packages("logistf") to enable that check.

# ============================ 1. Constants & setup ============================
FRAMES <- c("Military Conflict Frame", "Human Interest Frame", "Violence of War Frame",
            "Anti-War Protest Frame", "Media Self-Reference Frame", "Responsibility Frame",
            "Diagnostic Frame", "Prognostic Frame")

TIME_PHASES <- c("t1", "t2", "t3", "t4", "t5", "t6")
INFERENCE_DIR <- "results/inference"
OUTPUT_DIR    <- "results/statistics"
PLOTS_DIR     <- "results/visualizations"

# ADJUST THESE VALUES TO YOUR SPECIFIC PHASE DURATIONS (IN DAYS)
PHASE_DURATIONS <- c("t1" = 171, "t2" = 58, "t3" = 145, "t4" = 151, "t5" = 132, "t6" = 12)

# ---- ADDED: analysis-wide constants for the new diagnostics below ----
SPARSE_MIN_CELL <- 5    # rule-of-thumb minimum contingency-table cell count before
# a frame/model is flagged for separation risk and given a
# Firth (penalized) sensitivity model as a robustness check.

# ---- ADDED: centralized log of which Frame x RQ combinations triggered the
# sparsity threshold (and therefore a Firth sensitivity model), written to
# report_firth_usage.csv at the end of the script as a quick-reference summary
# alongside the existing per-coefficient rq*_firth_sensitivity.csv files.
firth_log <- tibble(RQ = character(), Frame = character(), Is_Firth = logical())

# ---- ADDED (visualization-consolidation pass): shared styling constants for
# the combined per-RQ marginal-effects figures built below (Section "Main
# Logic" / save_combined_marginal_effects()). Centralized here so font/palette
# choices are made once and reused identically across RQ1-RQ3.
PLOT_FONT <- "Times New Roman"

# If the optional `extrafont` package is installed and Times New Roman has
# already been imported once on this machine (extrafont::font_import()),
# this registers it with R's graphics devices so ggsave() embeds it
# correctly. Skips gracefully if extrafont isn't installed -- ggplot2 will
# then just request "Times New Roman" from the OS's own font-matching system,
# which works out-of-the-box on Windows/Mac, and on Linux if a
# metric-compatible alias (e.g. via the msttcorefonts or liberation-fonts
# packages) is installed.
if (requireNamespace("extrafont", quietly = TRUE)) {
  suppressWarnings(suppressMessages(extrafont::loadfonts(device = "all", quiet = TRUE)))
}

# Colorblind-safe categorical palette (Okabe-Ito), used for the combined
# marginal-effects figures wherever a discrete grouping variable (e.g. RQ2's
# Alignment) is plotted. Continuous grouping variables (e.g. RQ3's
# System_score) instead use the `viridis` package (already loaded above),
# which is colorblind-safe by construction.
CB_PALETTE <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7", "#000000")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PLOTS_DIR,  showWarnings = FALSE, recursive = TRUE)

# ---- ADDED: documented reference categories (must match manuscript reporting) ----
# Period          : reference = "t1"                  (Oct 7, 2023 - Mar 26, 2024)
# Alignment (RQ2) : reference = "bb"                  (balanced)
# System (RQ3)    : PRIMARY spec = System_score, numeric 0-3, modeled as a linear/
#                   ordinal predictor (consistent with the V-Dem RoW ordinal scale).
#                   SENSITIVITY spec = System, nominal 4-level factor, reference = "0"
#                   (Closed Autocracy), used to test the linearity assumption.

# ============================ 1.1 Outlet Classification Setup =================
outlet_map <- c(
  abcnews        = "ABC News", aljazeera      = "Al Jazeera",
  arabnews       = "Arab News", bbc            = "BBC",
  cgtn           = "CGTN", cnn            = "CNN",
  dailysabah     = "Daily Sabah", dw             = "DW",
  euronews       = "Euronews", france24       = "France 24",
  israelhayom    = "Israel Hayom", lemonde        = "Le Monde",
  mehrnews       = "Mehr News Agency", npr            = "NPR",
  scmp           = "SCMP", theguardian    = "The Guardian",
  straitstimes   = "Straits Times", theintercept   = "The Intercept",
  timesofindia   = "Times of India", voa            = "Voice of America",
  vox            = "Vox", wafa           = "Wafa"
)

keys_sorted <- names(outlet_map)[order(-nchar(names(outlet_map)))]
prefix_regex <- "^(www\\.|news[A-Za-z0-9]{2}\\.|news\\.|en\\.|english\\.)"

classify_outlet <- function(url) {
  if (is.na(url) || url == "") return(NA_character_)
  
  rest <- sub("^https?://", "", url, ignore.case = TRUE)
  rest <- sub(prefix_regex, "", rest, ignore.case = TRUE)
  rest_lower <- tolower(rest)
  
  for (k in keys_sorted) {
    if (startsWith(rest_lower, k)) return(outlet_map[[k]])
  }
  
  url_lower <- tolower(url)
  for (k in keys_sorted) {
    if (grepl(k, url_lower, fixed = TRUE)) return(outlet_map[[k]])
  }
  return(NA_character_)
}

# ---- ADDED: outlet -> country crosswalk, used only for the optional
# country-nested-in-outlet robustness check further below. This crosswalk is
# reconstructed from the manuscript's own "Operationalizing Political
# Alignment" / "Operationalizing Political System" country lists, cross-walked
# to each outlet's headquarters country. CAVEAT: verify this against your
# canonical upstream outlet-country mapping before reporting -- it is rebuilt
# here from the manuscript text, not drawn from your underlying data pipeline.
outlet_country_map <- c(
  "ABC News" = "US", "Al Jazeera" = "QA", "Arab News" = "SA", "BBC" = "UK",
  "CGTN" = "CN", "CNN" = "US", "Daily Sabah" = "TR", "DW" = "DE",
  "Euronews" = "FR", "France 24" = "FR", "Israel Hayom" = "IL", "Le Monde" = "FR",
  "Mehr News Agency" = "IR", "NPR" = "US", "SCMP" = "HK", "The Guardian" = "UK",
  "Straits Times" = "SG", "The Intercept" = "US", "Times of India" = "IN",
  "Voice of America" = "US", "Vox" = "US", "Wafa" = "PS"
)

classify_country <- function(outlet_name) {
  if (is.na(outlet_name)) return(NA_character_)
  out <- unname(outlet_country_map[outlet_name])
  if (is.na(out)) NA_character_ else out
}

# ============================ 2. Data loader =================================
# Updated: Default pattern loads all CSVs in INFERENCE_DIR
load_cluster_data <- function(pattern = "\\.csv$") {
  files <- list.files(INFERENCE_DIR, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NULL)
  
  map_dfr(files, function(file) {
    d <- read_csv(file, show_col_types = FALSE)
    
    # Classify outlet for EVERY row in the file
    if ("URL" %in% names(d)) {
      d <- d %>% mutate(`News Outlet` = vapply(URL, classify_outlet, character(1)))
    } else {
      d <- d %>% mutate(`News Outlet` = NA_character_)
    }
    d <- d %>% mutate(Country = vapply(`News Outlet`, classify_country, character(1)))  # ADDED
    
    d %>% mutate(SourceFile = basename(file))
  }) %>%
    pivot_longer(cols = all_of(FRAMES), names_to = "Frame", values_to = "Present") %>%
    mutate(
      Present = as.integer(Present),
      Period  = factor(str_extract(SourceFile, "t[1-6]"), levels = TIME_PHASES)
    )
}

# ============================ 3. Generic helpers =============================
# MODIFIED: chisq_cramer and safe_glm no longer blanket-suppress warnings. They
# now capture them (separation, low expected counts, non-convergence, etc.) and
# return them alongside the results so they can be reported rather than hidden.
chisq_cramer <- function(group, present) {
  tbl <- table(group, present)
  if (nrow(tbl) < 2 || ncol(tbl) < 2) {
    return(tibble(N = sum(tbl), Chi_Sq = NA_real_, Chi_df = NA_real_,
                  Chi_P = NA_real_, Cramer_V = NA_real_, Chi_Warning = NA_character_))
  }
  warn_msg <- character(0)
  res <- withCallingHandlers(
    tryCatch(chisq.test(tbl), error = function(e) NULL),
    warning = function(w) { warn_msg <<- c(warn_msg, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  warn_txt <- if (length(warn_msg) > 0) paste(unique(warn_msg), collapse = "; ") else NA_character_
  if (is.null(res)) {
    return(tibble(N = sum(tbl), Chi_Sq = NA_real_, Chi_df = NA_real_,
                  Chi_P = NA_real_, Cramer_V = NA_real_, Chi_Warning = warn_txt))
  }
  v <- tryCatch(assocstats(tbl)$cramer, error = function(e) NA_real_)
  tibble(N = sum(tbl), Chi_Sq = unname(res$statistic), Chi_df = unname(res$parameter),
         Chi_P = res$p.value, Cramer_V = v, Chi_Warning = warn_txt)
}

safe_glm <- function(formula, data, fam = binomial()) {
  warn_msg <- character(0)
  model <- withCallingHandlers(
    tryCatch(glm(formula, data = data, family = fam), error = function(e) NULL),
    warning = function(w) { warn_msg <<- c(warn_msg, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  if (!is.null(model)) {
    attr(model, "fit_warnings") <- if (length(warn_msg) > 0) paste(unique(warn_msg), collapse = "; ") else NA_character_
  }
  model
}

lrt <- function(reduced_formula, full_formula, data) {
  m0 <- safe_glm(reduced_formula, data)
  m1 <- safe_glm(full_formula, data)
  if (is.null(m0) || is.null(m1)) return(tibble(LR_ChiSq = NA_real_, LR_df = NA_real_, LR_P = NA_real_))
  an <- tryCatch(anova(m0, m1, test = "LRT"), error = function(e) NULL)
  if (is.null(an)) return(tibble(LR_ChiSq = NA_real_, LR_df = NA_real_, LR_P = NA_real_))
  tibble(LR_ChiSq = an$Deviance[2], LR_df = an$Df[2], LR_P = an[["Pr(>Chi)"]][2])
}

tidy_or <- function(model, frame) {
  if (is.null(model)) return(NULL)
  out <- safe_tidy(model, exponentiate = TRUE, conf.int = TRUE)
  if (is.null(out)) return(NULL)
  out %>%
    mutate(Frame = frame, .before = 1) %>%
    rename(OR = estimate, CI_low = conf.low, CI_high = conf.high, Wald_P = p.value)
  # Fit_Warnings column is already attached by safe_tidy(); no separate step needed.
}

bh <- function(p) p.adjust(p, method = "BH")

# ---- ADDED: helper functions supporting the new diagnostics throughout ----

# McFadden's pseudo-R^2 computed directly from glm()'s reported deviances
# (deviance = -2*logLik, so this is algebraically identical to 1 - logLik(model)/logLik(null)).
mcfadden_r2 <- function(model) {
  if (is.null(model)) return(NA_real_)
  tryCatch(1 - (model$deviance / model$null.deviance), error = function(e) NA_real_)
}

# Small safe wrapper so AIC() on a failed (NULL) model never errors out a script run.
safe_aic <- function(model) {
  if (is.null(model)) return(NA_real_)
  tryCatch(stats::AIC(model), error = function(e) NA_real_)
}

# One-row-per-model fit summary: N, AIC, McFadden's R^2, and any fit warnings.
model_fit_row <- function(model, frame, model_label) {
  if (is.null(model)) {
    return(tibble(Frame = frame, Model = model_label, N = NA_integer_, AIC = NA_real_,
                  McFadden_R2 = NA_real_, Fit_Warnings = NA_character_))
  }
  tibble(Frame = frame, Model = model_label, N = stats::nobs(model), AIC = stats::AIC(model),
         McFadden_R2 = mcfadden_r2(model), Fit_Warnings = attr(model, "fit_warnings"))
}

# Classifies a glm coefficient name into a hypothesis-coherent family, so that
# BH-FDR correction is applied within (e.g.) "all Alignment main effects across
# the 8 frames" rather than pooling main effects, interactions, and nuisance
# Period controls into one indiscriminate family.
# NOTE: case_when() evaluates every condition as a full vector regardless of
# whether earlier conditions already matched, so str_starts(term, control_prefix)
# cannot be inlined directly in the control_prefix == NA case -- some stringr
# versions error on a NA pattern rather than returning FALSE. The control-match
# vector is therefore computed safely beforehand.
classify_term <- function(term, predictor_prefix, control_prefix = NA_character_) {
  is_control <- if (!is.na(control_prefix)) {
    stringr::str_starts(term, control_prefix)
  } else {
    rep(FALSE, length(term))
  }
  dplyr::case_when(
    term == "(Intercept)" ~ "Intercept",
    stringr::str_detect(term, ":") ~ "Interaction",
    stringr::str_starts(term, predictor_prefix) ~ paste0(predictor_prefix, " main effect"),
    is_control ~ paste0(control_prefix, " (control)"),
    TRUE ~ "Other"
  )
}

# Applies BH correction within each Term_Family (pooled across the 8 frames).
# Intercepts and nuisance control terms (e.g., Period in the RQ2/RQ3 models,
# where it is a control rather than the focal hypothesis) are left untested --
# only families that correspond to an actual tested hypothesis are corrected.
fdr_by_family <- function(df) {
  df %>%
    group_by(Term_Family) %>%
    mutate(Wald_P_FDR = if_else(
      Term_Family == "Intercept" | stringr::str_detect(Term_Family, "\\(control\\)"),
      NA_real_,
      p.adjust(Wald_P, method = "BH")
    )) %>%
    ungroup()
}

# Minimum cell count across the full Group x Period x Present contingency
# table -- a simple rule-of-thumb flag for separation / sparse-data risk.
min_cell_n <- function(group, period, present) {
  tbl <- table(group, period, present)
  if (length(tbl) == 0) return(NA_integer_)
  min(tbl)
}

# Firth (penalized) logistic regression as a bias-reduction sensitivity check
# for frame/model combinations flagged as sparse. Optional dependency
# ("logistf"); skips gracefully (with a message) if not installed.
fit_firth <- function(formula, data) {
  if (!requireNamespace("logistf", quietly = TRUE)) {
    message("Package 'logistf' not installed - skipping Firth sensitivity check for this model. ",
            "Install with install.packages('logistf') to enable this diagnostic.")
    return(NULL)
  }
  m <- tryCatch(logistf::logistf(formula, data = data), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  tibble(
    term    = names(stats::coef(m)),
    OR      = exp(unname(stats::coef(m))),
    CI_low  = exp(unname(m$ci.lower)),
    CI_high = exp(unname(m$ci.upper)),
    Firth_P = unname(m$prob)
  )
}

# Fits a mixed-effects model and attaches convergence/singularity diagnostics
# as attributes instead of silently suppressing lme4's warnings/messages.
# (lme4 emits "boundary (singular) fit" via message(), not warning(), so both
# condition types are intercepted here.)
fit_glmer_diag <- function(formula, data) {
  warn_msg <- character(0)
  m <- withCallingHandlers(
    tryCatch(
      glmer(formula, data = data, family = binomial(),
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
      error = function(e) NULL
    ),
    warning = function(w) { warn_msg <<- c(warn_msg, conditionMessage(w)); invokeRestart("muffleWarning") },
    message = function(m) { warn_msg <<- c(warn_msg, trimws(conditionMessage(m))); invokeRestart("muffleMessage") }
  )
  if (!is.null(m)) {
    conv_msgs <- tryCatch(m@optinfo$conv$lme4$messages, error = function(e) NULL)
    singular  <- tryCatch(lme4::isSingular(m), error = function(e) NA)
    attr(m, "fit_warnings") <- paste(unique(c(warn_msg, conv_msgs)), collapse = "; ")
    attr(m, "is_singular")  <- singular
  }
  m
}

# Wraps broom::tidy()/broom.mixed::tidy() so that warnings or messages raised
# during confint() computation (profile-likelihood refits for glm; singular-
# fit checks or finite-difference Hessian fallbacks for merMod) are captured
# instead of leaking to the console, and merged with whatever safe_glm() or
# fit_glmer_diag() already recorded at fit time.
safe_tidy <- function(model, ...) {
  if (is.null(model)) return(NULL)
  warn_msg <- character(0)
  out <- withCallingHandlers(
    tryCatch(broom::tidy(model, ...), error = function(e) NULL),
    warning = function(w) { warn_msg <<- c(warn_msg, conditionMessage(w)); invokeRestart("muffleWarning") },
    message = function(m) { warn_msg <<- c(warn_msg, trimws(conditionMessage(m))); invokeRestart("muffleMessage") }
  )
  if (is.null(out)) return(NULL)
  prior_warn <- attr(model, "fit_warnings")
  all_warn <- unique(c(if (!is.null(prior_warn) && !is.na(prior_warn)) prior_warn else NULL, warn_msg))
  out$Fit_Warnings <- if (length(all_warn) > 0) paste(all_warn, collapse = "; ") else NA_character_
  out
}

# Logs how many articles could not be matched to a named outlet (and are
# therefore dropped from outlet-clustered robustness checks), instead of
# silently filtering them out.
log_outlet_coverage <- function(df, out_path) {
  log_tbl <- df %>%
    distinct(SourceFile, URL, `News Outlet`, Country) %>%
    summarise(N_articles_total   = n(),
              N_outlet_unclassified  = sum(is.na(`News Outlet`)),
              N_country_unclassified = sum(is.na(Country)),
              Pct_outlet_unclassified  = round(100 * N_outlet_unclassified / N_articles_total, 2),
              Pct_country_unclassified = round(100 * N_country_unclassified / N_articles_total, 2))
  write_csv(log_tbl, out_path)
  log_tbl
}

# ---- ADDED (second rigor pass): VIF/GVIF, Akaike weights, and marginal-
# effects plotting helpers ----

# VIF/GVIF diagnostic on a no-interaction (additive) model. car::vif()
# automatically returns GVIF (with a Df-scaled column) when the model
# contains factors with more than two levels, and a plain VIF vector
# otherwise; both cases are normalized into one tidy tibble shape. VIF is
# only meaningful for a model with 2+ predictors, so this is not applied to
# RQ1's single-predictor (Period-only) model.
check_vif <- function(model) {
  if (is.null(model)) return(NULL)
  tryCatch({
    v <- car::vif(model)
    if (is.matrix(v)) {
      tibble(term = rownames(v), GVIF = v[, 1], Df = v[, 2], GVIF_scaled = v[, 3])
    } else {
      tibble(term = names(v), GVIF = unname(v), Df = NA_real_, GVIF_scaled = NA_real_)
    }
  }, error = function(e) NULL)
}

# Akaike weights for a small set of competing models, given their AIC values.
# Wraps MuMIn::Weights() so a failure (e.g., a non-finite AIC from a failed
# fit) returns NAs instead of crashing the script.
akaike_weights <- function(aic_vec) {
  tryCatch(as.numeric(MuMIn::Weights(aic_vec)), error = function(e) rep(NA_real_, length(aic_vec)))
}

# MODIFIED (visualization-consolidation pass): this used to be plot_marginal_
# effects(), which immediately ggsave()'d one PNG per frame (8 separate files
# per RQ). It's now split into two steps so all 8 per-frame plots can be
# combined into a single figure instead:
#   1. build_marginal_effects_plot()        -- builds ONE small, styled panel
#                                              for one frame (this function)
#   2. save_combined_marginal_effects()      -- assembles all 8 panels for an
#                                              RQ into one labeled figure and
#                                              writes the single combined PNG
#
# Builds a marginal-effects panel (predicted probability by `condition`) for
# one fitted model via marginaleffects::plot_predictions(), styled with a
# light background, Times New Roman text, and a colorblind-safe palette.
# `color_type` tells the function what kind of grouping variable (if any) is
# the second element of `condition`, so the right kind of colorblind-safe
# scale gets applied:
#   "none"       - RQ1: condition is Period only, no color grouping
#   "discrete"   - RQ2: condition includes Alignment (3-level factor)
#   "continuous" - RQ3: condition includes System_score (numeric 0-3)
# If the model is NULL or plotting fails, a placeholder panel is returned
# instead (rather than silently dropping the frame) so every combined figure
# still shows all 8 frame labels in a consistent grid.
build_marginal_effects_plot <- function(model, frame_name, condition,
                                        color_type = c("none", "discrete", "continuous")) {
  color_type <- match.arg(color_type)
  
  placeholder <- function(msg) {
    ggplot() +
      annotate("text", x = 0, y = 0, label = msg, family = PLOT_FONT, size = 3.1, color = "grey40") +
      labs(title = str_wrap(frame_name, 26)) +
      theme_void(base_family = PLOT_FONT) +
      theme(plot.title = element_text(family = PLOT_FONT, size = 9, face = "bold", hjust = 0.5))
  }
  
  if (is.null(model)) return(placeholder("Model not available"))
  
  p <- tryCatch(
    plot_predictions(model, condition = condition) +
      labs(title = str_wrap(frame_name, 26), x = NULL, y = "Predicted probability"),
    error = function(e) NULL
  )
  if (is.null(p)) return(placeholder("Plot failed"))
  
  # ---- light background + Times New Roman, sized down for an 8-panel grid ----
  p <- p +
    theme_light(base_family = PLOT_FONT) +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background  = element_rect(fill = "white", color = NA),
      text         = element_text(family = PLOT_FONT),
      plot.title   = element_text(family = PLOT_FONT, size = 9, face = "bold"),
      axis.title   = element_text(family = PLOT_FONT, size = 8),
      axis.text    = element_text(family = PLOT_FONT, size = 7),
      legend.text  = element_text(family = PLOT_FONT, size = 8),
      legend.title = element_text(family = PLOT_FONT, size = 8)
    )
  
  # ---- colorblind-safe palette, matched to the grouping variable's type ----
  # ggplot2 only validates a scale against its mapped aesthetic at *render*
  # time, not when the scale is added via `+`, and marginaleffects can
  # represent a numeric "by" variable (e.g. RQ3's 4-valued System_score)
  # either as a true continuous gradient or as a small set of discrete
  # representative values depending on the model/data. apply_cb_scale() below
  # tries the scale type implied by color_type first and falls back to the
  # other type -- validated cheaply via ggplot_build(), which forces scale
  # resolution without a full render -- so a mismatch can't break the figure.
  p <- apply_cb_scale(p, color_type)
  
  p
}

# Helper for build_marginal_effects_plot() above: applies a colorblind-safe
# colour/fill scale, falling back to the other scale type (discrete vs.
# continuous) if the first attempt doesn't match what was actually plotted.
apply_cb_scale <- function(p, color_type) {
  build_ok <- function(plot_obj) {
    isTRUE(tryCatch({ ggplot2::ggplot_build(plot_obj); TRUE }, error = function(e) FALSE))
  }
  if (color_type == "discrete") {
    cand <- p + scale_colour_manual(values = CB_PALETTE) + scale_fill_manual(values = CB_PALETTE)
    if (build_ok(cand)) return(cand)
    cand <- p + scale_colour_viridis_d() + scale_fill_viridis_d()
    if (build_ok(cand)) return(cand)
  } else if (color_type == "continuous") {
    cand <- p + scale_colour_viridis_c() + scale_fill_viridis_c()
    if (build_ok(cand)) return(cand)
    cand <- p + scale_colour_viridis_d() + scale_fill_viridis_d()
    if (build_ok(cand)) return(cand)
  }
  p  # color_type == "none", or no candidate scale matched what was plotted
}

# Combines the 8 per-frame panels built by build_marginal_effects_plot() above
# into a single figure for one RQ (4 columns x 2 rows), with one shared legend,
# an overall title/subtitle, and Times New Roman applied figure-wide -- this
# is the ONE file written per RQ in place of the previous 8 separate PNGs.
save_combined_marginal_effects <- function(plot_list, rq_label, subtitle) {
  combined <- patchwork::wrap_plots(plot_list, ncol = 4, nrow = 2) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title    = paste0(rq_label, ": Marginal Effects by Frame"),
      subtitle = subtitle,
      theme = theme(
        text          = element_text(family = PLOT_FONT),
        plot.title    = element_text(family = PLOT_FONT, size = 16, face = "bold"),
        plot.subtitle = element_text(family = PLOT_FONT, size = 11),
        plot.background = element_rect(fill = "white", color = NA)
      )
    )
  combined <- combined &
    theme(legend.position = "bottom",
          text = element_text(family = PLOT_FONT))
  
  fname <- paste0("plot_", tolower(rq_label), "_marginal_effects_combined.png")
  suppressWarnings(ggsave(file.path(PLOTS_DIR, fname), combined, width = 16, height = 9, bg = "white"))
  invisible(NULL)
}

# ============================ Main Logic ============================
###############################################################################
# RQ1: Conflict phase (T1-T6)
###############################################################################
cat("Running RQ1 analysis & visualizations...\n")
df_rq1 <- load_cluster_data("^gaza_rq1_t[1-6]\\.csv$") # Still filterable if needed, or leave blank for all

if (!is.null(df_rq1)) {
  # FIXED (found during test execution, not part of the original audit list):
  # the original line read `Proportion = mean(Present)` *after* `Present` had
  # already been reassigned to `sum(Present)` earlier in the same summarize()
  # call. dplyr evaluates summarize() arguments sequentially, so `Proportion`
  # was silently computing mean(<a single number>), i.e. just echoing the raw
  # count back -- not the share of articles containing the frame. Proportion
  # is now computed as Present / N.
  rq1_desc <- df_rq1 %>%
    group_by(Period, Frame) %>%
    summarize(N = n(), Present = sum(Present), Proportion = Present / N, .groups = "drop") %>%  # FIXED: see note below
    mutate(Duration = PHASE_DURATIONS[as.character(Period)],
           Intensity = Present / Duration) %>%
    group_by(Frame) %>%
    mutate(Normalized_Intensity = Intensity / max(Intensity)) %>%
    ungroup()
  
  write_csv(rq1_desc, file.path(OUTPUT_DIR, "rq1_descriptives.csv"))
  
  # ---- ADDED: RQ1 inferential statistics (previously a placeholder comment) ----
  # Pearson's chi-square test of independence (Frame x Period) + Cramer's V,
  # then a per-frame binary logistic regression with Period as a categorical
  # predictor (T1 = reference), reporting ORs + 95% CI, McFadden's R^2, a
  # sparsity flag, and (where flagged) a Firth sensitivity model -- exactly the
  # RQ1 procedure described in the methodology text.
  chisq_rows <- list(); coef_rows <- list(); fit_rows <- list()
  sparsity_rows <- list(); firth_rows <- list(); pp_rows <- list()
  me_plots_rq1 <- list()  # ADDED (visualization-consolidation pass): collects the 8 per-frame
  # marginal-effects panels built below into one combined RQ1 figure
  
  for (fr in FRAMES) {
    d <- filter(df_rq1, Frame == fr)
    
    chisq_rows[[fr]] <- chisq_cramer(d$Period, d$Present) %>% mutate(Frame = fr, .before = 1)
    
    min_n <- min(table(d$Period, d$Present))  # 2-way minimum cell count (Period x Present)
    is_sparse <- !is.na(min_n) && min_n < SPARSE_MIN_CELL
    sparsity_rows[[fr]] <- tibble(Frame = fr, Min_Cell_N = min_n, Sparse_Flag = is_sparse)
    
    m_phase <- safe_glm(Present ~ Period, d)
    coef_rows[[fr]] <- tidy_or(m_phase, fr)
    fit_rows[[fr]]  <- model_fit_row(m_phase, fr, "Phase")
    
    # ---- MODIFIED (visualization-consolidation pass): build this frame's
    # marginal-effects panel (predicted probability by Period) and store it;
    # all 8 RQ1 panels are combined into one figure after the loop instead of
    # being saved as 8 separate PNGs ----
    me_plots_rq1[[fr]] <- build_marginal_effects_plot(m_phase, fr, condition = "Period", color_type = "none")
    # NOTE: VIF is not computed for RQ1 -- with a single predictor (Period),
    # there are no other terms to be collinear with, so VIF is undefined/not
    # meaningful here. See the RQ2/RQ3 additive models below for the VIF check.
    
    if (!is.null(m_phase)) {
      grid <- tibble(Period = factor(TIME_PHASES, levels = TIME_PHASES))
      pred <- tryCatch(predictions(m_phase, newdata = grid), error = function(e) NULL)
      if (!is.null(pred)) {
        pp_rows[[fr]] <- pred %>%
          transmute(Frame = fr, Period, PredProb = estimate, PredProb_low = conf.low, PredProb_high = conf.high)
      }
    }
    
    # ---- ADDED: centralized Firth-usage tracking (Requirement 5) ----
    firth_log <- bind_rows(firth_log, tibble(RQ = "RQ1", Frame = fr, Is_Firth = is_sparse))
    if (is_sparse) {
      fm <- fit_firth(Present ~ Period, d)
      if (!is.null(fm)) firth_rows[[fr]] <- fm %>% mutate(Frame = fr, .before = 1)
    }
  }
  
  rq1_chisq <- bind_rows(chisq_rows) %>% mutate(Chi_P_FDR = bh(Chi_P))
  
  rq1_coef <- bind_rows(coef_rows) %>%
    mutate(Term_Family = classify_term(term, predictor_prefix = "Period")) %>%
    fdr_by_family()
  
  rq1_fit       <- bind_rows(fit_rows)
  rq1_sparsity  <- bind_rows(sparsity_rows)
  rq1_pp        <- bind_rows(pp_rows)
  
  # ---- MODIFIED (visualization-consolidation pass): one combined figure
  # with all 8 RQ1 marginal-effects panels, in place of the 8 separate PNGs
  # previously written by plot_marginal_effects() inside the loop above ----
  save_combined_marginal_effects(me_plots_rq1, "RQ1", "Predicted probability of frame presence by conflict phase")
  
  write_csv(rq1_chisq,    file.path(OUTPUT_DIR, "rq1_chisq_cramersv.csv"))
  write_csv(rq1_coef,     file.path(OUTPUT_DIR, "rq1_logit_odds_ratios.csv"))
  write_csv(rq1_fit,      file.path(OUTPUT_DIR, "rq1_model_fit.csv"))
  write_csv(rq1_sparsity, file.path(OUTPUT_DIR, "rq1_sparsity_check.csv"))
  write_csv(rq1_pp,       file.path(OUTPUT_DIR, "rq1_predicted_probabilities.csv"))
  if (length(firth_rows) > 0) {
    write_csv(bind_rows(firth_rows), file.path(OUTPUT_DIR, "rq1_firth_sensitivity.csv"))
  }
  
  p1 <- ggplot(rq1_desc, aes(x = Period, group = Frame)) +
    geom_line(aes(y = Proportion, color = Frame), linewidth = 1) +
    geom_line(aes(y = Normalized_Intensity, color = Frame), linewidth = 0.7, linetype = "dashed", alpha = 0.6) +
    geom_point(aes(y = Proportion, color = Frame), size = 2) +
    labs(title = "RQ1: Evolution of Conflict Frames",
         x = "Conflict Phase (T1 - T6)", y = "Scale") +
    theme_light() + facet_wrap(~Frame, scales = "free_y")
  suppressWarnings(ggsave(file.path(PLOTS_DIR, "rq1_time.png"), p1, width = 12, height = 8))
  
  # ---- ADDED: heatmap (Period x Frame, fill = Proportion) ----
  p1_heat <- ggplot(rq1_desc, aes(x = Period, y = Frame, fill = Proportion)) +
    geom_tile(color = "white") +
    scale_fill_viridis(name = "Proportion\nof articles", labels = scales::percent) +
    labs(title = "RQ1: Frame Prevalence Heatmap Across Conflict Phases",
         x = "Conflict Phase", y = NULL) +
    theme_minimal()
  suppressWarnings(ggsave(file.path(PLOTS_DIR, "rq1_heatmap.png"), p1_heat, width = 10, height = 6))
  
  # ---- ADDED: stacked bar chart of frame-instance volume by phase ----
  # NOTE: frames are non-exclusive (multi-label coding), so bars stack raw
  # frame-instance counts, not mutually-exclusive shares of articles.
  p1_stack <- ggplot(rq1_desc, aes(x = Period, y = Present, fill = Frame)) +
    geom_col(position = "stack") +
    scale_fill_viridis_d(name = "Frame") +
    labs(title = "RQ1: Stacked Volume of Frame Mentions by Conflict Phase",
         subtitle = "Bars stack raw frame-instance counts (frames are non-exclusive; an article may carry multiple frames)",
         x = "Conflict Phase", y = "Number of frame instances") +
    theme_light()
  suppressWarnings(ggsave(file.path(PLOTS_DIR, "rq1_stackedbar.png"), p1_stack, width = 10, height = 6))
  
  # ---- ADDED: outlet-classification transparency log ----
  invisible(log_outlet_coverage(df_rq1, file.path(OUTPUT_DIR, "rq1_outlet_classification_log.csv")))
}

###############################################################################
# RQ2: Political alignment (IL, BB, PS) x time
###############################################################################
cat("Running RQ2 analysis & visualizations...\n")
df_rq2 <- load_cluster_data("^gaza_rq2_(il|bb|ps)_t[1-6]\\.csv$")

if (!is.null(df_rq2)) {
  df_rq2 <- df_rq2 %>%
    mutate(Alignment = factor(str_extract(SourceFile, "il|bb|ps"), levels = c("bb", "ps", "il")))
  # Reference category = "bb" (balanced). See documented reference categories above.
  
  rq2_desc <- df_rq2 %>%
    group_by(Period, Alignment, Frame) %>%
    summarize(N = n(), Proportion = mean(Present), .groups = "drop")
  write_csv(rq2_desc, file.path(OUTPUT_DIR, "rq2_descriptives.csv"))
  
  chisq_rows <- list(); lrt_main <- list(); lrt_int <- list()
  coef_rows <- list(); coef_add_rows <- list(); pp_rows <- list()
  fit_rows <- list(); sparsity_rows <- list(); firth_rows <- list()  # ADDED
  vif_rows <- list()  # ADDED
  me_plots_rq2 <- list()  # ADDED (visualization-consolidation pass): collects the 8 per-frame
  # marginal-effects panels built below into one combined RQ2 figure
  
  for (fr in FRAMES) {
    d <- filter(df_rq2, Frame == fr)
    
    chisq_rows[[fr]] <- chisq_cramer(d$Alignment, d$Present) %>% mutate(Frame = fr, .before = 1)
    
    # ---- ADDED: sparsity check (Alignment x Period x Present) ----
    min_n <- min_cell_n(d$Alignment, d$Period, d$Present)
    is_sparse <- !is.na(min_n) && min_n < SPARSE_MIN_CELL
    sparsity_rows[[fr]] <- tibble(Frame = fr, Min_Cell_N = min_n, Sparse_Flag = is_sparse)
    
    m_full <- safe_glm(Present ~ Alignment * Period, d)
    coef_rows[[fr]] <- tidy_or(m_full, fr)
    fit_rows[[fr]] <- model_fit_row(m_full, fr, "Interactive")  # ADDED
    
    m_add <- safe_glm(Present ~ Alignment + Period, d)
    coef_add_rows[[fr]] <- tidy_or(m_add, fr)
    fit_rows[[paste0(fr, "_add")]] <- model_fit_row(m_add, fr, "Additive")  # ADDED
    
    # ---- ADDED: VIF/GVIF diagnostic on the additive (no-interaction) model ----
    vif_out <- check_vif(m_add)
    if (!is.null(vif_out)) vif_rows[[fr]] <- vif_out %>% mutate(Frame = fr, .before = 1)
    
    # ---- MODIFIED (visualization-consolidation pass): build this frame's
    # marginal-effects panel for the interactive model and store it; all 8
    # RQ2 panels are combined into one figure after the loop instead of being
    # saved as 8 separate PNGs ----
    me_plots_rq2[[fr]] <- build_marginal_effects_plot(m_full, fr, condition = c("Period", "Alignment"),
                                                      color_type = "discrete")
    
    lrt_main[[fr]] <- lrt(Present ~ Period, Present ~ Alignment + Period, d) %>%
      mutate(Frame = fr, Test = "Alignment main effect", .before = 1)
    lrt_int[[fr]] <- lrt(Present ~ Alignment + Period, Present ~ Alignment * Period, d) %>%
      mutate(Frame = fr, Test = "Alignment x Period", .before = 1)
    
    if (!is.null(m_full)) {
      grid <- expand_grid(Alignment = factor(c("bb", "ps", "il"), levels = c("bb", "ps", "il")),
                          Period = factor(TIME_PHASES, levels = TIME_PHASES))
      # MODIFIED: predicted probabilities now carry 95% CIs via marginaleffects,
      # instead of bare point estimates from predict().
      pred <- tryCatch(predictions(m_full, newdata = grid), error = function(e) NULL)
      if (!is.null(pred)) {
        pp_rows[[fr]] <- pred %>%
          transmute(Frame = fr, Alignment, Period, PredProb = estimate,
                    PredProb_low = conf.low, PredProb_high = conf.high)
      }
    }
    
    # ---- ADDED: Firth sensitivity model for sparse frames + centralized log ----
    firth_log <- bind_rows(firth_log, tibble(RQ = "RQ2", Frame = fr, Is_Firth = is_sparse))
    if (is_sparse) {
      fm <- fit_firth(Present ~ Alignment * Period, d)
      if (!is.null(fm)) firth_rows[[fr]] <- fm %>% mutate(Frame = fr, .before = 1)
    }
  }
  
  rq2_chisq <- bind_rows(chisq_rows) %>% mutate(Chi_P_FDR = bh(Chi_P))
  rq2_lrt   <- bind_rows(c(lrt_main, lrt_int))
  rq2_lrt <- rq2_lrt %>% group_by(Test) %>% mutate(LR_P_FDR = bh(LR_P)) %>% ungroup()
  
  # MODIFIED: BH-FDR correction now applied within hypothesis-coherent term
  # families (Alignment main effect / Interaction / Period control) instead of
  # pooling every coefficient -- substantive and nuisance alike -- into one
  # indiscriminate family.
  rq2_coef <- bind_rows(coef_rows) %>%
    mutate(Term_Family = classify_term(term, predictor_prefix = "Alignment", control_prefix = "Period")) %>%
    fdr_by_family()
  
  rq2_coef_add <- bind_rows(coef_add_rows) %>%
    mutate(Term_Family = classify_term(term, predictor_prefix = "Alignment", control_prefix = "Period")) %>%
    fdr_by_family()
  
  rq2_pp        <- bind_rows(pp_rows)
  rq2_fit       <- bind_rows(fit_rows)        # ADDED
  rq2_sparsity  <- bind_rows(sparsity_rows)   # ADDED
  rq2_vif       <- bind_rows(vif_rows)        # ADDED
  
  # ---- MODIFIED (visualization-consolidation pass): one combined figure
  # with all 8 RQ2 marginal-effects panels, in place of the 8 separate PNGs
  # previously written by plot_marginal_effects() inside the loop above ----
  save_combined_marginal_effects(me_plots_rq2, "RQ2",
                                 "Predicted probability of frame presence by conflict phase and political alignment")
  
  write_csv(rq2_chisq, file.path(OUTPUT_DIR, "rq2_chisq_cramersv.csv"))
  write_csv(rq2_lrt,   file.path(OUTPUT_DIR, "rq2_lrt_main_and_interaction.csv"))
  write_csv(rq2_coef,     file.path(OUTPUT_DIR, "rq2_logit_odds_ratios_interaction.csv"))
  write_csv(rq2_coef_add, file.path(OUTPUT_DIR, "rq2_logit_odds_ratios_additive.csv"))
  write_csv(rq2_pp,    file.path(OUTPUT_DIR, "rq2_predicted_probabilities.csv"))
  write_csv(rq2_fit,      file.path(OUTPUT_DIR, "rq2_model_fit.csv"))       # ADDED
  write_csv(rq2_sparsity, file.path(OUTPUT_DIR, "rq2_sparsity_check.csv"))  # ADDED
  if (nrow(rq2_vif) > 0) write_csv(rq2_vif, file.path(OUTPUT_DIR, "rq2_vif_diagnostics.csv"))  # ADDED
  if (length(firth_rows) > 0) {
    write_csv(bind_rows(firth_rows), file.path(OUTPUT_DIR, "rq2_firth_sensitivity.csv"))  # ADDED
  }
  
  p2 <- ggplot(rq2_desc, aes(x = Period, y = Proportion, color = Alignment, group = Alignment)) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    scale_color_manual(values = c("bb" = "darkgray", "ps" = "forestgreen", "il" = "royalblue")) +
    labs(title = "RQ2: Media Framing by Political Alignment Over Time",
         x = "Conflict Phase", y = "Proportion of Coverage", color = "Alignment") +
    theme_light() +
    theme(panel.background = element_rect(fill = "white"),
          legend.position = "bottom") +
    facet_wrap(~Frame, scales = "free_y")
  suppressWarnings(ggsave(file.path(PLOTS_DIR, "rq2_alignment_time.png"), p2, width = 12, height = 8))
  
  # ---- ADDED: heatmap (Period x Frame, faceted by Alignment) ----
  p2_heat <- ggplot(rq2_desc, aes(x = Period, y = Frame, fill = Proportion)) +
    geom_tile(color = "white") +
    scale_fill_viridis(name = "Proportion\nof articles", labels = scales::percent) +
    facet_wrap(~Alignment, labeller = as_labeller(c(bb = "Balanced", ps = "Pro-Palestine", il = "Pro-Israel"))) +
    labs(title = "RQ2: Frame Prevalence Heatmap by Political Alignment", x = "Conflict Phase", y = NULL) +
    theme_minimal()
  suppressWarnings(ggsave(file.path(PLOTS_DIR, "rq2_heatmap.png"), p2_heat, width = 14, height = 6))
  
  # ---- ADDED: stacked bar chart, raw frame-instance counts stacked by Alignment ----
  # (Alignment categories are mutually exclusive, so stacking raw counts here is
  # a valid additive decomposition, unlike stacking across non-exclusive frames.)
  rq2_counts <- rq2_desc %>% mutate(Count = round(Proportion * N))
  p2_stack <- ggplot(rq2_counts, aes(x = Period, y = Count, fill = Alignment)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = c("bb" = "darkgray", "ps" = "forestgreen", "il" = "royalblue"),
                      labels = c(bb = "Balanced", ps = "Pro-Palestine", il = "Pro-Israel")) +
    facet_wrap(~Frame, scales = "free_y") +
    labs(title = "RQ2: Frame-Instance Volume by Political Alignment and Phase",
         x = "Conflict Phase", y = "Number of frame instances") +
    theme_light()
  suppressWarnings(ggsave(file.path(PLOTS_DIR, "rq2_stackedbar.png"), p2_stack, width = 14, height = 9))
  
  # ---- ADDED: outlet-classification transparency log ----
  invisible(log_outlet_coverage(df_rq2, file.path(OUTPUT_DIR, "rq2_outlet_classification_log.csv")))
}

###############################################################################
# RQ3: Political system (0-3) x time
###############################################################################
cat("Running RQ3 analysis & visualizations...\n")
df_rq3 <- load_cluster_data("^gaza_rq3_[0-3]_t[1-6]\\.csv$")

if (!is.null(df_rq3)) {
  df_rq3 <- df_rq3 %>%
    mutate(
      System = factor(str_extract(SourceFile, "(?<=rq3_)[0-3]"), levels = c("0", "1", "2", "3")),
      # MODIFIED: levels reordered ascending (was c("3","2","1","0")) so the
      # reference category is "0" (Closed Autocracy) -- the natural ordinal
      # floor -- instead of the previous, undocumented "Liberal Democracy"
      # reference. (Verified this does not change the existing plot's legend:
      # scale_*_brewer(labels = sys_labels) maps by name, not position.)
      # ADDED: numeric ordinal score, used as the PRIMARY predictor below, per
      # the methodology's own description of political system as an ordinal
      # variable (closed autocracy = 0 ... liberal democracy = 3).
      System_score = as.numeric(as.character(System))
    )
  
  sys_labels <- c("3" = "Liberal Democracy", "2" = "Electoral Democracy",
                  "1" = "Electoral Autocracy", "0" = "Closed Autocracy")
  
  rq3_desc <- df_rq3 %>%
    group_by(Period, System, Frame) %>%
    summarize(N = n(), Proportion = mean(Present), .groups = "drop")
  write_csv(rq3_desc, file.path(OUTPUT_DIR, "rq3_descriptives.csv"))
  
  chisq_rows <- list()
  lrt_main <- list(); lrt_int <- list()
  coef_ord_rows <- list(); coef_ord_add_rows <- list()      # ADDED: primary ordinal spec
  coef_nom_rows <- list(); coef_nom_add_rows <- list()      # nominal sensitivity spec (was discarded before)
  lin_test_rows <- list()                                   # ADDED: linear-vs-nominal comparison
  fit_rows <- list(); sparsity_rows <- list(); firth_rows <- list(); pp_rows <- list()
  vif_rows <- list()  # ADDED
  me_plots_rq3 <- list()  # ADDED (visualization-consolidation pass): collects the 8 per-frame
  # marginal-effects panels built below into one combined RQ3 figure
  
  for (fr in FRAMES) {
    d <- filter(df_rq3, Frame == fr)
    
    chisq_rows[[fr]] <- chisq_cramer(d$System, d$Present) %>% mutate(Frame = fr, .before = 1)
    
    min_n <- min_cell_n(d$System, d$Period, d$Present)
    is_sparse <- !is.na(min_n) && min_n < SPARSE_MIN_CELL
    sparsity_rows[[fr]] <- tibble(Frame = fr, Min_Cell_N = min_n, Sparse_Flag = is_sparse)
    
    # ---- PRIMARY specification: System modeled as ordinal/linear (System_score) ----
    m_full_ord <- safe_glm(Present ~ System_score * Period, d)
    coef_ord_rows[[fr]] <- tidy_or(m_full_ord, fr)
    fit_rows[[paste0(fr, "_ord_int")]] <- model_fit_row(m_full_ord, fr, "Ordinal Interactive")
    
    m_add_ord <- safe_glm(Present ~ System_score + Period, d)
    coef_ord_add_rows[[fr]] <- tidy_or(m_add_ord, fr)
    fit_rows[[paste0(fr, "_ord_add")]] <- model_fit_row(m_add_ord, fr, "Ordinal Additive")
    
    # ---- ADDED: VIF/GVIF diagnostic on the primary additive (no-interaction) model ----
    vif_out <- check_vif(m_add_ord)
    if (!is.null(vif_out)) vif_rows[[fr]] <- vif_out %>% mutate(Frame = fr, .before = 1)
    
    # ---- MODIFIED (visualization-consolidation pass): build this frame's
    # marginal-effects panel for the primary (ordinal) interactive model and
    # store it; all 8 RQ3 panels are combined into one figure after the loop
    # instead of being saved as 8 separate PNGs ----
    me_plots_rq3[[fr]] <- build_marginal_effects_plot(m_full_ord, fr, condition = c("Period", "System_score"),
                                                      color_type = "continuous")
    
    # ---- SENSITIVITY specification: System as a free (nominal) 4-level factor ----
    m_full_nom <- safe_glm(Present ~ System * Period, d)
    coef_nom_rows[[fr]] <- tidy_or(m_full_nom, fr)
    fit_rows[[paste0(fr, "_nom_int")]] <- model_fit_row(m_full_nom, fr, "Nominal Interactive")
    
    m_add_nom <- safe_glm(Present ~ System + Period, d)
    coef_nom_add_rows[[fr]] <- tidy_or(m_add_nom, fr)
    fit_rows[[paste0(fr, "_nom_add")]] <- model_fit_row(m_add_nom, fr, "Nominal Additive")
    
    # ---- ADDED: formal test of the linearity/ordinal-trend assumption ----
    # m_full_ord is nested in m_full_nom (a single linear score term is a
    # restricted special case of 3 freely-estimated category dummies), so an
    # LRT is valid; AIC is reported alongside as a non-nested-agnostic check.
    # Akaike weights (MuMIn::Weights()) are added on top of the AIC values
    # already computed, giving the relative probability that each
    # specification is the better Kullback-Leibler approximation.
    lin_int <- tryCatch(anova(m_full_ord, m_full_nom, test = "LRT"), error = function(e) NULL)
    lin_add <- tryCatch(anova(m_add_ord, m_add_nom, test = "LRT"), error = function(e) NULL)
    aic_add <- c(safe_aic(m_add_ord), safe_aic(m_add_nom))
    aic_int <- c(safe_aic(m_full_ord), safe_aic(m_full_nom))
    w_add <- akaike_weights(aic_add)
    w_int <- akaike_weights(aic_int)
    lin_test_rows[[fr]] <- tibble(
      Frame = fr,
      Spec = c("Additive", "Interactive"),
      LR_ChiSq = c(if (!is.null(lin_add)) lin_add$Deviance[2] else NA_real_,
                   if (!is.null(lin_int)) lin_int$Deviance[2] else NA_real_),
      LR_df = c(if (!is.null(lin_add)) lin_add$Df[2] else NA_real_,
                if (!is.null(lin_int)) lin_int$Df[2] else NA_real_),
      LR_P = c(if (!is.null(lin_add)) lin_add[["Pr(>Chi)"]][2] else NA_real_,
               if (!is.null(lin_int)) lin_int[["Pr(>Chi)"]][2] else NA_real_),
      AIC_ordinal = c(aic_add[1], aic_int[1]),
      AIC_nominal = c(aic_add[2], aic_int[2]),
      Ordinal_AIC_Weight = c(w_add[1], w_int[1]),  # ADDED
      Nominal_AIC_Weight = c(w_add[2], w_int[2])   # ADDED
    )
    
    lrt_main[[fr]] <- lrt(Present ~ Period, Present ~ System_score + Period, d) %>%
      mutate(Frame = fr, Test = "System main effect", .before = 1)
    lrt_int[[fr]] <- lrt(Present ~ System_score + Period, Present ~ System_score * Period, d) %>%
      mutate(Frame = fr, Test = "System x Period", .before = 1)
    
    if (!is.null(m_full_ord)) {
      grid <- expand_grid(System_score = c(0, 1, 2, 3), Period = factor(TIME_PHASES, levels = TIME_PHASES))
      pred <- tryCatch(predictions(m_full_ord, newdata = grid), error = function(e) NULL)
      if (!is.null(pred)) {
        pp_rows[[fr]] <- pred %>%
          transmute(Frame = fr, System_score, Period, PredProb = estimate,
                    PredProb_low = conf.low, PredProb_high = conf.high)
      }
    }
    
    # ---- ADDED: Firth sensitivity model for sparse frames + centralized log ----
    firth_log <- bind_rows(firth_log, tibble(RQ = "RQ3", Frame = fr, Is_Firth = is_sparse))
    if (is_sparse) {
      fm <- fit_firth(Present ~ System_score * Period, d)
      if (!is.null(fm)) firth_rows[[fr]] <- fm %>% mutate(Frame = fr, .before = 1)
    }
  }
  
  rq3_chisq <- bind_rows(chisq_rows) %>% mutate(Chi_P_FDR = bh(Chi_P))
  rq3_lrt   <- bind_rows(c(lrt_main, lrt_int)) %>% group_by(Test) %>% mutate(LR_P_FDR = bh(LR_P)) %>% ungroup()
  
  rq3_coef_ord <- bind_rows(coef_ord_rows) %>%
    mutate(Term_Family = classify_term(term, predictor_prefix = "System_score", control_prefix = "Period")) %>%
    fdr_by_family()
  rq3_coef_ord_add <- bind_rows(coef_ord_add_rows) %>%
    mutate(Term_Family = classify_term(term, predictor_prefix = "System_score", control_prefix = "Period")) %>%
    fdr_by_family()
  rq3_coef_nom <- bind_rows(coef_nom_rows) %>%
    mutate(Term_Family = classify_term(term, predictor_prefix = "System", control_prefix = "Period")) %>%
    fdr_by_family()
  rq3_coef_nom_add <- bind_rows(coef_nom_add_rows) %>%
    mutate(Term_Family = classify_term(term, predictor_prefix = "System", control_prefix = "Period")) %>%
    fdr_by_family()
  
  rq3_linearity <- bind_rows(lin_test_rows) %>% group_by(Spec) %>% mutate(LR_P_FDR = bh(LR_P)) %>% ungroup()
  rq3_fit       <- bind_rows(fit_rows)
  rq3_sparsity  <- bind_rows(sparsity_rows)
  rq3_pp        <- bind_rows(pp_rows)
  rq3_vif       <- bind_rows(vif_rows)  # ADDED
  
  # ---- MODIFIED (visualization-consolidation pass): one combined figure
  # with all 8 RQ3 marginal-effects panels, in place of the 8 separate PNGs
  # previously written by plot_marginal_effects() inside the loop above ----
  save_combined_marginal_effects(me_plots_rq3, "RQ3",
                                 "Predicted probability of frame presence by conflict phase and political system (ordinal score)")
  
  write_csv(rq3_chisq, file.path(OUTPUT_DIR, "rq3_chisq_cramersv.csv"))
  write_csv(rq3_lrt,   file.path(OUTPUT_DIR, "rq3_lrt_main_and_interaction.csv"))
  write_csv(rq3_coef_ord,     file.path(OUTPUT_DIR, "rq3_logit_ordinal_interaction.csv"))   # PRIMARY
  write_csv(rq3_coef_ord_add, file.path(OUTPUT_DIR, "rq3_logit_ordinal_additive.csv"))       # PRIMARY (recovered)
  write_csv(rq3_coef_nom,     file.path(OUTPUT_DIR, "rq3_logit_nominal_interaction.csv"))    # sensitivity (was "..._odds_ratios_interaction.csv")
  write_csv(rq3_coef_nom_add, file.path(OUTPUT_DIR, "rq3_logit_nominal_additive.csv"))       # sensitivity (recovered; was discarded)
  write_csv(rq3_linearity, file.path(OUTPUT_DIR, "rq3_linearity_test.csv"))
  write_csv(rq3_fit,      file.path(OUTPUT_DIR, "rq3_model_fit.csv"))
  write_csv(rq3_sparsity, file.path(OUTPUT_DIR, "rq3_sparsity_check.csv"))
  write_csv(rq3_pp,       file.path(OUTPUT_DIR, "rq3_predicted_probabilities.csv"))
  if (nrow(rq3_vif) > 0) write_csv(rq3_vif, file.path(OUTPUT_DIR, "rq3_vif_diagnostics.csv"))  # ADDED
  if (length(firth_rows) > 0) {
    write_csv(bind_rows(firth_rows), file.path(OUTPUT_DIR, "rq3_firth_sensitivity.csv"))
  }
  
  p3 <- ggplot(rq3_desc, aes(x = Period, y = Proportion, color = System, group = System)) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    scale_color_brewer(palette = "Set1", labels = sys_labels) +
    labs(title = "RQ3: Media Framing by Political System Over Time",
         x = "Conflict Phase", y = "Proportion of Coverage", color = "V-Dem System") +
    theme_light() +
    theme(panel.background = element_rect(fill = "white"),
          legend.position = "bottom") +
    facet_wrap(~Frame, scales = "free_y")
  suppressWarnings(ggsave(file.path(PLOTS_DIR, "rq3_system_time.png"), p3, width = 12, height = 8))
  
  # ---- ADDED: heatmap (Period x Frame, faceted by System) ----
  p3_heat <- ggplot(rq3_desc, aes(x = Period, y = Frame, fill = Proportion)) +
    geom_tile(color = "white") +
    scale_fill_viridis(name = "Proportion\nof articles", labels = scales::percent) +
    facet_wrap(~System, labeller = as_labeller(sys_labels)) +
    labs(title = "RQ3: Frame Prevalence Heatmap by Political System", x = "Conflict Phase", y = NULL) +
    theme_minimal()
  suppressWarnings(ggsave(file.path(PLOTS_DIR, "rq3_heatmap.png"), p3_heat, width = 14, height = 6))
  
  # ---- ADDED: stacked bar chart, raw frame-instance counts stacked by System ----
  rq3_counts <- rq3_desc %>% mutate(Count = round(Proportion * N))
  p3_stack <- ggplot(rq3_counts, aes(x = Period, y = Count, fill = System)) +
    geom_col(position = "stack") +
    scale_fill_brewer(palette = "Set1", labels = sys_labels) +
    facet_wrap(~Frame, scales = "free_y") +
    labs(title = "RQ3: Frame-Instance Volume by Political System and Phase",
         x = "Conflict Phase", y = "Number of frame instances", fill = "V-Dem System") +
    theme_light()
  suppressWarnings(ggsave(file.path(PLOTS_DIR, "rq3_stackedbar.png"), p3_stack, width = 14, height = 9))
  
  # ---- ADDED: outlet-classification transparency log ----
  invisible(log_outlet_coverage(df_rq3, file.path(OUTPUT_DIR, "rq3_outlet_classification_log.csv")))
}

###############################################################################
# 4. Robustness Checks
#
# MODIFIED: the original script only ran these checks for RQ2 ("Demonstrated
# on RQ2 Interactions"). They are now applied to RQ1 and RQ3's primary models
# as well, since the methodology frames this as general model validation, and
# RQ3 is explicitly described as analogous to RQ2. A fourth check (outlet
# nested within country) is added; see the outlet_country_map caveat above.
###############################################################################
cat("Running Robustness Checks (RQ1, RQ2, RQ3 primary models)...\n")

# ---- ADDED: generic runner for the four robustness checks, applied once per
# RQ below instead of duplicating ~80 lines of near-identical code three times.
run_robustness_checks <- function(df, frames, fixed_rhs, merged_rhs, rq_label) {
  df_rob <- df %>%
    filter(!is.na(`News Outlet`)) %>%
    mutate(Period_Merged = fct_collapse(Period,
                                        "Early" = c("t1", "t2"),
                                        "Mid"   = c("t3", "t4"),
                                        "Late"  = c("t5", "t6")))
  df_rob_country <- df_rob %>% filter(!is.na(Country))
  
  rob_glmer <- list(); rob_probit <- list(); rob_time <- list(); rob_country <- list()
  
  f_mixed   <- as.formula(paste("Present ~", fixed_rhs, "+ (1 | `News Outlet`)"))
  f_probit  <- as.formula(paste("Present ~", fixed_rhs))
  f_time    <- as.formula(paste("Present ~", merged_rhs))
  f_country <- as.formula(paste("Present ~", fixed_rhs, "+ (1 | Country/`News Outlet`)"))
  
  for (fr in frames) {
    d  <- filter(df_rob, Frame == fr)
    dc <- filter(df_rob_country, Frame == fr)
    
    # 1. Mixed-Effects Logistic Regression (Random Intercept for News Outlet)
    # MODIFIED: convergence/singular-fit diagnostics are now captured and
    # reported (via fit_glmer_diag) instead of being silently suppressed.
    m_mixed <- fit_glmer_diag(f_mixed, d)
    if (!is.null(m_mixed)) {
      rob_glmer[[fr]] <- safe_tidy(m_mixed, exponentiate = TRUE, conf.int = TRUE) %>%
        mutate(Frame = fr, Is_Singular = attr(m_mixed, "is_singular"), .before = 1)
    }
    
    # 2. Alternative Link Function (Probit)
    m_probit <- safe_glm(f_probit, d, fam = binomial(link = "probit"))
    if (!is.null(m_probit)) {
      rob_probit[[fr]] <- safe_tidy(m_probit, conf.int = TRUE) %>%
        mutate(Frame = fr, .before = 1)
    }
    
    # 3. Alternative Time Operationalization (Merged Phases)
    m_time <- safe_glm(f_time, d, fam = binomial())
    if (!is.null(m_time)) {
      rob_time[[fr]] <- safe_tidy(m_time, exponentiate = TRUE, conf.int = TRUE) %>%
        mutate(Frame = fr, .before = 1)
    }
    
    # 4. ADDED: Outlet nested within Country (see outlet_country_map caveat above)
    if (nrow(dc) > 0) {
      m_country <- fit_glmer_diag(f_country, dc)
      if (!is.null(m_country)) {
        rob_country[[fr]] <- safe_tidy(m_country, exponentiate = TRUE, conf.int = TRUE) %>%
          mutate(Frame = fr, Is_Singular = attr(m_country, "is_singular"), .before = 1)
      }
    }
  }
  
  if (length(rob_glmer) > 0) {
    write_csv(bind_rows(rob_glmer), file.path(OUTPUT_DIR, paste0("robustness_", rq_label, "_mixed_effects.csv")))
  }
  if (length(rob_probit) > 0) {
    write_csv(bind_rows(rob_probit), file.path(OUTPUT_DIR, paste0("robustness_", rq_label, "_probit.csv")))
  }
  if (length(rob_time) > 0) {
    write_csv(bind_rows(rob_time), file.path(OUTPUT_DIR, paste0("robustness_", rq_label, "_merged_time.csv")))
  }
  if (length(rob_country) > 0) {
    write_csv(bind_rows(rob_country), file.path(OUTPUT_DIR, paste0("robustness_", rq_label, "_country_nested.csv")))
  }
}

if (!is.null(df_rq1)) run_robustness_checks(df_rq1, FRAMES, "Period", "Period_Merged", "rq1")
if (!is.null(df_rq2)) run_robustness_checks(df_rq2, FRAMES, "Alignment * Period", "Alignment * Period_Merged", "rq2")
if (!is.null(df_rq3)) run_robustness_checks(df_rq3, FRAMES, "System_score * Period", "System_score * Period_Merged", "rq3")

# ---- ADDED: consolidated Firth-usage report (Requirement 5) ----
# One row per Frame x RQ, flagging whether the minimum-cell sparsity threshold
# was crossed (and a Firth sensitivity model therefore attempted) -- a
# quick-reference summary alongside the existing per-coefficient
# rq*_firth_sensitivity.csv files written within each RQ section above.
write_csv(firth_log, file.path(OUTPUT_DIR, "report_firth_usage.csv"))

cat("Done. Statistics in results/statistics/, plots in results/visualizations/.\n")
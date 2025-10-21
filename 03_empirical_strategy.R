# =============================================================================
# 03_empirical_strategy.R
# DiD, event-study, and productivity robustness checks
# =============================================================================

# 0. Packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, tidyverse, lubridate, fixest, broom, zoo)

setwd(here::here())
clean_dir <- here("datasets", "cleaned")
out_dir   <- here("output", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Load analysis dataset
df <- readRDS(file.path(clean_dir, "firm_sales_for_analysis.rds"))

# Ensure ym as Date
df <- df %>% mutate(ym = as.Date(ym))

# 2. Final variable setup
program_start <- as.Date("2013-01-01")
df <- df %>%
  mutate(
    post = if_else(ym >= program_start, 1L, 0L),
    enrolled = if_else(is.na(enrolled), 0L, as.integer(enrolled)),
    log_sales = log(sales + 1),
    firm_id = as.character(firm_id),
    firm_sector = as.character(firm_sector)
  ) %>%
  filter(!is.na(log_sales))  # drop rows with no sales

message("Rows available for regressions: ", nrow(df))

# 3. Main DiD (log sales) with firm FE and sector × month FE
did_spec <- feols(
  log_sales ~ enrolled:post | firm_id + firm_sector^ym,
  cluster = "firm_id",
  data = df
)
message("Main DiD estimated.")
print(summary(did_spec))

# 4. Pre-trend placebo: fake post in 2012
df_pre <- df %>% filter(ym < program_start) %>%
  mutate(fake_post = if_else(ym >= as.Date("2012-01-01"), 1L, 0L))

placebo_spec <- feols(
  log_sales ~ enrolled:fake_post | firm_id + firm_sector^ym,
  cluster = "firm_id",
  data = df_pre
)
message("Placebo DiD estimated.")
print(summary(placebo_spec))

# 5. Event-study (dynamic DiD)
# compute month relative using lubridate time_length to avoid difftime/Period issues
df_event <- df %>%
  mutate(month_relative = as.integer(round(time_length(interval(program_start, ym), "months")))) %>%
  filter(month_relative %in% -12:24) %>%
  mutate(month_factor = factor(month_relative, levels = -12:24))

event_spec <- feols(
  log_sales ~ i(month_factor, enrolled, ref = -1) | firm_id + firm_sector^ym,
  cluster = "firm_id",
  data = df_event
)
message("Event-study estimated.")
print(summary(event_spec))

# Save event coefficients to plot later
event_coefs <- broom::tidy(event_spec) %>%
  filter(str_detect(term, "month_factor::")) %>%
  mutate(month = as.integer(str_extract(term, "-?\\d+")))
saveRDS(event_coefs, file.path(clean_dir, "event_coefs.rds"))

# 6. Productivity robustness checks
# Two productivity definitions:
# A) Revenue per worker (revenue_t / employment_t) -> "revenue productivity"
# B) Sales per worker (sales / employment_t) -> "sales productivity"

# Merge monthly variables if available (employment_t and revenue_t)
monthly <- readRDS(file.path(clean_dir, "monthly_combined_cleaned.rds"))

# Create productivity measures at firm-month level by merging monthly into df
if (nrow(monthly) > 0) {
  prod_df <- df %>%
    left_join(monthly %>% select(firm_id, ym, employment_t, revenue_t),
              by = c("firm_id", "ym")) %>%
    mutate(
      revenue_per_worker = if_else(!is.na(revenue_t) & employment_t > 0, revenue_t / employment_t, NA_real_),
      sales_per_worker = if_else(!is.na(sales) & employment_t > 0, sales / employment_t, NA_real_)
    ) %>%
    mutate(
      log_revenue_per_worker = log(revenue_per_worker + 1),
      log_sales_per_worker = log(sales_per_worker + 1)
    )
  message("Productivity measures created.")
} else {
  prod_df <- df %>% mutate(
    revenue_per_worker = NA_real_,
    sales_per_worker = NA_real_,
    log_revenue_per_worker = NA_real_,
    log_sales_per_worker = NA_real_
  )
  message("No monthly auxiliary data found; productivity measures are NA.")
}

# Estimate DiD on productivity definitions (A and B)
did_prod_revenue <- feols(
  log_revenue_per_worker ~ enrolled:post | firm_id + firm_sector^ym,
  cluster = "firm_id",
  data = prod_df
)
message("Productivity (revenue per worker) DiD estimated.")
print(summary(did_prod_revenue))

did_prod_sales <- feols(
  log_sales_per_worker ~ enrolled:post | firm_id + firm_sector^ym,
  cluster = "firm_id",
  data = prod_df
)
message("Productivity (sales per worker) DiD estimated.")
print(summary(did_prod_sales))

# 7. Sensitivity analyses
# LOCF for missing sales
df_locf <- df %>%
  group_by(firm_id) %>%
  arrange(ym) %>%
  mutate(sales = zoo::na.locf(sales, na.rm = FALSE),
         log_sales = log(sales + 1)) %>%
  ungroup()

did_locf <- feols(log_sales ~ enrolled:post | firm_id + firm_sector^ym,
                  cluster = "firm_id", data = df_locf)
message("LOCF DiD estimated.")
print(summary(did_locf))

# Alternative eligibility (<=90 employees)
if ("employment_t" %in% names(monthly)) {
  eligible_firms_90 <- monthly %>%
    group_by(firm_id) %>%
    summarise(max_emp = max(employment_t, na.rm = TRUE), .groups = "drop") %>%
    filter(max_emp <= 90) %>%
    pull(firm_id)

  df_90 <- df %>% filter(firm_id %in% eligible_firms_90)

  did_90 <- feols(log_sales ~ enrolled:post | firm_id + firm_sector^ym,
                  cluster = "firm_id", data = df_90)
  message("Did for <=90 employees estimated.")
  print(summary(did_90))
} else {
  did_90 <- NULL
  message("No employment data to run <=90 eligibility sensitivity.")
}

# 8. Save model objects
save(did_spec, placebo_spec, event_spec, did_locf,
     did_prod_revenue, did_prod_sales, did_90,
     file = file.path(clean_dir, "03_models.RData"))
message("Saved model objects to cleaned folder.")

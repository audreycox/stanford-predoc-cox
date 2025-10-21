# =============================================================================
# 01_clean_data.R
# Load, clean, and prepare raw datasets
# =============================================================================

# 0. Packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(readr, dplyr, tidyr, janitor, lubridate, here, data.table, stringr, zoo)

# 1. Setup paths
setwd(here::here())
raw_dir     <- here("datasets")
monthly_dir <- here("datasets", "monthly_data")
clean_dir   <- here("datasets", "cleaned")
dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

# 2. Read firm_information.csv (expects firm_id,f irm_name, firm_sector)
firm_info_path <- file.path(raw_dir, "firm_information.csv")
if (!file.exists(firm_info_path)) stop("Missing file: ", firm_info_path)
firm_info <- read_csv(firm_info_path, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(firm_id = as.character(firm_id))

message("Loaded firm_information: ", nrow(firm_info), " rows")

# 3. Read aggregate_firm_sales.csv (expects firm_id, date, sales_t)
agg_sales_path <- file.path(raw_dir, "aggregate_firm_sales.csv")
if (!file.exists(agg_sales_path)) stop("Missing file: ", agg_sales_path)
agg_sales_raw <- read_csv(agg_sales_path, show_col_types = FALSE) %>% clean_names()

# Robust date parsing (handles common formats)
agg_sales <- agg_sales_raw %>%
  mutate(
    firm_id = as.character(firm_id),
    date_raw = date,
    date = parse_date_time(date_raw, orders = c("ymd", "Ymd", "y-m-d", "mdy", "dmy", "Y/m/d", "m/d/Y")),
    ym = floor_date(date, "month"),
    sales = as.numeric(sales_t)
  )

# Remove rows with NA sales_t (user requested this)
agg_sales <- agg_sales %>% filter(!is.na(sales))

# Fix admin typos on firm_id (trailing zeros that create fake firms)
# Pattern: remove two or more trailing zeros, e.g. "12300" -> "123"
agg_sales <- agg_sales %>% mutate(firm_id = ifelse(is.na(firm_id), NA_character_,
                                                   str_replace(firm_id, "(\\d+)0{2,}$", "\\1")))

message("Loaded aggregate_firm_sales: ", nrow(agg_sales), " rows after cleaning")

# 4. Read and combine monthly_data files (if folder exists)
monthly_combined <- tibble()
if (dir.exists(monthly_dir)) {
  monthly_files <- list.files(monthly_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(monthly_files) > 0) {
    message("Found ", length(monthly_files), " monthly files.")
    read_mon <- function(path) {
      df <- read_csv(path, show_col_types = FALSE) %>% clean_names()
      df <- df %>%
        mutate(
          firm_id = as.character(firm_id),
          date_raw = date,
          date = parse_date_time(date_raw, orders = c("ymd", "Ymd", "y-m-d", "mdy", "dmy", "Y/m/d", "m/d/Y")),
          ym = floor_date(date, "month")
        )
      df$firm_id <- ifelse(is.na(df$firm_id), NA_character_, str_replace(df$firm_id, "(\\d+)0{2,}$", "\\1"))
      df
    }
    monthly_combined <- rbindlist(lapply(monthly_files, read_mon), fill = TRUE) %>% as_tibble()
    message("Combined monthly files rows: ", nrow(monthly_combined))
  } else {
    message("No monthly files found in ", monthly_dir)
  }
} else {
  message("monthly_data folder not found; continuing without monthly auxiliary data.")
}

# 5. Merge basic datasets
# Merge agg_sales with firm_info; keep all agg_sales rows (we analyze sales panel)
firm_sales <- agg_sales %>%
  left_join(firm_info, by = "firm_id")

# 6. Restrict to eligible firms <= 100 employees if employment data available
if ("employment_t" %in% names(monthly_combined)) {
  emp_max <- monthly_combined %>%
    group_by(firm_id) %>%
    summarise(max_emp = max(employment_t, na.rm = TRUE), .groups = "drop")
  firm_sales <- firm_sales %>%
    left_join(emp_max, by = "firm_id") %>%
    filter(is.na(max_emp) | max_emp <= 100)
  message("Restricted to firms with max_emp <= 100 (or missing): retained ", nrow(firm_sales), " rows")
} else {
  message("No employment_t in monthly data; keep all firms (eligibility not enforced here)")
}

# 7. Save cleaned objects
saveRDS(firm_info, file.path(clean_dir, "firm_info_cleaned.rds"))
saveRDS(agg_sales, file.path(clean_dir, "agg_sales_cleaned.rds"))
saveRDS(monthly_combined, file.path(clean_dir, "monthly_combined_cleaned.rds"))
saveRDS(firm_sales, file.path(clean_dir, "firm_sales_cleaned.rds"))

write_csv(firm_info, file.path(clean_dir, "firm_info_cleaned.csv"))
write_csv(agg_sales, file.path(clean_dir, "agg_sales_cleaned.csv"))
write_csv(firm_sales, file.path(clean_dir, "firm_sales_cleaned.csv"))

message("Saved cleaned datasets to ", clean_dir)

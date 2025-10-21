# =============================================================================
# 02_exploratory_analysis.R
# Summarize cleaned data and generate descriptive figures
# =============================================================================

# 0. Packages and setup
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, janitor, ggthemes, here, kableExtra)

setwd(here::here())
clean_dir <- here("datasets", "cleaned")
out_dir   <- here("output", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Load cleaned data
firm_info <- readRDS(file.path(clean_dir, "firm_info_cleaned.rds"))
firm_sales <- readRDS(file.path(clean_dir, "firm_sales_cleaned.rds"))
monthly <- readRDS(file.path(clean_dir, "monthly_combined_cleaned.rds"))

# 2. Ensure ym exists and is Date
if (!"ym" %in% names(firm_sales)) {
  stop("firm_sales does not have ym. Run 01_clean_data.R first.")
}
firm_sales <- firm_sales %>%
  mutate(ym = as.Date(ym),
         year = year(ym),
         month = month(ym))

# 3. Define or infer treatment indicator 'enrolled'
# If already present, keep. Else infer from monthly$adopt_t if available or create placeholder.
if (!"enrolled" %in% names(firm_sales)) {
  if ("adopt_t" %in% names(monthly)) {
    message("Inferring enrolled from monthly adopt_t (ever adopted => enrolled = 1)")
    ever_adopt <- monthly %>%
      group_by(firm_id) %>%
      summarise(enrolled = if_else(any(adopt_t == 1, na.rm = TRUE), 1L, 0L), .groups = "drop")
    firm_sales <- firm_sales %>% left_join(ever_adopt, by = "firm_id")
  } else {
    message("No enrolled/adopt_t found — creating reproducible placeholder (random). Replace later with true variable.")
    set.seed(42)
    firm_list <- firm_sales %>% distinct(firm_id)
    n <- nrow(firm_list)
    # assign roughly 30% as treated as reasonable placeholder
    treated_ids <- sample(firm_list$firm_id, size = round(0.3 * n))
    firm_sales <- firm_sales %>%
      mutate(enrolled = if_else(firm_id %in% treated_ids, 1L, 0L))
  }
}

# 4. Create post indicator
program_start <- as.Date("2013-01-01")
firm_sales <- firm_sales %>% mutate(post = if_else(ym >= program_start, 1L, 0L))

# 5. Basic summary table (unique firms)
summary_table <- firm_sales %>%
  distinct(firm_id, enrolled, firm_sector) %>%
  group_by(enrolled) %>%
  summarise(n_firms = n(),
            n_sectors = n_distinct(firm_sector),
            .groups = "drop") %>%
  mutate(enrolled = as.character(enrolled),
         enrolled = if_else(enrolled == "1", "Treated", "Control"))

print(summary_table)
saveRDS(summary_table, file.path(clean_dir, "firm_summary_table.rds"))

# 6. Sales trend (average sales by month and group)
sales_trends <- firm_sales %>%
  group_by(ym, enrolled) %>%
  summarise(avg_sales = mean(sales, na.rm = TRUE), .groups = "drop")

p_sales <- ggplot(sales_trends, aes(x = ym, y = avg_sales, color = factor(enrolled))) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = program_start, linetype = "dashed") +
  labs(title = "Average Monthly Sales by Treatment Group",
       x = "Month", y = "Average Sales", color = "Enrolled") +
  theme_minimal(base_size = 13)
ggsave(file.path(out_dir, "avg_sales_trends.png"), p_sales, width = 8, height = 5, dpi = 300)
message("Saved avg_sales_trends.png")

# 7. Log-sales trend and distribution
firm_sales <- firm_sales %>% mutate(log_sales = log(sales + 1))
log_trends <- firm_sales %>%
  group_by(ym, enrolled) %>%
  summarise(avg_log_sales = mean(log_sales, na.rm = TRUE), .groups = "drop")

p_log <- ggplot(log_trends, aes(x = ym, y = avg_log_sales, color = factor(enrolled))) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = program_start, linetype = "dashed") +
  labs(title = "Average Monthly Log-Sales by Treatment Group",
       x = "Month", y = "Average log(Sales+1)", color = "Enrolled") +
  theme_minimal(base_size = 13)
ggsave(file.path(out_dir, "avg_log_sales_trends.png"), p_log, width = 8, height = 5, dpi = 300)
message("Saved avg_log_sales_trends.png")

p_dist <- ggplot(firm_sales, aes(x = log_sales, fill = factor(enrolled))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Log Sales by Group", x = "log(Sales+1)", fill = "Enrolled") +
  theme_minimal(base_size = 13)
ggsave(file.path(out_dir, "log_sales_distribution.png"), p_dist, width = 8, height = 5, dpi = 300)
message("Saved log_sales_distribution.png")

# 8. Employment trend if available
if ("employment_t" %in% names(firm_sales)) {
  emp_trends <- firm_sales %>%
    group_by(ym, enrolled) %>%
    summarise(avg_emp = mean(employment_t, na.rm = TRUE), .groups = "drop")
  p_emp <- ggplot(emp_trends, aes(x = ym, y = avg_emp, color = factor(enrolled))) +
    geom_line(linewidth = 1.1) +
    geom_vline(xintercept = program_start, linetype = "dashed") +
    labs(title = "Average Employment by Group", x = "Month", y = "Average Employment") +
    theme_minimal(base_size = 13)
  ggsave(file.path(out_dir, "avg_employment_trends.png"), p_emp, width = 8, height = 5, dpi = 300)
  message("Saved avg_employment_trends.png")
} else {
  message("No employment_t available in merged data; skipping employment plot.")
}

# 9. Save firm_sales used for modeling
saveRDS(firm_sales, file.path(clean_dir, "firm_sales_for_analysis.rds"))
message("Exploratory analysis done. Figures in ", out_dir)

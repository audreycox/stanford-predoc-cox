# =============================================================================
# 05_additional_figures.R
# Figure 2: avg monthly log-sales by group
# Figure 3: distribution of productivity changes (post - pre), two definitions
# =============================================================================

# 0) Packages & paths
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, here, patchwork)

setwd(here::here())
clean_dir <- here("datasets", "cleaned")
out_dir   <- here("output", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 1) Load data
fs <- readRDS(file.path(clean_dir, "firm_sales_for_analysis.rds")) %>%
  mutate(ym = as.Date(ym))
monthly <- readRDS(file.path(clean_dir, "monthly_combined_cleaned.rds"))

# Defensive checks
if (!"enrolled" %in% names(fs)) stop("Missing 'enrolled' in firm_sales_for_analysis.rds. Run Step 2.")
program_start <- as.Date("2013-01-01")

# -----------------------------------------------------------------------------
# FIGURE 2: Average monthly log-sales by treatment status
# -----------------------------------------------------------------------------
fig2_df <- fs %>%
  mutate(log_sales = log(sales + 1)) %>%
  group_by(ym, enrolled) %>%
  summarise(avg_log_sales = mean(log_sales, na.rm = TRUE), .groups = "drop")

p_fig2 <- ggplot(fig2_df, aes(x = ym, y = avg_log_sales, color = factor(enrolled))) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = program_start, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("#1b9e77", "#d95f02"),
                     labels = c("Control (0)", "Treated (1)")) +
  labs(
    title = "Average Monthly Log-Sales by Treatment Status",
    x = "Month", y = "Average log(Sales + 1)", color = "Enrolled"
  ) +
  theme_minimal(base_size = 13)

ggsave(file.path(out_dir, "sales_trends_treatment_control.png"),
       p_fig2, width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# FIGURE 3: Distribution of productivity changes (treated vs control)
# Two definitions:
#   A) log(revenue per worker)  = log(revenue_t / employment_t)
#   B) log(sales   per worker)  = log(sales      / employment_t)
# We compute firm-level pre vs post averages, then change (post - pre).
# -----------------------------------------------------------------------------

if (nrow(monthly) > 0 && all(c("employment_t", "revenue_t") %in% names(monthly))) {

  # Merge monthly inputs onto firm-sales panel
  prod_base <- fs %>%
    left_join(monthly %>% select(firm_id, ym, employment_t, revenue_t),
              by = c("firm_id", "ym")) %>%
    mutate(
      rev_per_worker   = if_else(!is.na(revenue_t) & employment_t > 0, revenue_t / employment_t, NA_real_),
      sales_per_worker = if_else(!is.na(sales)    & employment_t > 0, sales    / employment_t, NA_real_),
      log_rpw = log(rev_per_worker   + 1),
      log_spw = log(sales_per_worker + 1),
      post    = if_else(ym >= program_start, 1L, 0L)
    )

  # --- Robust pre/post averages without pivot_wider ---
  prod_changes <- prod_base %>%
    group_by(firm_id, enrolled) %>%
    summarise(
      pre_log_rpw  = mean(log_rpw[post == 0], na.rm = TRUE),
      post_log_rpw = mean(log_rpw[post == 1], na.rm = TRUE),
      pre_log_spw  = mean(log_spw[post == 0], na.rm = TRUE),
      post_log_spw = mean(log_spw[post == 1], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      d_log_rpw = post_log_rpw - pre_log_rpw,
      d_log_spw = post_log_spw - pre_log_spw
    )

  # Quick sanity checks
  message("prod_changes columns: ", paste(colnames(prod_changes), collapse = ", "))
  message("First 6 rows of prod_changes:")
  print(utils::head(prod_changes, 6))

  readr::write_csv(prod_changes, file.path(out_dir, "prod_changes_check.csv"))

  # A) Revenue-per-worker change
  p_rpw <- ggplot(prod_changes, aes(x = d_log_rpw, fill = factor(enrolled))) +
    geom_density(alpha = 0.45) +
    scale_fill_manual(values = c("#1b9e77", "#d95f02"),
                      labels = c("Control (0)", "Treated (1)")) +
    labs(title = "Change in Log(Revenue per Worker): Post − Pre",
         x = "Δ log(revenue/worker)", y = "Density", fill = "Enrolled") +
    theme_minimal(base_size = 13)

  # B) Sales-per-worker change
  p_spw <- ggplot(prod_changes, aes(x = d_log_spw, fill = factor(enrolled))) +
    geom_density(alpha = 0.45) +
    scale_fill_manual(values = c("#1b9e77", "#d95f02"),
                      labels = c("Control (0)", "Treated (1)")) +
    labs(title = "Change in Log(Sales per Worker): Post − Pre",
         x = "Δ log(sales/worker)", y = "Density", fill = "Enrolled") +
    theme_minimal(base_size = 13)

  # Side-by-side panel
  p_fig3 <- p_rpw + p_spw + plot_layout(ncol = 2)

  ggsave(file.path(out_dir, "productivity_distribution.png"),
         p_fig3, width = 10, height = 5, dpi = 300)

} else {
  message("Monthly data with employment_t and revenue_t not available — skipping Figure 3.")
}

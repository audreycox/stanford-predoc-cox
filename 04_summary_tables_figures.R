# =============================================================================
# 04_summary_tables_figures.R
# Produce publication-ready tables and figures
# =============================================================================

# 0. Packages and setup
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, broom, modelsummary, fixest, here, ggthemes, patchwork)
setwd(here::here())

clean_dir <- here("datasets", "cleaned")
out_dir <- here("output", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Load models and data
load(file.path(clean_dir, "03_models.RData"))
df <- readRDS(file.path(clean_dir, "firm_sales_for_analysis.rds"))
event_coefs <- readRDS(file.path(clean_dir, "event_coefs.rds"))

# 2. Table: main + sensitivity DiD coefficients
models_list <- list("Main DiD" = did_spec, "LOCF" = did_locf)
if (!is.null(did_90)) models_list[["<=90 employees"]] <- did_90
if (exists("did_prod_revenue")) models_list[["Prod (rev/worker)"]] <- did_prod_revenue
if (exists("did_prod_sales")) models_list[["Prod (sales/worker)"]] <- did_prod_sales

# Create a tidy regression table (HTML)
modelsummary(models_list,
             output = file.path(out_dir, "did_models_table.html"),
             statistic = "({std.error})",
             coef_map = list("enrolled:post" = "Treatment × Post"))

# 3. Plot: event study (recreate nicely)
p_event <- ggplot(event_coefs, aes(x = month, y = estimate)) +
  geom_point(size = 2, color = "#1b9e77") +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                width = 0.2, color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(title = "Event-Study: Dynamic Treatment Effects (Log Sales)",
       x = "Months relative to program start", y = "Estimated effect (log points)") +
  theme_minimal(base_size = 13)
ggsave(file.path(out_dir, "event_study_final.png"), p_event, width = 8, height = 5, dpi = 300)

# 4. Plot: main DiD coefficient with CIs
did_coefs <- broom::tidy(did_spec, conf.int = TRUE) %>%
  filter(term == "enrolled:post") %>%
  mutate(term_label = "Treatment × Post")

p_did <- ggplot(did_coefs, aes(x = term_label, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Main DiD Estimate (Log Sales)", x = "", y = "Coefficient (log points)") +
  theme_minimal(base_size = 13)
ggsave(file.path(out_dir, "did_main_effect.png"), p_did, width = 6, height = 4, dpi = 300)

# 5. Save a combined panel
combined <- p_did + p_event + plot_layout(ncol = 1, heights = c(1, 1.2))
ggsave(file.path(out_dir, "combined_panel.png"), combined, width = 8, height = 10, dpi = 300)

message("Saved figures and regression table to ", out_dir)

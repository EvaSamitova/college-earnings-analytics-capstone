# ==============================================================
# DA 485 – Program Type Visual Pack (Business vs Healthcare)
# Author: Eva Samitova
# ==============================================================

# --- Packages ---
library(tidyverse)
library(readxl)
library(janitor)

# --- Paths ---
file_path <- "C:/Users/User/OneDrive/__Личное__/BELLEVUE COLLEGE/Capstone485/capstone(1).xlsx"
out_dir   <- dirname(file_path)

# --- Load & clean ---
dat <- read_excel(file_path) %>% clean_names()

# --- Ensure numeric types (safe) ---
num_cols <- c("health","business","yr10_salary","grad_debt","net_price",
              "avg_sat","retention","grad_rate","pell_grant","student_loan",
              "repay","x6yr_25k")
dat <- dat %>% mutate(across(all_of(intersect(num_cols, names(dat))), as.numeric))

# --- Create program_type and keep Biz/Health only ---
df_program <- dat %>%
  mutate(
    program_type = case_when(
      business > health ~ "Business",
      health > business ~ "Healthcare",
      TRUE ~ "Mixed"
    )
  ) %>%
  filter(program_type %in% c("Business","Healthcare"),
         !is.na(yr10_salary))

# --- Palette + caption helpers ---
pal <- c("Business" = "#1f77b4", "Healthcare" = "#ff7f0e")
cap <- "Source: U.S. Dept. of Education, College Scorecard"

# ===================== A) CORE PLOT ===========================
p_box <- ggplot(df_program, aes(x = program_type, y = yr10_salary, fill = program_type)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 16, width = 0.6) +
  labs(title = "10-Year Median Earnings by Program Type",
       x = "Program Type", y = "Median Earnings (10 Years After Entry)",
       caption = cap) +
  theme_minimal(base_size = 13) +
  scale_fill_manual(values = pal) +
  theme(legend.position = "none")
p_box
# ggsave(file.path(out_dir, "program_type_boxplot.png"), p_box, w=7, h=5, dpi=300)

# ===================== B) EXTRA VISUALS =======================

# 1) Violin + box (distribution shape)
p_violin <- ggplot(df_program, aes(program_type, yr10_salary, fill = program_type)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.12, outlier.shape = 16, alpha = 0.9) +
  labs(title = "Earnings Distribution by Program Type",
       x = "Program Type", y = "Median Earnings (10 Years After Entry)",
       caption = cap) +
  scale_fill_manual(values = pal) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
p_violin
# ggsave(file.path(out_dir, "program_type_violin.png"), p_violin, w=7, h=5, dpi=300)

# 2) Mean with 95% CI
df_means <- df_program %>%
  group_by(program_type) %>%
  summarise(mean = mean(yr10_salary), se = sd(yr10_salary)/sqrt(n()), .groups="drop")
p_mean <- ggplot(df_means, aes(program_type, mean, fill = program_type)) +
  geom_col(width = 0.55) +
  geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), width=.15) +
  labs(title = "Average 10-Year Earnings ± 95% CI",
       x = "Program Type", y = "Mean Earnings",
       caption = cap) +
  scale_fill_manual(values = pal) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
p_mean
# ggsave(file.path(out_dir, "program_type_mean_ci.png"), p_mean, w=7, h=5, dpi=300)

# 3) Debt vs. Earnings with smooth lines
p_scatter_debt <- ggplot(df_program, aes(grad_debt, yr10_salary, color = program_type)) +
  geom_point(alpha = 0.5, size = 1.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Graduate Debt vs 10-Year Earnings",
       x = "Median Graduate Debt", y = "Median Earnings (10 Years After Entry)",
       caption = cap) +
  scale_color_manual(values = pal) +
  theme_minimal(base_size = 13)
p_scatter_debt
# ggsave(file.path(out_dir, "debt_vs_earnings_loess.png"), p_scatter_debt, w=7, h=5, dpi=300)

# 4) Net Price / Retention / Grad Rate boxplots
p_np <- ggplot(df_program, aes(program_type, net_price, fill = program_type)) +
  geom_boxplot(width = 0.6, alpha = 0.85, outlier.shape = 16) +
  labs(title = "Average Net Price by Program Type",
       x = "Program Type", y = "Average Net Price", caption = cap) +
  scale_fill_manual(values = pal) + theme_minimal(base_size = 13) +
  theme(legend.position = "none")
p_np

p_ret <- ggplot(df_program, aes(program_type, retention, fill = program_type)) +
  geom_boxplot(width = 0.6, alpha = 0.85, outlier.shape = 16) +
  labs(title = "Retention Rate by Program Type",
       x = "Program Type", y = "Retention (proportion)", caption = cap) +
  scale_fill_manual(values = pal) + theme_minimal(base_size = 13) +
  theme(legend.position = "none")
p_ret

p_grad <- ggplot(df_program, aes(program_type, grad_rate, fill = program_type)) +
  geom_boxplot(width = 0.6, alpha = 0.85, outlier.shape = 16) +
  labs(title = "Graduation Rate by Program Type",
       x = "Program Type", y = "Grad Rate (proportion)", caption = cap) +
  scale_fill_manual(values = pal) + theme_minimal(base_size = 13) +
  theme(legend.position = "none")
p_grad
# ggsave(file.path(out_dir, "net_price_box.png"), p_np, w=7, h=5, dpi=300)
# ggsave(file.path(out_dir, "retention_box.png"),  p_ret, w=7, h=5, dpi=300)
# ggsave(file.path(out_dir, "grad_rate_box.png"),  p_grad, w=7, h=5, dpi=300)

# 5) Facet by Control (public/private/for-profit)
df_program$control <- factor(df_program$control,
                             levels = c(1,2,3),
                             labels = c("Public","Private Nonprofit","For-Profit"))
p_facet_ctrl <- ggplot(df_program, aes(program_type, yr10_salary, fill = program_type)) +
  geom_boxplot(outlier.shape = 16, alpha = 0.85) +
  facet_wrap(~ control, nrow = 1) +
  labs(title = "10-Year Earnings by Program Type (Faceted by Control)",
       x = "Program Type", y = "Median Earnings (10 Years After Entry)",
       caption = cap) +
  scale_fill_manual(values = pal) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
p_facet_ctrl
# ggsave(file.path(out_dir, "earnings_by_program_facet_control.png"),
#        p_facet_ctrl, w=10, h=4.5, dpi=300)

# 6) Zoomed box (reduce outlier distortion, keeps points)
lims <- quantile(df_program$yr10_salary, c(.02,.98), na.rm = TRUE)
p_zoom <- ggplot(df_program, aes(program_type, yr10_salary, fill = program_type)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 16) +
  coord_cartesian(ylim = lims) +
  labs(title = "Earnings by Program Type (Zoomed to 2nd–98th Percentile)",
       x = "Program Type", y = "Median Earnings (10 Years After Entry)",
       caption = cap) +
  scale_fill_manual(values = pal) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
p_zoom
# ggsave(file.path(out_dir, "program_type_boxplot_zoom.png"), p_zoom, w=7, h=5, dpi=300)

# ===================== C) STATS TO REPORT =====================
stats_tbl <- df_program %>%
  group_by(program_type) %>%
  summarise(n = n(),
            mean   = mean(yr10_salary),
            median = median(yr10_salary),
            sd     = sd(yr10_salary),
            q1     = quantile(yr10_salary, .25),
            q3     = quantile(yr10_salary, .75),
            .groups = "drop")
print(stats_tbl)

tt <- t.test(yr10_salary ~ program_type, data = df_program)
print(tt)

cohens_d <- with(df_program,
                 (mean(yr10_salary[program_type=="Business"]) -
                    mean(yr10_salary[program_type=="Healthcare"])) /
                   sqrt((sd(yr10_salary[program_type=="Business"])^2 +
                           sd(yr10_salary[program_type=="Healthcare"])^2)/2))
cat("Cohen's d:", round(cohens_d, 3), "\n")

# ===================== END ====================================

# ==============================================================
# EXTRA VISUALS: (A) Correlation Heatmap  (B) ROI by Region
# ==============================================================

# Packages for these visuals
install.packages("ggcorrplot")
library(ggcorrplot)   # install.packages("ggcorrplot") if needed
library(scales)

# ------------------ (A) Correlation Heatmap -------------------
# Pick variables to match Sophia's style
corr_vars <- c("yr10_salary","grad_debt","net_price",
               "grad_rate","retention","pell_grant")

# Keep rows with complete data on these vars
df_corr <- dat %>%
  mutate(net_price = ifelse(net_price <= 0, NA_real_, net_price)) %>% # avoid /0 later
  select(any_of(corr_vars)) %>%
  drop_na()

# Compute Pearson correlations
cm <- cor(df_corr, use = "complete.obs", method = "pearson")

# Nice display names
nice_names <- c(
  yr10_salary = "ten_year_salary",
  grad_debt   = "grad_debt",
  net_price   = "net_price",
  grad_rate   = "grad_rate",
  retention   = "retention",
  pell_grant  = "pell_grant"
)
dimnames(cm) <- list(nice_names[rownames(cm)], nice_names[colnames(cm)])

# Lower-triangle heatmap with labels
p_corr <- ggcorrplot(
  cm, type = "lower", lab = TRUE, lab_size = 3.2,
  outline.color = "white", show.diag = TRUE, hc.order = TRUE
) +
  scale_fill_gradient2(low = "#d7301f", mid = "white", high = "#003c8f",
                       midpoint = 0, limits = c(-1,1), oob = squish) +
  labs(title = "Correlation Matrix (Pearson)", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(axis.title = element_blank())
p_corr
# ggsave(file.path(out_dir, "corr_heatmap_triangular.png"), p_corr, w=7.5, h=6.5, dpi=300)

# ------------------ (B) ROI by Region -------------------------
# ROI = 10-year salary / net price
df_roi <- dat %>%
  mutate(
    net_price = na_if(net_price, 0),                # avoid divide-by-zero
    roi = yr10_salary / net_price
  ) %>%
  filter(is.finite(roi), !is.na(region))

# Summary table (mean / median / sd) by region
roi_summary <- df_roi %>%
  group_by(region) %>%
  summarise(
    mean_ROI   = mean(roi, na.rm = TRUE),
    median_ROI = median(roi, na.rm = TRUE),
    sd_ROI     = sd(roi, na.rm = TRUE),
    n          = n(),
    .groups = "drop"
  ) %>%
  arrange(region)
print(roi_summary)

# Overlaid histogram by region (like Sofia's)
p_roi_hist <- ggplot(df_roi, aes(x = roi, fill = region)) +
  geom_histogram(position = "identity", alpha = 0.35, bins = 40, color = "grey30") +
  labs(
    title = "Distribution of ROI (10-Year Salary / Net Price) by Region",
    x = "ROI Ratio (10-Year Salary / Net Price)", y = "Number of Institutions",
    caption = "Source: U.S. Dept. of Education, College Scorecard"
  ) +
  theme_minimal(base_size = 13)
p_roi_hist
# ggsave(file.path(out_dir, "roi_hist_by_region.png"), p_roi_hist, w=8, h=5.5, dpi=300)

# (Optional) Faceted version if you prefer separate panels
p_roi_facet <- ggplot(df_roi, aes(x = roi, fill = region)) +
  geom_histogram(bins = 40, alpha = 0.8, color = "white") +
  facet_wrap(~ region, ncol = 2, scales = "free_y") +
  labs(
    title = "ROI (10-Year Salary / Net Price) by Region",
    x = "ROI Ratio", y = "Number of Institutions",
    caption = "Source: U.S. Dept. of Education, College Scorecard"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
# p_roi_facet
# ggsave(file.path(out_dir, "roi_hist_by_region_faceted.png"), p_roi_facet, w=8.5, h=6.5, dpi=300)

# ---------- ROI by Program Type (Business vs Healthcare) ----------
# Create ROI and keep finite values
df_roi_prog <- df_program %>%
  mutate(net_price = na_if(net_price, 0),
         roi = yr10_salary / net_price) %>%
  filter(is.finite(roi))

# Summary table (mean / median / sd) by program type
roi_prog_summary <- df_roi_prog %>%
  group_by(program_type) %>%
  summarise(
    n          = n(),
    mean_ROI   = mean(roi, na.rm = TRUE),
    median_ROI = median(roi, na.rm = TRUE),
    sd_ROI     = sd(roi, na.rm = TRUE),
    .groups = "drop"
  )
print(roi_prog_summary)

p_roi_hist_prog <- ggplot(df_roi_prog, aes(x = roi, fill = program_type)) +
  geom_histogram(position = "identity", alpha = 0.35, bins = 40, color = "grey30") +
  labs(
    title   = "Distribution of ROI (10-Year Salary / Net Price) by Program Type",
    x       = "ROI Ratio (10-Year Salary / Net Price)",
    y       = "Number of Institutions",
    caption = "Source: U.S. Dept. of Education, College Scorecard"
  ) +
  scale_fill_manual(values = c("Business" = "#1f77b4", "Healthcare" = "#ff7f0e")) +
  theme_minimal(base_size = 13)
p_roi_hist_prog
# ggsave(file.path(out_dir, "roi_hist_by_program_type.png"), p_roi_hist_prog, w=8, h=5.5, dpi=300)

p_roi_facet_prog <- ggplot(df_roi_prog, aes(x = roi, fill = program_type)) +
  geom_histogram(bins = 40, alpha = 0.85, color = "white") +
  facet_wrap(~ program_type, nrow = 1, scales = "free_y") +
  labs(
    title   = "ROI (10-Year Salary / Net Price) by Program Type",
    x       = "ROI Ratio",
    y       = "Number of Institutions",
    caption = "Source: U.S. Dept. of Education, College Scorecard"
  ) +
  scale_fill_manual(values = c("Business" = "#1f77b4", "Healthcare" = "#ff7f0e")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
# p_roi_facet_prog
# ggsave(file.path(out_dir, "roi_hist_by_program_type_faceted.png"), p_roi_facet_prog, w=8.5, h=4.5, dpi=300)

# Boxplot
p_roi_box_prog <- ggplot(df_roi_prog, aes(program_type, roi, fill = program_type)) +
  geom_boxplot(width = 0.6, alpha = 0.85, outlier.shape = 16) +
  labs(
    title   = "ROI by Program Type",
    x       = "Program Type",
    y       = "ROI (10-Year Salary / Net Price)",
    caption = "Source: U.S. Dept. of Education, College Scorecard"
  ) +
  scale_fill_manual(values = c("Business" = "#1f77b4", "Healthcare" = "#ff7f0e")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
p_roi_box_prog
# ggsave(file.path(out_dir, "roi_box_by_program_type.png"), p_roi_box_prog, w=7, h=5, dpi=300)

# t-test on ROI difference
t.test(roi ~ program_type, data = df_roi_prog)

p_roi_hist_prog_zoom <- ggplot(df_roi_prog, aes(x = roi, fill = program_type)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 40, color = "grey30") +
  coord_cartesian(xlim = c(0, 5)) +  # focus on 0–5 ROI
  labs(
    title = "Distribution of ROI (10-Year Salary / Net Price) by Program Type (Zoomed)",
    x = "ROI Ratio (10-Year Salary / Net Price)",
    y = "Number of Institutions",
    caption = "Source: U.S. Dept. of Education, College Scorecard"
  ) +
  scale_fill_manual(values = c("Business" = "#1f77b4", "Healthcare" = "#ff7f0e")) +
  theme_minimal(base_size = 13)
p_roi_hist_prog_zoom

roi_medians <- df_roi_prog %>%
  group_by(program_type) %>%
  summarise(med = median(roi, na.rm = TRUE))

p_roi_hist_prog_zoom +
  geom_vline(data = roi_medians, aes(xintercept = med, color = program_type),
             linetype = "dashed", size = 1.1) +
  annotate("text", x = roi_medians$med, y = 100, 
           label = paste("Median", roi_medians$program_type),
           color = c("#1f77b4","#ff7f0e"), vjust = -1, hjust = 0.5, size = 4)

ggplot(df_roi_prog, aes(x = roi, fill = program_type)) +
  geom_density(alpha = 0.4) +
  coord_cartesian(xlim = c(0, 5)) +
  labs(
    title = "ROI Density by Program Type",
    x = "ROI Ratio (10-Year Salary / Net Price)",
    y = "Density",
    caption = "Source: U.S. Dept. of Education, College Scorecard"
  ) +
  scale_fill_manual(values = c("Business" = "#1f77b4", "Healthcare" = "#ff7f0e")) +
  theme_minimal(base_size = 13)


# ==============================================================
# Program Type Analysis (Business vs Healthcare) — Clean Version
# ==============================================================

# --- Load packages ---
library(tidyverse)
library(readxl)
library(janitor)

# --- Load file ---
file_path <- "C:/Users/User/OneDrive/__Личное__/BELLEVUE COLLEGE/Capstone485/scorecard_clean.xlsx"
dat <- read_excel(file_path) %>% clean_names()

# --- Verify columns ---
print(names(dat))

# --- Ensure correct salary column name ---
if ("x10yr_salary" %in% names(dat) && !"yr10_salary" %in% names(dat)) {
  dat <- dat %>% rename(yr10_salary = x10yr_salary)
}
if ("10yr_salary" %in% names(dat) && !"yr10_salary" %in% names(dat)) {
  dat <- dat %>% rename(yr10_salary = `10yr_salary`)
}

# --- Convert numeric columns safely ---
num_cols <- c("health","business","yr10_salary","grad_debt","net_price",
              "avg_sat","retention","grad_rate","pell_grant","student_loan",
              "repay","x6yr_25k")
dat <- dat %>%
  mutate(across(all_of(intersect(num_cols, names(dat))), ~ suppressWarnings(as.numeric(.x))))

# --- Create Program Type ---
df_program <- dat %>%
  mutate(program_type = case_when(
    business > health ~ "Business",
    health > business ~ "Healthcare",
    TRUE ~ "Mixed"
  )) %>%
  filter(program_type %in% c("Business","Healthcare"),
         !is.na(yr10_salary))

# --- Visual setup ---
pal <- c("Business" = "#1f77b4", "Healthcare" = "#ff7f0e")
cap <- "Source: U.S. Dept. of Education, College Scorecard"

# ===================== VISUALS =====================

# 1️⃣ 10-Year Median Earnings
ggplot(df_program, aes(program_type, yr10_salary, fill = program_type)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 16, width = 0.6) +
  labs(title = "10-Year Median Earnings by Program Type",
       x = "Program Type", y = "Median Earnings (10 Years After Entry)",
       caption = cap) +
  scale_fill_manual(values = pal) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# 2️⃣ Graduate Debt vs 10-Year Earnings
ggplot(df_program, aes(grad_debt, yr10_salary, color = program_type)) +
  geom_point(alpha = 0.5, size = 1.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Graduate Debt vs 10-Year Earnings",
       x = "Median Graduate Debt", y = "Median Earnings (10 Years After Entry)",
       caption = cap) +
  scale_color_manual(values = pal) +
  theme_minimal(base_size = 13)

# 3️⃣ Net Price
ggplot(df_program, aes(program_type, net_price, fill = program_type)) +
  geom_boxplot(width = 0.6, alpha = 0.85, outlier.shape = 16) +
  labs(title = "Average Net Price by Program Type",
       x = "Program Type", y = "Average Net Price", caption = cap) +
  scale_fill_manual(values = pal) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# 4️⃣ Retention
ggplot(df_program, aes(program_type, retention, fill = program_type)) +
  geom_boxplot(width = 0.6, alpha = 0.85, outlier.shape = 16) +
  labs(title = "Retention Rate by Program Type",
       x = "Program Type", y = "Retention (proportion)", caption = cap) +
  scale_fill_manual(values = pal) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# 5️⃣ Graduation Rate
ggplot(df_program, aes(program_type, grad_rate, fill = program_type)) +
  geom_boxplot(width = 0.6, alpha = 0.85, outlier.shape = 16) +
  labs(title = "Graduation Rate by Program Type",
       x = "Program Type", y = "Grad Rate (proportion)", caption = cap) +
  scale_fill_manual(values = pal) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# 6️⃣ Faceted by Control (Public/Private/For-Profit)
df_program$control <- factor(df_program$control,
                             levels = c(1,2,3),
                             labels = c("Public","Private Nonprofit","For-Profit"))
ggplot(df_program, aes(program_type, yr10_salary, fill = program_type)) +
  geom_boxplot(outlier.shape = 16, alpha = 0.85) +
  facet_wrap(~ control, nrow = 1) +
  labs(title = "10-Year Earnings by Program Type (Faceted by Control)",
       x = "Program Type", y = "Median Earnings (10 Years After Entry)",
       caption = cap) +
  scale_fill_manual(values = pal) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# 7️⃣ ROI (Salary ÷ Net Price)
df_roi_prog <- df_program %>%
  mutate(net_price = na_if(net_price, 0),
         roi = yr10_salary / net_price) %>%
  filter(is.finite(roi))

# Histogram (zoomed 0–5)
ggplot(df_roi_prog, aes(x = roi, fill = program_type)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 40, color = "grey30") +
  coord_cartesian(xlim = c(0, 5)) +
  labs(title = "Distribution of ROI (10-Year Salary / Net Price) by Program Type (Zoomed)",
       x = "ROI Ratio", y = "Number of Institutions", caption = cap) +
  scale_fill_manual(values = pal) +
  theme_minimal(base_size = 13)

# Density
ggplot(df_roi_prog, aes(x = roi, fill = program_type)) +
  geom_density(alpha = 0.4) +
  coord_cartesian(xlim = c(0, 5)) +
  labs(title = "ROI Density by Program Type",
       x = "ROI Ratio", y = "Density", caption = cap) +
  scale_fill_manual(values = pal) +
  theme_minimal(base_size = 13)

# ===================== TABLES / STATS =========================

stats_tbl <- df_program %>%
  group_by(program_type) %>%
  summarise(
    n = n(),
    mean_salary   = mean(yr10_salary, na.rm = TRUE),
    median_salary = median(yr10_salary, na.rm = TRUE),
    sd_salary     = sd(yr10_salary, na.rm = TRUE),
    mean_debt     = mean(grad_debt, na.rm = TRUE),
    mean_price    = mean(net_price, na.rm = TRUE),
    retention     = mean(retention, na.rm = TRUE),
    grad_rate     = mean(grad_rate, na.rm = TRUE),
    .groups = "drop"
  )
print(stats_tbl)

# T-tests
print(t.test(yr10_salary ~ program_type, data = df_program))

roi_prog_summary <- df_roi_prog %>%
  group_by(program_type) %>%
  summarise(n = n(),
            mean_ROI = mean(roi),
            median_ROI = median(roi),
            sd_ROI = sd(roi),
            .groups = "drop")
print(roi_prog_summary)
print(t.test(roi ~ program_type, data = df_roi_prog))

# Cohen’s d for earnings (already clearly significant)
d_earn <- with(df_program,
               (mean(yr10_salary[program_type=="Business"]) -
                  mean(yr10_salary[program_type=="Healthcare"])) /
                 sqrt((sd(yr10_salary[program_type=="Business"])^2 +
                         sd(yr10_salary[program_type=="Healthcare"])^2)/2))

# Cliff’s delta for ROI (robust to skew)
# install.packages("effsize") once if needed
library(effsize)
cliff.delta(roi ~ program_type, data = df_roi_prog)

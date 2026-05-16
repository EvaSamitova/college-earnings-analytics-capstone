# ==============================================================
# DA 485 Project – Program Type Visualization (Eva Samitova)
# ==============================================================
# Purpose: Compare 10-Year Median Earnings between Business vs Healthcare Programs
# ==============================================================

# --- Step 0: Load Required Packages ---
library(tidyverse)
library(readxl)
library(janitor)

# --- Step 1: Load Dataset ---
file_path <- "C:/Users/User/OneDrive/__Личное__/BELLEVUE COLLEGE/Capstone485/capstone(1).xlsx"
dat <- read_excel(file_path) %>% clean_names()

# --- Step 2: Verify Column Names ---
names(dat)
# should show: health, business, yr10_salary, grad_debt, net_price, avg_sat, retention, grad_rate, etc.

# --- Step 3: Make sure numeric columns are truly numeric ---
num_cols <- c("health","business","yr10_salary","grad_debt","net_price",
              "avg_sat","retention","grad_rate","pell_grant","student_loan",
              "repay","x6yr_25k")

dat <- dat %>%
  mutate(across(all_of(num_cols), as.numeric))   # safely converts everything numeric

# --- Step 4: Create program_type and filter to Business vs Healthcare only ---
df_program <- dat %>%
  mutate(
    program_type = case_when(
      business > health ~ "Business",
      health > business ~ "Healthcare",
      TRUE ~ "Mixed"
    )
  ) %>%
  filter(program_type %in% c("Business", "Healthcare"),
         !is.na(yr10_salary))

# --- Step 5: Create Boxplot of 10-Year Median Earnings ---
ggplot(df_program, aes(x = program_type, y = yr10_salary, fill = program_type)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 16, width = 0.6) +
  labs(
    title = "10-Year Median Earnings by Program Type",
    x = "Program Type",
    y = "Median Earnings (10 Years After Entry)",
    caption = "Source: U.S. Department of Education, College Scorecard"
  ) +
  theme_minimal(base_size = 13) +
  scale_fill_manual(values = c("Business" = "#1f77b4", "Healthcare" = "#ff7f0e")) +
  theme(legend.position = "none")

# --- Step 6: Summary Statistics by Program Type ---
df_program %>%
  group_by(program_type) %>%
  summarise(
    n = n(),
    mean_salary   = mean(yr10_salary, na.rm = TRUE),
    median_salary = median(yr10_salary, na.rm = TRUE),
    sd_salary     = sd(yr10_salary, na.rm = TRUE),
    q1            = quantile(yr10_salary, 0.25, na.rm = TRUE),
    q3            = quantile(yr10_salary, 0.75, na.rm = TRUE)
  )

# --- Step 7 (Optional): Statistical Comparison (t-test) ---
t.test(yr10_salary ~ program_type, data = df_program)

# --- Step 8 (Optional): Save Boxplot to File ---
ggsave("C:/Users/User/OneDrive/__Личное__/BELLEVUE COLLEGE/Capstone485/program_type_boxplot.png",
       width = 7, height = 5, dpi = 300)


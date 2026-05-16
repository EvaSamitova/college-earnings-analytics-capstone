# --- Packages ---
library(tidyverse)
library(readxl)
library(janitor)
library(recipes)
library(performance)
library(skimr)

# ========= FILE INPUT =========
file_path <- "C:/Users/User/OneDrive/__Личное__/BELLEVUE COLLEGE/Capstone485/capstone(1).xlsx"
sheet_to_read <- 1            # change if needed
out_dir <- dirname(file_path) # save outputs next to your Excel

# Read the Excel (first sheet by default)
df_raw <- readxl::read_excel(path = file_path, sheet = sheet_to_read) %>%
  janitor::clean_names()

# ========= STEP 0: Basic validation =========
df <- df_raw %>% distinct()

# quick NA %
na_pct <- sapply(df, function(x) mean(is.na(x))) %>% sort(decreasing = TRUE)
print(na_pct)

# Identify likely percent variables you may have (robust to what exists)
pct_candidates <- c("pell_grant","student_loan","repay","health","business",
                    "retention","grad_rate","x6yr_25k","gt_25k_p6","gt_25k",
                    "c150_4_pooled_supp","c150_l4_pooled_supp","ret_ft4","ret_ftl4")
pct_candidates <- intersect(pct_candidates, names(df))

# Convert 0–100 to 0–1 if needed
df <- df %>%
  mutate(across(all_of(pct_candidates),
                ~ ifelse(is.numeric(.x) & .x > 1 & .x <= 100, .x/100, .x)))

# ========= STEP 1: Transformations (log + scaling) =========
# Try to align with your likely column names
# Earnings
earn_vars <- intersect(c("x10yr_salary","md_earn_wne_p10","earn10","salary10"), names(df))
earn_var  <- if (length(earn_vars)) earn_vars[1] else NA_character_

# Other continuous drivers
avg_sat_var   <- intersect(c("avg_sat","sat_avg2","sat_avg_all","sat_composite"), names(df)) %>% {. [1]}
net_price_var <- intersect(c("net_price","avg_net_price","npt4_pub","npt4_priv","net_price2"), names(df)) %>% {. [1]}
grad_debt_var <- intersect(c("grad_debt","grad_debt_mdn_supp"), names(df)) %>% {. [1]}

log_vars <- c(earn_var, avg_sat_var, net_price_var, grad_debt_var) %>% discard(is.na)

# Dimensions (categoricals)
control_var <- intersect(c("control"), names(df)) %>% {. [1]}
degree_var  <- intersect(c("degree","preddeg"), names(df)) %>% {. [1]}
region_var  <- intersect(c("region","stabbr"), names(df)) %>% {. [1]}
program_var <- intersect(c("program_type"), names(df)) %>% {. [1]}

# Build recipe
rec <- recipe(~ ., data = df) %>%
  # create log_* safely (log1p handles zeros)
  step_mutate(across(all_of(log_vars), ~ log1p(.), .names = "log_{col}")) %>%
  # treat categoricals
  step_mutate(
    !!control_var := if (!is.na(control_var)) factor(.data[[control_var]]) else NULL,
    !!degree_var  := if (!is.na(degree_var))  factor(.data[[degree_var]])  else NULL,
    !!region_var  := if (!is.na(region_var))  factor(.data[[region_var]])  else NULL,
    !!program_var := if (!is.na(program_var)) factor(.data[[program_var]]) else NULL
  ) %>%
  # scale numeric predictors
  step_normalize(all_numeric_predictors(), -all_outcomes())

prepped <- prep(rec)
df_transformed <- bake(prepped, new_data = NULL)

# ========= STEP 2: Outliers (winsorize 1% / 99%) on original scale, then re-prep =========
winsorize <- function(x, probs = c(0.01, 0.99)) {
  if (!is.numeric(x)) return(x)
  q <- quantile(x, probs = probs, na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

winsor_vars <- c(earn_var, avg_sat_var, net_price_var, grad_debt_var) %>% discard(is.na)
df_wins <- df %>%
  mutate(across(all_of(winsor_vars), winsorize))

rec2 <- recipe(~ ., data = df_wins) %>%
  step_mutate(across(all_of(log_vars), ~ log1p(.), .names = "log_{col}")) %>%
  step_mutate(
    !!control_var := if (!is.na(control_var)) factor(.data[[control_var]]) else NULL,
    !!degree_var  := if (!is.na(degree_var))  factor(.data[[degree_var]])  else NULL,
    !!region_var  := if (!is.na(region_var))  factor(.data[[region_var]])  else NULL,
    !!program_var := if (!is.na(program_var)) factor(.data[[program_var]]) else NULL
  ) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

prepped2 <- prep(rec2)
df_ready <- bake(prepped2, new_data = NULL)

# ========= STEP 3: Categorical encoding (dummies) =========
# Drop obvious ID-ish fields if present
id_cols <- intersect(names(df_ready), c("school_id","school_name","instnm","city","state","unitid"))
df_model_input <- df_ready %>% select(-all_of(id_cols))

# Choose predictors that likely exist
proportion_vars <- intersect(c("pell_grant","pctpell","student_loan","pctfloan","repay","rpy_3yr_rt_supp",
                               "retention","ret_ft4","ret_ftl4","grad_rate","c150_4_pooled_supp",
                               "c150_l4_pooled_supp","health","business","x6yr_25k","gt_25k_p6"),
                             names(df_model_input))

log_continuous <- paste0("log_", c(avg_sat_var, net_price_var, grad_debt_var)) %>% intersect(names(df_model_input))

categoricals <- c(control_var, degree_var, region_var, program_var) %>% discard(is.na) %>% intersect(names(df_model_input))

predictors <- c(log_continuous, proportion_vars, categoricals)

x_form <- as.formula(paste("~", paste(predictors, collapse = " + ")))
X <- model.matrix(x_form, data = df_model_input) %>% as_tibble()

# Dependent y (log_earnings preferred)
log_earn_name <- paste0("log_", earn_var)
y <- if (!is.na(earn_var) && log_earn_name %in% names(df_model_input)) {
  df_model_input[[log_earn_name]]
} else if (!is.na(earn_var)) {
  log1p(df_model_input[[earn_var]])
} else {
  stop("Could not find an earnings column (e.g., 10yr_salary or md_earn_wne_p10).")
}

# ========= STEP 4: VIF =========
dat_for_vif <- bind_cols(tibble(y = y), X) %>% select(where(~ !all(is.na(.))))
vif_fit <- lm(y ~ ., data = dat_for_vif)
vif_tbl <- performance::check_collinearity(vif_fit) %>% as_tibble()
print(vif_tbl %>% arrange(desc(VIF)))

# ========= STEP 5: Descriptive summary =========
num_vars <- intersect(c(earn_var, log_earn_name, avg_sat_var, paste0("log_", avg_sat_var),
                        net_price_var, paste0("log_", net_price_var),
                        grad_debt_var, paste0("log_", grad_debt_var),
                        proportion_vars),
                      names(df_model_input))

summary_tbl <- df_model_input %>%
  select(all_of(num_vars)) %>%
  skimr::skim()

print(summary_tbl)

# ========= SAVE ARTIFACTS =========
readr::write_csv(df_model_input, file.path(out_dir, "group2_final_clean.csv"))
readr::write_csv(as_tibble(vif_tbl), file.path(out_dir, "group2_vif_report.csv"))

message("Saved: ",
        file.path(out_dir, "group2_final_clean.csv"), " and ",
        file.path(out_dir, "group2_vif_report.csv"))

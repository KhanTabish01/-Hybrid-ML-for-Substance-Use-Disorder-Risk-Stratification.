# Latent Class Analysis on NSDUH 2015-2019 Dataset - MODIFIED SCRIPT
# This script has been updated to remove specified features and add new ones.
# It now handles missing data using the Missing At Random (MAR) assumption via poLCA's na.rm = FALSE.

# --- 1. SETUP ---

# Load required libraries
library(tidyverse)    # For data manipulation (dplyr, ggplot2)
library(poLCA)        # For Latent Class Analysis
library(readr)        # For efficient data reading
library(dplyr)        # For data manipulation
library(ggplot2)      # For plotting
library(gridExtra)    # For arranging multiple ggplot objects

# Set working directory (adjust as needed)
# setwd("your/path/here")

# --- 2. DATA LOADING AND CLEANING ---

# Define columns to extract
# UPDATED: 'irwrkstat' and 'irki17_2' REMOVED.
# ADDED: 'irmaritstat', 'amdelt', 'ymdelt'.
cols <- c(
  "catag3",      # Age group
  "health",      # Health condition
  "ireduhighst2",  # Highest completed education
  "newrace2",      # Race/Ethnicity
  "irsex",       # Sex
  "irpinc3",       # Income range
  "irhhsiz2",      # Number of people in household
  "irmaritstat",   # NEW: Marital status
  "amdelt",      # NEW: Adult Lifetime Major Depressive Episode (MDE)
  "ymdelt",      # NEW: Youth Lifetime Major Depressive Episode (MDE)
  "year"         # Year (kept for context, but not in LCA)
)

# Read the data (adjust file path as needed)
# Ensure "NSDUH_2015-2019.csv" is in your working directory or provide the full path.
df <- read_csv("NSDUH_2015-2019.csv", col_select = all_of(cols))

# Data Cleaning Function for ordinal/categorical data
ord_clean_data <- function(x) {
  # Survey codes for "Bad Data", Don't Know, Skip, Refused, or Blank
  # The original function already handles -9, 85, 89, 94-99.
  # For AMDELT/YMDELT, '.' is often read as NA directly by read_csv for numeric columns.
  # If it were read as a string ".", it would need explicit handling here.
  # Assuming read_csv correctly interprets '.' as NA if the column type is numeric.
  x[x == -9 | (x >= 94 & x < 100) | x == 85 | x == 89] <- NA
  
  # Codes for "Have never done..." or "Have not done in the past X days"
  # Equivalent to 0 for numbered questions
  x[x == 91 | x == 93] <- 0
  
  return(x)
}

# Create a copy for cleaning
df2 <- df

# Apply cleaning function to all columns (as they are treated as categorical)
df2[cols] <- lapply(df2[cols], ord_clean_data)

# Special handling for binary variable: sex (1=Male, 2=Female)
# Convert to 1=Male, 0=Female for easier interpretation later
df2$irsex[df2$irsex == 2] <- 0

# NEW: Special handling for AMDELT and YMDELT (1=Yes, 2=No, '.'=NA)
# Convert 'No' (2) to 0 for consistent binary factoring
df2$amdelt[df2$amdelt == 2] <- 0
df2$ymdelt[df2$ymdelt == 2] <- 0


# --- 3. DATA PREPARATION FOR LCA ---

# Create df2_clean. All factor conversions and new variable creations will happen on this full set.
df2_clean <- df2

# Create categorical factor variables for analysis
# Age groups - MODIFIED: Added 'levels' argument for robustness
df2_clean$age_cat <- factor(df2_clean$catag3,
                            levels = c(1, 2, 3, 4, 5),
                            labels = c("12-17", "18-25", "26-34", "35-49", "50+"))

# Recode the 11 education levels into 5 groups before applying labels.
df2_clean <- df2_clean %>%
  mutate(education_group = case_when(
    ireduhighst2 <= 6    ~ 1, # Less than High School Diploma
    ireduhighst2 %in% c(7, 8) ~ 2, # HS Grad / GED
    ireduhighst2 == 9    ~ 3, # Some College
    ireduhighst2 == 10   ~ 4, # Associate's Degree
    ireduhighst2 == 11   ~ 5  # College Grad or higher
  ))

# Now create the education factor with the correct number of labels - MODIFIED: Added 'levels' argument for robustness
df2_clean$education_cat <- factor(df2_clean$education_group,
                                  levels = c(1, 2, 3, 4, 5),
                                  labels = c("Less than HS", "HS Grad/GED",
                                             "Some College", "Associate's Degree", "College+"))


# Race/ethnicity - MODIFIED: Added 'levels' argument for robustness
df2_clean$race_cat <- factor(df2_clean$newrace2,
                             levels = c(1, 2, 3, 4, 5, 6, 7),
                             labels = c("NonHisp White", "NonHisp Black/Afr Am", "NonHisp Native Am/AK Native",
                                        "NonHisp Native HI/Other Pac Isl", "NonHisp Asian",
                                        "NonHisp more than one race", "Hispanic"))

# Health status - MODIFIED: Added 'levels' argument for robustness
df2_clean$health_cat <- factor(df2_clean$health,
                               levels = c(1, 2, 3, 4, 5),
                               labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"))

# MODIFIED: Corrected recoding of the 7 income levels into 4 groups to match labels.
df2_clean <- df2_clean %>%
  mutate(income_group = case_when(
    irpinc3 %in% c(1, 2)     ~ 1, # Less than $20,000 (i.e., <$10K and $10K-$19K)
    irpinc3 %in% c(3, 4, 5) ~ 2, # $20,000 - $49,999 (i.e., $20K-$29K, $30K-$39K, $40K-$49K)
    irpinc3 == 6            ~ 3, # $50,000 - $74,999
    irpinc3 == 7            ~ 4  # $75,000 or more
  ))

# Now create the income factor with the correct number of labels - MODIFIED: Added 'levels' argument for robustness
df2_clean$income_cat <- factor(df2_clean$income_group,
                               levels = c(1, 2, 3, 4),
                               labels = c("< $20K", "$20K-$49K", "$50K-$74K", "$75K+")) # Labels adjusted to match new grouping

# Household size categories
df2_clean$household_size <- cut(df2_clean$irhhsiz2,
                                breaks = c(0, 1, 2, 4, max(df2_clean$irhhsiz2, na.rm = TRUE)), # Use max for upper bound
                                labels = c("1", "2", "3-4", "5+"),
                                right = TRUE, include.lowest = TRUE) # Include 0 for break 0

# Sex - MODIFIED: Added 'levels' argument for robustness
df2_clean$sex <- factor(df2_clean$irsex,
                        levels = c(0, 1), # 0 for Female, 1 for Male
                        labels = c("Female", "Male"))

# NEW: Marital Status - MODIFIED: Added 'levels' argument for robustness
# Levels: 1=Married, 2=Widowed, 3=Divorced or Separated, 4=Never Been Married
df2_clean$marital_status_cat <- factor(df2_clean$irmaritstat,
                                       levels = c(1, 2, 3, 4),
                                       labels = c("Married", "Widowed", "Divorced/Separated", "Never Married"))

# NEW: Adult MDE (1=Yes, 0=No, NA for aged 12-17/Unknown) - MODIFIED: Added 'levels' argument for robustness
df2_clean$mde_adult_cat <- factor(df2_clean$amdelt,
                                  levels = c(0, 1), # 0 for No MDE, 1 for Yes MDE
                                  labels = c("No MDE (Adult)", "Yes MDE (Adult)"))

# NEW: Youth MDE (1=Yes, 0=No, NA for aged 18+/Unknown) - MODIFIED: Added 'levels' argument for robustness
df2_clean$mde_youth_cat <- factor(df2_clean$ymdelt,
                                  levels = c(0, 1), # 0 for No MDE, 1 for Yes MDE
                                  labels = c("No MDE (Youth)", "Yes MDE (Youth)"))

# NEW: Create a combined Lifetime MDE variable to handle age-specific NAs
# This variable will take the MDE status relevant to the respondent's age group.
df2_clean <- df2_clean %>%
  mutate(mde_lifetime_cat = case_when(
    age_cat == "12-17" ~ mde_youth_cat, # For youth (12-17), use YMDELT
    age_cat %in% c("18-25", "26-34", "35-49", "50+") ~ mde_adult_cat, # For adults (18+), use AMDELT
    TRUE ~ NA_character_ # Should ideally not hit this if age_cat is always defined
  ))

# Convert the new combined MDE variable to a factor
df2_clean$mde_lifetime_cat <- factor(df2_clean$mde_lifetime_cat,
                                     levels = c("No MDE (Adult)", "Yes MDE (Adult)", "No MDE (Youth)", "Yes MDE (Youth)"),
                                     labels = c("No MDE", "Yes MDE", "No MDE", "Yes MDE")) # Map both to general "No MDE", "Yes MDE"

# Define the variables that will be used in LCA
lca_vars_to_use <- c("age_cat", "education_cat", "race_cat", "health_cat", "income_cat",
                     "marital_status_cat", "mde_lifetime_cat", "household_size", "sex")

# Instead of dropping NA here, poLCA will handle NAs with na.rm=FALSE
# We still need to select these columns to create lca_data.
lca_vars <- df2_clean %>%
  dplyr::select(all_of(lca_vars_to_use))

# Convert all variables to numeric factors starting from 1 (as required by poLCA)
# Note: When na.rm = FALSE, poLCA expects NAs in the data.
# The as.numeric conversion of factors will turn NA factor levels into actual NA numeric values.
lca_data <- lca_vars %>%
  mutate_all(as.numeric)

# Check the data structure to ensure all variables are numeric
str(lca_data)
summary(lca_data)

# Prepare the formula for poLCA
# UPDATED: Formula reflects the new set of variables for LCA, using the combined MDE
lca_formula <- cbind(age_cat, education_cat, race_cat, health_cat, income_cat,
                     marital_status_cat, mde_lifetime_cat, household_size, sex) ~ 1

# --- 4. LATENT CLASS ANALYSIS ---

# Improved function with multiple random starts and better error handling
run_lca <- function(nclass, data, formula, seed = 123, nrep = 5) {
  set.seed(seed)
  tryCatch({
    # MODIFIED: na.rm = FALSE to handle NAs under MAR assumption
    poLCA(formula, data = data, nclass = nclass, nrep = nrep,
          maxiter = 5000, tol = 1e-6, verbose = TRUE, na.rm = FALSE)
  }, error = function(e) {
    message(paste("Error with", nclass, "classes:", e$message))
    return(NULL)
  })
}

# Function to calculate entropy correctly with comprehensive error handling
calculate_entropy <- function(model) {
  tryCatch({
    if (is.null(model) || is.null(model$posterior) || !is.matrix(model$posterior) ||
        nrow(model$posterior) == 0 || ncol(model$posterior) == 0) {
      return(NA_real_)
    }
    
    posterior <- model$posterior
    N <- nrow(posterior)
    nclass <- ncol(posterior)
    
    if (N <= 0 || nclass <= 1) {
      return(NA_real_)
    }
    
    posterior_clean <- pmax(posterior, 1e-10) # Avoid log(0)
    observed_entropy <- -sum(posterior_clean * log(posterior_clean))
    max_possible_entropy <- N * log(nclass)
    
    if (max_possible_entropy <= 0) {
      return(NA_real_)
    }
    
    entropy <- 1 - (observed_entropy / max_possible_entropy)
    entropy <- pmax(0, pmin(1, entropy)) # Ensure entropy is between 0 and 1
    
    return(entropy)
    
  }, error = function(e) {
    message(paste("Error calculating entropy:", e$message))
    return(NA_real_)
  })
}

# Fit LCA models with 2 to 8 classes (as requested)
cat("Fitting LCA models with 2-8 classes...\n")
lca_models <- list()
for (k in 2:8) { # Loop from 2 to 5 classes
  cat(paste("Fitting model with", k, "classes...\n"))
  lca_models[[as.character(k)]] <- run_lca(k, lca_data, lca_formula)
}

# Filter out NULL models (failed fits)
successful_models <- lca_models[!sapply(lca_models, is.null)]

# Check if we have any successful models
if (length(successful_models) == 0) {
  cat("No models were successfully fitted.\n")
  stop("Analysis halted due to model fitting errors")
}

# Extract and display fit statistics with safer entropy calculation
fit_stats <- data.frame(
  nclass = as.numeric(names(successful_models)),
  log_likelihood = sapply(successful_models, function(x) {
    if (!is.null(x$llik)) as.numeric(x$llik) else NA
  }),
  aic = sapply(successful_models, function(x) {
    if (!is.null(x$aic)) as.numeric(x$aic) else NA
  }),
  bic = sapply(successful_models, function(x) {
    if (!is.null(x$bic)) as.numeric(x$bic) else NA
  }),
  n_params = sapply(successful_models, function(x) {
    if (!is.null(x$npar)) as.numeric(x$npar) else NA
  }),
  entropy = sapply(successful_models, calculate_entropy)
)

# Order by number of classes
fit_stats <- fit_stats[order(fit_stats$nclass), ]

# --- ADDED: Calculate CAIC and Rate of Change, and Generate Model Selection Plot ---
# Calculate CAIC for each model: CAIC = -2*log-likelihood + n_params*(log(N) + 1)
N_obs <- nrow(lca_data)
fit_stats$caic <- -2 * fit_stats$log_likelihood + fit_stats$n_params * (log(N_obs) + 1)

# Calculate rate of change for each criterion (as percent change from previous)
calc_rate <- function(x) c(NA, diff(x) / head(x, -1) * 100)
fit_stats$aic_rate <- calc_rate(fit_stats$aic)
fit_stats$bic_rate <- calc_rate(fit_stats$bic)
fit_stats$caic_rate <- calc_rate(fit_stats$caic)

# Prepare data for plotting (long format for ggplot2)
library(tidyr)
fit_stats_long <- fit_stats %>%
  pivot_longer(cols = c(aic, bic, caic), names_to = "criterion", values_to = "value")

# For rate of change, use AIC rate (can be changed to BIC/CAIC if preferred)
fit_stats$rate_of_change <- fit_stats$aic_rate

# Plot AIC, BIC, CAIC as bars and rate of change as a line
p_criteria <- ggplot(fit_stats_long, aes(x = nclass, y = value, fill = criterion)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("aic" = "dodgerblue", "bic" = "coral", "caic" = "gray60"),
                    labels = c("AIC", "BIC", "CAIC")) +
  geom_line(data = fit_stats, aes(x = nclass, y = (rate_of_change/100) * (max(aic, na.rm=TRUE) - min(aic, na.rm=TRUE)) + min(aic, na.rm=TRUE)), 
            color = "gold", size = 1.2, inherit.aes = FALSE) +
  geom_point(data = fit_stats, aes(x = nclass, y = (rate_of_change/100) * (max(aic, na.rm=TRUE) - min(aic, na.rm=TRUE)) + min(aic, na.rm=TRUE)), 
             color = "gold", size = 3, inherit.aes = FALSE) +
  geom_text(data = fit_stats, aes(x = nclass, y = (rate_of_change/100) * (max(aic, na.rm=TRUE) - min(aic, na.rm=TRUE)) + min(aic, na.rm=TRUE), 
                                  label = ifelse(!is.na(rate_of_change), sprintf("%.2f%%", rate_of_change), "")), 
            vjust = -1, color = "goldenrod4", size = 4, inherit.aes = FALSE) +
  scale_y_continuous(
    name = "Information Criteria",
    sec.axis = sec_axis(~ (. - min(fit_stats$aic, na.rm=TRUE)) / (max(fit_stats$aic, na.rm=TRUE) - min(fit_stats$aic, na.rm=TRUE)) * 3, 
                        name = "Rate of change", labels = scales::percent_format(accuracy = 0.01))
  ) +
  labs(title = "Model Selection Criteria and Rate of Change",
       x = "Number of clusters",
       fill = "Criterion") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

print(p_criteria)
ggsave("modified_lca_model_selection_criteria.png", p_criteria, width = 10, height = 6)
# --- END ADDED ---

cat("\nModel Fit Statistics:\n")
print(fit_stats)

# Plot AIC and BIC to visually inspect the best number of classes
p_fit <- ggplot(fit_stats, aes(x = nclass)) +
  geom_line(aes(y = aic, color = "AIC"), linewidth = 1) +
  geom_point(aes(y = aic, color = "AIC"), size = 3) +
  geom_line(aes(y = bic, color = "BIC"), linewidth = 1) +
  geom_point(aes(y = bic, color = "BIC"), size = 3) +
  scale_color_manual(values = c("AIC" = "blue", "BIC" = "red")) +
  labs(title = "Model Fit Statistics", x = "Number of Classes", y = "Value", color = "Metric") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

print(p_fit)



# Calculate rate of change for AIC (as percent change from previous model)
# Note: The poLCA models start from k=2, so the first rate is NA.
calc_rate <- function(x) c(NA, diff(x) / abs(head(x, -1)) * 100)
fit_stats <- fit_stats %>%
  mutate(aic_rate_of_change = calc_rate(aic))

# Prepare data for the bar plots (AIC and BIC)
fit_stats_long_bars <- fit_stats %>%
  select(nclass, aic, bic) %>%
  pivot_longer(
    cols = c(aic, bic),
    names_to = "criterion",
    values_to = "value"
  )

# --- Plotting ---
# We will plot AIC/BIC as bars on the primary Y-axis.
# We will plot Entropy and Rate of Change as lines on a secondary Y-axis.

# 1. Determine the range of the primary and secondary axes
primary_min <- min(fit_stats_long_bars$value, na.rm = TRUE)
primary_max <- max(fit_stats_long_bars$value, na.rm = TRUE)

secondary_max <- max(c(fit_stats$entropy, fit_stats$aic_rate_of_change / 100), na.rm = TRUE)
secondary_max <- secondary_max * 1.1 # Add 10% buffer

# 2. Calculate the scaling factor to map the secondary axis to the primary axis
scale_factor <- (primary_max - primary_min) / secondary_max

# 3. Create the plot
p_combined_criteria <- ggplot(fit_stats_long_bars, aes(x = nclass)) +
  
  # --- Primary Axis: Bars for AIC and BIC ---
  geom_bar(
    aes(y = value, fill = criterion),
    stat = "identity",
    position = position_dodge(width = 0.9)
  ) +
  
  # --- Secondary Axis: Lines for Entropy and Rate of Change ---
  # Add Entropy Line (scaled to primary axis)
  geom_line(
    data = fit_stats,
    aes(y = entropy * scale_factor + primary_min, color = "Entropy"),
    linewidth = 1.2
  ) +
  geom_point(
    data = fit_stats,
    aes(y = entropy * scale_factor + primary_min, color = "Entropy"),
    size = 3
  ) +
  
  # Add Rate of Change Line (scaled to primary axis)
  geom_line(
    data = fit_stats,
    aes(y = (aic_rate_of_change / 100) * scale_factor + primary_min, color = "Rate of Change"),
    linewidth = 1.2,
    linetype = "dashed" # Use a dashed line to distinguish it
  ) +
  # Add text labels for the Rate of Change
  geom_text(
    data = fit_stats,
    aes(
      y = (aic_rate_of_change / 100) * scale_factor + primary_min,
      label = ifelse(!is.na(aic_rate_of_change), sprintf("%.2f%%", aic_rate_of_change), "")
    ),
    vjust = -1.5, # Adjust vertical position
    color = "black",
    size = 3.5
  ) +
  
  # --- Scales and Labels ---
  # Define the primary and secondary Y-axes
  scale_y_continuous(
    name = "Information Criteria (AIC & BIC)",
    sec.axis = sec_axis(
      trans = ~ (. - primary_min) / scale_factor,
      name = "Entropy / Rate of Change",
      labels = scales::percent_format(accuracy = 1)
    )
  ) +
  
  # Manually set colors and labels for everything
  scale_fill_manual(
    name = "Criteria",
    values = c("aic" = "dodgerblue", "bic" = "coral"),
    labels = c("AIC", "BIC")
  ) +
  scale_color_manual(
    name = "Metrics",
    values = c("Entropy" = "darkgreen", "Rate of Change" = "gold"),
    labels = c("Entropy", "Rate of Change (AIC)")
  ) +
  
  labs(
    title = "Model Fit Comparison: AIC, BIC, Entropy, and Rate of Change",
    x = "Number of Classes",
    y = "Information Criteria (AIC & BIC)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.box = "horizontal"
  )

# Print and save the plot
print(p_combined_criteria)
ggsave("modified_lca_combined_criteria_plot.png", p_combined_criteria, width = 12, height = 7, dpi = 300)

# --- END ADDED CODE ---




# --- 5. INTERPRET RESULTS ---

# Select the best model based on the lowest BIC
best_k_index <- which.min(fit_stats$bic)
best_k <- fit_stats$nclass[best_k_index]
best_model <- successful_models[[as.character(best_k)]]

cat(paste("\nBest model has", best_k, "classes based on BIC.\n"))
cat(paste("Entropy:", round(fit_stats$entropy[best_k_index], 3), "\n"))

# Display the results for the best model
cat("\n=== BEST MODEL RESULTS ===\n")
print(best_model)

# Extract class probabilities
class_probs <- best_model$P
cat(paste("\nClass Sizes (n =", sum(best_model$N), "):\n"))
for (i in 1:best_k) {
  cat(paste("Class", i, ":", round(class_probs[i] * 100, 1), "%\n"))
}

# --- 6. PROFILING AND VISUALIZATION ---

# Assign individuals to their most likely class
# When na.rm = FALSE, predclass will have NA for rows with missing data.
# We need to assign it back to the original df2_clean based on row order.
df2_clean$latent_class <- best_model$predclass

# Create comprehensive profile plot function
# Data for plots should now be df2_clean, but only for complete cases to avoid NA issues in plotting.
# This will effectively plot profiles for the cases that poLCA used to fit the model.
plot_class_profiles <- function(data, var_name, title) {
  data_for_plot <- data %>%
    drop_na(latent_class, !!sym(var_name)) # Drop NAs only for plotting
  
  if (nrow(data_for_plot) == 0) {
    message(paste("No complete cases for plotting:", var_name))
    return(ggplot() + labs(title = paste("No data to plot for", title)))
  }
  
  data_for_plot %>%
    group_by(latent_class, !!sym(var_name)) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(latent_class) %>%
    mutate(prop = n / sum(n)) %>%
    ggplot(aes(x = factor(latent_class), y = prop, fill = !!sym(var_name))) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = title, x = "Latent Class", y = "Proportion", fill = gsub("_cat", "", var_name)) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right",
          plot.title = element_text(size = 14, face = "bold"))
}

# Generate profile plots for key variables
profile_vars <- c("age_cat", "education_cat", "income_cat", "health_cat", "race_cat",
                  "marital_status_cat", "mde_lifetime_cat",
                  "household_size", "sex")

profile_plots <- lapply(profile_vars, function(var) {
  plot_class_profiles(df2_clean, var, paste("Distribution of", gsub("_cat", "", var), "by Class"))
})

# Create individual plots for saving
p_age <- plot_class_profiles(df2_clean, "age_cat", "Distribution of Age by Class")
p_edu <- plot_class_profiles(df2_clean, "education_cat", "Distribution of Education by Class")
p_income <- plot_class_profiles(df2_clean, "income_cat", "Distribution of Income by Class")
p_health <- plot_class_profiles(df2_clean, "health_cat", "Distribution of Health by Class")
p_race <- plot_class_profiles(df2_clean, "race_cat", "Distribution of Race by Class")
p_marital <- plot_class_profiles(df2_clean, "marital_status_cat", "Distribution of Marital Status by Class")
p_mde_lifetime <- plot_class_profiles(df2_clean, "mde_lifetime_cat", "Distribution of Lifetime MDE by Class")
p_household_size <- plot_class_profiles(df2_clean, "household_size", "Distribution of Household Size by Class")
p_sex <- plot_class_profiles(df2_clean, "sex", "Distribution of Sex by Class")


# Display all profile plots in a grid (adjust ncol as needed for readability)
grid.arrange(grobs = profile_plots, ncol = 3)

# --- 7. SAVE RESULTS ---

cat("\nSaving results...\n")
# Save the best model object for later use
saveRDS(best_model, "modified_lca_best_model.rds")

# Save the dataframe that includes the latent class assignments
# This will now include NAs for individuals with missing data in manifest variables
write_csv(df2_clean, "modified_lca_nsduh_with_classes.csv")

# Save the table of fit statistics
write_csv(fit_stats, "modified_lca_fit_statistics.csv")

# Save plots
ggsave("modified_lca_model_fit_statistics.png", p_fit, width = 10, height = 6)
ggsave("modified_lca_age_by_class.png", p_age, width = 10, height = 6)
ggsave("modified_lca_education_by_class.png", p_edu, width = 10, height = 6)
ggsave("modified_lca_income_by_class.png", p_income, width = 10, height = 6)
ggsave("modified_lca_health_by_class.png", p_health, width = 10, height = 6)
ggsave("modified_lca_race_by_class.png", p_race, width = 10, height = 6)
ggsave("modified_lca_marital_status_by_class.png", p_marital, width = 10, height = 6)
ggsave("modified_lca_mde_lifetime_by_class.png", p_mde_lifetime, width = 10, height = 6)
ggsave("modified_lca_household_size_by_class.png", p_household_size, width = 10, height = 6)
ggsave("modified_lca_sex_by_class.png", p_sex, width = 10, height = 6)

cat("Analysis complete! Files saved with 'modified_lca_' prefix.\n")
cat("- modified_lca_best_model.rds: Best LCA model object\n")
cat("- modified_lca_nsduh_with_classes.csv: Data with class assignments (includes NAs for missing data)\n")
cat("- modified_lca_fit_statistics.csv: Model fit statistics\n")
cat("- modified_lca_model_fit_statistics.png: AIC/BIC comparison plot\n")
cat("- All profile plots for the new feature set (e.g., modified_lca_age_by_class.png, modified_lca_marital_status_by_class.png, etc.)\n")

# --- 8. ADDITIONAL SUMMARY STATISTICS ---

# Create a summary table of class characteristics
cat("\n=== CLASS PROFILES SUMMARY ===\n")

# For summary table, we should still use complete cases to make sense of modal categories
# as poLCA's predclass will be NA for cases with missing manifest variables.
class_summary_data <- df2_clean %>%
  drop_na(latent_class) # Only use rows where latent class was assigned

# Function to get modal category for each class
get_modal_category <- function(data, var_name, class_var = "latent_class") {
  data %>%
    group_by(!!sym(class_var), !!sym(var_name)) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(!!sym(class_var)) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    pull(!!sym(var_name))
}

# Create class profile summary
class_summary <- data.frame(
  Class = 1:best_k,
  Size_Percent = round(class_probs * 100, 1),
  Modal_Age = get_modal_category(class_summary_data, "age_cat"),
  Modal_Education = get_modal_category(class_summary_data, "education_cat"),
  Modal_Income = get_modal_category(class_summary_data, "income_cat"),
  Modal_Health = get_modal_category(class_summary_data, "health_cat"),
  Modal_Race = get_modal_category(class_summary_data, "race_cat"),
  Modal_MaritalStatus = get_modal_category(class_summary_data, "marital_status_cat"),
  Modal_MDELifetime = get_modal_category(class_summary_data, "mde_lifetime_cat"),
  Modal_HouseholdSize = get_modal_category(class_summary_data, "household_size"),
  Modal_Sex = get_modal_category(class_summary_data, "sex")
)

print(class_summary)

# Save class summary
write_csv(class_summary, "modified_lca_class_summary.csv")
cat("- modified_lca_class_summary.csv: Summary of class characteristics\n")





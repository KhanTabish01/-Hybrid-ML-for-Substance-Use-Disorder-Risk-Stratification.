# --- PRE-REQUISITES ---
# This script assumes you have:
# 1. Run the original LCA script from the PDF.
# 2. Saved the 4-class model as "all_lca_models/lca_model_4_classes.rds".
# 3. Have the original "NSDUH_2015-2019.csv" file in your working directory.


# --- 1. LOAD LIBRARIES AND DATA ---
# Load necessary libraries
library(tidyverse)
library(readr)
library(poLCA)
library(caret) # Used for one-hot encoding
library(ggplot2) # Added for visualization

# Announce the start of the process
cat("--- Step 1: Preparing Data for Modeling (with Feature Engineering) ---\n")

# --- A. Load the 4-Class LCA Model ---
cat("Loading the saved 4-class LCA model...\n")
tryCatch({
  four_class_model <- readRDS("all_lca_models/lca_model_4_classes.rds")
}, error = function(e) {
  stop("Error: Could not find 'all_lca_models/lca_model_4_classes.rds'. Please ensure you have run the previous scripts to save the model.")
})


# --- B. EFFICIENTLY LOAD OR CREATE CLEANED DATA ---
cleaned_data_file <- "cleaned_factored_data.rds"

if (file.exists(cleaned_data_file)) {
  cat("Found existing cleaned data file. Loading 'cleaned_factored_data.rds'...\n")
  df2_clean <- readRDS(cleaned_data_file)
} else {
  cat("No cleaned data file found. Creating it from scratch...\n")
  
  # --- Data Loading and Cleaning ---
  cat("Loading and cleaning the original NSDUH dataset...\n")
  cols_lca <- c("catag3", "health", "ireduhighst2", "newrace2", "irsex", "irpinc3", 
                "irhhsiz2", "irmaritstat", "amdelt", "ymdelt")
  df <- read_csv("NSDUH_2015-2019.csv", col_select = all_of(cols_lca))
  
  ord_clean_data <- function(x) {
    x[x == -9 | (x >= 94 & x < 100) | x == 85 | x == 89] <- NA
    x[x == 91 | x == 93] <- 0
    return(x)
  }
  df2 <- as.data.frame(lapply(df, ord_clean_data))
  df2$irsex[df2$irsex == 2] <- 0
  df2$amdelt[df2$amdelt == 2] <- 0
  df2$ymdelt[df2$ymdelt == 2] <- 0
  
  # --- Factor Creation ---
  cat("Creating factor variables for predictors...\n")
  df2_clean <- df2 %>%
    mutate(
      age_cat = factor(catag3, levels = 1:5, labels = c("12-17", "18-25", "26-34", "35-49", "50+")),
      education_group = case_when(
        ireduhighst2 <= 6 ~ 1, ireduhighst2 %in% c(7, 8) ~ 2,
        ireduhighst2 == 9 ~ 3, ireduhighst2 == 10 ~ 4, ireduhighst2 == 11 ~ 5),
      education_cat = factor(education_group, levels = 1:5, labels = c("Less than HS", "HS Grad/GED", "Some College", "Associate's Degree", "College+")),
      race_cat = factor(newrace2, levels = 1:7, labels = c("NonHisp White", "NonHisp Black/Afr Am", "NonHisp Native Am/AK Native", "NonHisp Native HI/Other Pac Isl", "NonHisp Asian", "NonHisp more than one race", "Hispanic")),
      health_cat = factor(health, levels = 1:5, labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
      income_group = case_when(
        irpinc3 %in% 1:2 ~ 1, irpinc3 %in% 3:5 ~ 2,
        irpinc3 == 6 ~ 3, irpinc3 == 7 ~ 4),
      income_cat = factor(income_group, levels = 1:4, labels = c("<$20K", "$20K-$49K", "$50K-$74K", "$75K+")),
      household_size = cut(irhhsiz2, breaks = c(0, 1, 2, 4, max(irhhsiz2, na.rm = TRUE)), labels = c("1", "2", "3-4", "5+"), right = TRUE, include.lowest = TRUE),
      sex = factor(irsex, levels = 0:1, labels = c("Female", "Male")),
      marital_status_cat = factor(irmaritstat, levels = 1:4, labels = c("Married", "Widowed", "Divorced/Separated", "Never Married")),
      mde_adult_cat = factor(amdelt, levels = 0:1, labels = c("No MDE (Adult)", "Yes MDE (Adult)")),
      mde_youth_cat = factor(ymdelt, levels = 0:1, labels = c("No MDE (Youth)", "Yes MDE (Youth)")),
      mde_lifetime_cat = factor(case_when(
        age_cat == "12-17" ~ as.character(mde_youth_cat),
        age_cat %in% c("18-25", "26-34", "35-49", "50+") ~ as.character(mde_adult_cat),
        TRUE ~ NA_character_
      ), levels = c("No MDE (Adult)", "Yes MDE (Adult)", "No MDE (Youth)", "Yes MDE (Youth)"), labels = c("No MDE", "Yes MDE", "No MDE", "Yes MDE"))
    )
  
  cat("Saving the cleaned data to 'cleaned_factored_data.rds' for faster access next time.\n")
  saveRDS(df2_clean, cleaned_data_file)
}


# --- C. Add Target Variable and Assign Classes ---
cat("Loading the target variable 'udpyilal'...\n")
target_var_df <- read_csv("NSDUH_2015-2019.csv", col_types = cols_only(udpyilal = "d"))

cat("Assigning latent classes and adding target variable...\n")
df2_clean$latent_class <- four_class_model$predclass
df2_clean$udpyilal <- target_var_df$udpyilal

# Clean the target variable
df2_clean <- df2_clean %>%
  mutate(udpyilal = case_when(
    udpyilal == 1 ~ 1,
    udpyilal == 0 ~ 0,
    TRUE ~ NA_integer_
  ))

# --- NEW: 2. CREATE HIGH-IMPACT INTERACTION FEATURES ---
cat("Engineering new interaction features...\n")
df2_clean <- df2_clean %>%
  mutate(
    # Interaction: Being male and having a history of MDE
    is_male_with_mde = if_else(sex == "Male" & mde_lifetime_cat == "Yes MDE", 1, 0),
    # Interaction: Being a young adult (18-25) with a history of MDE
    is_young_with_mde = if_else(age_cat == "18-25" & mde_lifetime_cat == "Yes MDE", 1, 0),
    # Interaction: Being an adolescent in a disrupted family environment
    is_adolescent_family_disruption = if_else(age_cat == "12-17" & marital_status_cat == "Divorced/Separated", 1, 0)
  ) %>%
  # Replace any NAs created in the new columns with 0
  mutate(across(starts_with("is_"), ~if_else(is.na(.), 0, .)))


# Remove rows where the target or latent class is missing
df_model_ready <- df2_clean %>%
  filter(!is.na(latent_class) & !is.na(udpyilal))


# --- 3. ONE-HOT ENCODE PREDICTOR VARIABLES ---
cat("One-hot encoding original predictor variables...\n")
lca_vars_to_use <- c("age_cat", "education_cat", "race_cat", "health_cat", "income_cat",
                     "marital_status_cat", "mde_lifetime_cat", "household_size", "sex")
dmy <- dummyVars(~ ., data = df_model_ready[, lca_vars_to_use], fullRank = TRUE)
df_encoded_predictors <- data.frame(predict(dmy, newdata = df_model_ready[, lca_vars_to_use]))

# Combine encoded predictors, new interaction features, and the target/class variables
df_final_for_modeling <- cbind(
  df_encoded_predictors,
  df_model_ready[, c("is_male_with_mde", "is_young_with_mde", "is_adolescent_family_disruption")],
  latent_class = df_model_ready$latent_class,
  udpyilal = df_model_ready$udpyilal
)
cat("Data has been successfully encoded with new features.\n")


# --- 4. SPLIT AND SAVE DATA FOR EACH CLASS ---
cat("Splitting and saving data for each of the 4 classes...\n")
dir.create("data", showWarnings = FALSE)
for (i in 1:4) {
  class_data <- df_final_for_modeling %>%
    filter(latent_class == i) %>%
    dplyr::select(-latent_class)
  
  file_path <- file.path("data", paste0("class_", i, "_modeling_data.csv"))
  write_csv(class_data, file_path)
  cat(paste(" - Saved data for Class", i, "to:", file_path, "(", nrow(class_data), "rows)\n"))
}

# --- NEW: 5. VISUALIZE FINAL CLASS SIZES ---
cat("\n--- Visualizing Final Class Sizes for Modeling ---\n")

# Create a summary dataframe of the final counts per class
class_counts <- df_model_ready %>%
  count(latent_class) %>%
  mutate(latent_class = as.factor(latent_class))

# Create the bar plot
final_counts_plot <- ggplot(class_counts, aes(x = latent_class, y = n, fill = latent_class)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.5) + # Add count labels on top of bars
  labs(
    title = "Final Number of Observations per Class for Modeling",
    x = "Latent Class",
    y = "Number of Individuals"
  ) +
  theme_minimal()

# Print the plot to the RStudio Viewer
print(final_counts_plot)

cat("\n--- Step 1 Complete. All data files saved in the 'data' folder. ---\n")





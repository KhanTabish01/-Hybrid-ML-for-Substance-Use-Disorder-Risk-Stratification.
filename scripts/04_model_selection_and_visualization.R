# --- PRE-REQUISITES ---
# This script assumes you have:
# 1. The 'cleaned_factored_data.rds' file from Step 1.
# 2. The saved 4-class model at "all_lca_models/lca_model_4_classes.rds".
# 3. The original "NSDUH_2015-2019.csv" file.

# --- 1. LOAD LIBRARIES ---
# You may need to install gtsummary: install.packages("gtsummary")
library(tidyverse)
library(readr)
library(poLCA)
library(gtsummary)
library(gt) # Added for creating beautiful tables

# --- NEW: Check for and install the 'cardx' package if it's missing ---
# This prevents the script from pausing with an interactive installation prompt.
if (!requireNamespace("cardx", quietly = TRUE)) {
  install.packages("cardx")
}
# --- NEW: Check for and install the 'gt' package if it's missing ---
if (!requireNamespace("gt", quietly = TRUE)) {
  install.packages("gt")
}


# --- 2. CREATE TABLE 1: DESCRIPTIVE STATISTICS ---
cat("--- Generating Table 1: Descriptive Statistics ---\n")

# --- A. Load and Prepare the Data ---
# Load the cleaned data with factor variables
df_clean <- readRDS("cleaned_factored_data.rds")

# Load the target variable and add it to the dataframe
target_var_df <- read_csv("NSDUH_2015-2019.csv", col_types = cols_only(udpyilal = "d"))
df_clean$udpyilal <- target_var_df$udpyilal

# Clean the target variable and create a factor for labeling in the table
df_for_table1 <- df_clean %>%
  mutate(sud_status = case_when(
    udpyilal == 1 ~ "Yes (SUD)",
    udpyilal == 0 ~ "No (No SUD)",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(sud_status)) %>%
  mutate(sud_status = factor(sud_status, levels = c("No (No SUD)", "Yes (SUD)")))

# --- B. Generate the gtsummary Table ---
# --- FIX: Set the compact theme BEFORE creating the table ---
theme_gtsummary_compact()

# Select the key variables for the summary
descriptive_table <- df_for_table1 %>%
  select(
    age_cat, sex, education_cat, race_cat, health_cat,
    income_cat, marital_status_cat, household_size,
    mde_lifetime_cat, sud_status
  ) %>%
  # Create the summary table, stratifying by SUD status
  tbl_summary(
    by = sud_status,
    # UPDATED: Labels now include original variable names
    label = list( 
      age_cat ~ "Age Group (age_cat)",
      sex ~ "Sex (sex)",
      education_cat ~ "Education Level (education_cat)",
      race_cat ~ "Race/Ethnicity (race_cat)",
      health_cat ~ "Self-Reported Health (health_cat)",
      income_cat ~ "Income Level (income_cat)",
      marital_status_cat ~ "Marital Status (marital_status_cat)",
      household_size ~ "Household Size (household_size)",
      mde_lifetime_cat ~ "Lifetime MDE History (mde_lifetime_cat)"
    )
  ) %>%
  add_overall() %>% # Add a column for the total population
  # --- REMOVED p-value column ---
  bold_labels() %>%
  modify_caption("Table 1: Descriptive Statistics of the Sample, by SUD Status") %>%
  # --- NEW: Change the column header ---
  modify_header(label = "**Features**")

# Print the table to the console
# In RStudio, this will appear in the Viewer pane as a formatted table.
print(descriptive_table)


# --- 3. CREATE TABLE 2: CLASS DEFINITIONS ---
cat("\n\n--- Generating Table 2: Class Definitions ---\n")

# --- A. Load the Saved 4-Class LCA Model to get population shares ---
tryCatch({
  four_class_model <- readRDS("all_lca_models/lca_model_4_classes.rds")
}, error = function(e) {
  stop("Error: Could not find 'all_lca_models/lca_model_4_classes.rds'.")
})
P <- four_class_model$P

# --- B. Define the Data for the Table ---
# This data is based on our interpretation of the LCA profiles
class_definitions_df <- tibble(
  Cluster = c("Class 1", "Class 2", "Class 3", "Class 4"),
  Description = c(
    "Older Adults with Health & Economic Challenges: Primarily composed of individuals aged 50+, this group reports the poorest health, has lower levels of education (mostly high school graduates), and the lowest income (over 54% earning less than $20k). They are predominantly female and likely to be married, though this class also has a high proportion of divorced/separated individuals.",
    "Young, Unmarried Adults: This is the largest class, defined by young adults (74% are aged 18-25) who have never been married (83%). While they report very good health, they have low incomes (77% under $20k) and are still early in their educational journey (a mix of high school graduates and some college).",
    "Established, High-SES Married Males: This group represents highly educated (61% college graduates or higher) and higher-income, middle-aged (35-49) males. They are overwhelmingly Non-Hispanic White (76%), married (70%), and report very good health.",
    "Adolescents in Larger Households: This class is almost exclusively composed of adolescents aged 12-17 (97%) who are still in school and living in larger households (94% have 3+ people). As expected, they are never married and report low family income. This group has a notable probability of having experienced a Major Depressive Episode."
  ),
  Percentage = c(
    sprintf("%.1f%%", P[1] * 100),
    sprintf("%.1f%%", P[2] * 100),
    sprintf("%.1f%%", P[3] * 100),
    sprintf("%.1f%%", P[4] * 100)
  )
)

# --- C. Create a Formatted Table with the 'gt' Package ---
class_definitions_table <- class_definitions_df %>%
  gt() %>%
  tab_header(
    title = "Table 2: Class Definitions and Descriptions"
  ) %>%
  cols_align(
    align = "left",
    columns = everything()
  ) %>%
  # --- NEW: Apply compact styling ---
  tab_options(
    data_row.padding = px(5),
    table.font.size = px(14)
  )

# Print the final formatted table
print(class_definitions_table)

# You can also save this table
# gtsave(class_definitions_table, "class_definitions.png")







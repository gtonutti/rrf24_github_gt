# Reproducible Research Fundamentals 
# 02. Data construction
# RRF - 2024 - Construction

# Preliminary - Load Data ----
# Load household-level data (HH)
hh_data <- read_dta(file.path(data_path, "Intermediate/TZA_CCT_HH.dta"))

# Load HH-member data
mem_data <- read_dta(file.path(data_path, "Intermediate/TZA_CCT_HH_mem.dta"))

# Load secondary data
secondary_data <- read_dta(file.path(data_path, "Intermediate/TZA_amenity_tidy.dta"))

# Exercise 1: Plan construction outputs ----
# Plan the following outputs:
# 1. Area in acres. HH data set
# 2. Household consumption (food and nonfood) in USD. HH
# 3. Any HH member sick. Member - collapsing at household level
# 4. Any HH member can read or write. Member - collapsing at household level
# 5. Average sick days. Member - collapsing at household level
# 6. Total treatment cost in USD. Member - collapsing at household level
# 7. Total medical facilities. 

# Exercise 2: Standardize conversion values ----
# Define standardized conversion values:
# 1. Conversion factor for acres.
# 2. USD conversion factor.
usd <- 0.00037
acre_conv<-2.47
# Data construction: Household (HH) ----
# Instructions:
# 1. Convert farming area to acres where necessary.
# 2. Convert household consumption for food and nonfood into USD.
hh_data<- hh_data %>%
    mutate(area_acre=case_when(
        ar_farm_unit==2 ~ ar_farm,
        ar_farm_unit==3 ~ ar_farm*acre_conv
        )) %>%
    mutate(area_acr=replace_na(area_acre,0)) %>%
    set_variable_labels(area_acre="Area farmed in acres")

hh_data<- hh_data %>%
    mutate(across(c(food_cons, nonfood_cons),
           ~ .x*usd,
           .names= "{.col}_usd"))
    
# Exercise 3: Handle outliers ----
# you can use custom Winsorization function to handle outliers.
winsor_function <- function(dataset, var, min = 0.00, max = 0.95) {
    var_sym <- sym(var)
    
    percentiles <- quantile(
        dataset %>% pull(!!var_sym), probs = c(min, max), na.rm = TRUE
    )
    
    min_percentile <- percentiles[1]
    max_percentile <- percentiles[2]
    
    dataset %>%
        mutate(
            !!paste0(var, "_w") := case_when(
                is.na(!!var_sym) ~ NA_real_,
                !!var_sym <= min_percentile ~ percentiles[1],
                !!var_sym >= max_percentile ~ percentiles[2],
                TRUE ~ !!var_sym
            )
        )
}

# Tips: Apply the Winsorization function to the relevant variables.
# Create a list of variables that require Winsorization and apply the function to each.
wins_var<-c("area_acre", "food_cons_usd", "nonfood_cons_usd")

for (var in wins_var) {
    hh_data<-winsor_function(hh_data,var)
}

# Exercise 4.1: Create indicators at household level ----
# Instructions:
# Collapse HH-member level data to HH level.
# Plan to create the following indicators:
# 1. Any member was sick.
hh_mem_collapsed <- mem_data %>%
    group_by(hhid) %>%
    summarise(
    hh_ever_sick = max(sick, na.rm = TRUE),
# 2. Any member can read/write.
    hh_read = max(read, na.rm = TRUE),
# 3. Average sick days.
    hh_avg_sick =if_else(all(is.na(days_sick)), NA_real_, mean(days_sick, na.rm = TRUE)),
# 4. Total treatment cost in USD.
    hh_treat_cost =if_else(all(is.na(treat_cost)), NA_real_, sum(treat_cost, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(treat_cost_usd = if_else(is.na(treat_cost_usd), 
                                    mean(treat_cost_usd, na.rm = TRUE), 
                                    treat_cost_usd)) %>%
    # Apply labels to the variables
    set_variable_labels(
        read = "Any member can read/write",
        sick = "Any member was sick in the last 4 weeks",
        days_sick = "Average sick days",
        treat_cost_usd = "Total cost of treatment (USD)"
    )
# Exercise 4.2: Data construction: Secondary data ----
# Instructions:
# Calculate the total number of medical facilities by summing relevant columns.
# Apply appropriate labels to the new variables created.
secondary_data <- secondary_data %>%
    mutate(n_medical = rowSums(select(., n_clinic, n_hospital), 
                               na.rm = TRUE)) 

# Exercise 5: Merge HH and HH-member data ----
# Instructions:
# Merge the household-level data with the HH-member level indicators.
# After merging, ensure the treatment status is included in the final dataset.
merged_data<- hh_data %>%
    left_join(hh_mem_collapsed, by="hhid")
treat_status <- read_dta(file.path(data_path, "Raw/treat_status.dta"))

final_hh_data <- final_hh_data %>%
    left_join(treat_status, by = "vid") 

# Exercise 6: Save final dataset ----

# Save the final merged data for analysis
write_dta(final_hh_data, file.path(data_path, "Final/TZA_CCT_analysis.dta"))

# Save the final secondary data for analysis
write_dta(secondary_data, file.path(data_path, "Final/TZA_amenity_analysis.dta"))


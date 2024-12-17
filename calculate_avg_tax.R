# Load required packages
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# New construction value assumptions by type and tier
new_construction_values <- list(
  "Single Family" = list(
    "entry" = 450000,      # Entry-level in affordable areas
    "median" = 700000,     # Median new construction
    "premium" = 1500000    # Premium neighborhoods
  ),
  "Condo" = list(
    "mid_rise" = 525000,   # Mid-rise neighborhood developments
    "median" = 575000,     # Median new construction
    "luxury" = 700000      # Luxury high-rise units
  ),
  "Large Multi-Family (7+ units)" = list(
    "mid_market" = 312500, # Average of mid-market range ($275k-350k)
    "luxury" = 400000      # Luxury developments
  ),
  "Small Multi-Family (2-6 units)" = list(
    "mid_market" = 312500, # Using same as large multi-family
    "luxury" = 400000      # Luxury boutique developments
  )
)

# Load the base data
bedroom_data <- read.csv("property_tax_by_bedroom.csv")

# Calculate effective tax rates from existing data
effective_rates <- bedroom_data %>%
  filter(year == 2022) %>%
  group_by(property_type) %>%
  summarize(
    effective_tax_rate = mean(avg_tax_per_unit / avg_value_per_unit, na.rm = TRUE)
  )

# Create expanded dataset with new construction values and corresponding tax rates
new_construction_rates <- bedroom_data %>%
  filter(year == 2022) %>%
  mutate(
    new_construction_value = case_when(
      # Single Family
      property_type == "Single Family" & bedroom_type == "beds_2" ~ 
        new_construction_values[["Single Family"]][["entry"]],
      property_type == "Single Family" & bedroom_type == "beds_3plus" ~
        new_construction_values[["Single Family"]][["median"]],
      property_type == "Single Family" & bedroom_type == "beds_4plus" ~
        new_construction_values[["Single Family"]][["premium"]],
      
      # Condos
      property_type == "Condo" & bedroom_type == "beds_1" ~
        new_construction_values[["Condo"]][["mid_rise"]],
      property_type == "Condo" & bedroom_type == "beds_2" ~
        new_construction_values[["Condo"]][["median"]],
      property_type == "Condo" & bedroom_type == "beds_3plus" ~
        new_construction_values[["Condo"]][["luxury"]],
      
      # Large Multi-Family
      property_type == "Large Multi-Family (7+ units)" & bedroom_type %in% c("studio", "beds_1") ~
        new_construction_values[["Large Multi-Family (7+ units)"]][["mid_market"]],
      property_type == "Large Multi-Family (7+ units)" & bedroom_type %in% c("beds_2", "beds_3plus", "mixed_studio_to_3") ~
        new_construction_values[["Large Multi-Family (7+ units)"]][["luxury"]],
      
      # Small Multi-Family
      property_type == "Small Multi-Family (2-6 units)" & bedroom_type %in% c("mixed_1_to_2") ~
        new_construction_values[["Small Multi-Family (2-6 units)"]][["mid_market"]],
      property_type == "Small Multi-Family (2-6 units)" & bedroom_type %in% c("mixed_1_to_3", "beds_3plus") ~
        new_construction_values[["Small Multi-Family (2-6 units)"]][["luxury"]],
      
      TRUE ~ avg_value_per_unit  # Fallback to existing value
    )
  ) %>%
  # Join with effective tax rates
  left_join(effective_rates, by = "property_type") %>%
  # Calculate new construction tax based on value and effective rate
  mutate(
    new_construction_tax = new_construction_value * effective_tax_rate
  )

# Write the adjusted rates to a new file
write.csv(new_construction_rates, "new_construction_tax_rates.csv", row.names = FALSE)

# Print summary of new construction values and taxes
summary_stats <- new_construction_rates %>%
  group_by(property_type, bedroom_type) %>%
  summarize(
    avg_existing_value = mean(avg_value_per_unit, na.rm = TRUE),
    new_value = mean(new_construction_value, na.rm = TRUE),
    avg_existing_tax = mean(avg_tax_per_unit, na.rm = TRUE),
    new_tax = mean(new_construction_tax, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats)

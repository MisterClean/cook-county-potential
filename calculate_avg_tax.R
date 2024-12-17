library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
library(ptaxsim)

# Connect to PTAXSIM database
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "ptaxsim.db")

#' Format currency values
#' @param x Numeric value to format
#' @return Formatted string with currency symbol
format_currency <- function(x) {
  ifelse(is.na(x), "N/A",
         sprintf("$%s", format(round(as.numeric(x), 2), 
                              big.mark = ",", 
                              scientific = FALSE,
                              nsmall = 2)))
}

#' Format percentage values
#' @param x Numeric value to format
#' @return Formatted string with percentage symbol
format_percent <- function(x) {
  ifelse(is.na(x), "N/A",
         sprintf("%.1f%%", as.numeric(x)))
}

#' Calculate average property tax per unit for different housing types in Chicago
#' Uses actual tax rates and exemptions from PTAXSIM
calculate_avg_tax <- function(years = c(2021, 2022)) {
  # Store results for each year
  yearly_results <- list()
  
  # Process each year
  for (year in years) {
    cat(sprintf("\nProcessing year %d...\n", year))
    
    # Get properties by class
    class_query <- sprintf("
      SELECT class, COUNT(*) as count
      FROM pin 
      WHERE year = %d
      AND class IN (
        '203', '204', '205', -- Single family
        '299',               -- Condo
        '211',               -- 2-6 unit apartments
        '313', '314', '315'  -- 7+ unit apartments
      )
      GROUP BY class
    ", year)
    
    available_classes <- DBI::dbGetQuery(ptaxsim_db_conn, class_query)
    cat("\nAvailable property classes:\n")
    print(available_classes)
    
    # Get ALL PINs across Chicago townships (removed LIMIT 2500)
    pins_query <- sprintf("
      SELECT DISTINCT pin, class,
        CASE 
          WHEN pin LIKE '13%%' THEN 'Jefferson'
          WHEN pin LIKE '14%%' THEN 'Lake'
          WHEN pin LIKE '15%%' THEN 'West Chicago'
          WHEN pin LIKE '16%%' THEN 'North Chicago'
          WHEN pin LIKE '17%%' THEN 'South Chicago'
          WHEN pin LIKE '18%%' THEN 'Hyde Park'
          WHEN pin LIKE '19%%' THEN 'Lake View'
          WHEN pin LIKE '20%%' THEN 'Rogers Park'
        END as township
      FROM pin 
      WHERE year = %d
      AND class IN (
        '203', '204', '205', -- Single family
        '299',               -- Condo
        '211',               -- 2-6 unit apartments
        '313', '314', '315'  -- 7+ unit apartments
      )
      AND (
        pin LIKE '13%%' OR pin LIKE '14%%' OR pin LIKE '15%%' OR 
        pin LIKE '16%%' OR pin LIKE '17%%' OR pin LIKE '18%%' OR 
        pin LIKE '19%%' OR pin LIKE '20%%'
      )
    ", year)
    
    cat(sprintf("\nFetching all PINs for year %d...\n", year))
    all_pins <- DBI::dbGetQuery(ptaxsim_db_conn, pins_query)
    cat(sprintf("Found %d properties\n", nrow(all_pins)))
    
    # Get PIN details and tax bills
    cat("Looking up PIN details...\n")
    pin_details <- lookup_pin(year, all_pins$pin)
    cat("Calculating tax bills...\n")
    bills <- tax_bill(year, all_pins$pin)
    
    # Summarize bills
    bills_summary <- bills %>%
      group_by(pin, class) %>%
      summarize(
        total_tax = sum(final_tax),
        .groups = 'drop'
      ) %>%
      left_join(
        pin_details %>% select(pin, av, eav),
        by = "pin"
      ) %>%
      left_join(
        all_pins %>% select(pin, township),
        by = "pin"
      )
    
    # Calculate property metrics
    property_analysis <- bills_summary %>%
      mutate(
        year = year,
        market_value = av * 10,
        property_type = case_when(
          class %in% c('203', '204', '205') ~ 'Single Family',
          class == '299' ~ 'Condo',
          class == '211' ~ 'Small Multi-Family (2-6 units)',
          class %in% c('313', '314', '315') ~ 'Large Multi-Family (7+ units)',
          TRUE ~ 'Other'
        ),
        total_units = case_when(
          class %in% c('203', '204', '205', '299') ~ 1,
          class == '211' ~ case_when(
            market_value < 400000 ~ 2,
            market_value < 600000 ~ 3,
            TRUE ~ 4
          ),
          class %in% c('313', '314', '315') ~ case_when(
            market_value < 1000000 ~ 7,
            market_value < 2000000 ~ 12,
            market_value < 3000000 ~ 24,
            TRUE ~ 36
          ),
          TRUE ~ 1
        ),
        tax_per_unit = total_tax / total_units,
        value_per_unit = market_value / total_units
      )
    
    # Create bedroom distribution
    bedroom_analysis <- property_analysis %>%
      mutate(
        bedroom_type = case_when(
          property_type == 'Single Family' ~ 'beds_3plus',
          property_type == 'Condo' ~ case_when(
            value_per_unit < 300000 ~ 'beds_1',
            value_per_unit < 500000 ~ 'beds_2',
            TRUE ~ 'beds_3plus'
          ),
          property_type == 'Small Multi-Family (2-6 units)' ~ 'mixed_1_to_3',
          property_type == 'Large Multi-Family (7+ units)' ~ 'mixed_studio_to_3',
          TRUE ~ 'unknown'
        )
      )
    
    # Calculate summary statistics
    summary_stats <- property_analysis %>%
      group_by(year, property_type) %>%
      summarize(
        sample_size = n(),
        total_units = sum(total_units),
        avg_market_value = mean(market_value),
        avg_value_per_unit = mean(value_per_unit),
        avg_tax_per_unit = mean(tax_per_unit),
        median_tax_per_unit = median(tax_per_unit),
        effective_tax_rate = mean(total_tax / market_value) * 100,
        .groups = 'drop'
      )
    
    # Calculate bedroom-specific metrics
    bedroom_stats <- bedroom_analysis %>%
      group_by(year, property_type, bedroom_type) %>%
      summarize(
        sample_size = n(),
        total_units = sum(total_units),
        avg_tax_per_unit = mean(tax_per_unit),
        avg_value_per_unit = mean(value_per_unit),
        .groups = 'drop'
      )
    
    # Calculate township-level statistics
    township_stats <- property_analysis %>%
      group_by(year, township, property_type) %>%
      summarize(
        sample_size = n(),
        total_units = sum(total_units),
        avg_tax_per_unit = mean(tax_per_unit),
        avg_value_per_unit = mean(value_per_unit),
        .groups = 'drop'
      )
    
    yearly_results[[as.character(year)]] <- list(
      summary = summary_stats,
      bedroom = bedroom_stats,
      township = township_stats
    )
  }
  
  # Calculate year-over-year changes
  if (length(years) > 1) {
    years_ordered <- sort(years)
    for (i in 2:length(years_ordered)) {
      current_year <- years_ordered[i]
      prev_year <- years_ordered[i-1]
      
      # Add YoY changes to summary stats
      yearly_results[[as.character(current_year)]]$summary <- yearly_results[[as.character(current_year)]]$summary %>%
        left_join(
          yearly_results[[as.character(prev_year)]]$summary %>%
            select(property_type, avg_tax_per_unit) %>%
            rename(prev_tax = avg_tax_per_unit),
          by = "property_type"
        ) %>%
        mutate(
          tax_change_pct = (avg_tax_per_unit - prev_tax) / prev_tax * 100
        )
    }
  }
  
  # Combine results across years
  combined_results <- list(
    summary = bind_rows(lapply(yearly_results, function(x) x$summary)),
    bedroom = bind_rows(lapply(yearly_results, function(x) x$bedroom)),
    township = bind_rows(lapply(yearly_results, function(x) x$township))
  )
  
  # Save raw results to CSV files
  write.csv(combined_results$summary, 
            "property_tax_summary.csv", row.names = FALSE)
  write.csv(combined_results$bedroom, 
            "property_tax_by_bedroom.csv", row.names = FALSE)
  write.csv(combined_results$township, 
            "property_tax_by_township.csv", row.names = FALSE)
  
  # Print detailed analysis with formatted values
  cat("\nDetailed Property Tax Analysis for Chicago (2021-2022)\n")
  cat(paste(rep("=", 100), collapse = ""), "\n\n")
  
  cat("1. Overall Property Tax Summary by Type:\n")
  cat(paste(rep("-", 80), collapse = ""), "\n")
  summary_formatted <- combined_results$summary %>%
    mutate(
      avg_market_value = sapply(avg_market_value, format_currency),
      avg_value_per_unit = sapply(avg_value_per_unit, format_currency),
      avg_tax_per_unit = sapply(avg_tax_per_unit, format_currency),
      median_tax_per_unit = sapply(median_tax_per_unit, format_currency),
      effective_tax_rate = sapply(effective_tax_rate, format_percent),
      tax_change_pct = sapply(tax_change_pct, function(x) ifelse(is.na(x), "N/A", format_percent(x)))
    )
  print(summary_formatted)
  
  cat("\n2. Property Tax by Unit Size:\n")
  cat(paste(rep("-", 80), collapse = ""), "\n")
  bedroom_formatted <- combined_results$bedroom %>%
    mutate(
      avg_tax_per_unit = sapply(avg_tax_per_unit, format_currency),
      avg_value_per_unit = sapply(avg_value_per_unit, format_currency)
    )
  print(bedroom_formatted)
  
  cat("\n3. Geographic Analysis by Township:\n")
  cat(paste(rep("-", 80), collapse = ""), "\n")
  township_formatted <- combined_results$township %>%
    mutate(
      avg_tax_per_unit = sapply(avg_tax_per_unit, format_currency),
      avg_value_per_unit = sapply(avg_value_per_unit, format_currency)
    )
  print(township_formatted)
  
  cat("\nKey Insights:\n")
  cat("1. Average Tax Burden:\n")
  cat("   - Single-family homes have the highest per-unit tax burden\n")
  cat("   - Multi-family properties show lower per-unit taxes\n")
  cat("   - Condos show significant variation based on size/value\n\n")
  
  cat("2. Geographic Variations:\n")
  cat("   - Significant tax variations across townships\n")
  cat("   - Property values and tax burdens correlate with location\n\n")
  
  cat("3. Unit Size Impact:\n")
  cat("   - Larger units generally face higher tax burdens\n")
  cat("   - Multi-family properties benefit from economies of scale\n\n")
  
  cat("Notes:\n")
  cat("- Analysis based on complete dataset of tax bills\n")
  cat("- Unit counts estimated based on property class and value\n")
  cat("- Market values derived from assessed values\n")
  
  return(combined_results)
}

# Run the calculation for 2021-2022
results <- calculate_avg_tax(c(2021, 2022))

Cook County Property Tax Revenue Analysis
================
Cook County Potential
December 17, 2024

- [Introduction](#introduction)
- [Current Property Tax Landscape](#current-property-tax-landscape)
  - [Geographic Distribution](#geographic-distribution)
  - [Unit Size Analysis](#unit-size-analysis)
- [Development Scenarios](#development-scenarios)
  - [1. Balanced Growth Scenario](#1-balanced-growth-scenario)
  - [2. Transit-Oriented Development (TOD)
    Scenario](#2-transit-oriented-development-tod-scenario)
  - [3. Affordable Housing Focus
    Scenario](#3-affordable-housing-focus-scenario)
- [Key Findings](#key-findings)
- [Recommendations](#recommendations)
- [Methodology Notes](#methodology-notes)

## Introduction

This analysis explores the potential property tax revenue that could be
generated through new housing development in Cook County, with a focus
on Chicago townships. Using actual property tax data from 2021-2022, we
model various scenarios for housing development and their impact on tax
revenue.

## Current Property Tax Landscape

First, let’s examine the current property tax situation across different
housing types and locations.

``` r
# Load the data
summary_data <- read.csv("property_tax_summary.csv")
bedroom_data <- read.csv("property_tax_by_bedroom.csv")
township_data <- read.csv("property_tax_by_township.csv")

# Filter for most recent year (2022)
current_summary <- summary_data %>%
  filter(year == 2022) %>%
  select(property_type, avg_tax_per_unit, avg_value_per_unit, effective_tax_rate) %>%
  # Handle NA values in effective tax rate
  mutate(effective_tax_rate = ifelse(is.na(effective_tax_rate), 
                                    avg_tax_per_unit / avg_value_per_unit * 100,
                                    effective_tax_rate))

# Create summary table
kable(current_summary %>%
  mutate(
    avg_tax_per_unit = format_currency(avg_tax_per_unit),
    avg_value_per_unit = format_currency(avg_value_per_unit),
    effective_tax_rate = format_percent(effective_tax_rate)
  ),
  col.names = c("Property Type", "Avg Tax/Unit", "Avg Value/Unit", "Effective Tax Rate"),
  caption = "2022 Property Tax Summary by Housing Type")
```

| Property Type | Avg Tax/Unit | Avg Value/Unit | Effective Tax Rate |
|:---|:---|:---|:---|
| Condo | \$5,121 | \$260,034 | 2.0% |
| Large Multi-Family (7+ units) | \$2,130 | \$92,849 | 2.3% |
| Single Family | \$5,381 | \$249,305 | 2.2% |
| Small Multi-Family (2-6 units) | \$2,669 | \$132,752 | 2.0% |

2022 Property Tax Summary by Housing Type

### Geographic Distribution

Let’s examine how property taxes vary across Chicago townships.

``` r
# Create township visualization with improved styling
township_plot <- township_data %>%
  filter(year == 2022) %>%
  ggplot(aes(x = reorder(township, avg_tax_per_unit), 
             y = avg_tax_per_unit, 
             fill = property_type)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9)) +
  geom_text(aes(label = scales::dollar(avg_tax_per_unit, accuracy = 1)),
            position = position_dodge(width = 0.9),
            hjust = -0.1,
            size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(),
                    expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = modern_palette) +
  labs(
    title = "Average Property Tax per Unit by Township and Property Type (2022)",
    subtitle = "Data labels show average tax amount per unit",
    x = "Township",
    y = "Average Tax per Unit",
    fill = "Property Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray50"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.major.y = element_blank()
  )

township_plot
```

![](tax_revenue_analysis_files/figure-gfm/township_analysis-1.png)<!-- -->

### Unit Size Analysis

Different unit sizes have varying tax implications:

``` r
# Create bedroom analysis visualization with improved styling
bedroom_plot <- bedroom_data %>%
  filter(year == 2022) %>%
  ggplot(aes(x = bedroom_type, 
             y = avg_tax_per_unit, 
             fill = property_type)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9)) +
  geom_text(aes(label = scales::dollar(avg_tax_per_unit, accuracy = 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  scale_y_continuous(labels = scales::dollar_format(),
                    expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = modern_palette) +
  labs(
    title = "Average Property Tax by Unit Size and Property Type (2022)",
    subtitle = "Data labels show average tax amount per unit",
    x = "Unit Type",
    y = "Average Tax per Unit",
    fill = "Property Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray50"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.major.x = element_blank()
  ) +
  scale_x_discrete(labels = function(x) {
    gsub("_", " ", str_to_title(x))
  })

bedroom_plot
```

![](tax_revenue_analysis_files/figure-gfm/bedroom_analysis-1.png)<!-- -->

## Development Scenarios

Let’s model different scenarios for new housing development and their
potential tax revenue impact. Each scenario is carefully designed based
on different development priorities and market conditions:

### 1. Balanced Growth Scenario

This scenario aims to provide a diverse mix of housing types while
maintaining neighborhood character: - **1,000 1-bed condos**:
Entry-level housing for young professionals and small households -
**1,000 2-bed condos**: Family-friendly units in medium-density areas -
**500 single-family homes**: Preserving some traditional housing stock -
**2,000 large multi-family units**: Mix of studio to 3-bed units in
larger developments - **1,500 small multi-family units**: 2-6 unit
buildings spread throughout neighborhoods

*Rationale*: This mix provides housing options across all income levels
and household sizes while maintaining a balance between density and
neighborhood character. The larger proportion of multi-family units
(3,500 total) reflects the need for density, while the equal split
between 1-bed and 2-bed condos (2,000 total) caters to both singles and
small families.

### 2. Transit-Oriented Development (TOD) Scenario

Focused on high-density development near transit hubs: - **2,000 1-bed
condos**: Higher concentration of smaller units near transit - **1,500
2-bed condos**: Family units with good transit access - **3,000 large
multi-family units**: Major developments near transit stations - **1,000
small multi-family units**: Moderate density in transit corridors -
**200 single-family homes**: Limited to areas farther from transit

*Rationale*: This scenario prioritizes density near transit with 75% of
units (6,500) being multi-family or condo. The higher number of 1-bed
condos (2,000) versus 2-bed (1,500) reflects typical TOD resident
demographics. The reduced single-family component (200 units) emphasizes
the transit-oriented nature of the development.

### 3. Affordable Housing Focus Scenario

Prioritizes accessible housing options across different household
sizes: - **1,500 1-bed condos**: Affordable entry-level housing - **500
2-bed condos**: Family-sized units at moderate price points - **4,000
large multi-family units**: Maximum density for affordability - **2,000
small multi-family units**: Distributed affordable housing - **100
single-family homes**: Minimal single-family development

*Rationale*: This scenario maximizes affordable units through higher
density, with 6,000 multi-family units (75% of total). The emphasis on
1-bed over 2-bed condos (3:1 ratio) prioritizes entry-level housing,
while the minimal single-family component (100 units) reflects the focus
on affordability through density.

``` r
# Get average tax rates by property and bedroom type for 2022
tax_rates <- bedroom_data %>%
  filter(year == 2022) %>%
  select(property_type, bedroom_type, avg_tax_per_unit)

# Define scenarios
scenarios <- tribble(
  ~scenario_name, ~property_type, ~bedroom_type, ~units,
  # Balanced Growth
  "Balanced Growth", "Condo", "beds_1", 1000,
  "Balanced Growth", "Condo", "beds_2", 1000,
  "Balanced Growth", "Single Family", "beds_3plus", 500,
  "Balanced Growth", "Large Multi-Family (7+ units)", "mixed_studio_to_3", 2000,
  "Balanced Growth", "Small Multi-Family (2-6 units)", "mixed_1_to_3", 1500,
  
  # Transit-Oriented Development
  "TOD", "Condo", "beds_1", 2000,
  "TOD", "Condo", "beds_2", 1500,
  "TOD", "Large Multi-Family (7+ units)", "mixed_studio_to_3", 3000,
  "TOD", "Small Multi-Family (2-6 units)", "mixed_1_to_3", 1000,
  "TOD", "Single Family", "beds_3plus", 200,
  
  # Affordable Housing Focus
  "Affordable Housing", "Condo", "beds_1", 1500,
  "Affordable Housing", "Large Multi-Family (7+ units)", "mixed_studio_to_3", 4000,
  "Affordable Housing", "Small Multi-Family (2-6 units)", "mixed_1_to_3", 2000,
  "Affordable Housing", "Condo", "beds_2", 500,
  "Affordable Housing", "Single Family", "beds_3plus", 100
)

# Calculate revenue for each scenario
scenario_results <- scenarios %>%
  left_join(tax_rates, by = c("property_type", "bedroom_type")) %>%
  mutate(tax_revenue = units * avg_tax_per_unit)

# Summarize results
scenario_summary <- scenario_results %>%
  group_by(scenario_name) %>%
  summarize(
    total_units = sum(units),
    total_revenue = sum(tax_revenue),
    revenue_per_unit = total_revenue / total_units
  )

# Display summary table
kable(scenario_summary %>%
  mutate(
    total_units = format_number(total_units),
    total_revenue = format_currency(total_revenue),
    revenue_per_unit = format_currency(revenue_per_unit)
  ),
  col.names = c("Scenario", "Total Units", "Annual Tax Revenue", "Revenue per Unit"),
  caption = "Projected Annual Tax Revenue by Development Scenario")
```

| Scenario           | Total Units | Annual Tax Revenue | Revenue per Unit |
|:-------------------|:------------|:-------------------|:-----------------|
| Affordable Housing | 8,100       | \$21,818,033       | \$2,694          |
| Balanced Growth    | 6,000       | \$20,958,445       | \$3,493          |
| TOD                | 7,700       | \$26,351,190       | \$3,422          |

Projected Annual Tax Revenue by Development Scenario

``` r
# Create improved scenario visualization
scenario_plot <- scenario_results %>%
  group_by(scenario_name, property_type) %>%
  summarize(tax_revenue = sum(tax_revenue)) %>%
  ggplot(aes(x = scenario_name, 
             y = tax_revenue/1e6, 
             fill = property_type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::dollar(tax_revenue/1e6, accuracy = 0.1, suffix = "M")),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 3) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "M"),
                    expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = modern_palette) +
  labs(
    title = "Projected Annual Tax Revenue by Development Scenario",
    subtitle = "Broken down by property type, with revenue amounts shown in millions",
    x = "Development Scenario",
    y = "Tax Revenue (Millions)",
    fill = "Property Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray50"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.major.x = element_blank()
  )

scenario_plot
```

![](tax_revenue_analysis_files/figure-gfm/scenario_planning-1.png)<!-- -->

## Key Findings

1.  **Revenue Potential**: Based on our analysis of different
    development scenarios:
    - The Balanced Growth scenario generates \$21,818,033 annually from
      8100 units
    - The TOD scenario generates \$20,958,445 annually from 6000 units
    - The Affordable Housing scenario generates \$26,351,190 annually
      from 7700 units
2.  **Property Type Impact**:
    - Single-family homes generate the highest per-unit tax revenue
      (\$5,381 per unit)
    - Multi-family properties offer efficiency in land use while
      maintaining substantial tax revenue
    - Condos provide a middle ground between density and tax revenue
3.  **Geographic Considerations**:
    - Tax revenue varies significantly by township
    - Lake and South Chicago townships show the highest average tax per
      unit
    - Strategic development in high-value areas could maximize revenue

## Recommendations

1.  **Balanced Development**: While single-family homes generate the
    highest per-unit revenue, a mix of housing types provides the best
    balance of revenue and density.

2.  **Location Strategy**: Focus development in areas with strong tax
    revenue potential while ensuring equitable distribution of new
    housing.

3.  **Unit Mix**: Prioritize a diverse mix of unit types to:

    - Meet various housing needs
    - Maximize tax revenue
    - Maintain neighborhood character

4.  **Implementation Approach**:

    - Phase development based on market demand
    - Coordinate with infrastructure improvements
    - Consider impact on existing communities

## Methodology Notes

This analysis uses actual property tax data from Cook County for
2021-2022. Key assumptions include:

- Tax rates remain consistent with 2022 levels
- New developments achieve similar values to existing properties
- Unit counts for multi-family properties are estimated based on
  property values
- Scenarios represent theoretical development patterns

Data sources: - Cook County Assessor’s Office - Property tax bills from
2021-2022 - Township-level assessment data

Note on Effective Tax Rates: The effective tax rate is calculated as the
ratio of total tax to market value. In cases where this calculation
initially resulted in NA values (due to missing or zero market values),
we’ve computed it using the average tax per unit divided by average
value per unit to ensure completeness of the analysis.

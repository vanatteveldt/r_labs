## US_Demographics.csv
## 
## Rebuild US_Demographics.csv from the 2020-2024 ACS 5-year estimates
## Code (obviously) written by claude
##
## The original file used the 2012-2016 ACS 5-year estimates; the column
## definitions below reproduce it exactly.
##
## Needs a free Census API key (https://api.census.gov/data/key_signup.html):
##   census_api_key("YOUR_KEY", install = TRUE)
##
## Compliance note: 
##   This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.

library(tidycensus)
library(tidyverse)
acs_year <- 2024   # = the 2020-2024 5-year file, released 2026-01-29

## ---------------------------------------------------------------------------
## 1. Single-variable measures
## ---------------------------------------------------------------------------

census_api_key("8876955420b1939ae49b2817241e40314a09b4e5", install=T)
readRenviron("~/.Renviron")
simple <- get_acs(
  geography = "county",
  variables = c(
    total_population = "B01003_001",  # total population
    median_hh_inc    = "B19013_001",  # median household income
    race_total       = "B03002_001",  # total, Hispanic-origin-by-race table
    nhwhite          = "B03002_003",  # not Hispanic/Latino, white alone
    female           = "B01001_026",  # total female
    clf              = "B23025_003",  # civilian labor force (16+)
    unemployed       = "B23025_005"   # civilian labor force, unemployed
  ),
  year = acs_year, survey = "acs5", output = "wide"
) |>
  select(GEOID, NAME, ends_with("E")) |>          # drop margins of error
  rename_with(\(x) str_remove(x, "E$"), .cols = -c(GEOID, NAME))

## ---------------------------------------------------------------------------
## 2. Age groups: sum the relevant brackets of B01001 (sex by age)
## ---------------------------------------------------------------------------
## male   under 30 = _003 .. _011,  65+ = _020 .. _025
## female under 30 = _027 .. _035,  65+ = _044 .. _049

under30 <- sprintf("B01001_%03d", c(3:11, 27:35))
plus65  <- sprintf("B01001_%03d", c(20:25, 44:49))

age <- get_acs(geography = "county", table = "B01001",
               year = acs_year, survey = "acs5") |>
  filter(variable %in% c(under30, plus65)) |>
  mutate(group = if_else(variable %in% under30, "under30", "plus65")) |>
  summarise(n = sum(estimate), .by = c(GEOID, group)) |>
  pivot_wider(names_from = group, values_from = n)

## ---------------------------------------------------------------------------
## 3. Education: B15003 is educational attainment for the population 25+
## ---------------------------------------------------------------------------
## _001 = total 25+;  _022 .. _025 = bachelor's, master's, professional, doctorate

edu <- get_acs(geography = "county", table = "B15003",
               year = acs_year, survey = "acs5") |>
  filter(variable %in% c("B15003_001", sprintf("B15003_%03d", 22:25))) |>
  mutate(group = if_else(variable == "B15003_001", "adults25", "bachelor_plus")) |>
  summarise(n = sum(estimate), .by = c(GEOID, group)) |>
  pivot_wider(names_from = group, values_from = n)

## ---------------------------------------------------------------------------
## 4. Combine and compute the derived columns
## ---------------------------------------------------------------------------

## Suffixes to strip so "Autauga County" becomes "Autauga".
## Longest first; note "Planning Region" only occurs in Connecticut.
county_suffix <- paste0(
  " (City and Borough|Census Area|Planning Region|Municipality|",
  "Municipio|County|Parish|Borough|city|City)$"
)

demographics <- simple |>
  left_join(age, by = "GEOID") |>
  left_join(edu, by = "GEOID") |>
  separate_wider_delim(NAME, delim = ", ",
                       names = c("county", "state"), too_many = "merge") |>
  mutate(
    fips              = GEOID,
    county            = str_remove(county, county_suffix),
    nonwhite_pct      = 100 * (1 - nhwhite / race_total),
    female_pct        = 100 * female / total_population,
    age29andunder_pct = 100 * under30 / total_population,
    age65andolder_pct = 100 * plus65 / total_population,
    clf_unemploy_pct  = 100 * unemployed / clf,
    college_pct   = 100 * (bachelor_plus / adults25)
  ) |>
  filter(!str_starts(fips, "72")) |>   # drop Puerto Rico, as in the original
  select(fips, state, county, total_population, nonwhite_pct, female_pct,
         age29andunder_pct, age65andolder_pct, median_hh_inc,
         clf_unemploy_pct, college_pct) |>
  arrange(fips)

## The original file stored fips as a number (1001, not "01001").
## Keeping it as a 5-character string is better practice, but uncomment
## the next line if you need drop-in compatibility with existing exercises.
# demographics <- mutate(demographics, fips = as.integer(fips))

write_csv(demographics, "data/US_Demographics.csv")

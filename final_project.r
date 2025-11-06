#------------------------------------------------------------
# View census data and attributes

#v21 <- load_variables(2021, "acs5", cache = FALSE)

#View(v21)

#------------------------------------------------------------
# Package installations and Census API key

install.packages(c("tidycensus", "tidyverse", "tigris", "tmap", "sf"))

library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
library(sf)
options(tigris_use_cache = TRUE)

# set your Census API key once (get it from https://api.census.gov/data/key_signup.html)
census_api_key("69b75ff19dfd3dc5f64f9364938bced7aab036bb", install = TRUE)

Sys.getenv("CENSUS_API_KEY")

#------------------------------------------------------------
# Median Income in NYC
nyc <- get_acs(
  geography = "county",
  variables = c(medincome = "B19013_001"),
  state = "NY",
  county = c("Bronx", "Kings", "New York", "Richmond", "Queens"),
  year = 2021
)

nyc |>
  mutate(NAME = gsub(" County, New York", "", NAME)) |>
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(
    title = "Household income by borough in New York City",
    subtitle = "2017-2021 American Community Survey",
    y = "",
    x = "ACS estimate (bars represent margin of error)"
  )


#------------------------------------------------------------

# Get median household income by tracts (ZCTA), data from the 2017-2021 5-year ACS
nyc_income <- get_acs(
  geography = "tract",
  variables = c(medincome = "B19013_001"),
  year = 2021,
  state = "NY",
  county = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
  geometry = TRUE
)





nyc_counties <- counties(state = "NY", cb = TRUE, year = 2023) |>
  filter(NAME %in% c("New York", "Bronx", "Kings"))
# combines into a single polygon for all three boroughs.
nyc_boundary <- st_union(nyc_counties)
# clip ZIPs to NYC boundary
nyc_income_subset <- st_intersection(nyc_income, nyc_boundary)

# Plot with tmap
tmap_mode("view")
tm_shape(nyc_income_subset) +
  tm_polygons("estimate",
    title = "Median Income ($)",
    style = "quantile",
    palette = "viridis"
  ) +
  tm_borders(col = "white", lwd = 1) +
  tm_layout(main.title = "NYC Median Household Income by Community Districts (ACS 2021)")

#------------------------------------------------------------
# Get median household income by tract data from the 2017-2021 5-year ACS
tmap_mode("view")
nyc_tracts <- get_acs(
  geography = "tract",
  variables = c(medincome = "B19013_001"),
  state = "NY",
  county = c("Bronx", "Kings", "New York"),
  year = 2021,
  geometry = TRUE
)

ny_boroughs <- get_acs(
  geography = "county", state = "NY",
  variables = "B19013_001", geometry = TRUE
)

# Plot with ggplot2
tm_shape(nyc_tracts) +
  tm_polygons("estimate",
    title = "Median Income ($)",
    style = "quantile",
    palette = "viridis"
  ) +
  tm_shape(ny_boroughs) +
  tm_borders(col = "white", lwd = 1) +
  tm_layout(main.title = "NYC Median Household Income by Tract (ACS 2021)")


#------------------------------------------------------------
# Get median household income by pumas data from the 2017-2021 5-year ACS
nyc_pumas <- get_acs(
  geography = "public use microdata area",
  variables = c(medincome = "B19013_001"),
  state = "NY",
  year = 2021,
  geometry = TRUE
)
# Get NYC counties (boroughs)
nyc_counties <- counties(state = "NY", cb = TRUE, year = 2023) |>
  filter(NAME %in% c("New York", "Bronx", "Kings"))
# combines into a single polygon for all three boroughs.
nyc_boundary <- st_union(nyc_counties)
# clip PUMAs to NYC boundary
nyc_pumas_subset <- st_intersection(nyc_pumas, nyc_boundary)

# Plot with ggplot2
tm_shape(nyc_pumas_subset) +
  tm_polygons("estimate",
    title = "Median Income ($)",
    style = "quantile",
    palette = "viridis"
  ) +
  tm_borders(col = "white", lwd = 1) +
  tm_layout(main.title = "NYC PUMAs - Median Household Income (2021)")

#------------------------------------------------------------
# Get median household income by blockl data from the 2017-2021 5-year ACS

nyc_blocks <- get_acs(
  geography = "block group",
  variables = c(medincome = "B19013_001"),
  state = "NY",
  county = c("Bronx", "New York", "Kings"),
  year = 2021,
  geometry = TRUE
)

# Plot with ggplot2
tm_shape(nyc_blocks) +
  tm_polygons("estimate",
    title = "Median Income ($)",
    style = "quantile",
    palette = "viridis"
  ) +
  tm_borders(col = "white", lwd = 1) +
  tm_layout(main.title = "NYC Median Household Income by Block (ACS 2021)")


#------------------------------------------------------------

# Get total population units by ZIP (ZCTA), data from the 2017-2021 5-year ACS
nyc_pop_by_zip_data <- get_acs(
  geography = "zip code tabulation area",
  variables = c(total_population = "B01003_001"),
  year = 2021
)
nyc_pop_by_zip_data

nyc_population_by_zip <- get_acs(
  geography = "zip code tabulation area",
  variables = c(total_population = "B01003_001"),
  year = 2021,
  geometry = TRUE
)

nyc_counties <- counties(state = "NY", cb = TRUE, year = 2023) |>
  filter(NAME %in% c("New York", "Bronx", "Kings"))
# combines into a single polygon for all three boroughs.
nyc_boundary <- st_union(nyc_counties)
# clip ZIPs to NYC boundary
nyc_pop_by_zip_subset <- st_intersection(nyc_population_by_zip, nyc_boundary)

# Plot with tmap
tmap_mode("view")
tm_shape(nyc_pop_by_zip_subset) +
  tm_polygons("estimate",
    title = "Population",
    style = "quantile",
    palette = "viridis"
  ) +
  tm_borders(col = "white", lwd = 1) +
  tm_layout(main.title = "NYC Population by ZIP Code (ACS 2021)")

#------------------------------------------------------------

# Get total housing units by ZIP (ZCTA), data from the 2017-2021 5-year ACS
nyc_housing_by_zip_data <- get_acs(
  geography = "zip code tabulation area",
  variables = c(total_housing_units = "B25001_001"),
  year = 2021
)
nyc_housing_by_zip_data

nyc_housing_by_zip <- get_acs(
  geography = "zip code tabulation area",
  variables = c(total_housing_units = "B25001_001"),
  year = 2021,
  geometry = TRUE
)

nyc_counties <- counties(state = "NY", cb = TRUE, year = 2023) |>
  filter(NAME %in% c("New York", "Bronx", "Kings"))
# combines into a single polygon for all three boroughs.
nyc_boundary <- st_union(nyc_counties)
# clip ZIPs to NYC boundary
nyc_housing_by_zip_subset <- st_intersection(nyc_housing_by_zip, nyc_boundary)

# Plot with tmap
tmap_mode("view")
tm_shape(nyc_housing_by_zip_subset) +
  tm_polygons("estimate",
    title = "Housing Units",
    style = "quantile",
    palette = "viridis"
  ) +
  tm_borders(col = "white", lwd = 1) +
  tm_layout(main.title = "NYC Housing Units by ZIP Code (ACS 2021)")


#------------------------------------------------------------

# Get population by age by ZIP (ZCTA), data from the 2017-2021 5-year ACS
nyc_pop_by_age_by_zip_data <- get_acs(
  geography = "zip code tabulation area",
  variables = c(pop_by_age = "B01001_*"),
  year = 2021
)
nyc_pop_by_age_by_zip_data

nyc_pop_by_age_by_zip <- get_acs(
  geography = "zip code tabulation area",
  variables = c(pop_by_age = "B01001_*"),
  year = 2021,
  geometry = TRUE
)

nyc_counties <- counties(state = "NY", cb = TRUE, year = 2023) |>
  filter(NAME %in% c("New York", "Bronx", "Kings"))
# combines into a single polygon for all three boroughs.
nyc_boundary <- st_union(nyc_counties)
# clip ZIPs to NYC boundary
nyc_pop_by_age_by_zip_subset <- st_intersection(nyc_pop_by_age_by_zip, nyc_boundary)

# Plot with tmap
tmap_mode("view")
tm_shape(nyc_pop_by_age_by_zip_subset) +
  tm_polygons("estimate",
    title = "Age Group",
    style = "quantile",
    palette = "viridis"
  ) +
  tm_borders(col = "white", lwd = 1) +
  tm_layout(main.title = "NYC Population by Age by ZIP Code (ACS 2021)")


#------------------------------------------------------------

# Get residential density by block (population  per km^2), data from the 2017-2021 5-year ACS
nyc_blocks_density <- get_acs(
  geography = "block group",
  variables = c(population_by_block = "B01003_001"),
  state = "NY",
  county = c("Bronx", "New York", "Kings"),
  year = 2021,
  geometry = TRUE
)

nyc_blocks_density <- nyc_blocks_density |>
  mutate(
    area_sqkm = as.numeric(st_area(geometry)) / 1e6, # m^2 to km^2
    density = estimate / area_sqkm
  ) # population per km^2

head(nyc_blocks_density |> select(GEOID, estimate, area_sqkm, density))

# Plot with ggplot2
tm_shape(nyc_blocks_density) +
  tm_polygons("density",
    title = "Population Density (per km^{2}",
    style = "quantile",
    palette = "viridis"
  ) +
  tm_borders(col = "white", lwd = 1) +
  tm_layout(main.title = "NYC Residential Density by Block (ACS 2021)")


#------------------------------------------------------------

# Get housing density by block (housing units per km^2), data from the 2017-2021 5-year ACS
nyc_blocks_housing_density <- get_acs(
  geography = "block group",
  variables = c(housing_units_by_block = "B25001_001"),
  state = "NY",
  county = c("Bronx", "New York", "Kings"),
  year = 2021,
  geometry = TRUE
)

nyc_blocks_housing_density <- nyc_blocks |>
  mutate(
    area_sqkm = as.numeric(st_area(geometry)) / 1e6, # m^2 to km^2
    density = estimate / area_sqkm
  ) # population per km^2

head(nyc_blocks_housing_density |> select(GEOID, estimate, area_sqkm, density))

# Plot with ggplot2
tm_shape(nyc_blocks_housing_density) +
  tm_polygons("density",
    title = "Housing Density (per km^{2}",
    style = "quantile",
    # fill.scale = "tm_scale_intervals()",
    palette = "-viridis"
  ) +
  tm_borders(col = "white", lwd = 1) +
  tm_layout(main.title = "NYC Housing Density by Block (ACS 2021)")


#------------------------------------------------------------

# Metric                    | Table / Variable          | Notes                                                                         |
#| ----------------------   | ------------------------- | ---------------------------- #
#  | Total population       | `B01003_001`              | Population per block group                                                    |
#  | Occupied housing units | `B25002_002`              | Only occupied units                                                           |
#  | Average household size | `B25010_001`              | People per household                                                          |
#  | Units in structure     | `B25024_001`–`B25024_010` | Building type breakdown
#                                         (1-unit, 2-unit, 3–4, 5–9, 10–19, 20–49, 50+, mobile) |


variables <- c(
  pop = "B01003_001",
  occupied_units = "B25002_002",
  avg_hh_size = "B25010_001",
  units_total = "B25024_001",
  single_family_1 = "B25024_002", # 1-unit, detached or attached
  two_units = "B25024_003", # 2 units
  three_four_units = "B25024_004", # 3–4 units
  five_nine_units = "B25024_005", # 5–9 units
  ten_nineteen_units = "B25024_006", # 10–19 units
  twenty_forty_nine_units = "B25024_007", # 20–49 units
  fifty_plus_units = "B25024_008", # 50+ units
  mobile_home = "B25024_009", # mobile home
  other_units = "B25024_010" # other types
)

# get block group data
nyc_blocks_residential_density <- get_acs(
  geography = "block group",
  variables = variables,
  state = "NY",
  county = c("Bronx", "New York", "Kings"),
  year = 2021,
  geometry = TRUE
)

summary(nyc_blocks_residential_density$geometry)

# pivot wider
nyc_blocks_wide <- nyc_blocks_residential_density |>
  select(GEOID, NAME, variable, estimate, geometry) |>
  pivot_wider(names_from = variable, values_from = estimate)



# calculate densities
nyc_blocks_wide <- nyc_blocks_wide |>
  mutate(
    area_sqkm = as.numeric(st_area(geometry)) / 1e6, # area in km^2
    pop_density = pop / area_sqkm, # traditional npeople per km^2
    housing_density = occupied_units / area_sqkm, # units per km^2
    occupied_units_safe = ifelse(occupied_units == 0, NA, occupied_units),
    hh_adjusted_density = pop / occupied_units_safe # people per occupied unit (accounting for crowding/housing size)
    # hh_adjusted_density = ifelse(is.infinite(hh_adjusted_density), NA, hh_adjusted_density)
  )

nyc_blocks_wide <- nyc_blocks_wide %>%
  filter(!st_is_empty(geometry))

summary(nyc_blocks_wide$geometry)
any(st_is_empty(nyc_blocks_wide$geometry))


summary(nyc_blocks_wide$hh_adjusted_density)

table(nyc_blocks_wide$occupied_units == 0)


# nyc_blocks_wide <- nyc_blocks_wide %>%
#  mutate(
#    pct_single_family = B25024_002 / units_total * 100,
#    pct_multi_family_5plus = (B25024_006 + B25024_007 + B25024_008) / units_total * 100
#  ) # high-rise apartments vs single-family neighborhoods

# head(nyc_blocks_wide |> select(GEOID, estimate, area_sqkm, density))

# calculate building type percentages, differentiate single-family vs multi-family neighborhoods
nyc_blocks_wide <- nyc_blocks_wide %>%
  mutate(
    pct_single_family = single_family_1 / units_total * 100, # prop of 1-unit homes
    pct_2_units = two_units / units_total * 100,
    pct_3_4_units = three_four_units / units_total * 100,
    pct_5_9_units = five_nine_units / units_total * 100,
    pct_10_19_units = ten_nineteen_units / units_total * 100,
    pct_20_49_units = twenty_forty_nine_units / units_total * 100,
    pct_50_plus_units = fifty_plus_units / units_total * 100,
    pct_mobile_home = mobile_home / units_total * 100,
    pct_other_units = other_units / units_total * 100,
    pct_multi_family_5plus = (five_nine_units + ten_nineteen_units +
      twenty_forty_nine_units + fifty_plus_units) / units_total * 100
  ) # proportion of larger multi-family buildings (5+ units)


nyc_blocks_wide <- nyc_blocks_wide %>%
  mutate(
    building_type = case_when(
      pct_single_family > 50 ~ "Mostly Single-Family",
      pct_multi_family_5plus > 50 ~ "Mostly Multi-Family 5+",
      TRUE ~ "Mixed"
    )
  )

# Household Adjusted Density
tm_shape(nyc_blocks_wide) +
  tm_polygons("hh_adjusted_density",
    title = "Housing-Adjusted Density\n(People per Occupied Unit)",
    style = "quantile",
    palette = "viridis"
  ) +
  tm_facets(by = "building_type") +
  tm_layout(main.title = "NYC Block Groups: Housing-Adjusted Density & Building Type")

# colored by building type:
tm_shape(nyc_blocks_wide) +
  tm_polygons("building_type",
    palette = c(
      "Mostly Single-Family" = "skyblue",
      "Mostly Multi-Family 5+" = "tomato",
      "Mixed" = "lightgreen"
    ),
    title = "Neighborhood Building Type"
  ) +
  tm_layout(main.title = "NYC Block Groups: Building Type")


# Plot with tmap
tm_shape(nyc_blocks_wide) +
  tm_polygons("pct_multi_family_5plus",
    title = "% Large Multi-Family Buildings",
    style = "quantile",
    # fill.scale = "tm_scale_intervals()",
    palette = "magma"
  ) +
  tm_borders(col = "white", lwd = 1) +
  tm_facets(by = "building_type") +
  tm_layout(main.title = "NYC Block Groups - Multi-Family vs Single-Family")


#------------------------------------------------------------

tm_shape(nyc_blocks_wide) +
  # Base layer: housing-adjusted density gradient
  tm_polygons("hh_adjusted_density",
    palette = "viridis",
    style = "quantile",
    title = "Housing-Adjusted Density\n(People per Occupied Unit)",
    alpha = 0.8
  ) + # semi-transparent to see overlay

  # Overlay borders to indicate building type
  tm_borders(
    lwd = 0.8,
    col = "building_type",
    palette = c(
      "Mostly Single-Family" = "skyblue",
      "Mostly Multi-Family 5+" = "tomato",
      "Mixed" = "lightgreen"
    ),
    title = "Building Type"
  ) +

  tm_layout(main.title = "NYC Block Groups: Housing-Adjusted Density & Building Type")


#------------------------------------------------------------

# Get ACS data for  grade-school student-age population
student_vars <- c(
  "B01001_004", "B01001_005", "B01001_006", # male 5–17
  "B01001_028", "B01001_029", "B01001_030" # female 5–17
)

nyc_blocks_students <- get_acs(
  geography = "block group",
  variables = student_vars,
  state = "NY",
  county = c("Bronx", "New York", "Kings"),
  year = 2021,
  geometry = TRUE
)

# Sum the variables to get total student-age population
nyc_blocks_students <- nyc_blocks_students |>
  group_by(GEOID) |>
  summarise(student_pop = sum(estimate)) # sum across variables

nyc_blocks_students

nyc_blocks_student_density <- nyc_blocks_students |>
  mutate(
    area_sqkm = as.numeric(st_area(geometry)) / 1e6, # m^2 to km^2
    density = student_pop / area_sqkm
  ) # population per km^2

head(nyc_blocks_student_density |> select(GEOID, student_pop, area_sqkm, density))

# Plot with ggplot2
tm_shape(nyc_blocks_student_density) +
  tm_polygons("density",
    title = "Grade School Student Density (per km^{2}",
    style = "quantile",
    # fill.scale = "tm_scale_intervals()",
    palette = "-viridis"
  ) +
  tm_borders(col = "white", lwd = 1) +
  tm_layout(main.title = "NYC Grade School Student Density by Block (ACS 2021)")


#------------------------------------------------------------

# Get ACS data for college-age student-age population
student_vars <- c(
  "B01001_007", "B01001_008", "B01001_009", "B01001_009", "B01001_010", "B01001_011", "B01001_012", # male 18-24
  "B01001_031", "B01001_032", "B01001_033", "B01001_034", "B01001_035", "B01001_036" # female 18-24
)

nyc_blocks_students <- get_acs(
  geography = "block group",
  variables = student_vars,
  state = "NY",
  county = c("Bronx", "New York", "Kings"),
  year = 2021,
  geometry = TRUE
)

# Sum the variables to get total student-age population
nyc_blocks_students <- nyc_blocks_students |>
  group_by(GEOID) |>
  summarise(student_pop = sum(estimate)) # sum across variables

nyc_blocks_students

nyc_blocks_student_density <- nyc_blocks_students |>
  mutate(
    area_sqkm = as.numeric(st_area(geometry)) / 1e6, # m^2 to km^2
    density = student_pop / area_sqkm
  ) # population per km^2

head(nyc_blocks_student_density |> select(GEOID, student_pop, area_sqkm, density))

# Plot with ggplot2
tm_shape(nyc_blocks_student_density) +
  tm_polygons("density",
    title = "College Student Density (per km^{2}",
    style = "quantile",
    # fill.scale = "tm_scale_intervals()",
    palette = "-viridis"
  ) +
  tm_borders(col = "white", lwd = 1) +
  tm_layout(main.title = "NYC College-Age Student Density by Block (ACS 2021)")

#------------------------------------------------------------

# Needs to be modified

if (!dir.exists(file.path("data", "final_project"))) {
  dir.create(file.path("data", "final_project"), showWarnings = FALSE, recursive = TRUE)
}

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("styler")) install.packages("styler")

library <- function(pkg) {
  ## Mask base::library() to automatically install packages if needed
  ## Masking is important here so downlit picks up packages and links
  ## to documentation
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) install.packages(pkg)
  stopifnot(require(pkg, character.only = TRUE, quietly = TRUE))
}

library(tidyverse)
library(glue)
library(readxl)
library(tidycensus)

get_acs_all_years <- function(variable, geography = "cbsa",
                              start_year = 2009, end_year = 2023) {
  fname <- glue("{variable}_{geography}_{start_year}_{end_year}.csv")
  fname <- file.path("data", "mp02", fname)

  if (!file.exists(fname)) {
    YEARS <- seq(start_year, end_year)
    YEARS <- YEARS[YEARS != 2020] # Drop 2020 - No survey (covid)

    ALL_DATA <- map(YEARS, function(yy) {
      tidycensus::get_acs(geography, variable, year = yy, survey = "acs1") |>
        mutate(year = yy) |>
        select(-moe, -variable) |>
        rename(!!variable := estimate)
    }) |> bind_rows()

    write_csv(ALL_DATA, fname)
  }

  read_csv(fname, show_col_types = FALSE)
}

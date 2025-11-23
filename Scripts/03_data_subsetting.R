################################################################################
################################################################################
### INTERNET VACANCY INDEX                                                   ###
################################################################################
################################################################################
###
### Author: Johann Wagner
### Creation Date: 2024-05-10
### QA by:
### QA Date:
### Purpose: To subset the most recent, finalised, long format IVI results.
###          This filtering minimises code duplication and potential coding
###          error.
### Notes:
###
###
###
###
################################################################################





# Setup and Configuration -------------------------------------------------
if(!exists("script_checklist")) {
  source(
    here::here(
      "Scripts",
      "00_setup_and_configuration.R"
    ),
    echo = TRUE,
    max.deparse.length = 1000
  )
}






# Data Loading ------------------------------------------------------------
if(!("01_data_loading" %in% script_checklist)) {
  source(
    here::here(
      "Scripts",
      "01_data_loading.R"
    ),
    echo = TRUE,
    max.deparse.length = 1000
  )
}





# Data Wrangling ------------------------------------------------------------
if(!("02_data_wrangling" %in% script_checklist)) {
  source(
    here::here(
      "Scripts",
      "02_data_wrangling.R"
    ),
    echo = TRUE,
    max.deparse.length = 1000
  )
}





# Data Subsetting ---------------------------------------------------------
## Key Points -------------------------------------------------------------
summary_ivi_long_anzsco2_state_australia_past_12_months <- summary_ivi_long_anzsco2_state |>
  filter(
    type  == "SA",
    level == 0,
    between(
      date,
      measurement_date_ymd - months(11),
      measurement_date_ymd
    )
  )


summary_ivi_long_anzsco2_state_australia_most_recent <- summary_ivi_long_anzsco2_state |>
  filter(
    type  == "SA",
    level == 0,
    date  == measurement_date_ymd
  )


summary_ivi_long_anzsco2_state_australia_2019 <- summary_ivi_long_anzsco2_state |>
  filter(
    type  == "SA",
    level == 0,
    between(
      date,
      as_date("2019-01-01"),
      as_date("2019-12-01")
    )
  )


summary_ivi_long_anzsco2_state_regional_most_recent <- summary_ivi_long_anzsco2_regional |>
  filter(
    type  == "3MA_of_Original",
    level == 1,
    date  == measurement_date_ymd,
    !str_detect(region, "Other Regions")
  ) |>
  mutate(
    metropolitan_regional = case_when(
      region %in% c(
        "Sydney",
        "Melbourne",
        "Brisbane",
        "Adelaide",
        "Perth",
        "Hobart & Southeast Tasmania",
        "Darwin",
        "Canberra & ACT"
      ) ~ "Metropolitan",
      .default = "Regional"
    )
  )



## Flex Tables -------------------------------------------------------------
summary_ivi_long_anzsco2_state_australia_state_most_recent <- summary_ivi_long_anzsco2_state |>
  filter(
    type        == "SA",
    anzsco_code == 0,
    date        == measurement_date_ymd
  )


summary_ivi_long_anzsco2_state_anzsco1_most_recent <- summary_ivi_long_anzsco2_state |>
  filter(
    type         == "SA",
    state        == "AUST",
    anzsco_code %in% 1:8,
    date         == measurement_date_ymd
  )


summary_ivi_long_skill_state_skill_most_recent <- summary_ivi_long_skill_state |>
  filter(
    type        == "SA",
    state       == "AUST",
    skill_level != 0,
    date        == measurement_date_ymd
  )



## Data Visualisation ------------------------------------------------------
summary_ivi_long_skill_state_time_series <- summary_ivi_long_skill_state |>
  filter(
    type        == "SA",
    state       == "AUST",
    skill_level == 0
  )





# Script Check ------------------------------------------------------------
# To ensure scripts are only run once if already in Global Environment.

script_checklist <- c(script_checklist, "02_data_subsetting")

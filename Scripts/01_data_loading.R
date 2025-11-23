################################################################################
################################################################################
### INTERNET VACANCY INDEX                                                   ###
################################################################################
################################################################################
###
### Author: Johann Wagner
### Creation Date: 2024-05-08
### QA by:
### QA Date:
### Purpose: To load in the most recent, finalised, long format IVI results.
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
# # This IVI data includes further wrangled variables such as previous month and
# # year advertisements, growth numbers and growth rates for both previous month
# # and year.
# summary_ivi_long_anzsco2_state <- read_rds(
#   str_c(
#     file_path_rds_files_root_directory,
#     "Summary_Long_Data_2digit_State.RDS",
#     sep = "/"
#   )
#   ) |>
#   clean_names()
#
# summary_ivi_long_anzsco2_regional <- read_rds(
#   str_c(
#     file_path_rds_files_root_directory,
#     "Summary_Long_Data_2digit_Regional.RDS",
#     sep = "/"
#   )
#   ) |>
#   clean_names()
#
# summary_ivi_long_skill_state <- read_rds(
#   str_c(
#     file_path_rds_files_root_directory,
#     "Summary_Long_Data_SkillLev_State.RDS",
#     sep = "/"
#   )
#   ) |>
#   clean_names()

ivi_long_anzsco2_state <- read_rds(
  str_c(
    file_path_rds_files_root_directory,
    "IVI_Long_Data_2digit_State.RDS",
    sep = "/"
  )
) |>
  clean_names()

ivi_long_anzsco2_regional <- read_rds(
  str_c(
    file_path_rds_files_root_directory,
    "Long_Data_2digit_Regional.RDS",
    sep = "/"
  )
) |>
  clean_names()

ivi_long_skill_state <- read_rds(
  str_c(
    file_path_rds_files_root_directory,
    "Long_Data_SkillLev_State.RDS",
    sep = "/"
  )
) |>
  clean_names()

# ABS Labour Force Survey Data - Unemployment Rate
abs_lfs_data <- read_excel(
  here(
    "Data",
    "abs_unemployment_rate.xlsx"
  ),
  sheet = "Data1"
)




# Script Check ------------------------------------------------------------
# To ensure scripts are only run once if already in Global Environment.

script_checklist <- c(script_checklist, "01_data_loading")

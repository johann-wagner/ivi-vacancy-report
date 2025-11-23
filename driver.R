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
### Purpose: To source() .R and .qmd files in the Scripts folder driving the
###          analysis / model.
### Notes:
###
###
###
###
################################################################################





# tictoc::tic()
################################################################################
########### USE THIS TO TAKE MANUAL CONTROL OVER THE PARAMETERS! ###############
################################################################################
# date_today is the main parameter determining the ivi_release_date,
# spotlight_occupation, and next_ivi_release_date variables.
# Manually change using lubridate::as_date("yyyy-mm-dd") to run previous reports.
date_today <- Sys.Date()





# Setup and Configuration -------------------------------------------------
source(
  here::here(
    "Scripts",
    "00_setup_and_configuration.R"
    ),
  echo = TRUE,
  max.deparse.length = 1000
  )





# Render all previous Vacancy Reports -------------------------------------
################################################################################
########### USE THIS TO BULK-PRODUCE ALL PREVIOUS VACANCY REPORTS! #############
################################################################################
# This code creates a tibble using the data in draft_release_schedule.xlsx.
# This tibble is then iterated over with each row representing the respective
# date_control_parameter, spotlight_occupation, and next_ivi_release_date.
# These are then inputed into the quarto_render function using the pwalk function.
# vacancy_reports <- draft_release_schedule_clean |>
#   filter(!is.na(spotlight_occupation)) |>
#   mutate(
#
#     # Create the file name for each month.
#     output_file = str_c(
#       "Vacancy Report - ",
#       ivi_release_date,
#       " - ",
#       str_c(
#         measurement_month,
#         " ",
#         measurement_year
#       ),
#       ".docx"
#     ),
#
#     # Create a list variable with the respective ivi_release_dates
#     execute_params = map(
#       ivi_release_date,
#       \(ivi_release_date) list(date_today = ivi_release_date)
#     )
#     ) |>
#
#   select(
#     output_file,
#     execute_params
#     )
#
# # Iterate the output_file and execute_params across quarto_render function.
# pwalk(
#   vacancy_reports,
#   quarto_render,
#   input = here(
#     "Quarto",
#     "ivi_report.qmd"
#   ),
#   .progress = TRUE
# )


test_filename <- str_c(
  "Vacancy Report - ",
  date_control_parameter,
  " - ",
  measurement_date_my,
  ".docx"
)


# Quarto Report Automation ------------------------------------------------
quarto_render(
  input = here(
    "Quarto",
    "ivi_report.qmd"
    ),
  quarto_args = c("--output", test_filename),
  execute_params = list(
    date_today = date_today
  )
)



# Move rendered report into Outputs --------------------------------------------
# List files that contain ".docx"
file_path_ivi_report <- dir_ls(here(), regexp = ".docx$")

# Move the files
file_move(file_path_ivi_report, here("Output", "Vacancy Reports"))



# # Quarto Website Update ------------------------------------------------
# quarto_render(
#   input = here(
#     "Quarto",
#     "website_update.qmd"
#   ),
#   output_file = str_c(
#     "Website Update - ",
#     date_control_parameter,
#     " - ",
#     measurement_date_my,
#     ".docx"
#   ),
#   execute_params = list(
#     date_today = date_today
#   )
# )
#
#
#
# # Move rendered website update into Outputs --------------------------------------------
# # List files that contain ".docx"
# file_path_website_update <- dir_ls(here(), regexp = ".docx$")
#
# # Move the files
# file_move(file_path_website_update, here("Output", "Website Updates"))
#
# # tictoc::toc()

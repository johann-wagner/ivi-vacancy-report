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
### Purpose: To load in packages, any custom-functions, and parameters
### Notes:
###
###
###
###
################################################################################





# Loading in Packages ------------------------------------------------------
## Tidyverse -------------------------------------------------------------------
# To enable a coherent system of packages for data manipulation, exploration,
# and visualisation
### https://www.tidyverse.org/
library(tidyverse)



## Data Loading ---------------------------------------------------------------
# To easily read and load in excel / xlsx files
### https://readxl.tidyverse.org/
library(readxl)



## Data Cleaning -----------------------------------------------------------
# To easily examine and clean dirty data (tidyverse-oriented).
### https://github.com/sfirke/janitor
library(janitor)



## Data Visualisation -----------------------------------------------------------
# To create data visualisation based on 'The Grammar of Graphics'.
### https://ggplot2.tidyverse.org/
library(ggplot2)

# To easily scale and convert data values to human-friendly perceptual units.
### https://scales.r-lib.org/
library(scales)

# To support filled areas with geometric and image-based patterns.
### https://coolbutuseless.github.io/package/ggpattern/
# Not in Posit Package Manager, please use
# install.packages("ggpattern", method = "wininet").
# Also use install.packages("magick", method = "wininet")
library(magick)
library(ggpattern)


# Extra themes, geoms, and scales for 'ggplot2'.
### https://github.com/jrnold/ggthemes
# Not in Posit Package Manager, please use
# install.packages("ggthemes", method = "wininet")
library(ggthemes)

# Extra fonts for 'ggplot2'.
### https://github.com/wch/extrafont
library(extrafont)



## Data Reporting ---------------------------------------------------------------
# To unify the functionality of many packages from the RMarkdown ecosystem.
### https://github.com/quarto-dev/quarto
library(quarto)

# To enable a unified authoring framework for data science.
### https://github.com/rstudio/rmarkdown
library(rmarkdown)

# To easily produce tables for reporting and publications
### https://davidgohel.github.io/flextable/index.html
library(flextable)

# To allow numbers to be presented in English language.
### https://cran.r-project.org/web/packages/english/english.pdf
# Not in Posit Package Manager, please use
# install.packages("english", method = "wininet")
library(english)



## Misc --------------------------------------------------------------------
# To easily manage file paths relative to the top-level of the working folder.
### https://github.com/jennybc/here_here
library(here)

# To provide a cross-platform, uniform interface to file system operations.
### https://fs.r-lib.org/index.html
library(fs)





# Draft Release Schedule -------------------------------------------------------
# The Draft Release Schedule includes information on the IVI release dates and
# the spotlight occupation. This data must be loaded in and wrangled to generate
# the relevant parameters that flow through as inputs into further variables.
draft_release_schedule_raw <- read_excel(
  here(
    "draft_release_schedule.xlsx"
  )
)

draft_release_schedule_clean <- draft_release_schedule_raw |>
  mutate(
    ivi_release_date = as_date(ivi_release_date),
    next_ivi_release_date = lead(ivi_release_date)
  )



## Create Parameter Objects  -------------------------------------------------
date_control_parameter <- draft_release_schedule_clean |>
  filter(
    ivi_release_date >= as_date(date_today)
  ) |>
  slice(1) |>
  pull(ivi_release_date) |>
  as_date()

spotlight_occupation <- draft_release_schedule_clean |>
  filter(ivi_release_date == date_control_parameter) |>
  pull(spotlight_occupation) |>
  as.character()

next_ivi_release_date <- draft_release_schedule_clean |>
  filter(ivi_release_date == date_control_parameter) |>
  pull(next_ivi_release_date) |>
  as_date() |>
  format("%d %B %Y")



# Custom-functions --------------------------------------------------------
## flextable themes -------------------------------------------------------
# Create flextable theme for Key Points - Values Boxes
theme_jsa_ivi_value_boxes <- function(ft) {
  ft <- ft |>
    fontsize(
      size = 22,
      i = 1
    ) |>
    fontsize(
      size = 10,
      i = 2
    ) |>
    fontsize(
      size = 6,
      i = 3
    ) |>
    fontsize(
      size = 20,
      i = c(4, 5),
      j = c(1, 2)
    ) |>
    fontsize(
      size = 9,
      i = c(4, 5),
      j = 3
    ) |>
    fontsize(
      size = 9,
      i = c(6, 7)
    ) |>

    font(
      fontname = "Arial (Body)"
    ) |>

    bold(
      i = 1:5,
      j = 1:2
    ) |>

    line_spacing(
      i = 6:7,
      space = 1.15
    ) |>

    align(
      align = "center",
      i = 1:5,
      part = "body"
    ) |>
    align(
      align = "left",
      i = 6:7,
      part = "body"
    ) |>
    valign(
      valign = "bottom",
      i = 1,
      part = "body"
    ) |>
    valign(
      valign = "top",
      i = 2,
      part = "body"
    ) |>
    valign(
      valign = "top",
      i = 3,
      part = "body"
    ) |>

    delete_part(part = "header") |>

    border_remove() |>

    bg(
      i = 1:3,
      bg = "#2F005F"
    ) |>
    bg(
      i = 4:5,
      bg = "#F6E8FF"
    ) |>
    bg(
      i = 6:7,
      bg = "#F3EFEB"
    ) |>

    color(
      i = 1:3,
      color = "white"
    ) |>
    color(
      color = ifelse(
        monthly_absolute_change_most_recent_previous_month_value > 0,
        "#047817",
        "#B80000"
      ),
      i = 4,
      j = 1
    ) |>
    color(
      color = ifelse(
        annual_percentage_change_most_recent_previous_year_value > 0,
        "#047817",
        "#B80000"
      ),
      i = 5,
      j = 1
    ) |>
    color(
      i = 4:7,
      j = 2:3,
      color = "black"
    ) |>

    set_table_properties(
      layout = "fixed"
    ) |>

    height(
      height = 1.5,
      i = 1,
      unit   = "cm"
    ) |>
    height(
      height = 1.0,
      i = 2,
      unit   = "cm"
    ) |>
    height(
      height = 0.5,
      i = 3,
      unit   = "cm"
    ) |>
    height(
      height = 1.8,
      i = 4:5,
      unit   = "cm"
    ) |>
    height(
      height = 2.85,
      i = 6:7,
      unit   = "cm"
    ) |>
    hrule(
      rule = "exact",
      part = "body"
    ) |>

    width(
      width = 1.1,
      j = 1,
      unit = "cm"
    ) |>
    width(
      width = 2.4,
      j = 2,
      unit = "cm"
    ) |>
    width(
      width = 1.6,
      j = 3,
      unit = "cm"
    ) |>

    merge_h(
      i = c(1, 2, 3, 6, 7)
    )

  return(ft)
}

# Create flextable theme for IVI Summary Sheet - Base
theme_jsa_ivi_summary_sheet_base <- function(ft) {
  set_flextable_defaults(
    border.color = "#7F7F7F"
  )

  ft <- ft |>
    fontsize(
      size = 9,
      part = "all"
    ) |>

    font(
      fontname = "Arial (Body)",
      part     = "all"
    ) |>

    align(
      align = c("left", rep("right", 7)),
      part  = "all"
    ) |>

    theme_zebra(
      odd_body  = "#F1F4F9",
      even_body = "#FFFFFF"
    ) |>

    color(
      ~ `Monthly change (no.)` == 0,
      color = "black",
      j     = 4
    ) |>
    color(
      ~ `Monthly change (no.)` < 0,
      color = "#B80000",
      j     = 4
    ) |>
    color(
      ~ `Monthly change (no.)` > 0,
      color = "#14CA6B",
      j     = 4
    ) |>
    color(
      ~ `Annual change (no.)` == 0,
      color = "black",
      j     = 7
    ) |>
    color(
      ~ `Annual change (no.)` < 0,
      color = "#B80000",
      j     = 7
    ) |>
    color(
      ~ `Annual change (no.)` > 0,
      color = "#14CA6B",
      j     = 7
    ) |>

    border_outer(part = "body") |>
    vline(
      j    = c(1, 2, 3, 5, 6, 8),
      part = "body"
    ) |>

    height(
      height = 0.76,
      part   = "body",
      unit   = "cm"
    )

    width_list = c(6.5, 2.1, 2, 0.6, 1.6, 2, 0.6, 1.6)

    for(i in seq_along(width_list)) {
      ft <- ft |>
        width(
          width = width_list[i],
          j     = i,
          unit  = "cm"
        )
    }

    return(ft)
}


# Create flextable theme for IVI Summary Sheet - Australia/State
theme_jsa_ivi_summary_sheet_state <- function(ft) {
  ft <- ft |>
    bold(
      bold = TRUE,
      part = "header"
    ) |>
    bg(
      bg   = "#2C054E",
      part = "header"
    ) |>
    bg(
      bg = "#F5E8FF",
      i  = 1
    ) |>
    bold(
      bold = TRUE,
      i    = 1
    ) |>
    color(
      color = "#FFFFFF",
      part  = "header"
    ) |>
    height(
      height = 2,
      part   = "header",
      unit   = "cm"
    )

  return(ft)
}


# Create flextable theme for IVI Summary Sheet - ANZSCO Major Groups
theme_jsa_ivi_summary_sheet_anzsco_skill <- function(ft) {
  ft <- ft |>
    delete_part(part = "header") |>
    border_outer()

  return(ft)
}



## ggplot2 themes ----------------------------------------------------------
theme_jsa_ivi_job_ads_unemployment_rate <- function() {
  theme_minimal() +
    theme(
      # Remove gridlines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      # Add x-axis tick markers
      axis.line.x = element_line(colour = "#595959"),
      axis.ticks.x   = element_line(colour = "#595959"),

      # Add font style
      axis.text.x        = element_text(family = "Arial", size = 8, colour = "#595959"),
      axis.text.y        = element_text(family = "Arial", size = 8, colour = "#595959"),
      axis.title.y.right = element_text(angle = 90),
      axis.title         = element_text(family = "Arial", size = 8, colour = "#595959")
    )
}



## Data Transformations -----------------------------------------------------
# Apply rounding and formatting changes
mutate_rounding_formatting <- function(ivi_data) {
  ivi_data <- ivi_data |>

    mutate(
      job_advertisements = case_when(
        job_advertisements >= 100000 ~ signif(job_advertisements, 4),
        job_advertisements >= 10000  ~ signif(job_advertisements, 3),
        job_advertisements >= 1000   ~ signif(job_advertisements, 2),
      ),

      job_advertisements_character = case_when(
        job_advertisements >= 100000 ~ signif(job_advertisements, 4),
        job_advertisements >= 10000  ~ signif(job_advertisements, 3),
        job_advertisements >= 1000   ~ signif(job_advertisements, 2),
      ) |>
        comma(),

      x1m_growth_no = case_when(
        abs(x1m_growth_no) >= 10000          ~ signif(x1m_growth_no, 3),
        abs(x1m_growth_no) >= 100            ~ signif(x1m_growth_no, 2),
        between(x1m_growth_no, 5, 10)        ~ 10,
        between(x1m_growth_no, -10, -5)      ~ -10,
        between(abs(x1m_growth_no), 10, 100) ~ signif(x1m_growth_no, 1),
        .default                             = 0
      ),

      x1m_growth_no_character = case_when(
        abs(x1m_growth_no) >= 10000          ~ signif(x1m_growth_no, 3),
        abs(x1m_growth_no) >= 100            ~ signif(x1m_growth_no, 2),
        between(x1m_growth_no, 5, 10)        ~ 10,
        between(x1m_growth_no, -10, -5)      ~ -10,
        between(abs(x1m_growth_no), 10, 100) ~ signif(x1m_growth_no, 1),
        .default                             = 0
      ) |>
        comma(),

      x1m_growth_percent = case_when(
        x1m_growth_no == 0 ~ 0,
        .default           = x1m_growth_percent
      ),

      x1m_growth_percent_character = x1m_growth_percent |>
        percent(accuracy = 0.1),

      x1y_growth_no = case_when(
        abs(x1y_growth_no) >= 10000          ~ signif(x1y_growth_no, 3),
        abs(x1y_growth_no) >= 100            ~ signif(x1y_growth_no, 2),
        between(x1y_growth_no, 5, 10)        ~ 10,
        between(x1y_growth_no, -10, -5)      ~ -10,
        between(abs(x1y_growth_no), 10, 100) ~ signif(x1y_growth_no, 1),
        .default                             = 0
      ),

      x1y_growth_no_character = case_when(
        abs(x1y_growth_no) >= 10000          ~ signif(x1y_growth_no, 3),
        abs(x1y_growth_no) >= 100            ~ signif(x1y_growth_no, 2),
        between(x1y_growth_no, 5, 10)        ~ 10,
        between(x1y_growth_no, -10, -5)      ~ -10,
        between(abs(x1y_growth_no), 10, 100) ~ signif(x1y_growth_no, 1),
        .default                             = 0
      ) |>
        comma(),

      x1y_growth_percent = case_when(
        x1y_growth_no == 0 ~ 0,
        .default           = x1y_growth_percent
      ),

      x1y_growth_percent_character = x1y_growth_percent |>
        percent(accuracy = 0.1)
    )

  return(ivi_data)
}


# Add up and down arrow columns based on percentage change variables
mutate_add_arrow_variables <- function(ivi_data) {
  ivi_data <- ivi_data |>

    mutate(
      x1m_growth_arrow = case_when(
        x1m_growth_no >  0 ~ "▲",
        x1m_growth_no <  0 ~ "▼",
        x1m_growth_no == 0 ~ "▬"
      ),

      x1y_growth_arrow = case_when(
        x1y_growth_no >  0 ~ "▲",
        x1y_growth_no <  0 ~ "▼",
        x1y_growth_no == 0 ~ "▬"
      )
    )

  return(ivi_data)
}


# Change variable names
select_relevant_variables <- function(ivi_data, variable_col1) {
  ivi_data <- ivi_data |>

    select(
      `   `                     = {{ variable_col1 }},
      `Number of job ads (no.)` = job_advertisements_character,
      `Monthly change (no.)`    = x1m_growth_no_character,
      `  `                      = x1m_growth_arrow,
      `Monthly change (%)`      = x1m_growth_percent_character,
      `Annual change (no.)`     = x1y_growth_no_character,
      ` `                       = x1y_growth_arrow,
      `Annual change (%)`       = x1y_growth_percent_character
    )

  return(ivi_data)
}





# Parameters ---------------------------------------------------------------
## Dates -------------------------------------------------------------------
# Measurement Date - Month Year
measurement_date_my <- {date_control_parameter %m-% months(1)} |>
  format("%B %Y")

# Measurement Date - Month
measurement_date_m <- {date_control_parameter %m-% months(1)} |>
  format("%B")

# Measurement Date - yyyy-mm-01
measurement_date_ymd <- {date_control_parameter %m-% months(1)} |>
  update(mday = 01)

# Measurement Date - mm-Monthyyyy
measurement_date_m_my <- {date_control_parameter %m-% months(1)} |>
  format("%m-%B%Y")

# Measurement Date - mm-Monthyyyy
measurement_date_y <- {date_control_parameter %m-% months(1)} |>
  format("%Y")

# Release Date - Day Month Year
release_date_dmy <- date_control_parameter |>
  format("%d %B %Y")

# ABS Labour Force Date - Month Year
abs_lfs_release_date_my <- {date_control_parameter %m-% months(2)} |>
  format("%B %Y")

# ABS Job Vacancies, Australia Reference Date - Month Year
abs_job_vacancies_reference_date_my <- date_control_parameter |>
  quarter(type = "date_first") %m-% months(2) |>
  format("%B %Y")




## Fonts -------------------------------------------------------------------
font_import(paths = here("Fonts"), prompt = FALSE)
loadfonts(quiet = TRUE)



## File Paths --------------------------------------------------------------
file_path_ivi_root_directory <- here() |>
  dirname() |>
  dirname() |>
  file.path()

file_path_rds_files_root_directory <- file_path_ivi_root_directory |>
  str_c(
    "01 IVI Files",
    "07 Long Dataframes",
    ".RDS Files",
    "Archive",
    measurement_date_y,
    measurement_date_m_my,
    sep = "/"
  )



# Script Check ------------------------------------------------------------
# To ensure scripts are only run once if already in Global Environment.
script_checklist <- c("00_setup_and_configuration")




# ARCHIVE: Date Control Parameter Logic ----------------------------------------
# This code was an attempt to have user input determine what month's data to use.
# I found it tricky to use effectively.
# ifelse(
#
#   menu(
#     c("Yes!", "No, I want to create a previous month's Report. Please provide answer in following format: yyyy-mm-dd"),
#     title = "Do you want to produce the next IVI Vacancy Report?"
#   ) == 1,
#
#   # Get the date for next month's release
#   {
#     date_control_parameter <- draft_release_schedule_clean |>
#       filter(
#         ivi_release_date > as_date(Sys.Date())
#       ) |>
#       slice(1) |>
#       pull(ivi_release_date) |>
#       as_date()
#   },
#
#   # Get a manually entered date
#   {
#     date_control_parameter <- readline("Please provide exact IVI release date in the following format: yyyy-mm-dd")
#
#     date_control_parameter <- as_date(date_control_parameter)
#   }
# )

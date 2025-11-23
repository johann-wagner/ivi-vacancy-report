################################################################################
################################################################################
### INTERNET VACANCY INDEX                                                   ###
################################################################################
################################################################################
###
### Author: Johann Wagner
### Creation Date: 2024-05-09
### QA by:
### QA Date:
### Purpose: To transform the most recent IVI data calculating relevant counts
###          and statistics for the Vacancy Report.
### Notes:
###
###
###abs_lfs_data
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





# Data Subsetting ------------------------------------------------------------
if(!("03_data_subsetting" %in% script_checklist)) {
  source(
    here::here(
      "Scripts",
      "03_data_subsetting.R"
    ),
    echo = TRUE,
    max.deparse.length = 1000
  )
}





# Data Transformation -----------------------------------------------------
## Summary Sheet Flex Tables ---------------------------------------------------
### Australia/States --------------------------------------------------------
table_summary_sheet_state <- summary_ivi_long_anzsco2_state_australia_state_most_recent |>

  # Apply rounding and formatting changes
  mutate_rounding_formatting() |>

  # Add up and down arrow columns based on percentage change variables
  mutate_add_arrow_variables()



### ANZSCO: Major Groups ---------------------------------------------------
table_summary_sheet_anzsco <- summary_ivi_long_anzsco2_state_anzsco1_most_recent |>

  # Ensure ANZSCO Major Group titles are
  mutate(
    anzsco_title = anzsco_title |>
      str_to_title() |>
      str_replace(
        pattern     = "And",
        replacement = "and"
      )
    ) |>

  # Apply rounding and formatting changes
  mutate_rounding_formatting() |>

  # Add up and down arrow columns based on percentage change variables
  mutate_add_arrow_variables()



### Skill ---------------------------------------------------
table_summary_sheet_skill <- summary_ivi_long_skill_state_skill_most_recent |>

  # Add Skill Level description names
  mutate(
    skill_level_group = case_when(
      skill_level == 1 ~ str_c(skill_level_group, " - Bachelor degree or higher"),
      skill_level == 2 ~ str_c(skill_level_group, " - Advanced Diploma or Diploma"),
      skill_level == 3 ~ str_c(skill_level_group, " - Certificate IV or III* (Skilled VET)"),
      skill_level == 4 ~ str_c(skill_level_group, " - Certificate II or III"),
      skill_level == 5 ~ str_c(skill_level_group, " - Certificate I or secondary education"),
    )
  ) |>

  # Apply rounding and formatting changes
  mutate_rounding_formatting() |>

  # Add up and down arrow columns based on percentage change variables
  mutate_add_arrow_variables()





## Key Points Counts and Statistics ---------------------------------------
# Let's create a tibble to hold all the 'Key Points' counts and statistics
statistics_and_counts_key_points <- tibble(
  statistic_count_name      = as.character(),
  statistic_count_value     = as.numeric(),
  statistic_count_character = as.character()
) |>


### First paragraph ---------------------------------------------------------
  # Calculate the number of positive and negative changes in vacancy numbers
  # over the past 12 months
  add_row(
    statistic_count_name = "positive_past_12_months",

    statistic_count_value = {
      summary_ivi_long_anzsco2_state_australia_past_12_months |>
        summarise(
          length(x1m_growth_no[x1m_growth_no > 0])
        ) |>
        pull()
    },

    statistic_count_character = if_else(
      statistic_count_value <= 1,
      words(statistic_count_value),
      as.character(statistic_count_value)
    )
  ) |>

  add_row(
    statistic_count_name = "negative_past_12_months",

    statistic_count_value = {
      summary_ivi_long_anzsco2_state_australia_past_12_months |>
        summarise(
          length(x1m_growth_no[x1m_growth_no < 0])
        ) |>
        pull()
    },

    statistic_count_character = if_else(
      statistic_count_value <= 1,
      words(statistic_count_value),
      as.character(statistic_count_value)
    )
  ) |>

  # Compare monthly average of 2019 with the most recent vacancy number
  add_row(
    statistic_count_name = "monthly_average_most_recent_to_2019_comparison",

    statistic_count_value = {
      # Calculate most recent vacancy number
      {summary_ivi_long_anzsco2_state_australia_most_recent |>
          pull(job_advertisements)} /

        # Calculate monthly average vacancy number for 2019
        {summary_ivi_long_anzsco2_state_australia_2019 |>
            summarise(
              mean(job_advertisements)
            ) |>
            pull()}

      # To calculate the percentage change
    } - 1,

    statistic_count_character = statistic_count_value |>
      abs() |>
      percent()
  ) |>



### Bullet Point 1 ----------------------------------------------------------
  # Calculate the monthly percentage change between most recent and previous month
  add_row(
    statistic_count_name = "monthly_percentage_change_most_recent_previous_month",

    statistic_count_value = {
      table_summary_sheet_state |>
        filter(state == "AUST") |>
        pull(x1m_growth_percent)
    },

    statistic_count_character = {
      table_summary_sheet_state |>
        filter(state == "AUST") |>
        pull(x1m_growth_percent_character) |>
        str_remove(pattern = "-")
    }
  ) |>

  # Calculate the monthly absolute change between most recent and previous month
  add_row(
    statistic_count_name = "monthly_absolute_change_most_recent_previous_month",

    statistic_count_value = {
      table_summary_sheet_state |>
        filter(state == "AUST") |>
        pull(x1m_growth_no)
    },

    statistic_count_character = {
      table_summary_sheet_state |>
        filter(state == "AUST") |>
        pull(x1m_growth_no_character) |>
        str_remove(pattern = "-")
    }
  ) |>

  # Calculate the seasonally adjusted most recent vacancy number
  add_row(
    statistic_count_name = "most_recent_seasonally_adjusted",

    statistic_count_value = {
      table_summary_sheet_state |>
        filter(state == "AUST") |>
        pull(job_advertisements)
    },

    statistic_count_character = {
      table_summary_sheet_state |>
        filter(state == "AUST") |>
        pull(job_advertisements_character)
    }
  ) |>

#### Bullet Point 1.1 --------------------------------------------------------
  # Calculate the annual percentage change between most recent and previous year
  add_row(
    statistic_count_name = "annual_percentage_change_most_recent_previous_year",

    statistic_count_value = {
      table_summary_sheet_state |>
        filter(state == "AUST") |>
        pull(x1y_growth_percent)
    },

    statistic_count_character = {
      table_summary_sheet_state |>
        filter(state == "AUST") |>
        pull(x1y_growth_percent_character) |>
        str_remove(pattern = "-")
    }
  ) |>
  # Calculate the annual percentage change between most recent and previous month
  add_row(
    statistic_count_name = "annual_absolute_change_most_recent_previous_year",

    statistic_count_value = {
      table_summary_sheet_state |>
        filter(state == "AUST") |>
        pull(x1y_growth_no)
    },

    statistic_count_character = {
      table_summary_sheet_state |>
        filter(state == "AUST") |>
        pull(x1y_growth_no_character) |>
        str_remove(pattern = "-")
    }
  ) |>


#### Bullet Point 1.2 --------------------------------------------------------



### Bullet Point 2 --------------------------------------------------------
  # Calculate the number of positive and negative changes in vacancy numbers
  # across all states and territories
  add_row(
    statistic_count_name = "positive_states",

    statistic_count_value = {
      table_summary_sheet_state |>
        filter(state != "AUST") |>
        summarise(
          length(x1m_growth_no[x1m_growth_no > 0])
        ) |>
        pull()
    },

    statistic_count_character = if_else(
      statistic_count_value <= 1,
      words(statistic_count_value),
      as.character(statistic_count_value)
    )
  ) |>

  add_row(
    statistic_count_name = "negative_states",

    statistic_count_value = {
      table_summary_sheet_state |>
        filter(state != "AUST") |>
        summarise(
          length(x1m_growth_no[x1m_growth_no < 0])
        ) |>
        pull()
    },

    statistic_count_character = if_else(
      statistic_count_value <= 1,
      words(statistic_count_value),
      as.character(statistic_count_value)
    )
  ) |>


  # Calculate the top 3 largest monthly percentage change by state and territory
  add_row(
    statistic_count_name = c(
      "top_3_state_monthly_percentage_change_first",
      "top_3_state_monthly_percentage_change_second",
      "top_3_state_monthly_percentage_change_third"
      ),

    statistic_count_value = {
      table_summary_sheet_state |>
        filter(state != "AUST") |>
        arrange(desc(abs(x1m_growth_percent))) |>
        slice(1:3) |>
        pull(x1m_growth_percent)
    },

    statistic_count_character = {
      table_summary_sheet_state |>
        filter(state != "AUST") |>
        arrange(desc(abs(x1m_growth_percent))) |>
        slice(1:3) |>
        pull(x1m_growth_percent_character) |>
        str_remove(pattern = "-")
    }
  ) |>

    # Calculate the top 3 largest monthly absolute change by state and territory
  add_row(
    statistic_count_name = c(
      "top_3_state_monthly_absolute_change_first",
      "top_3_state_monthly_absolute_change_second",
      "top_3_state_monthly_absolute_change_third"
    ),

    statistic_count_value = {
      table_summary_sheet_state |>
        filter(state != "AUST") |>
        arrange(desc(abs(x1m_growth_percent))) |>
        slice(1:3) |>
        pull(x1m_growth_no)
    },

    statistic_count_character = {
      table_summary_sheet_state |>
        filter(state != "AUST") |>
        arrange(desc(abs(x1m_growth_percent))) |>
        slice(1:3) |>
        pull(x1m_growth_no_character) |>
        str_remove(pattern = "-")
    }
  ) |>

  # Find which states have the top 3 largest monthly percentage change by
  # state and territory
  add_row(
    statistic_count_name = c(
      "top_3_state_monthly_state_change_first",
      "top_3_state_monthly_state_change_second",
      "top_3_state_monthly_state_change_third"
    ),

    statistic_count_value = NA,

    statistic_count_character = {
      table_summary_sheet_state |>
        filter(state != "AUST") |>
        arrange(desc(abs(x1m_growth_percent))) |>
        slice(1:3) |>
        pull(state_full_name)
    }
  ) |>



### Bullet Point 3 --------------------------------------------------------
  # Calculate the number of positive and negative changes in vacancy numbers
  # across all skill level groups
  add_row(
    statistic_count_name = "positive_skills",

    statistic_count_value = {
      table_summary_sheet_skill |>
        summarise(
          length(x1m_growth_no[x1m_growth_no > 0])
        ) |>
        pull()
    },

    statistic_count_character = if_else(
      statistic_count_value <= 1,
      words(statistic_count_value),
      as.character(statistic_count_value)
    )
  ) |>

  add_row(
    statistic_count_name = "negative_skills",

    statistic_count_value = {
      table_summary_sheet_skill |>
        summarise(
          length(x1m_growth_no[x1m_growth_no < 0])
        ) |>
        pull()
    },

    statistic_count_character = if_else(
      statistic_count_value <= 1,
      words(statistic_count_value),
      as.character(statistic_count_value)
    )
  ) |>

  # Calculate the top 2 largest monthly percentage change by skill
  add_row(
    statistic_count_name = c(
      "top_2_skills_monthly_percentage_change_first",
      "top_2_skills_monthly_percentage_change_second"
    ),

    statistic_count_value = {
      table_summary_sheet_skill |>
        arrange(desc(abs(x1m_growth_percent))) |>
        slice(1:2) |>
        pull(x1m_growth_percent)
    },

    statistic_count_character = {
      table_summary_sheet_skill |>
        arrange(desc(abs(x1m_growth_percent))) |>
        slice(1:2) |>
        pull(x1m_growth_percent_character) |>
        str_remove(pattern = "-")
    }
  ) |>

  # Calculate the top 2 largest monthly absolute change by skill
  add_row(
    statistic_count_name = c(
      "top_2_skills_monthly_absolute_change_first",
      "top_2_skills_monthly_absolute_change_second"
    ),

    statistic_count_value = {
      table_summary_sheet_skill |>
        arrange(desc(abs(x1m_growth_percent))) |>
        slice(1:2) |>
        pull(x1m_growth_no)
    },

    statistic_count_character = {
      table_summary_sheet_skill |>
        arrange(desc(abs(x1m_growth_percent))) |>
        slice(1:2) |>
        pull(x1m_growth_no_character) |>
        str_remove(pattern = "-")
    }
  ) |>

  # Find which skills have the top 2 largest monthly percentage change by
  # skill
  add_row(
    statistic_count_name = c(
      "top_2_skills_monthly_skills_change_first",
      "top_2_skills_monthly_skills_change_second"
    ),

    statistic_count_value = NA,

    statistic_count_character = {
      table_summary_sheet_skill |>
        # Adjust text
        mutate(
          skill_level_group_adjusted = case_when(
              skill_level == 1 ~ str_c(
                "Skill Level ",
                skill_level,
                " (commensurate with Bachelor degree or higher)"
                ),

              skill_level == 2 ~ str_c(
                "Skill Level ",
                skill_level,
                " (commensurate with Advanced Diploma or Diploma)"
                ),

              skill_level == 3 ~ str_c(
                "Skill Level ",
                skill_level,
                " (commensurate with Certificate IV or III)"
                ),

              skill_level == 4 ~ str_c(
                "Skill Level ",
                skill_level,
                " (commensurate with Certificate II or III)"
                ),

              skill_level == 5 ~ str_c(
                "Skill Level ",
                skill_level,
                " (commensurate with Certificate I or secondary education)"
                )
            )
        ) |>
        arrange(desc(abs(x1m_growth_percent))) |>
        slice(1:2) |>
        pull(skill_level_group_adjusted)
    }
  ) |>


### Bullet Point 4 --------------------------------------------------------
  # Calculate the number of positive and negative changes in vacancy numbers
  # across all anzsco major group occupations
  add_row(
    statistic_count_name = "positive_anzsco",

    statistic_count_value = {
      table_summary_sheet_anzsco |>
        summarise(
          length(x1m_growth_no[x1m_growth_no > 0])
        ) |>
        pull()
    },

    statistic_count_character = if_else(
      statistic_count_value <= 1,
      words(statistic_count_value),
      as.character(statistic_count_value)
    )
  ) |>

  add_row(
    statistic_count_name = "negative_anzsco",

    statistic_count_value = {
      table_summary_sheet_anzsco |>
        summarise(
          length(x1m_growth_no[x1m_growth_no < 0])
        ) |>
        pull()
    },

    statistic_count_character = if_else(
      statistic_count_value <= 1,
      words(statistic_count_value),
      as.character(statistic_count_value)
    )
  ) |>

  # Calculate the top 2/bottom 1 largest monthly percentage change by anzsco
  add_row(
    statistic_count_name = c(
      "top_2_anzsco_monthly_percentage_change_first",
      "top_2_anzsco_monthly_percentage_change_second",
      "bottom_1_anzsco_monthly_percentage_change"
    ),

    statistic_count_value = {
      table_summary_sheet_anzsco |>
        arrange(desc(abs(x1m_growth_percent))) |>
        slice(1:2, 8) |>
        pull(x1m_growth_percent)
    },

    statistic_count_character = {
      table_summary_sheet_anzsco |>
        arrange(desc(abs(x1m_growth_percent))) |>
        slice(1:2, 8) |>
        pull(x1m_growth_percent_character) |>
        str_remove(pattern = "-")
    }
  ) |>

  # Calculate the top 2 / bottom 1 largest monthly absolute change by anzsco
  add_row(
    statistic_count_name = c(
      "top_2_anzsco_monthly_absolute_change_first",
      "top_2_anzsco_monthly_absolute_change_second",
      "bottom_1_anzsco_monthly_absolute_change"
    ),

    statistic_count_value = {
      table_summary_sheet_anzsco |>
        arrange(desc(abs(x1m_growth_percent))) |>
        slice(1:2, 8) |>
        pull(x1m_growth_no)
    },

    statistic_count_character = {
      table_summary_sheet_anzsco |>
        arrange(desc(abs(x1m_growth_percent))) |>
        slice(1:2, 8) |>
        pull(x1m_growth_no_character) |>
        str_remove(pattern = "-")
    }
  ) |>

  # Find which anzscos have the top 2 / bottom 1 largest monthly percentage change by
  # anzsco
  add_row(
    statistic_count_name = c(
      "top_2_anzsco_monthly_anzsco_change_first",
      "top_2_anzsco_monthly_anzsco_change_second",
      "bottom_1_anzsco_monthly_anzsco_change"
    ),

    statistic_count_value = NA,

    statistic_count_character = {
      table_summary_sheet_anzsco |>
        arrange(desc(abs(x1m_growth_percent))) |>
        slice(1:2, 8) |>
        pull(anzsco_title)
    }
  ) |>


### Bullet Point 5 --------------------------------------------------------
# Recruitment activity was concentrated in [regional vs. metropolitan] Australia, with [regional vs. metropolitan %share] of job advertisements in **`r measurement_date_my`** found in Australia's [capital cities vs. regions].
  add_row(
    statistic_count_name = c(
      "metropolitan_proportion",
      "regional_proportion"
    ),

    statistic_count_value = {
      summary_ivi_long_anzsco2_state_regional_most_recent |>
        summarise(
          job_advertisements = sum(job_advertisements),
          .by = metropolitan_regional
          ) |>
        mutate(
          job_advertisements_proportion = job_advertisements / sum(job_advertisements)
        ) |>
        arrange(metropolitan_regional) |>
        pull(job_advertisements_proportion)
    },

    statistic_count_character = statistic_count_value |>
      percent(accuracy = 0.1)
  ) |>


  # Over the last 12 months, job advertisements have [increased/decreased] in regional areas ([up/down] by [%]); however, a [smaller/larger] [increase/decrease] was recorded for capital cities ([up/down] by [%]).
  add_row(
    statistic_count_name = c(
      "annual_percentage_change_most_recent_previous_year_metropolitan",
      "annual_percentage_change_most_recent_previous_year_regional"
    ),

    statistic_count_value = {
      summary_ivi_long_anzsco2_state_regional_most_recent |>
        summarise(
          py_job_advertisements = sum(py_job_advertisements),
          job_advertisements    = sum(job_advertisements),
          .by = metropolitan_regional
        ) |>
        mutate(
          x1y_growth_percent = (job_advertisements - py_job_advertisements) / py_job_advertisements
        ) |>
        arrange(metropolitan_regional) |>
        pull(x1y_growth_percent)
    },

    statistic_count_character = statistic_count_value |>
      percent(accuracy = 0.1) |>
      str_remove("-")
  )

### Bullet Point 6 --------------------------------------------------------



### Values Boxes --------------------------------------------------------
# # Create a tibble containing all the relevant data.
# # Ensure cells that will be merged contain the same data.
# most_recent_seasonally_adjusted <- statistics_and_counts_key_points |>
#   filter(
#     statistic_count_name == "most_recent_seasonally_adjusted"
#   ) |>
#   pull(statistic_count_character)
#
# monthly_percentage_change_most_recent_previous_month <- statistics_and_counts_key_points |>
#   filter(
#     statistic_count_name == "monthly_percentage_change_most_recent_previous_month"
#   ) |>
#   pull(statistic_count_character)
#
# annual_percentage_change_most_recent_previous_year <- statistics_and_counts_key_points |>
#   filter(
#     statistic_count_name == "annual_percentage_change_most_recent_previous_year"
#   ) |>
#   pull(statistic_count_character)
#
# table_values_boxes <- tibble(
#   column_1 = c(
#     most_recent_seasonally_adjusted,
#
#     "Seasonally adjusted",
#
#     {
#       case_when(
#         monthly_percentage_change_most_recent_previous_month >  0 ~ "▲",
#         monthly_percentage_change_most_recent_previous_month <  0 ~ "▼",
#         monthly_percentage_change_most_recent_previous_month == 0 ~ "▬"
#       )
#     },
#
#     {
#       case_when(
#         annual_percentage_change_most_recent_previous_year >  0 ~ "▲",
#         annual_percentage_change_most_recent_previous_year <  0 ~ "▼",
#         annual_percentage_change_most_recent_previous_year == 0 ~ "▬"
#       )
#     }
#   ),
#   column_2 = c(
#     most_recent_seasonally_adjusted,
#
#     "Seasonally adjusted",
#
#     monthly_percentage_change_most_recent_previous_month,
#
#     annual_percentage_change_most_recent_previous_year
#
#   ),
#   column_3 = c(
#     most_recent_seasonally_adjusted,
#
#     "Seasonally adjusted",
#
#     "Monthly change",
#
#     "Annual change"
#   )
# )
#
#
#
#
#

## Data Visualisation ------------------------------------------------------
data_viz_ivi_australia <- summary_ivi_long_skill_state_time_series |>
  mutate(
    date = date |> as_date()
  ) |>
  select(
    date,
    job_advertisements
  )



# Filter to seasonally adjusted unemployment rate for all persons
data_viz_abs_unemployment_rate <- abs_lfs_data |>

  # Remove irrelevant metadata
  slice(-c(1:9)) |>

  # Convert Excel date to date class and rename/adjust unemployment_rate
  mutate(
    date              = convert_to_date(`...1`) |>
      as_date(),
    unemployment_rate = as.numeric(`Unemployment rate ;  Persons ;...66`) / 100
  ) |>

  # We only care about Jan 2006 to most recent IVI data
  filter(
    between(date, as_date("2006-01-01"), measurement_date_ymd)
  ) |>

  # We only care about seasonally adjusted unemployment rate for all persons
  select(date, unemployment_rate)





# Script Check ------------------------------------------------------------
# To ensure scripts are only run once if already in Global Environment.

script_checklist <- c(script_checklist, "03_data_transformation")

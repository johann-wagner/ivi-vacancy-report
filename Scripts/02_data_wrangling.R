################################################################################
################################################################################
### INTERNET VACANCY INDEX                                                   ###
################################################################################
################################################################################
###
### Author: Johann Wagner
### Creation date: 2025-07-14
### QA by:
### QA date:
### Purpose: This script was created to create the summary long format, which
###          is used in later scripts downstream. These summary files used to
###          be created in the pre-IVI migration workflows, but are now going
###          to be embedded into the report automation. The summary files
###          do not need to be saved as they are not used for any other products.
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





# Data Wrangling ---------------------------------------------------------
# The below code was copy-pasted from IVI_Data_Processing_Master.R.
# It was further adapted and improved by Johann.
# Create long data summary function
create_long_data_summary <- function(long_data, id_cols){

  id_col_names <- names(long_data)[id_cols]

  # Filter and create base values for indexing
  base_df <- long_data %>%
    drop_na(job_advertisements) %>%
    group_by(across(all_of(id_col_names))) %>%
    filter(date == min(unique(date))) %>%
    rename(ind_base = job_advertisements) %>%
    ungroup() %>%
    select(-date)


  # Join base values and calculate growth metrics:
  # - previous month,
  # - previous year,
  # - 1-month absolute change
  # - 1-month percentage change
  #   12-month absolute change
  # - 12-month percentage change
  summary_long_data <- long_data %>%
    left_join(base_df, by = id_col_names) %>%
    group_by(across(all_of(id_col_names))) %>%
    arrange(ser, date) %>%
    mutate(
      index_value = (job_advertisements / ind_base) * 100,
      pm_job_advertisements = lag(job_advertisements, 1),
      py_job_advertisements = lag(job_advertisements, 12),
      x1m_growth_no = job_advertisements - pm_job_advertisements,
      x1m_growth_percent = (job_advertisements - pm_job_advertisements) / pm_job_advertisements,
      x1y_growth_no = job_advertisements - py_job_advertisements,
      x1y_growth_percent = (job_advertisements - py_job_advertisements) / py_job_advertisements
    ) %>%
    ungroup() %>%
    select(-ind_base) %>%

    return(summary_long_data)
}

# "Long_Data_2digit_State"      = 1:7,
# "Long_Data_2digit_Regional"   = 1:8,
# "Long_Data_4digit_State"      = 1:8,
# "Long_Data_4digit_Regional"   = 1:9,
# "Long_Data_SkillLev_State"    = 1:7,
# "Long_Data_SkillLev_Regional" = 1:8


summary_ivi_long_anzsco2_state <- create_long_data_summary(ivi_long_anzsco2_state, 1:7)
summary_ivi_long_anzsco2_regional <- create_long_data_summary(ivi_long_anzsco2_regional, 1:8)
summary_ivi_long_skill_state <- create_long_data_summary(ivi_long_skill_state, 1:7)

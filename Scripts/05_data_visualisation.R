################################################################################
################################################################################
### INTERNET VACANCY INDEX                                                   ###
################################################################################
################################################################################
###
### Author: Johann Wagner
### Creation Date: 2024-05-13
### QA by:
### QA Date:
### Purpose: To create data visualisations used in the Vacancy Report.
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





# Data Transformation ------------------------------------------------------------
if(!("04_data_transformation" %in% script_checklist)) {
  source(
    here::here(
      "Scripts",
      "04_data_transformation.R"
    ),
    echo = TRUE,
    max.deparse.length = 1000
  )
}





# Data Visualisation - Time Series Plot ----------------------------------------
scale_factor <- 0.0000002

data_viz_ivi_abs <- data_viz_ivi_australia |>
  left_join(data_viz_abs_unemployment_rate, join_by(date))

ggplot_data_viz_ivi_australia <- data_viz_ivi_abs |>

  ggplot(aes(
    x = date,
    y = job_advertisements
  )) +

  # IVI Job Advertisements
  geom_area_pattern(
    pattern       = "gradient",
    pattern_fill  = "#C5A9ED",
    pattern_fill2 = "#6929C4"
  ) +
  geom_line(
    colour = "#6929C4",
    linewidth = 1.2
  ) +

  # Pre-GFC
  geom_segment(
    x         = as_date("2008-04-01"),
    y         = 0,
    xend      = as_date("2008-04-01"),
    yend      = 320000,
    linetype  = "dashed",
    colour    = "red",
    linewidth = 0.5,
    alpha     = 0.75
  ) +
  geom_segment(
    x         = as_date("2011-03-01"),
    y         = 0,
    xend      = as_date("2011-03-01"),
    yend      = 280000,
    linetype  = "dashed",
    colour    = "red",
    linewidth = 0.5,
    alpha     = 0.75
  ) +
  geom_segment(
    x         = as_date("2020-04-01"),
    y         = 0,
    xend      = as_date("2020-04-01"),
    yend      = 380000,
    linetype  = "dashed",
    colour    = "red",
    linewidth = 0.5,
    alpha     = 0.75
  ) +

  # ABS Unemployment Rate
  geom_line(
    aes(y = unemployment_rate / scale_factor),
    colour = "#009D9A",
    linewidth = 1.2
  ) +

  annotate(
    "text",
    x = as_date("2008-04-01"),
    y = 360000,
    size = 8/.pt,
    label = "Pre-GFC\npeak recruitment\n(April 2008)"
  ) +
  annotate(
    "text",
    x = as_date("2011-03-01"),
    y = 320000,
    size = 8/.pt,
    label = "Mining boom\npeak recruitment\n(March 2011)"
  ) +
  annotate(
    "text",
    x = as_date("2018-04-01"),
    y = 360000,
    size = 8/.pt,
    label = "COVID-19 downturn\n(April 2020)"
  ) +

  scale_x_date(
    # limits      = c(
    #   first(data_viz_ivi_australia$date),
    #   measurement_date_ymd
    #   ),
    date_labels = "%Y",
    date_breaks = "1 year",
    expand = c(0.01, 80),
    # guide = guide_axis(n.dodge = 2)
    ) +
  scale_y_continuous(
    labels = comma,
    limits = c(0, 400000),
    expand = c(0, 2500),
    sec.axis = sec_axis(
      name = "Unemployment rate",
      ~ . * scale_factor,
      labels = percent,
      )
  ) +

  labs(
    x = NULL,
    y = "Job advertisements"
  ) +

  theme_jsa_ivi_job_ads_unemployment_rate()
